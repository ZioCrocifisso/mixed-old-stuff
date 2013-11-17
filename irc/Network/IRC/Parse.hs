module Network.IRC.Parse (
	parse
) where

import Control.Applicative hiding ((<|>), many)

import Network.IRC.Message

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim hiding (parse)
import Text.Parsec.String

parse :: String -> Either ParseError Message
parse = runParser message () "IRC Message"

message :: Parser Message
message = Message
		<$> option Me (try sender)
		<*> command
		<*> many (try shortArgument)
		<*> option Nothing (Just <$> longArgument)

sender :: Parser Sender
sender = do
	char ':'
	s <- try user <|> servername
	char ' '
	return s

servername :: Parser Sender
servername = ServerName <$> host

user :: Parser Sender
user = User
	<$> nick
	<*> (char '!' *> username)
	<*> (char '@' *> host)

nick :: Parser String
nick = (:) <$> letter <*> many (letter <|> number <|> oneOf "-[]\\`^{}")

number :: Parser Char
number = oneOf "0123456789"

username :: Parser String
username = many1 $ noneOf " \0\r\n@"

host :: Parser String
host = username

nonWhite :: Parser Char
nonWhite = noneOf " \0\r\n"

command :: Parser String
command = many1 letter <|> (show <$> many1 number)

shortArgument :: Parser String
shortArgument = do
	char ' '
	(:) <$> noneOf ": \0\r\n" <*> many nonWhite

longArgument :: Parser String
longArgument = do
	string " :"
	many (noneOf "\0\r\n")
