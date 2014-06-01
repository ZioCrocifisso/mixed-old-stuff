module Network.IRC.Command where

import Data.List (intercalate)

import Network.IRC.Message

raw :: String -> [String] -> Maybe String -> Message
raw = Message Me

raw1s :: String -> String -> Message
raw1s c = flip (raw c) Nothing . (: [])

raw1l :: String -> String -> Message
raw1l c = raw c [] . Just

pass = raw1s "PASS"

nick = raw1s "NICK"

user u h s = raw "USER" [u, h, s] . Just

server name hopc = raw "SERVER" [name, hopc] . Just

oper u p = raw "OPER" [u, p] Nothing

quit = raw1l "QUIT"

squit s = raw "SQUIT" [s] . Just

join cs ks = raw "JOIN" [intercalate "," cs, intercalate "," ks] Nothing

part cs = raw "PART" [intercalate "," cs] Nothing

mode d m a = raw "MODE" (d : m : a) Nothing

topic c = raw "TOPIC" [c] . Just

names = raw1s "NAMES" . intercalate ","

list = raw1s "LIST" . intercalate ","

invite n c = raw "INVITE" [n, c] Nothing

kick c u = raw "KICK" [c, u] . Just

version = raw1s "VERSION"

stats q s = raw "STATS" [q, s] Nothing

links r m = raw "LINKS" [r, m] Nothing

time = raw1s "TIME"

connect t p r = raw "CONNECT" [t, p, r] Nothing

trace = raw1s "TRACE"

admin = raw1s "ADMIN"

info = raw1s "INFO"

msg t = raw "PRIVMSG" [t] . Just

notice t = raw "NOTICE" [t]

who n o = raw "WHO" (n : ["o" | o]) Nothing

whois ns = raw1s "WHO" . intercalate ","

whowas n c = raw "WHOWAS" [n, c] Nothing

kill n = raw "KILL" [n]

ping s = raw "PING" [s] Nothing

pong s = raw "PONG" [s] Nothing

error = raw1l "ERROR"

away = raw1l "AWAY"

rehash = raw "REHASH" [] Nothing

restart = raw "RESTART" [] Nothing

summon u s = raw "SUMMON" [u, s] Nothing

users s = raw "SUMMON" [s] Nothing
