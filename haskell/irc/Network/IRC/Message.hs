module Network.IRC.Message (
	Message(..),
	Sender(..)
) where

data Message = Message {
	mSender :: Sender,
	mCommand :: String,
	mArguments :: [String],
	mLongArgument :: Maybe String
} deriving (Show)

data Sender =
	  Me
	| User {
		mNickname :: String,
		mUsername :: String,
		mHostname :: String
	} | ServerName {
		mServername :: String
	} deriving (Show)
