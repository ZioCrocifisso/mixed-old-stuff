module Network.IRC.Write (
	write
) where

import Network.IRC.Message

write (Message s c a l) = writeSender s ++ c ++ foldl writeShort "" a ++ writeLong l

writeSender Me = ""
writeSender (User n u h) = ":" ++ n ++ "!" ++ u ++ "@" ++ h ++ " "
writeSender (ServerName s) = ':' : s ++ " "

writeShort a x = a ++ " " ++ x

writeLong (Just x) = " :" ++ x
writeLong Nothing = ""
