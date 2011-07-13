module Main where

import XMPP
import Network
import Data.Time.Clock
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad(when)
import Control.Exception
import Control.Concurrent
import System.Posix
import System.Posix.IO
import System.Time

import Calc(calc)
import DB
import Types
import Notifiers

-- The bot's JID is "bot@example.com"
--botUsername="notes"
--botPassword = "qwe987"

botServer = "draugr.de"

botUsername = "dbg"
botPassword = "qwe876"


main :: IO ()
main = withSocketsDo $	do
	conn <- connect "note.db"
	c <- openStream botServer
	getStreamStart c	
	forkIO (runXMPP c $ checkNotifier conn)
	print "Hmm"
	runXMPP c $ do
		startAuth botUsername botServer botPassword
		sendPresence		
		addHandler (isChat `conj` hasBody) (\x -> response conn (maybe "" id $ getAttr "from" x) (maybe "" id $ getMessageBody x)) True
		addHandler (isPresence) responsePresence True						

checkNotifier :: IConnection c => c -> XMPP ()
checkNotifier conn = do 	
	t <- liftIO $ (getClockTime >>= (\(TOD sec _) -> return sec))
	xxx <- liftIO $ listActiveNotifiers conn t 	
	responseNotifiers conn xxx
	liftIO $ sleep 5
	checkNotifier conn

responseNotifiers :: IConnection c => c -> [(Int, String, Integer, String)] -> XMPP ()
responseNotifiers conn [] = return ()
responseNotifiers conn ((id, owner, time, content):xs) = do
	sendMessage owner content
	liftIO $ print (owner,content)
	liftIO $ setNotifierSended conn id
	responseNotifiers conn xs
	return ()	

responsePresence :: XMLElem -> XMPP ()
responsePresence x
	| getAttr "type" x == Just "subscribe" = sendStanza (CData ("<presence to='" ++ (maybe "" id $ getAttr "from" x) ++ "' type='subscribed'/>")) 
	| otherwise = return ()


showRecordList :: [Record] -> String
showRecordList (x:xs) = ">> " ++ show (recordId x)++ content x ++ "\n" ++ showRecordList xs
showRecordList [] = []

showNotifierList :: [(Int, String, Integer, String)] -> String
showNotifierList (x:xs) = ">> " ++ show x ++ "\n" ++ showNotifierList xs
showNotifierList [] = []

noSpace :: String -> String
noSpace (x:xs) = if x == ' ' then noSpace xs else (x:xs)
noSpace [] = []

removeEscapes :: String -> String 
removeEscapes [] = []
removeEscapes (x:xs)
	| x == '&' = "&amp;" ++ removeEscapes xs
	| x == '<' = "&lt;" ++ removeEscapes xs
	| x == '>' = "&gt;" ++ removeEscapes xs
	| otherwise = x:(removeEscapes xs)

response :: Connection -> String -> String -> XMPP ()
response conn sender "" = sendMessage sender "Something wrong"
response conn sender (' ':xs) = response conn sender xs
response conn sender ".hi" = sendMessage sender "Hi!"
response conn sender ('.':'c':'a':'l':'c':xs) = sendMessage sender $ show $ calc xs
response conn sender ('.':'a':'d':'d':'N':'o':'t':'i':'f':'i':'e':'r':xs) = do 
	let (time, content) = parseNotifier xs [] 0 in (liftIO $ addNotifier conn (getBareJid sender) time content)
	sendMessage sender "Hmm, maybe added!"
response conn sender ".ls" = do
	xxx <- liftIO $ listNotes conn $ getBareJid sender
	sendMessage sender $ removeEscapes $ showRecordList xxx	
response conn sender ".lsN"= do
	xxx <- liftIO $ listOwnerNotifiers conn $ getBareJid sender
	sendMessage sender $ removeEscapes $ showNotifierList xxx
response conn sender ('.':'s':'e':'a':'r':'c':'h':xs)=do
	xxx <- liftIO $ listNotesLike conn (getBareJid sender) (noSpace xs)
	sendMessage sender $ showRecordList xxx
response conn sender ".time"= do
	xxx <- liftIO getClockTime
	sendMessage sender $ show xxx 
response conn sender ('.':'d':'e':'l':xs) = do
	xxx <- liftIO $ removeNote conn (getBareJid sender) $ noSpace xs
	if xxx == 0 then 
		sendMessage sender "Error! No notes deleted" else
		sendMessage sender $ "Ok, " ++ show xxx ++ " message deleted"
response conn sender ('.':'a':'d':'d':' ':xs) = do 
	xxx <- liftIO $ addNote conn (Record {recordId=0,owner=getBareJid sender, content = xs}) 
	liftIO $ commit conn
	if xxx > 0 then 
		sendMessage sender "Added!" else
		sendMessage sender "Sheet, nothing added!"
response conn sender s = sendMessage sender "Hi! Its useful note-bot. \n\
                                            \Use .add for add note \n.ls for list all your notes \n.del for deleting note by id from .list \n\
					    \.time for time \n.search for pattern-searching, %some% for example\n.calc - phenominal calculator \n\
					    \.addNotifier {31.04.2011} {04:59} for add notifier \n.lsN for list all active notifiers \n     Enjoy!" 

