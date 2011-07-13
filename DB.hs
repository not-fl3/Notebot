module DB where 

import Database.HDBC
import Database.HDBC.Sqlite3
import Types
import Control.Monad(when)

connect :: String -> IO Connection
connect fp = do
	conn <- connectSqlite3 fp
	prepDB conn
	return conn

prepDB :: IConnection conn => conn -> IO ()
prepDB conn = do
	tables <- getTables conn
	when (not ("notes" `elem` tables)) $
		do 
			Database.HDBC.run conn "CREATE TABLE notes (\
								\recordId INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
								\owner TEXT NOT NULL,\
								\content TEXT NOT NULL)" []
			return ()
	when (not ("notifiers" `elem` tables)) $	
		do 
			Database.HDBC.run conn "CREATE TABLE notifiers (\
									\notifierId INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
									\owner TEXT NOT NULL,\
									\unixTime INEGER NOT NULL,\
									\content TEXT NOT NULL,\
									\sended BOOL NOT NULL)" []
			return ()
	commit conn

addNote :: IConnection conn => conn -> Record -> IO Integer
addNote dbh record = do
  xxx <- Database.HDBC.run dbh "INSERT OR IGNORE INTO notes (owner, content) VALUES (?, ?)"
                                       [toSql (owner record), toSql (content record)]
  return (xxx)

removeNote :: IConnection conn => conn -> String -> String -> IO Integer
removeNote dbh owner sid = do
  xxx <- Database.HDBC.run dbh "DELETE FROM notes WHERE owner=? AND recordId = ?" [toSql owner, toSql sid]
  return (xxx)

setNotifierSended :: IConnection conn => conn -> Int -> IO ()
setNotifierSended conn id = do
	Database.HDBC.run conn "UPDATE notifiers SET sended=1 WHERE notifierId = ?" [toSql id]
	return ()
	
addNotifier :: IConnection c => c -> String -> Integer -> String -> IO ()
addNotifier dbh owner t s = do
	Database.HDBC.run dbh "INSERT OR IGNORE INTO notifiers (owner, unixTime, content,sended) VALUES (?, ?, ?, 0)" [toSql owner, toSql t, toSql s]
	return ()

listActiveNotifiers :: IConnection c => c -> Integer -> IO [(Int, String, Integer, String)]
listActiveNotifiers conn t = do
	res <- quickQuery' conn "SELECT * FROM notifiers WHERE sended=0 AND unixTime < ?" [toSql t]
	return (map fromSql' res) where 
		fromSql' [id, owner, time, content, sended] = (fromSql id, fromSql owner, fromSql time, fromSql content)

listOwnerNotifiers :: IConnection c => c -> String -> IO [(Int, String, Integer, String)]
listOwnerNotifiers conn s = do
	res <- quickQuery' conn "SELECT * FROM notifiers WHERE sended=0 AND owner = ?" [toSql s]
	return (map fromSql' res) where 
		fromSql' [id, owner, time, content, sended] = (fromSql id, fromSql owner, fromSql time, fromSql content)

listNotes :: IConnection conn => conn ->String -> IO [Record]
listNotes conn owner = do 
  res <- quickQuery' conn "SELECT * FROM notes WHERE owner=?" [toSql owner]
  return (map convRecordRow res)

listNotesLike :: IConnection conn => conn -> String -> String -> IO [Record]
listNotesLike conn owner like = do 
  res <- quickQuery' conn "SELECT * FROM notes WHERE owner=? AND content LIKE ?" [toSql owner, toSql like]
  return (map convRecordRow res)

convRecordRow :: [SqlValue] -> Record
convRecordRow [recordId, owner, content] =
  Record {recordId = fromSql recordId, owner = fromSql owner, content = fromSql content}
convRecordRow x = error $ "Can't convert record row " ++ show x

