module Notifiers where 
import Types
import System.Time
 
data Notifier = Notifier {notifierId :: Int, notifierOwner :: String, notifierTime :: Integer, notifierContent :: String }
  deriving (Eq, Show, Read)

-- Find {%s} and returns founded string and rest string
parseBrackets :: String -> String -> (String, String)
parseBrackets ('}':xs) rest = (reverse rest, xs)
parseBrackets ('{':xs) rest = parseBrackets xs rest
parseBrackets str@(x:xs) rest = parseBrackets xs $ x:rest
parseBrackets [] str = ([], str)

timeToUnix :: (Maybe Time, Maybe Date) -> Integer
timeToUnix (Just t, Just d) = (\(TOD sec _) -> sec ) (toClockTime CalendarTime {ctYear = year d, ctMonth = toEnum.(+(-1)) $ month d, ctDay = day d, ctHour = h t - 4, ctMin = m t, ctSec = s t, ctPicosec = 804267000000, ctWDay = Monday, ctYDay = 230, ctTZName = "MSD", ctTZ = 0, ctIsDST = True})
timeToUnix (Nothing, Just d) = 0
timeToUnix (Just t, Nothing) = 0
timeToUnix (Nothing, Nothing) = 0

parseNotifier :: String -> String -> Int -> (Integer,String)
parseNotifier s date 0 = let (x, y) = parseBrackets s [] in parseNotifier y x 1 
parseNotifier s date 1 = let (x, y) = parseBrackets s [] in (timeToUnix (parseTime x, parseDate date), y)

parseDate :: String -> Maybe Date
parseDate s = toDate (words $ map (\x -> if x == '.' then ' ' else x) s)
		where 
			toDate :: [String] -> Maybe Date
			toDate (x:y:z:_) = Just Date {day = read x, month = read y, year = read z}
			toDate xs = Nothing

parseTime :: String -> Maybe Time
parseTime s = toTime (words $ map (\x -> if x == ':' then ' ' else x) s)
		where 
			toTime :: [String] -> Maybe Time
			toTime (x:y:z:_) = Just Time {h = read x, m = read y, s = read z}
			toTime (x:y:_) = Just Time {h = read x, m = read y, s = 0}			
			toTime xs = Nothing

