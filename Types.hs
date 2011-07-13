module Types where 

data Record = Record {recordId :: Int, owner :: String, content :: String }
  deriving (Eq, Show, Read)

data Time = Time {h::Int, m::Int, s::Int}
	deriving Show
		
data Date = Date {day::Int, month::Int, year::Int}
	deriving Show
