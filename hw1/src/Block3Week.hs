module Block3Week
  ( Day
  , dayToInt
  , intToDay
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  ) where

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

week :: [Day]
week =
  [ Monday
  , Tuesday
  , Wednesday
  , Thursday
  , Friday
  , Saturday
  , Sunday
  ]

dayToInt :: Day -> Int
dayToInt d = case d of
  Monday    -> 0
  Tuesday   -> 1
  Wednesday -> 2
  Thursday  -> 3
  Friday    -> 4
  Saturday  -> 5
  Sunday    -> 6

intToDay :: Int -> Day
intToDay i = week !! i

nextDay :: Day -> Day
nextDay d = let a = dayToInt d in intToDay((a+1) `mod` 7)

afterDays :: Int -> Day -> Day
afterDays i d = let a = dayToInt d in  intToDay((a + i) `mod` 7)

isWeekend :: Day -> Bool
isWeekend d = let a = dayToInt d in (a == 6) || (a == 7)

daysToParty :: Day -> Int
daysToParty d | a > 4 = 11 - a
              | otherwise = 4 - a
  where
    a = dayToInt d
