import Data.Char (isDigit)

data Date = Date Int Int Int

instance Show Date where
  show (Date d m y) = 
    let dayStr    = if d < 10 then "0" ++ (show d) else (show d)
        monthStr  = if m < 10 then "0" ++ (show m) else (show m)
    in  dayStr ++ "." ++ monthStr ++ "." ++ (show y) 

instance Eq Date where
  (==) (Date d1 m1 y1) (Date d2 m2 y2) = (d1 == d2) && (m1 == m2) && (y1 == y2)

instance Ord Date where
  compare (Date d1 m1 y1) (Date d2 m2 y2)
    | y1 /= y2        = compare y1 y2
    | m1 /= m2        = compare m1 m2
    | otherwise       = compare d1 d2



instance Read Date where
  readsPrec _ input = 
    let (day, (dot : str)) = span isDigit input
        (month, (dot2 : year)) = span isDigit str
    in  [(Date (read day :: Int) (read month :: Int) (read year :: Int), "")]

daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dayInYearHelper :: Date -> [Int] -> Int -> Int
dayInYearHelper (Date day 0 _) _ res = res + day
dayInYearHelper (Date day month year) (hDays: tDays) res = dayInYearHelper (Date day (pred month) year) tDays (res + hDays)

dayInYear (Date day month year) = dayInYearHelper (Date day (pred month)year ) daysInMonth 0

dayInYearToDateHelper days (hDays: tDays) monthNum 
  | days > 365           = dayInYearToDateHelper (days `mod` 365) (hDays: tDays) monthNum
  | days <= hDays        = Date days (succ monthNum) 0
  | otherwise            = dayInYearToDateHelper (days - hDays) tDays (succ monthNum)

dayInYearToDate days = dayInYearToDateHelper days daysInMonth 0

-- прибавление числа к дате 
addDaysToDate :: Date -> Int -> Date
addDaysToDate (Date day month year) days = 
    let oldDayNumber = 365 * year + (dayInYear (Date day month year)) 
        newDayNumber = oldDayNumber + days 
        newYear = newDayNumber `div` 365
        newDayInYear = newDayNumber `mod` 365
        (Date newDay newMonth y ) = dayInYearToDate newDayNumber 
    in Date newDay newMonth newYear 

-- вычитание дат
datesDifference :: Date -> Date -> Int
datesDifference (Date d1 m1 y1) (Date d2 m2 y2) = (365 * y2) + (dayInYear (Date d2 m2 y2)) - (365 * y1) - (dayInYear (Date d1 m1 y1))



