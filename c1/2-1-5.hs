
data Date = Date Year Month Day deriving (Show)

data Month  =
    Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Eq, Ord, Show, Enum, Bounded)

type Year   = Integer
type Day    = Integer
type Days   = Integer

isLeapYear :: Year -> Bool
isLeapYear y = y `mod` 4 == 0

validDate :: Date -> Bool
validDate (Date y m d)
    | 1 <= d && d <= daysInMonth m y = True
    | otherwise                      = False

-- Computer the days in a month for any year
daysInMonth :: Month -> Year -> Days
daysInMonth m y = case m of Jan -> 31
                            Feb -> if isLeapYear y then 29 else 28
                            Mar -> 31
                            Apr -> 30
                            May -> 31
                            Jun -> 30
                            Jul -> 31
                            Aug -> 31
                            Sep -> 30
                            Oct -> 31
                            Nov -> 30
                            Dec -> 31

