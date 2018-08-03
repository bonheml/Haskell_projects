data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday |
               Saturday deriving (Enum, Show)

data Month   = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov |
               Dec deriving (Enum, Read)

type Date    = (Int, Month, Int)

leap :: Int -> Bool

leap x = (x `mod` 4 == 0 && x `mod` 100 /= 0) ||
          (x `mod` 100 == 0 && x `mod` 400 == 0)

mLength :: Int -> Int -> Int
mLength y m
     | m == 2 && leap y       = 29
     | m == 2 && not (leap y) = 28
     | otherwise              = 30 + (m + (m `div` 8)) `mod` 2

mLengths :: Int -> [Int]
mLengths x = [mLength x m | m <- [1..12]]

daysOfYear :: Date -> Int
daysOfYear (d, m, y)
    | fromEnum m > 0 = d + sum [mLength y cm | cm <- [1 .. fromEnum m]]
    | otherwise      = d

numDays :: Date -> Int
numDays (d, m, y)
    | d > 0               = daysOfYear (d, m, y) + numDays (0, m, y - 1)
    | d == 0 && y >= 1753 = sum (mLengths y) + numDays (0, m, y - 1)
    | otherwise           = 0

dayOfWeek :: Date -> Weekday
dayOfWeek (d, m, y) = (toEnum (numDays (d, m, y) `mod` 7)) :: Weekday