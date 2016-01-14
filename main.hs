----------------------------------------------------
-- Функция, которая проверяет все три свойства из --
-- http://festival.1september.ru/articles/600309/ --
----------------------------------------------------
check :: Int -> Bool
check x | (not $ elem (last n) "2378") &&
          (checkZeros n) &&
          (x `mod` 4 == 0 || x `mod` 8 == 1) &&
          (x `mod` 9 == 0 || x `mod` 3 == 1) = True
        | True = False
  where
    n = show x
    checkZeros n = 0 == (length $
                         takeWhile (=='0') $
                         reverse n) `mod` 2

-------------------------------------------------------------------
-- http://stackoverflow.com/questions/4333459/is-it-square-check --
-------------------------------------------------------------------
isSquare :: Int -> Bool
isSquare x =
  let x' = truncate $ sqrt (fromIntegral x :: Double)
  in  x' * x' == x

main = putStrLn $
  show $
  length $
  takeWhile (==True) $
  map (\(a,b) -> a == b) $
  zip checkSquare checkList
  where checkList = map check list
        checkSquare = map isSquare list
        list   = [1..1000]
