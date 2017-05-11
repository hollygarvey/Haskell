import System.Environment
import Control.Monad

addNums :: [String] -> Int
addNums s = foldr (+) 0 $ map (\x -> read x :: Int) s

listIt xs | (' ' `elem` xs)==False = [xs]
          | otherwise = (takeWhile (/=' ') xs):(listIt $ tail $ dropWhile (/=' ') xs)


main :: IO ()
main = do 
  args <- getArgs
  let sum = addNums args
  putStrLn $ "Your sum is: " ++ (show sum)
  putStr "Enter a filename: "
  name <- getLine
  file <- readFile name
  let lined = lines file
  let words = listIt file
  putStrLn $ "There are " ++ (show $ length file) ++ " characters, " ++ (show $ length lined) ++ " lines lines, and "++ (show $ (length words)+(length lined)-1) ++" words in your file"


data Either1 a b = Left1 a| Right1 b 

instance Functor (Either1 a) where 
   fmap f (Left1 x) = Left1 x
   fmap f (Right1 y) = Right1 (f y)

instance Applicative (Either1 a) where 
  pure x = Right1 x
  (Left1 f) <*> _ = Left1 f
  (Right1 f) <*> x = fmap f x

instance Monad (Either1 a) where
  return x = Right1 x
  Left1 x >>= _ = Left1 x
  Right1 x >>= f = f x