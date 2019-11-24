-- Name: Pham Hoang Minh
-- Student number: s1024376

module Mastermind
where
import System.Random
import Data.Char

dice :: Int -> IO Int
dice x = getStdRandom (randomR (0,x-1))

--roll :: IO Int
--roll  =  do  a <- dice
--             b <- dice
--             return (a + b)

-- Input-related functions
isNumber' :: String -> Bool -- to check if a String is a number
isNumber' [] = True
isNumber' (x:xs) | isDigit x = isNumber' xs
                 | otherwise = False

getNumber :: String -> IO Int -- get input as a number
getNumber s = do putStr s
                 x <- getLine
                 if (isNumber' x) then
                    do return (read x :: Int)
                 else
                    do putStrLn "Error, not a number"
                       getNumber s

getColor :: Int -> IO String --get color input
getColor n = do putStr "Color "
                putStr (show n)
                x <- getLine
                return x

getSource :: Int -> Int -> IO Source --Ask the user to input the colors
getSource len num = do if len == 0 then return []
                       else do x <- getColor num
                               xs <- getSource (len-1) (num+1)
                               return (x:xs)

-- Data
type Code = [String] -- code to crack
type Source = [String] -- colors

-- game mechanics
createCode :: Int -> Int -> Source -> IO Code --create a random code with the given source
createCode len col source = do if len == 0 then return []
                               else do value <- dice col
                                       x <- return(source !! value)
                                       xs <- createCode (len - 1) col source
                                       return (x:xs)

isEqual :: Code -> [String] -> Bool --check if the answer input is correct or not
isEqual [] [] = True
isEqual s [] = False
isEqual (x:xs) (y:ys) | x == y = isEqual xs ys
                      | otherwise = False
                  
countCorrect :: Code -> [String] -> Int --count the positions with the correct color
countCorrect _ [] = 0
countCorrect (x:xs) (y:ys) | x == y = (1 + countCorrect xs ys)
                           | otherwise = countCorrect xs ys

-- play ground
play :: Code -> Int -> IO() --game mechanics implementation
play code times = do if times == 0 then
                        do putStrLn "you failed!"
                           putStrLn "The answer is: "
                           print code
                     else
                       do putStr "Guess? : "
                          guess <- getLine
                          if (isEqual code (words(guess))) then
                              putStrLn "Congrats!"
                          else do correct <- return (countCorrect code (words(guess)))
                                  putStrLn "Wrong ----- "
                                  putStr "Hint: the number of correct colors in the right positions: "
                                  putStr (show correct)
                                  putStr " - Mismatch positions: "
                                  print (length code - correct)
                                  play code (times - 1) 

--Starting the game
makeSource :: Int -> IO Source
makeSource y = do if (y > 9) then getSource y 1
                  else return ["red","blue","green","white","yellow","mint","pink","orange","violet"]

mastermind :: IO() --main function, use this to start the game
mastermind = do manual
                x <- getNumber "Please enter the desired code length: " 
                y <- getNumber "Please enter the number of colors: "
                z <- getNumber "Please enter the number of tries: "
                putStrLn "Please remember that if you enter a longer/shorter input than the code length, it will fail!"
                source <- makeSource y
                code <- createCode x y source
                --print code --testing the game
                putChar '\n' 
                play code z


-- manual
manual :: IO()
manual = do putStrLn "Welcome to the mastermind game."
            putStrLn "If you play with 9 or less colors, then the colors will be the following: "
            putStrLn "1.|red| - 2.|blue| - 3.|green| - 4.|white|"
            putStrLn "5.|yellow| - 6.|mint| - 7.|pink|"
            putStrLn "8.|orange| - 9.|violet| \n"
            putStrLn "For example: if the number of colors is 4, then the code will be consisted of (red, blue, green, white)"
            putStrLn "- Example input: \"red blue blue white\" (separated by spaces) "
            putStrLn "If you choose more than 9 colors, then you have to input your own colors. \n"
