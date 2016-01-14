module Handler.FizzBuzz where

import Import

fizzbuzz :: [(String,Int)] -> Int -> String
fizzbuzz fb i = if i `gcd` (product $ map snd fb) == 1 then show i else concatMap (\(t,m) -> if i `mod` m == 0 then t else "") fb

getFizzBuzzR :: Int -> Handler Html
getFizzBuzzR limit = 
    let
      fb = zip ["Fizz","Buzz"] [3,5]
    in defaultLayout $ do
        setTitle "FizzBuzz!"
        let workset = [1..(min 10000 limit)] in $(widgetFile "fizzbuzz")

