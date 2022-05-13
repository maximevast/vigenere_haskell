module Utils where

import Data.Char
import Data.List
import Data.Maybe

alphabet :: [Char]
alphabet = ['A'..'Z'] ++ ['0'..'9'] ++ [ '.', ',', '?', '!', '\'', ';']
alphabet_lenght :: Int
alphabet_lenght = length(alphabet)

-- elemIndex come from Data.List and fromJust from Data.Maybe
-- find the index of a given char in the alphabet
index_of :: Char -> Int
index_of a = fromJust $ elemIndex a alphabet

-- find the corresponding char from alphabet at index 'a'
at_index :: Int -> Char
at_index a = alphabet !! a

-- toUpper come from Data.Char
-- convert a list of Char to a list of upper case Char
to_upper_case :: [Char] -> [Char]
to_upper_case = map toUpper

-- recursively removes spaces using Data.List.delete
remove_spaces :: [Char] -> [Char]
remove_spaces xs
  | ' ' `elem` xs = remove_spaces(delete ' ' xs)
  | otherwise     = xs

-- return a new list of char repeating key until the end of plain_text
extend_key :: [Char] -> [Char] -> [Char]
extend_key xs ys = take(length xs) $ concat(repeat ys)

-- return true if xs is a subset of alphabet; false otherwise
is_valid :: [Char] -> Bool
is_valid xs = all (`elem` alphabet) xs
