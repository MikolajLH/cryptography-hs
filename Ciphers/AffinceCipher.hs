module Ciphers.AffineCipher where

import Data.List (elemIndex)
import Data.Maybe (maybe)

egcd :: Integral a => a -> a -> (a, a, a)
egcd a 0 = (a, 1, 0)
egcd a b = 
    let (g, x1, y1) = egcd b (a `mod` b)
        x = y1
        y = x1 - (a `div` b) * y1
   in (g, x, y)

modularInverse :: Integral a => a -> a -> Maybe a
modularInverse m a = let (g, x, y) = egcd m a in if g == 1 then Just y else Nothing

encrypt :: (String, Int, Int) -> String -> String
encrypt (alphabet, keyA, keyB) plaintext
    | let (g, _, _) = egcd keyA (length alphabet) in g /= 1 = error "gcd(keyA, length alphabet) /= 1"
    | otherwise = [
        maybe letter
        (\index -> alphabet !! enc (m, keyA, keyB) index)
        (letter `elemIndex` alphabet) | letter <- plaintext ]
    where
        enc (m, a, b) x = (a * x + b) `mod` m
        m = length alphabet

decrypt :: (String, Int, Int) -> String -> String
decrypt (alphabet, keyA, keyB) ciphertext = 
    case invA of
        Nothing -> error "gcd(keyA, length alphabet) /= 1"
        Just a -> [
            maybe letter
            (\index -> alphabet !! dec (m, a, keyB) index)
            (letter `elemIndex` alphabet) | letter <- ciphertext ]
    where
        m = length alphabet
        invA = modularInverse m keyA
        dec (m, a, b) y = (a * (y - b)) `mod` m