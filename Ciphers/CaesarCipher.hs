module Ciphers.CaesarCipher where

import Data.List (elemIndex)
import Data.Maybe (maybe)

encrypt :: (String, Int) ->  String -> String
encrypt (alphabet, shift) plaintext = [
        -- for each letter in the plaintext find its index in the alphabet and change it to (index + shift) % length alphabet, then map this letter to the letter in alphabet with modified index
        -- if the letter is not present in the alphabet, leave it as it is
        maybe letter
        (\index -> alphabet !! ((index + shift) `mod` length alphabet))
        (letter `elemIndex` alphabet) | letter <- plaintext]


decrypt :: (String, Int) -> String -> String
-- same as encrypt, but the shift is done in the opposite direction
decrypt (alphabet, shift) = encrypt (alphabet, -shift)