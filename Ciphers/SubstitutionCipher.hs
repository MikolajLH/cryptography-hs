module Ciphers.SubstituionCipher where

import Data.List


encrypt :: (String, String) -> String -> String
encrypt (alphabet, permutation) plaintext = [
    maybe letter snd
    (find (\(p, c) -> letter == p) (zip alphabet permutation)) | letter <- plaintext]

decrypt :: (String, String) -> String -> String
decrypt (alphabet, permutation) = encrypt (permutation, alphabet)