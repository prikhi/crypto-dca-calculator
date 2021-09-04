module Main where

import           Finance.Crypto.DCA.Main


main :: IO ()
main = getArgs >>= run
