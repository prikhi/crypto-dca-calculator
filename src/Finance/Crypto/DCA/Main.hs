{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{- | CLI application harness.

-}
module Finance.Crypto.DCA.Main
    ( run
    , getArgs
    , Args(..)
    ) where

import           Control.Monad                  ( forM_ )
import           Data.Fixed                     ( Pico )
import           Data.Scientific                ( FPFormat(Fixed)
                                                , formatScientific
                                                )
import           Data.Version                   ( showVersion )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , argPos
                                                , cmdArgs
                                                , def
                                                , help
                                                , helpArg
                                                , name
                                                , program
                                                , summary
                                                , typ
                                                )

import           Paths_crypto_dca_calculator    ( version )

import qualified Data.List                     as L



-- | Run the executable.
run :: Args -> IO ()
run (cleanArgs -> CleanedArgs{..}) = do
    let percents = map ((* caPercentPerStep) . fromInteger) [1 .. caSteps]
        prices =
            map (\percent -> caCurrentPrice * (1 - (percent / 100))) percents
        spendPerStep     = caTotalSpend / fromInteger caSteps
        amountsAndPrices = map (\p -> (spendPerStep / p, p)) prices
        totalBought      = sum $ map fst amountsAndPrices
        avgBuy           = caTotalSpend / totalBought

    renderHeader ["Amount", "Price"]
    forM_ amountsAndPrices $ \(amt, price) ->
        putStrLn $ renderAmount amt <> "\t" <> renderPrice price
    putStrLn ""

    renderHeader ["Total", "Avg Price"]
    putStrLn $ renderAmount totalBought <> "\t" <> renderPrice avgBuy
  where
    renderAmount = formatScientific Fixed (Just 8) . realToFrac
    renderPrice  = formatScientific Fixed (Just 2) . realToFrac
    renderHeader names = do
        let lengths = map length names
        putStrLn $ L.intercalate "\t\t" names
        putStrLn $ L.intercalate "\t\t" $ map (`replicate` '-') lengths


data CleanedArgs = CleanedArgs
    { caPercentPerStep :: Pico
    , caSteps          :: Integer
    , caCurrentPrice   :: Pico
    , caTotalSpend     :: Pico
    }
    deriving (Show, Read, Eq)

cleanArgs :: Args -> CleanedArgs
cleanArgs Args {..} = CleanedArgs
    { caPercentPerStep = realToFrac argPercentPerStep
    , caSteps          = argSteps
    , caCurrentPrice   = realToFrac argCurrentPrice
    , caTotalSpend     = realToFrac argTotalSpend
    }


-- | CLI arguments supported by the executable.
data Args = Args
    { argPercentPerStep :: Double
    , argSteps          :: Integer
    , argCurrentPrice   :: Double
    , argTotalSpend     :: Double
    }
    deriving (Show, Read, Eq, Data, Typeable)


-- | Parse the CLI arguments with 'System.Console.CmdArgs'.
getArgs :: IO Args
getArgs = cmdArgs argSpec


-- | Defines & documents the CLI arguments.
argSpec :: Args
argSpec =
    Args { argPercentPerStep = def &= argPos 0 &= typ "PERCENT"
         , argSteps          = def &= argPos 1 &= typ "STEPS"
         , argCurrentPrice   = def &= argPos 2 &= typ "PRICE"
         , argTotalSpend     = def &= argPos 3 &= typ "TOTAL_SPEND"
         }
        &= summary
               (  "crypto-dca-calculator v"
               <> showVersion version
               <> ", Pavan Rikhi 2021"
               )
        &= program "crypto-dca-calculator"
        &= helpArg [name "h"]
        &= help "Short, one-line summary of project here."
