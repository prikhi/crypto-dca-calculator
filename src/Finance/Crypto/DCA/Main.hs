{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{- | CLI application harness.

-}
module Finance.Crypto.DCA.Main
    ( run
    , getArgs
    , Args(..)
    ) where

import           Data.Fixed                     ( Pico )
import           Data.Version                   ( showVersion )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , argPos
                                                , cmdArgs
                                                , def
                                                , explicit
                                                , help
                                                , helpArg
                                                , name
                                                , program
                                                , summary
                                                , typ
                                                )

import           Finance.Crypto.DCA.Calculate
import           Finance.Crypto.DCA.Render
import           Paths_crypto_dca_calculator    ( version )



-- | Run the executable.
run :: Args -> IO ()
run (cleanArgs -> CleanedArgs{..}) = do
    let ladder = calculateLadderBuys caPercentPerStep
                                     caSteps
                                     caCurrentPrice
                                     caTotalSpend
                                     caFeePercent
    putStrLn $ renderTable caFlipColumns ladder


data CleanedArgs = CleanedArgs
    { caPercentPerStep :: Pico
    , caSteps          :: Integer
    , caCurrentPrice   :: Pico
    , caTotalSpend     :: Pico
    , caFeePercent     :: Pico
    , caFlipColumns    :: Bool
    }
    deriving (Show, Read, Eq)

cleanArgs :: Args -> CleanedArgs
cleanArgs Args {..} = CleanedArgs
    { caPercentPerStep = realToFrac argPercentPerStep
    , caSteps          = argSteps
    , caCurrentPrice   = realToFrac argCurrentPrice
    , caTotalSpend     = realToFrac argTotalSpend
    , caFeePercent     = realToFrac argFeePercent
    , caFlipColumns    = argFlipColumns
    }


-- | CLI arguments supported by the executable.
data Args = Args
    { argPercentPerStep :: Double
    , argSteps          :: Integer
    , argCurrentPrice   :: Double
    , argTotalSpend     :: Double
    , argFeePercent     :: Double
    , argFlipColumns    :: Bool
    }
    deriving (Show, Read, Eq, Data, Typeable)


-- | Parse the CLI arguments with 'System.Console.CmdArgs'.
getArgs :: IO Args
getArgs = cmdArgs argSpec


-- | Defines & documents the CLI arguments.
argSpec :: Args
argSpec =
    Args
            { argPercentPerStep = def &= argPos 0 &= typ "PERCENT"
            , argSteps          = def &= argPos 1 &= typ "STEPS"
            , argCurrentPrice   = def &= argPos 2 &= typ "PRICE"
            , argTotalSpend     = def &= argPos 3 &= typ "TOTAL_SPEND"
            , argFeePercent     = 0
                                  &= explicit
                                  &= name "f"
                                  &= name "fee-percent"
                                  &= typ "PERCENT"
                                  &= help "Account for the given exchange fee."
            , argFlipColumns    =
                False &= explicit &= name "c" &= name "flip-columns" &= help
                    "Flip the order of the Amount & Price columns."
            }
        &= summary
               (  "crypto-dca-calculator v"
               <> showVersion version
               <> ", Pavan Rikhi 2021"
               )
        &= program "crypto-dca-calculator"
        &= helpArg [name "h"]
        &= help "Short, one-line summary of project here."
