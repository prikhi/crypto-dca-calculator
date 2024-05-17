{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

-- | CLI application harness.
module Finance.Crypto.DCA.Main
    ( run
    , getArgs
    , Args (..)
    ) where

import Data.Fixed (Pico)
import Data.Version (showVersion)
import System.Console.CmdArgs
    ( Data
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
    , (&=)
    )

import Finance.Crypto.DCA.Calculate
import Finance.Crypto.DCA.Render
import Paths_crypto_dca_calculator (version)


-- | Run the executable.
run :: Args -> IO ()
run (cleanArgs -> CleanedArgs {..}) = do
    let ladder =
            calculateLadderBuys
                LadderConfig
                    { lcPercentPerStep = caPercentPerStep
                    , lcStepCount = caSteps
                    , lcCurrentPrice = caCurrentPrice
                    , lcTotalToSpend = caTotalSpend
                    , lcFeePercentage = caFeePercent
                    , lcAmountPrecision = caAmountPrecision
                    , lcPricePrecision = caPricePrecision
                    }
    putStrLn $
        renderTable
            caFlipColumns
            caPricePrecision
            caAmountPrecision
            ladder


data CleanedArgs = CleanedArgs
    { caPercentPerStep :: Pico
    , caSteps :: Integer
    , caCurrentPrice :: Pico
    , caTotalSpend :: Pico
    , caFeePercent :: Pico
    , caFlipColumns :: Bool
    , caPricePrecision :: Integer
    , caAmountPrecision :: Integer
    }
    deriving (Show, Read, Eq)


cleanArgs :: Args -> CleanedArgs
cleanArgs Args {..} =
    CleanedArgs
        { caPercentPerStep = realToFrac argPercentPerStep
        , caSteps = argSteps
        , caCurrentPrice = realToFrac argCurrentPrice
        , caTotalSpend = realToFrac argTotalSpend
        , caFeePercent = realToFrac argFeePercent
        , caFlipColumns = argFlipColumns
        , caPricePrecision = argPricePrecision
        , caAmountPrecision = argAmountPrecision
        }


-- | CLI arguments supported by the executable.
data Args = Args
    { argPercentPerStep :: Double
    , argSteps :: Integer
    , argCurrentPrice :: Double
    , argTotalSpend :: Double
    , argFeePercent :: Double
    , argFlipColumns :: Bool
    , argPricePrecision :: Integer
    , argAmountPrecision :: Integer
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
        , argSteps = def &= argPos 1 &= typ "STEPS"
        , argCurrentPrice = def &= argPos 2 &= typ "PRICE"
        , argTotalSpend = def &= argPos 3 &= typ "TOTAL_SPEND"
        , argFeePercent =
            0
                &= explicit
                &= name "f"
                &= name "fee-percent"
                &= typ "PERCENT"
                &= help "Account for the given exchange fee."
        , argFlipColumns =
            False
                &= explicit
                &= name "c"
                &= name "flip-columns"
                &= help
                    "Flip the order of the Amount & Price columns."
        , argPricePrecision =
            2
                &= explicit
                &= name "p"
                &= name "price-precision"
                &= help
                    "Decimal places in the Price column (default: 2)"
        , argAmountPrecision =
            8
                &= explicit
                &= name "a"
                &= name "amount-precision"
                &= help
                    "Decimal places in the Amount column (default: 8)"
        }
        &= summary
            ( "crypto-dca-calculator v"
                <> showVersion version
                <> ", Pavan Rikhi 2022"
            )
        &= program "crypto-dca-calculator"
        &= helpArg [name "h"]
        &= help "Generate Laddered Buys for Cryptocurrency Purchases."
