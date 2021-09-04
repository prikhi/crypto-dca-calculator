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
                                                , explicit
                                                , help
                                                , helpArg
                                                , name
                                                , program
                                                , summary
                                                , typ
                                                )
import           Text.Tabular                   ( Header(..)
                                                , Properties(..)
                                                , Table(..)
                                                )
import           Text.Tabular.AsciiArt          ( render )

import           Paths_crypto_dca_calculator    ( version )



-- | Run the executable.
run :: Args -> IO ()
run (cleanArgs -> args@CleanedArgs{..}) = do
    let percents = map ((* caPercentPerStep) . fromInteger) [1 .. caSteps]
        prices =
            map (\percent -> caCurrentPrice * (1 - (percent / 100))) percents
        spendPerStep     = caTotalSpend / fromInteger caSteps
        amountsAndPrices = map (\p -> (spendPerStep / p, p)) prices
        totalBought      = sum $ map fst amountsAndPrices
        avgBuy           = caTotalSpend / totalBought

    putStrLn $ renderTable args percents amountsAndPrices totalBought avgBuy

renderTable
    :: CleanedArgs -> [Pico] -> [(Pico, Pico)] -> Pico -> Pico -> String
renderTable CleanedArgs { caFlipColumns } percents amountsAndPrices totalBought avgBuy
    = let
          percentHeaders = map
              ( Header
              . (<> "%")
              . formatScientific Fixed (Just 2)
              . realToFrac
              . negate
              )
              percents
          rowHeaders = Group
              DoubleLine
              [ Group NoLine percentHeaders
              , Group NoLine $ flipRow [Header "Total", Header "Avg"]
              ]
          columnHeaders =
              Group SingleLine $ flipRow [Header "Amount", Header "Price"]

          tableData =
              map (\(a, p) -> flipRow [renderAmount a, renderPrice p])
                  amountsAndPrices
                  <> flipRow
                         [ flipRow [renderAmount totalBought, ""]
                         , flipRow ["", renderPrice avgBuy]
                         ]

          table = Table rowHeaders columnHeaders tableData
      in
          render id id id table
  where
    flipRow      = if caFlipColumns then reverse else id
    renderAmount = formatScientific Fixed (Just 8) . realToFrac
    renderPrice  = formatScientific Fixed (Just 2) . realToFrac


data CleanedArgs = CleanedArgs
    { caPercentPerStep :: Pico
    , caSteps          :: Integer
    , caCurrentPrice   :: Pico
    , caTotalSpend     :: Pico
    , caFlipColumns    :: Bool
    }
    deriving (Show, Read, Eq)

cleanArgs :: Args -> CleanedArgs
cleanArgs Args {..} = CleanedArgs
    { caPercentPerStep = realToFrac argPercentPerStep
    , caSteps          = argSteps
    , caCurrentPrice   = realToFrac argCurrentPrice
    , caTotalSpend     = realToFrac argTotalSpend
    , caFlipColumns    = argFlipColumns
    }


-- | CLI arguments supported by the executable.
data Args = Args
    { argPercentPerStep :: Double
    , argSteps          :: Integer
    , argCurrentPrice   :: Double
    , argTotalSpend     :: Double
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
            , argFlipColumns    =
                False &= explicit &= name "f" &= name "flip-columns" &= help
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
