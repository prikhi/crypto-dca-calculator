{-# LANGUAGE RecordWildCards #-}

-- | Rendering a series of laddered buys.
module Finance.Crypto.DCA.Render
    ( renderTable
    ) where

import Data.Fixed (Pico)
import Data.Scientific
    ( FPFormat (Fixed)
    , formatScientific
    )
import Text.Tabular
    ( Header (..)
    , Properties (..)
    , Table (..)
    )
import Text.Tabular.AsciiArt (render)

import Finance.Crypto.DCA.Calculate
    ( Ladder (..)
    , Step (..)
    )


-- | Render the buys in a table, with total/average rows afterwards.
renderTable
    :: Bool
    -- ^ flip the default @Amount, Price@ columns to @Price, Amount@.
    -> Integer
    -- ^ decimal places to render the @Price@ column in.
    -> Integer
    -- ^ decimal places to render the @Amount@ column in.
    -> Ladder
    -- ^ the calculated series of laddered buys.
    -> String
renderTable flipColumns pricePrecision amountPrecision Ladder {..} =
    let
        percentHeaders = map (Header . renderPercent . sPercent) lSteps
        totalHeaders =
            if lTotalFee /= 0
                then [Header "Total", Header "Fee", Header "Avg"]
                else [Header "Total", Header "Avg"]
        rowHeaders =
            Group
                DoubleLine
                [Group NoLine percentHeaders, Group NoLine $ flipRow totalHeaders]
        columnHeaders =
            Group SingleLine $ flipRow [Header "Amount", Header "Price"]

        footerRows =
            flipRow $
                map flipRow $
                    concat
                        [ [[renderAmount lTotalBought, renderPrice lTotalSpent]]
                        , [["", renderPrice lTotalFee] | lTotalFee /= 0]
                        , [["", renderPrice lAverageSpend]]
                        ]

        tableData = map (flipRow . stepToRow) lSteps <> footerRows
        table = Table rowHeaders columnHeaders tableData
     in
        render id id id table
  where
    flipRow :: [a] -> [a]
    flipRow = if flipColumns then reverse else id
    stepToRow :: Step -> [String]
    stepToRow Step {..} = [renderAmount sAmount, renderPrice sPrice]
    renderPercent :: Pico -> String
    renderPercent =
        (<> "%") . formatScientific Fixed (Just 2) . realToFrac . negate
    renderAmount :: Pico -> String
    renderAmount =
        formatScientific Fixed (Just $ fromInteger amountPrecision) . realToFrac
    renderPrice :: Pico -> String
    renderPrice =
        formatScientific Fixed (Just $ fromInteger pricePrecision) . realToFrac
