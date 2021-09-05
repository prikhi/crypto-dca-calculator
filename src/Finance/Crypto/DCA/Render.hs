{-# LANGUAGE RecordWildCards #-}
{- | Rendering a series of laddered buys.
-}
module Finance.Crypto.DCA.Render
    ( renderTable
    ) where

import           Data.Fixed                     ( Pico )
import           Data.Scientific                ( FPFormat(Fixed)
                                                , formatScientific
                                                )
import           Text.Tabular                   ( Header(..)
                                                , Properties(..)
                                                , Table(..)
                                                )
import           Text.Tabular.AsciiArt          ( render )

import           Finance.Crypto.DCA.Calculate   ( Ladder(..)
                                                , Step(..)
                                                )


-- | Render the buys in a table, with total/average rows afterwards.
renderTable
    :: Bool
    -- ^ flip the default @Amount, Price@ columns to @Price, Amount@.
    -> Ladder
    -- ^ the calculated series of laddered buys.
    -> String
renderTable flipColumns Ladder {..} =
    let
        percentHeaders = map (Header . renderPercent . sPercent) lSteps
        rowHeaders     = Group
            DoubleLine
            [ Group NoLine percentHeaders
            , Group NoLine $ flipRow [Header "Total", Header "Avg"]
            ]
        columnHeaders =
            Group SingleLine $ flipRow [Header "Amount", Header "Price"]

        footerRows = flipRow $ map
            flipRow
            [[renderAmount lTotalBought, ""], ["", renderPrice lAverageSpend]]

        tableData = map (flipRow . stepToRow) lSteps <> footerRows
        table     = Table rowHeaders columnHeaders tableData
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
    renderAmount = formatScientific Fixed (Just 8) . realToFrac
    renderPrice :: Pico -> String
    renderPrice = formatScientific Fixed (Just 2) . realToFrac
