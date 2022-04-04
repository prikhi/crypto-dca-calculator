{-# LANGUAGE RecordWildCards #-}
{- | Laddered DCA calculations.
-}
module Finance.Crypto.DCA.Calculate
    ( calculateLadderBuys
    , LadderConfig(..)
    , Ladder(..)
    , Step(..)
    ) where

import           Control.Arrow                  ( (&&&) )
import           Data.Fixed                     ( Fixed(MkFixed)
                                                , Pico
                                                , resolution
                                                )


-- | A series of laddered-buys and related amounts.
data Ladder = Ladder
    { lSteps        :: [Step]
    , lTotalBought  :: Pico
    , lTotalSpent   :: Pico
    , lAverageSpend :: Pico
    , lTotalFee     :: Pico
    }

-- | Each purchase step of a ladder.
data Step = Step
    { sPercent :: Pico
    , sAmount  :: Pico
    , sPrice   :: Pico
    }
    deriving (Show, Read, Eq)


-- | Configuration data for generating a series of laddered-buys.
data LadderConfig = LadderConfig
    { lcPercentPerStep  :: Pico
    -- ^ percent of price drop per step
    , lcStepCount       :: Integer
    -- ^ number of steps in ladder
    , lcCurrentPrice    :: Pico
    -- ^ current price
    , lcTotalToSpend    :: Pico
    -- ^ total amount to spend
    , lcFeePercentage   :: Pico
    -- ^ The fee percentage charged by the exchange
    , lcAmountPrecision :: Integer
    -- ^ Amount precision in decimal places
    , lcPricePrecision  :: Integer
    -- ^ Price precision in decimal places
    }
    deriving (Show, Read, Eq, Ord)

-- | Calculate the 'Ladder' based on the input arguments.
calculateLadderBuys :: LadderConfig -> Ladder
calculateLadderBuys LadderConfig {..} =
    let (steps, remainingSpend) = foldr mkStep
                                        ([], lcTotalToSpend - lTotalFee)
                                        [1 .. lcStepCount - 1]
        lSteps       = steps <> [mkFinalStep lcStepCount remainingSpend]
        lTotalBought = sum $ map sAmount lSteps
        lTotalSpent  = sum $ map
            ( truncateToDecimal lcPricePrecision
            . uncurry (*)
            . (sAmount &&& sPrice)
            )
            lSteps
        lAverageSpend = lcTotalToSpend / lTotalBought
    in  Ladder { .. }
  where
    -- | TODO: should calculate per-step, since might not reach total
    -- spend.
    lTotalFee :: Pico
    lTotalFee = (lcFeePercentage / 100) * lcTotalToSpend
    spendPerStep :: Pico
    spendPerStep = (lcTotalToSpend - lTotalFee) / fromIntegral lcStepCount
    mkStep :: Integer -> ([Step], Pico) -> ([Step], Pico)
    mkStep stepNum (prevSteps, remaining) =
        let
            sPercent = fromInteger stepNum * lcPercentPerStep
            sPrice =
                truncateToDecimal lcPricePrecision
                    $ lcCurrentPrice
                    * (1 - (sPercent / 100))
            sAmount =
                truncateToDecimal lcAmountPrecision $ spendPerStep / sPrice
            cost = truncateToDecimal lcPricePrecision $ sPrice * sAmount
        in
            (Step { .. } : prevSteps, remaining - cost)
    mkFinalStep :: Integer -> Pico -> Step
    mkFinalStep stepNum toSpend =
        let sPercent = fromInteger stepNum * lcPercentPerStep
            sPrice =
                truncateToDecimal lcPricePrecision
                    $ lcCurrentPrice
                    * (1 - (sPercent / 100))
            sAmount = truncateToDecimal lcAmountPrecision $ toSpend / sPrice
        in  Step { .. }


truncateToDecimal :: Integer -> Pico -> Pico
truncateToDecimal decimalPlace p@(MkFixed i) =
    let multiplier          = 10 ^ decimalPlace
        truncatedResolution = resolution p `div` multiplier
    in  MkFixed $ i `div` truncatedResolution * truncatedResolution
