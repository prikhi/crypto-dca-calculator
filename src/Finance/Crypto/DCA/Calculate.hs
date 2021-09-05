{-# LANGUAGE RecordWildCards #-}
{- | Laddered DCA calculations.
-}
module Finance.Crypto.DCA.Calculate
    ( calculateLadderBuys
    , Ladder(..)
    , Step(..)
    ) where

import           Data.Fixed                     ( Pico )


-- | A series of laddered-buys and related amounts.
data Ladder = Ladder
    { lSteps        :: [Step]
    , lTotalBought  :: Pico
    , lAverageSpend :: Pico
    }

-- | Each purchase step of a ladder.
data Step = Step
    { sPercent :: Pico
    , sAmount  :: Pico
    , sPrice   :: Pico
    }
    deriving (Show, Read, Eq)


-- | Calculate the 'Ladder' based on the input arguments.
calculateLadderBuys
    :: Pico
    -- ^ percent of price drop per step
    -> Integer
    -- ^ number of steps in ladder
    -> Pico
    -- ^ current price
    -> Pico
    -- ^ total amount to spend
    -> Ladder
calculateLadderBuys percentPerStep steps currentPrice totalToSpend =
    let lSteps        = map mkStep [1 .. steps]
        lTotalBought  = sum $ map sAmount lSteps
        lAverageSpend = totalToSpend / lTotalBought
    in  Ladder { .. }
  where
    spendPerStep :: Pico
    spendPerStep = totalToSpend / fromIntegral steps
    mkStep :: Integer -> Step
    mkStep stepNum =
        let sPercent = fromInteger stepNum * percentPerStep
            sPrice   = currentPrice * (1 - (sPercent / 100))
            sAmount  = spendPerStep / sPrice
        in  Step { .. }
