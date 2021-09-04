{-# LANGUAGE DeriveDataTypeable #-}
{- | CLI application harness.

-}
module Finance.Crypto.DCA.Main
    ( run
    , getArgs
    , Args(..)
    ) where

import           Data.Version                   ( showVersion )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , cmdArgs
                                                , help
                                                , helpArg
                                                , name
                                                , program
                                                , summary
                                                )

import           Paths_crypto_dca_calculator    ( version )


-- | Run the executable.
run :: Args -> IO ()
run Args{} = putStrLn "hello world"


-- | CLI arguments supported by the executable.
data Args = Args {}
    deriving (Show, Read, Eq, Data, Typeable)


-- | Parse the CLI arguments with 'System.Console.CmdArgs'.
getArgs :: IO Args
getArgs = cmdArgs argSpec


-- | Defines & documents the CLI arguments.
argSpec :: Args
argSpec =
    Args{}
        &= summary
               (  "crypto-dca-calculator v"
               <> showVersion version
               <> ", Pavan Rikhi 2021"
               )
        &= program "crypto-dca-calculator"
        &= helpArg [name "h"]
        &= help "Short, one-line summary of project here."
