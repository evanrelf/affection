{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Teletype
  ( Teletype (..)
  , readLine
  , writeLine
  , runTeletypeIO
  )
where

import Effect (Eff, Member, interpret, send)
import Effect.Lift (Lift, lift)


data Teletype m a where
  ReadLine :: Teletype m String
  WriteLine :: String -> Teletype m ()


readLine :: Member Teletype r => Eff r String
readLine = send ReadLine


writeLine :: Member Teletype r => String -> Eff r ()
writeLine message = send $ WriteLine message


runTeletypeIO :: Member (Lift IO) r => Eff (Teletype ': r) a -> Eff r a
runTeletypeIO = interpret $ \case
  ReadLine -> lift $ getLine
  WriteLine message -> lift $ putStrLn message
