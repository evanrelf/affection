{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Teletype
  ( Teletype (..)
  , readLine
  , writeLine
  , runTeletypeIO
  )
where

import Effect (Eff, Member, interpret, send)


data Teletype a where
  ReadLine :: Teletype String
  WriteLine :: String -> Teletype ()


readLine :: Member Teletype r => Eff r String
readLine = send ReadLine


writeLine :: Member Teletype r => String -> Eff r ()
writeLine message = send $ WriteLine message


runTeletypeIO :: Member IO r => Eff (Teletype ': r) a -> Eff r a
runTeletypeIO = interpret @IO $ \case
  ReadLine -> getLine
  WriteLine message -> putStrLn message
