module Effect
  ( module Effect.Internal
  , module Data.OpenUnion
  )
where

import Data.OpenUnion (Member (..), Members)
import Effect.Internal (Eff, interpret, runM, send)
