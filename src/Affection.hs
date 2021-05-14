module Affection
  ( module Affection.Internal
  , module Affection.Internal.Eff
  , module Affection.Internal.OpenUnion
  )
where

import Affection.Internal (interpret, runM, send)
import Affection.Internal.Eff (Eff)
import Affection.Internal.OpenUnion (Member, Members)
