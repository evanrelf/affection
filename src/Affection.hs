module Affection
  ( module Affection.Internal
  , module Affection.Internal.Eff
  , module Affection.Internal.Union
  )
where

import Affection.Internal (interpret, runM, send)
import Affection.Internal.Eff (Eff)
import Affection.Internal.Union (Member, Members)
