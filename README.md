# effect

Toy effect system based on freer monad

## Required language extensions

Minimum required language extensions needed when defining an effect:

- `DataKinds`
- `FlexibleContexts`
- `GADTs`
- `PolyKinds`
- `TypeOperators`

Minimum required language extensions needed when using effects:

- `FlexibleContexts` (when using `Member`)
- `DataKinds` (when using `Members`)
