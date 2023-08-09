# affection

Toy effect system based on freer monad

## Example

```haskell
data Teletype a where
  ReadLine :: Teletype String
  WriteLine :: String -> Teletype ()

readLine :: Member Teletype r => Eff r String
readLine = send ReadLine

writeLine :: Member Teletype r => String -> Eff r ()
writeLine message = send $ WriteLine message

runTeletypeIO :: Member IO r => Eff (Teletype : r) a -> Eff r a
runTeletypeIO = interpret @IO $ \case
  ReadLine -> getLine
  WriteLine message -> putStrLn message

program :: Member Teletype r => Eff r ()
program = do
  writeLine "Enter your name:"
  name <- readLine
  writeLine ("Hello, " <> name <> "!")

main :: IO ()
main =
  program
    & runTeletypeIO
    & runM
```
