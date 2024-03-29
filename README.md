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

runTeletypeIO :: Member (Lift IO) r => Eff (Teletype : r) a -> Eff r a
runTeletypeIO = interpret $ \case
  ReadLine -> liftIO getLine
  WriteLine message -> liftIO $ putStrLn message

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
