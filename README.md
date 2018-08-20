# MixPanel haskell client

## Minimal example

```haskell
import MixPanel

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let env = mkEnv (Token "foobar") manager
  successOrErr <- runMixPanel env $ track "Testing" (Nothing :: Maybe ())
  print successOrErr
```
