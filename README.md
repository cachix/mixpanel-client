# MixPanel haskell client

## Minimal example

```haskell
import MixPanel

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  env = mkEnv (Token "foobar") manager
  print $ runMixPanel env (track "Testing" (Nothing :: Maybe ())
```
