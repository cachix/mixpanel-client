# MixPanel client for Haskell

Implements part of [MixPanel HTTP API](https://mixpanel.com/help/reference/http).

## Getting started

```haskell
import MixPanel

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let env = mkEnv (AuthToken "foobar") manager
  successOrErr <- runMixPanel env $ track "Testing" (Nothing :: Maybe ())
  print successOrErr
```
