# MixPanel client for Haskell

Implements part of [MixPanel HTTP API](https://mixpanel.com/help/reference/http).

- [x] track
- [x] alias
- [WIP] engage
- [ ] import
- [ ] export

## Getting started

```haskell
import MixPanel

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let env = mkEnv (AuthToken "foobar") manager
  successOrErr <- runMixPanel env $ track "Testing" mempty
  print successOrErr
```

## Design questions

a) Should properties be exposed as `Value` (from Data.Aeson) or as `ToJSON a =>`?


## TODO

- batch requests
- verbose mode as default?
