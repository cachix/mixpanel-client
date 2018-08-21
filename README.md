# MixPanel client for Haskell

Implements part of [MixPanel HTTP API](https://mixpanel.com/help/reference/http).

- [x] track
- [x] alias
- [x] engage
- [ ] import
- [ ] export

## Getting started

```haskell
import Data.Aeson ((.=))
import GHC.Exts (fromList)
import MixPanel


main :: IO ()
main = do
  -- Setup
  manager <- newManager tlsManagerSettings
  let env = mkEnv (AuthToken "foobar") manager

  -- track a simple event
  Right Success <- runMixPanel env
    $ track "Played Video" mempty

  -- track an event with extra properties
  Right Success <- runMixPanel env
    $ track "Played Video"
    $ fromList [ "distinct_id" .= ("generated-id" :: String)
               , "customProperty" .= True ]

  -- alias the user
  Right Success <- runMixPanel env
    $ alias "generated-id" "user@example.com"

  -- profile engagement
  Right Success <- runMixPanel env
    $ engage "user@example.com"
    $ Set (fromList [ "customProperty" .= ("foobar" :: String)])

  putStrLn "All good!"
```

## Design questions

a) Why does it use `Object/Array` intemediate values from `Data.Aeson`?


## TODO

- date formatting
- batch requests
- expose extra url query paramerers
- engage special properties more type safe?
