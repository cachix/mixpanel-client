# MixPanel client for Haskell


[![Build Status](https://travis-ci.com/hercules-ci/mixpanel-client.svg?branch=master)](https://travis-ci.com/domenkozar/mixpanel-client)
[![Hackage](https://img.shields.io/hackage/v/mixpanel-client.svg)](https://hackage.haskell.org/package/mixpanel-client)


Implements major features of [MixPanel HTTP API](https://mixpanel.com/help/reference/http):

- [x] track
- [x] alias
- [x] engage
- [ ] import
- [ ] export

## Getting started

```haskell
import Data.Aeson      ( (.=) )
import Data.Time.Clock ( getCurrentTime )
import GHC.Exts        ( fromList )
import MixPanel        ( Operation(Set), engage, track, alias
                       , AuthToken(..), mkEnv
                       -- reexports
                       , newManager, tlsManagerSettings)


main :: IO ()
main = do
  -- setup
  manager <- newManager tlsManagerSettings
  let env = mkEnv (AuthToken "foobar") manager

  -- track a simple event
  Right () <- track env "Played Video" mempty

  -- track an event with extra properties
  Right () <- track env "Played Video Unique per user"
    $ fromList [ "distinct_id" .= ("generated-id" :: String)
               , "customProperty" .= True ]

  -- alias the user
  Right () <- alias env "generated-id" "user@example.com"

  -- profile engagement
  now <- getCurrentTime
  Right () <- engage env "user@example.com"
    $ Set (fromList [ "$created" .= now])

  putStrLn "All good!"
```

## Design questions

a) Why does it use `Object/Array` intemediate values from `Data.Aeson`?


## TODO

- batch requests
- expose extra url query paramerers
- engage special properties more type safe?
- filter out null values in requests to mixpanel
- api support for tracking revenue
