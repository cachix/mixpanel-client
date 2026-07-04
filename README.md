# MixPanel client for Haskell


[![Build Status](https://github.com/domenkozar/mixpanel-client/actions/workflows/build.yml/badge.svg)](https://github.com/domenkozar/mixpanel-client/actions/workflows/build.yml)
[![Hackage](https://img.shields.io/hackage/v/mixpanel-client.svg)](https://hackage.haskell.org/package/mixpanel-client)


Implements major features of [MixPanel HTTP API](https://docs.mixpanel.com/reference/overview):

- [x] track
- [x] alias
- [x] engage
- [x] import
- [ ] export

## Getting started

```haskell
import Data.Aeson            ( (.=) )
import Data.Time.Clock       ( getCurrentTime )
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import GHC.Exts              ( fromList )
import MixPanel              ( Operation(Set), engage, track, alias
                            , importEvents, mkImportEvent, ProjectToken(..)
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

  -- batch import events via the /import endpoint.
  -- Each event needs a caller-supplied $insert_id (used for deduplication);
  -- Mixpanel accepts up to 2000 events per request.
  let events =
        [ mkImportEvent "Played Video" (utcTimeToPOSIXSeconds now)
            "generated-id" "unique-insert-id-1"
            [ "customProperty" .= True ]
        ]
  -- Authenticate with a Service Account, or with the project token as below.
  _ <- importEvents env (ProjectToken (AuthToken "foobar") Nothing) events

  putStrLn "All good!"
```

## Design questions

a) Why does it use `Object/Array` intemediate values from `Data.Aeson`?


## TODO

- expose extra url query paramerers
- engage special properties more type safe?
- filter out null values in requests to mixpanel
- api support for tracking revenue
