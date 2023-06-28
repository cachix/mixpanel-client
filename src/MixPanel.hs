module MixPanel
  ( -- * MixPanel API
    module MixPanel.API

  -- * Env setup
  , MixPanel.Env.Env
  , MixPanel.Env.mkEnv

  -- * Types
  , MixPanel.Types.AuthToken(..)
  , MixPanel.Types.DidSucceed(..)
  , MixPanel.Types.DistinctId
  , MixPanel.Types.Operation(..)
  , MixPanel.Types.Toggle(..)

  -- * HTTP manager reexports
  , HTTP.newManager
  , HTTP.tlsManagerSettings
  ) where

import qualified Network.HTTP.Client           as HTTP
import           Network.HTTP.Client.TLS       as HTTP

import           MixPanel.API
import           MixPanel.Env
import           MixPanel.Types.Core           as MixPanel.Types
import           MixPanel.Types.EngageData     as MixPanel.Types
