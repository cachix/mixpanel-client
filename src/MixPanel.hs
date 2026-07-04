module MixPanel
  ( -- * MixPanel API
    module MixPanel.API

  -- * Env setup
  , MixPanel.Env.Env
  , MixPanel.Env.mkEnv

  -- * Auth
  , MixPanel.Auth.ProjectToken(..)
  , MixPanel.Auth.ServiceAccount(..)
  , MixPanel.Auth.Credentials(..)
  , MixPanel.Auth.Auth(..)

  -- * Types
  , MixPanel.Types.AuthToken(..)
  , MixPanel.Types.DidSucceed(..)
  , MixPanel.Types.DistinctId
  , MixPanel.Types.Operation(..)
  , MixPanel.Types.Toggle(..)

  -- * Import (batch) types
  , MixPanel.Types.ImportPayload(..)
  , MixPanel.Types.ImportEvent(..)
  , MixPanel.Types.ImportProperties(..)
  , MixPanel.Types.mkImportEvent
  , MixPanel.Types.mkMergeEvent
  , MixPanel.Types.mkIdentifiedEvent
  , MixPanel.Types.ImportResponse(..)
  , MixPanel.Types.FailedRecord(..)

  -- * HTTP manager reexports
  , HTTP.newManager
  , HTTP.tlsManagerSettings
  ) where

import qualified Network.HTTP.Client           as HTTP
import           Network.HTTP.Client.TLS       as HTTP

import           MixPanel.API
import           MixPanel.Auth
import           MixPanel.Env
import           MixPanel.Types.Core           as MixPanel.Types
import           MixPanel.Types.EngageData     as MixPanel.Types
import           MixPanel.Types.ImportData     as MixPanel.Types
