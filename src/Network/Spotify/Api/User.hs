{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Network.Spotify.Api.User
    ( MeApi
    , meApi
    , meRequest
    ) where

import           Network.HTTP.Client            (Manager)
import           Servant
import           Servant.Client                 (ClientM, client)

import           Network.Spotify.Api.Types
import           Network.Spotify.Api.Types.User as User
import           Network.Spotify.Internal.Utils (spotifyApiBaseUrl)

-- | Get current user's profile
type MeApi = "me"
    :> Header "Authorization" Authorization
    :> Get '[JSON] User.User

meApi :: Proxy MeApi
meApi = Proxy

meRequest ::
    Authorization -> Manager -> ClientM User.User
meRequest auth manager =
    let meApiClient = client meApi
    in meApiClient (Just auth) manager spotifyApiBaseUrl
