module Network.Spotify.Client
    ( AuthorizeApi
    , MeApi
    , TokenApi
    , authorizeApi
    , authorizeLink
    , meApi
    , meRequest
    , spotifyAccountsBaseUrl
    , spotifyApiBaseUrl
    , tokenApi
    , tokenRequest
    ) where

import           Network.Spotify.Api.Auth
import           Network.Spotify.Api.User
import           Network.Spotify.Internal.Utils (spotifyApiBaseUrl)

-- TODO: Document all public values in this client library
