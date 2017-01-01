{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module      : Network.Spotify.Api.User
Description : Spotify Web API user related endpoints.
Stability   : experimental
-}
module Network.Spotify.Api.User
    ( module Network.Spotify.Api.User
    ) where

import           Data.ISO3166_CountryCodes                    (CountryCode)
import           Data.Text                                    (pack)
import           Network.HTTP.Client                          (Manager)
import           Servant
import           Servant.Client                               (BaseUrl, ClientM,
                                                               client)

import           Network.Spotify.Api.Types
import           Network.Spotify.Api.Types.Paging             as Paging
import           Network.Spotify.Api.Types.PlaylistSimplified as Playlist
import           Network.Spotify.Api.Types.User               as User
import           Network.Spotify.Internal.Utils               (spotifyApiBaseUrl)

-- | A valid access token from the Spotify Accounts service
type AuthorizedRequest = Header "Authorization" Authorization

-- | Get current user's profile
type MeApi = "me" :> AuthorizedRequest :> Get '[JSON] User.User

meRequest :: Maybe Authorization -> Manager -> BaseUrl -> ClientM User.User

getMe :: Authorization -> Manager -> ClientM User.User
getMe auth manager = meRequest (Just auth) manager spotifyApiBaseUrl

-- | Get a list of the playlists owned or followed by a Spotify user.
type MePlaylistsApi = "me" :> "playlists"
    :> AuthorizedRequest
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "market" CountryCode
    :> Get '[JSON] (Paging.Paging [Playlist.PlaylistSimplified])

instance ToHttpApiData CountryCode where
    toQueryParam = pack . show

mePlaylistsRequest :: Maybe Authorization -> Maybe Int -> Maybe Int ->
    Maybe CountryCode ->
    Manager -> BaseUrl -> ClientM (Paging.Paging [Playlist.PlaylistSimplified])

getMePlaylists :: Authorization -> Paging.PageRequest ->
    Maybe CountryCode -> Manager -> ClientM (Paging.Paging [Playlist.PlaylistSimplified])
getMePlaylists auth page market manager = mePlaylistsRequest
    (Just auth) (Paging.requestLimit page) (Paging.requestOffset page)
    market manager spotifyApiBaseUrl

-- Make User API client
type UserApi = MeApi :<|> MePlaylistsApi

userApi :: Proxy UserApi
userApi = Proxy

(meRequest :<|> mePlaylistsRequest) = client userApi
