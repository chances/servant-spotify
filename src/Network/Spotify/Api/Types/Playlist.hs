{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Spotify.Api.Types.Playlist
Description : Information about a playlist.
Stability   : experimental
-}
module Network.Spotify.Api.Types.Playlist where

import           Data.Aeson                           (FromJSON (parseJSON),
                                                       ToJSON, defaultOptions,
                                                       genericParseJSON,
                                                       genericToJSON, toJSON)
import           Data.Text                            (Text)
import           GHC.Generics                         (Generic)

import           Network.Spotify.Api.Types.Followers  (Followers)
import           Network.Spotify.Api.Types.Image      (Image)
import           Network.Spotify.Api.Types.Paging     (Paging)
import           Network.Spotify.Api.Types.SpotifyUri (SpotifyUri)
import           Network.Spotify.Api.Types.SpotifyUrl (SpotifyUrl)
import           Network.Spotify.Api.Types.Track      (Track)
import           Network.Spotify.Api.Types.User       (User)

-- | Information about a playlist.
data Playlist = Playlist
    { collaborative :: Bool -- ^ 'True' if the owner allows other users to
                            --   modify the playlist. 'False' otherwise.
    , description   :: Maybe Text -- ^ The playlist description. _Only returned
                                  --   for modified, verified playlists,
                                  --   otherwise 'Nothing'._
    , external_urls :: SpotifyUrl -- ^ Known external URLs for this playlist.
    , followers     :: Followers -- ^ Information about the followers of the
                                 --   playlist.
    , href          :: Text -- ^ A link to the Web API endpoint providing full
                            --   details of the playlist.
    , id            :: Text -- ^ The Spotify ID for the playlist.
    , images        :: [Image] -- ^ Images for the playlist. The array may be
                               --   empty or contain up to three images. The
                               --   images are returned by size in descending
                               --   order. See Working with Playlists.
                               --   
                               --   _Note: If returned, the source URL for the
                               --   image is temporary and will expire in less
                               --   than a day._
    , name          :: Text -- ^ The name of the playlist.
    , owner         :: User -- ^ The user who owns the playlist.
    , public        :: Maybe Bool -- ^ The playlist's public/private status:
                                  --   `True` the playlist is public, 'False'
                                  --   the playlist is private, 'Nothing' the
                                  --   playlist status is not relevant.
    , snapshot_id   :: Text -- ^ The version identifier for the current
                            --   playlist. Can be supplied in other requests
                            --   to target a specific playlist version.
    , tracks        :: Paging [PlaylistTrack] -- ^ Information about the tracks
                                              --   of the playlist.
    , uri           :: SpotifyUri Playlist -- ^ The 'SpotifyUri' for
                                               --   the playlist.
    } deriving (Generic)

instance FromJSON Playlist where
    parseJSON = genericParseJSON defaultOptions
instance ToJSON Playlist where
    toJSON = genericToJSON defaultOptions

-- | A collection containing track information for a 'PlaylistSimplified'.
data PlaylistTrack = PlaylistTrack
    { added_at :: Maybe Text -- ^ The date and time the track was added.
                             --   _Note that some very old playlists may return
                             --   'Nothing' in this field._
    , added_by :: Maybe User -- ^ The Spotify user who added the track.
                             --   Note: Some very old playlists may return
                             --   'Nothing' in this field.
    , is_local :: Bool -- ^ Whether this track is a local file or not.
    , track    :: Track -- ^ Information about the track.
    } deriving (Generic)

instance FromJSON PlaylistTrack where
    parseJSON = genericParseJSON defaultOptions
instance ToJSON PlaylistTrack where
    toJSON = genericToJSON defaultOptions
