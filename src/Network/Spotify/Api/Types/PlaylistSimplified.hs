{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Network.Spotify.Api.Types.PlaylistSimplified
Description : Information about a simplified representation of a playlist.
Stability   : experimental
-}
module Network.Spotify.Api.Types.PlaylistSimplified where

import           Data.Aeson                           (FromJSON (parseJSON),
                                                       ToJSON, genericParseJSON,
                                                       genericToJSON, object,
                                                       toJSON, withObject, (.:),
                                                       (.=))
import           Data.Text                            (Text)
import           GHC.Generics                         (Generic)

import           Network.Spotify.Api.Types.Image      (Image)
import           Network.Spotify.Api.Types.SpotifyUri (SpotifyUri)
import           Network.Spotify.Api.Types.SpotifyUrl (SpotifyUrl)
import           Network.Spotify.Api.Types.User       (User)
import           Network.Spotify.Internal.Utils

-- | Simplified information about a playlist.
data PlaylistSimplified = PlaylistSimplified
    { collaborative :: Bool -- ^ 'True' if the owner allows other users to
                            --   modify the playlist. 'False' otherwise.
    , external_urls :: SpotifyUrl -- ^ Known external URLs for this playlist.
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
                                  --   `True` the playlist is public, 'False' the
                                  --   playlist is private, 'Nothing' the
                                  --   playlist status is not relevant.
    , snapshot_id   :: Text -- ^ The version identifier for the current
                            --   playlist. Can be supplied in other requests
                            --   to target a specific playlist version.
    , tracks        :: Tracks -- ^ A collection containing a link (href) to the
                              --   Web API endpoint where full details of the
                              --   playlist's tracks can be retrieved, along
                              --   with the total number of tracks in the
                              --   playlist.
    , uri           :: SpotifyUri PlaylistSimplified -- ^ The 'SpotifyUri' for
                                                     --   the playlist.
    } deriving (Generic)

instance FromJSON PlaylistSimplified where
    parseJSON = genericParseJSON doOmitNothingFields
instance ToJSON PlaylistSimplified where
    toJSON = genericToJSON doOmitNothingFields

-- | A collection containing track information for a 'PlaylistSimplified'.
data Tracks = Tracks
    { tracksHref :: Text -- ^ A link to the Web API endpoint where full details
                         --   of the playlist's tracks can be retrieved.
    , total      :: Int -- ^ The total number of tracks in the playlist.
    }

instance FromJSON Tracks where
    parseJSON = withObject "Tracks" $ \o -> Tracks
        <$> o .: "href"
        <*> o .: "total"
instance ToJSON Tracks where
    toJSON Tracks{..} = object
        [ "href"   .= tracksHref
        , "total"  .= total
        ]
