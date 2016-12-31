{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Spotify.Api.Types.ArtistSimplified where

import           Data.Aeson                           (FromJSON (parseJSON),
                                                       ToJSON, defaultOptions,
                                                       genericParseJSON,
                                                       genericToJSON, toJSON)
import           Data.Text                            (Text)
import           GHC.Generics                         (Generic)

import           Network.Spotify.Api.Types.SpotifyUri (SpotifyUri)
import           Network.Spotify.Api.Types.SpotifyUrl (SpotifyUrl)

-- | Information about an artist.
data ArtistSimplified = ArtistSimplified
    { external_urls :: SpotifyUrl -- | Known external URLs for this artist.
    , href          :: Text -- | A link to the Web API endpoint providing full
                            --   details of the artist.
    , id            :: Text -- | The Spotify ID for the artist.
    , name          :: Text -- | The name of the artist.
    , uri           :: SpotifyUri ArtistSimplified -- | The SpotifyUri for the
                                                   --   artist.
    } deriving (Generic)

instance FromJSON ArtistSimplified where
    parseJSON = genericParseJSON defaultOptions
instance ToJSON ArtistSimplified where
    toJSON = genericToJSON defaultOptions
