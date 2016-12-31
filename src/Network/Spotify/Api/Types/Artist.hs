{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Spotify.Api.Types.Artist where

import           Data.Aeson                           (FromJSON (parseJSON),
                                                       ToJSON, defaultOptions,
                                                       genericParseJSON,
                                                       genericToJSON, toJSON)
import           Data.Text                            (Text)
import           GHC.Generics                         (Generic)

import           Network.Spotify.Api.Types.Followers  (Followers)
import           Network.Spotify.Api.Types.Image      (Image)
import           Network.Spotify.Api.Types.SpotifyUri (SpotifyUri)
import           Network.Spotify.Api.Types.SpotifyUrl (SpotifyUrl)

-- | Information about an artist.
data Artist = Artist
    { external_urls :: SpotifyUrl -- | Known external URLs for this artist.
    , followers     :: Followers -- | Information about the followers of the
                                 --   artist.
    , genres        :: [Text] -- | A list of the genres the artist is
                              --   associated with. For example: "Prog Rock",
                              --   "Post-Grunge". (If not yet classified, the
                              --   array is empty.)
    , href          :: Text -- | A link to the Web API endpoint providing full
                            --   details of the artist.
    , id            :: Text -- | The Spotify ID for the artist.
    , images        :: [Image] -- | Images of the artist in various sizes,
                               --   widest first.
    , name          :: Text -- | The name of the artist.
    , popularity    :: Int -- | The popularity of the artist. The value will be
                           --   between 0 and 100, with 100 being the most
                           --   popular. The artist's popularity is calculated
                           --   from the popularity of all the artist's tracks.
    , uri           :: SpotifyUri Artist -- | The SpotifyUri for the artist.
    } deriving (Generic)

instance FromJSON Artist where
    parseJSON = genericParseJSON defaultOptions
instance ToJSON Artist where
    toJSON = genericToJSON defaultOptions
