{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Spotify.Api.Types.AlbumSimplified
Description : Information about a simplified representation of an album.
Stability   : experimental
-}
module Network.Spotify.Api.Types.AlbumSimplified where

import           Data.Aeson                                 (FromJSON (parseJSON),
                                                             ToJSON,
                                                             defaultOptions,
                                                             genericParseJSON,
                                                             genericToJSON,
                                                             toJSON)
import           Data.ISO3166_CountryCodes                  (CountryCode)
import           Data.Text                                  (Text)
import           GHC.Generics                               (Generic)

import           Network.Spotify.Api.Types.ArtistSimplified (ArtistSimplified)
import           Network.Spotify.Api.Types.Image            (Image)
import           Network.Spotify.Api.Types.SpotifyUri       (SpotifyUri)
import           Network.Spotify.Api.Types.SpotifyUrl       (SpotifyUrl)

-- | Information about an album.
data AlbumSimplified = AlbumSimplified
    { album_type        :: AlbumType -- ^ The type of the album.
    , artists           :: [ArtistSimplified] -- ^ The artists of the album.
                                              --   Each artist object includes
                                              --   a link in href to more
                                              --   detailed information about
                                              --   the artist.
    , available_markets :: [CountryCode] -- ^ The markets in which the album is
                                         --   available: ISO 3166-1 alpha-2
                                         --   country codes.
                                         --   
                                         --   Note: An album is considered
                                         --   available in a market when at
                                         --   least 1 of its tracks is
                                         --   available in that market.
    , external_urls     :: SpotifyUrl -- ^ Known external URLs for this album.
    , href              :: Text -- ^ A link to the Web API endpoint providing
                                --   full details of the album.
    , id                :: Text -- ^ The Spotify ID for the album.
    , images            :: [Image] -- ^ The cover art for the album in various
                                   --   sizes, widest first.
    , name              :: Text -- ^ The name of the album. In case of an album
                                --   takedown, the value may be an empty string.
    , uri               :: SpotifyUri AlbumSimplified -- ^ The 'SpotifyUri' for
                                                      --   the album.
    } deriving (Generic)

instance FromJSON AlbumSimplified where
    parseJSON =
        genericParseJSON defaultOptions
instance ToJSON AlbumSimplified where
    toJSON = genericToJSON defaultOptions

instance FromJSON CountryCode
instance ToJSON CountryCode

data AlbumType =
      FullAlbum
    | Single
    | Compilation
    deriving Generic

instance FromJSON AlbumType where
    parseJSON = genericParseJSON defaultOptions
instance ToJSON AlbumType where
    toJSON = genericToJSON defaultOptions
