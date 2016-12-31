{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Spotify.Api.Types.Track
Description : Information about a track.
Stability   : experimental
-}
module Network.Spotify.Api.Types.Track where

import           Data.Aeson                                 (FromJSON (parseJSON),
                                                             ToJSON,
                                                             defaultOptions,
                                                             genericParseJSON,
                                                             genericToJSON,
                                                             object, toJSON,
                                                             withObject, (.:),
                                                             (.=))
import           Data.ISO3166_CountryCodes                  (CountryCode)
import           Data.Text                                  (Text)
import           GHC.Generics                               (Generic)

import           Network.Spotify.Api.Types.AlbumSimplified  (AlbumSimplified)
import           Network.Spotify.Api.Types.ArtistSimplified (ArtistSimplified)
import           Network.Spotify.Api.Types.ExternalId       (ExternalId)
import           Network.Spotify.Api.Types.SpotifyUri       (SpotifyUri)
import           Network.Spotify.Api.Types.SpotifyUrl       (SpotifyUrl)

-- | Information about a track.
data Track = Track
    { album             :: AlbumSimplified -- ^ The album on which the track
                                           --   appears. The album object
                                           --   includes a link in href to full
                                           --   information about the album.
    , artists           :: [ArtistSimplified] -- ^ The artists who performed
                                              --   the track. Each artist
                                              --   object includes a link in
                                              --   href to more detailed
                                              --   information about the artist.
    , available_markets :: [CountryCode] -- ^ A list of the countries in which
                                         --   the track can be played,
                                         --   identified by their ISO 3166-1
                                         --   alpha-2 code.
    , disc_number       :: Int -- ^ The disc number (usually `1` unless the
                               --   album consists of more than one disc).
    , duration_ms       :: Int -- ^ The track length in milliseconds.
    , explicit          :: Bool -- ^ Whether or not the track has explicit
                                --   lyrics ('True' = yes it does; 'False' = no
                                --   it does not OR unknown).
    , external_ids      :: ExternalId -- ^ Known 'ExternalId's for the track.
    , external_urls     :: SpotifyUrl -- ^ External URLs for this track.
    , href              :: Text -- ^ A link to the Web API endpoint providing
                                --   full details of the track.
    , id                :: Text -- ^ The Spotify ID for the track.
    , is_playable       :: Bool -- ^ Part of the response when Track Relinking
                                --   is applied. If 'True', the track is
                                --   playable in the given market. Otherwise
                                --   'False'.
    , linked_from       :: Maybe LinkedTrack -- ^ Part of the response when
                                             --   Track Relinking is applied
                                             --   and is only part of the
                                             --   response if the track
                                             --   linking, in fact, exists. The
                                             --   requested track has been
                                             --   replaced with a different
                                             --   track. The track in the
                                             --   linked_from object contains
                                             --   information about the
                                             --   originally requested track.
    , name              :: Text -- ^ The name of the track.
    , popularity        :: Int -- ^ The popularity of the track. The value will
                               --   be between 0 and 100, with 100 being the
                               --   most popular.
                               --   .
                               --   The popularity is based, in most part, on
                               --   the total number of plays the track has had
                               --   and how recent those plays are.
                               --   .
                               --   See [Track Object (Full)](https://developer.spotify.com/web-api/object-model/#track-object-full)
                               --   in the Spotify Web API docs for more
                               --   detailed info.
    , preview_url       :: Text -- ^ A link to a 30 second preview (MP3 format)
                                --   of the track.
    , track_number      :: Int -- ^ The number of the track. If an album has
                               --   several discs, the track number is the
                               --   number on the specified disc.
    , uri               :: SpotifyUri Track -- ^ The 'SpotifyUri' for
                                            --   the track.
    } deriving (Generic)

instance FromJSON Track where
    parseJSON = genericParseJSON defaultOptions
instance ToJSON Track where
    toJSON = genericToJSON defaultOptions

data LinkedTrack = LinkedTrack
    { linkedTrackExternalUrls :: SpotifyUrl -- ^ Known external URLs for this
                                            --   track.
    , linkedTrackHref         :: Text -- ^ A link to the Web API endpoint
                                      --   providing full details of the track.
    , linkedTrackId           :: Text -- ^ The Spotify ID for the track.
    , linkedTrackUri          :: SpotifyUri Track -- ^ The SpotifyUri for the
                                                  --   track.
    }

instance FromJSON LinkedTrack where
    parseJSON = withObject "Linked Track" $ \o -> LinkedTrack
        <$> o .: "external_urls"
        <*> o .: "href"
        <*> o .: "id"
        <*> o .: "uri"

instance ToJSON LinkedTrack where
  toJSON o = object
    [ "external_urls" .= linkedTrackExternalUrls o
    , "href"          .= linkedTrackHref o
    , "id"            .= linkedTrackId o
    , "url"           .= linkedTrackUri o
    ]
