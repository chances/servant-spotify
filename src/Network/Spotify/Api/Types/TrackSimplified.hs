{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Spotify.Api.Types.TrackSimplified where

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
import           Network.Spotify.Api.Types.SpotifyUri       (SpotifyUri)
import           Network.Spotify.Api.Types.SpotifyUrl       (SpotifyUrl)
import           Network.Spotify.Api.Types.Track            (LinkedTrack (..))

-- | Simplified information about a track.
data TrackSimplified = TrackSimplified
    { artists           :: [ArtistSimplified] -- ^ The artists who performed
                                              --   the track. Each artist
                                              --   object includes a link in
                                              --   href to more detailed
                                              --   information about the artist.
    , available_markets :: [CountryCode] -- ^ A list of the countries in which
                                         --   the track can be played,
                                         --   identified by their ISO 3166-1
                                         --   alpha-2 code. (A 'CountryCode')
    , disc_number       :: Int -- ^ The disc number (usually `1` unless the
                               --   album consists of more than one disc).
    , duration_ms       :: Int -- ^ The track length in milliseconds.
    , explicit          :: Bool -- ^ Whether or not the track has explicit
                                --   lyrics ('True' = yes it does; 'False' = no
                                --   it does not OR unknown).
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
    , preview_url       :: Text -- ^ A link to a 30 second preview (MP3 format)
                                --   of the track.
    , track_number      :: Int -- ^ The number of the track. If an album has
                               --   several discs, the track number is the
                               --   number on the specified disc.
    , uri               :: SpotifyUri TrackSimplified -- ^ The 'SpotifyUri' for
                                                      --   the track.
    } deriving (Generic)

instance FromJSON TrackSimplified where
    parseJSON = genericParseJSON defaultOptions
instance ToJSON TrackSimplified where
    toJSON = genericToJSON defaultOptions
