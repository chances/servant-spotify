{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Spotify.Api.Types.Album where

import           Data.Aeson                                 (FromJSON (parseJSON),
                                                             (.:), (.=))
import           Data.Aeson                                 as A
import           Data.ISO3166_CountryCodes                  (CountryCode)
import           Data.Text                                  (Text, pack, unpack)
import           GHC.Generics                               (Generic)

import           Network.Spotify.Api.Types.ArtistSimplified (ArtistSimplified)
import           Network.Spotify.Api.Types.ExternalId       (ExternalId)
import           Network.Spotify.Api.Types.Image            (Image)
import           Network.Spotify.Api.Types.Paging           (Paging)
import           Network.Spotify.Api.Types.SpotifyUri       (SpotifyUri)
import           Network.Spotify.Api.Types.SpotifyUrl       (SpotifyUrl)
import           Network.Spotify.Api.Types.TrackSimplified  (TrackSimplified)
import           Network.Spotify.Internal.Utils

-- | Information about an album.
data Album = Album
    { album_type             :: AlbumType -- ^ The type of the album.
    , artists                :: [ArtistSimplified] -- ^ The artists of the
                                                   --   album.
                                                   --   .
                                                   --   Each artist object
                                                   --   includes a link in href
                                                   --   to more detailed
                                                   --   information about the
                                                   --   artist.
    , available_markets      :: [CountryCode] -- ^ The markets in which the
                                              --   album is available: ISO
                                              --   3166-1 alpha-2 country codes.
                                              --   .
                                              --   Note that an album is
                                              --   considered available in a
                                              --   market when at least 1 of
                                              --   its tracks is available in
                                              --   that market.
    , copyrights             :: [Copyright] -- ^ The copyright statements of
                                            --   the album.
    , external_ids           :: ExternalId -- ^ Known external IDs for the
                                           --   album.
    , external_urls          :: SpotifyUrl -- ^ Known external URLs for this
                                           --   album.
    , genres                 :: [Text] -- ^ A list of the genres used to
                                       --   classify the album. For example:
                                       --   "Prog Rock", "Post-Grunge". (If not
                                       --   yet classified, the array is empty.)
    , href                   :: Text -- ^ A link to the Web API endpoint
                                     --   providing full details of the album.
    , id                     :: Text -- ^ The Spotify ID for the album.
    , images                 :: [Image] -- ^ The cover art for the album in
                                        --   various sizes, widest first.
    , label                  :: Text -- ^ The label for the album.
    , name                   :: Text -- ^ The name of the album. In case of an
                                     --   album takedown, the value may be an
                                     --   empty string.
    , popularity             :: Int -- ^ The popularity of the album. The value
                                    --   will be between 0 and 100, with 100
                                    --   being the most popular. The popularity
                                    --   is calculated from the popularity of
                                    --   the album's individual tracks.
    , release_date           :: Text -- ^ The date the album was first released,
                                     --   for example "1981-12-15". Depending
                                     --   on the precision, it might be shown
                                     --   as "1981" or "1981-12".
    , release_date_precision :: ReleaseDatePrecision -- ^ The precision with
                                                     --   which the release
                                                     --   date is known.
    , tracks                 :: Paging [TrackSimplified] -- ^ The tracks of the
                                                         --   album.
    , uri                    :: SpotifyUri Album -- ^ The 'SpotifyUri' for the
                                                 --   album.
    } deriving (Generic)

instance A.FromJSON Album where
    parseJSON = A.genericParseJSON A.defaultOptions
instance A.ToJSON Album where
    toJSON = A.genericToJSON A.defaultOptions

data AlbumType =
      FullAlbum
    | Single
    | Compilation
    deriving Generic

instance A.FromJSON AlbumType where
    parseJSON = A.genericParseJSON A.defaultOptions
instance A.ToJSON AlbumType where
    toJSON = A.genericToJSON A.defaultOptions

data Copyright = Copyright
    { text  :: Text -- ^ The copyright text for an album.
    , type_ :: Text -- ^ The type of copyright: C = the copyright, P = the
                    --   sound recording (performance) copyright.
    }

instance A.FromJSON Copyright where
    parseJSON = A.withObject "Copyright" $ \o -> Copyright
        <$> o .: "text"
        <*> o .: "type"

instance A.ToJSON Copyright where
  toJSON o = A.object
    [ "text" .= text o
    , "type" .= type_ o
    ]

data ReleaseDatePrecision =
      Year
    | Month
    | Day

instance Show ReleaseDatePrecision where
    show precision = case precision of
        Year  -> "year"
        Month -> "month"
        Day   -> "day"

instance Read ReleaseDatePrecision where
    readsPrec _ str = wrap str $ \precision ->
        case precision of
            "year"  -> Year
            "month" -> Month
            "day"   -> Day
            _       -> Year -- This should never happen...

instance A.FromJSON ReleaseDatePrecision where
    parseJSON = A.withText "Release Date Precision" $ \str ->
        let precision = read $ unpack str :: ReleaseDatePrecision
            in return precision

instance A.ToJSON ReleaseDatePrecision where
    toJSON precision = A.String $ pack (show precision)
