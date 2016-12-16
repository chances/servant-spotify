{-# LANGUAGE OverloadedStrings #-}

module Network.Spotify.Api.Types.SpotifyUri where

import           Data.Aeson                           (FromJSON (parseJSON),
                                                       ToJSON, toJSON, withText)
import           Data.List.Split                      (splitOn)
import           Data.Text                            (Text, pack, unpack)

import           Network.Spotify.Api.Types.SpotifyUrl (SpotifyUrl (..),
                                                       spotifyUrlTypeFromText)

-- | The resource identifier that you can enter, for example, in the Spotify
--   Desktop client's search box to locate an artist, album, or track.
--   .
--   e.g. `spotify:track:6rqhFgbbKwnb9MLmUQDhG6`
data SpotifyUri a = SpotifyUri
    { spotifyId    :: Text -- | The Spotify URI resource ID of the resource
                           --   pointed to by this Spotify URI
    , resourceType :: Text -- | The textual type of the resourse this URI
                           --   identifies
    , getApiLink   :: SpotifyUrl
    }

instance FromJSON (SpotifyUri a) where
    parseJSON = withText "Spotify URI" $ \str -> do
        let pieces = splitOn ":" (unpack str)
        return $ parseSpotifyUri (tail pieces)

parseSpotifyUri :: [String] -> SpotifyUri a
parseSpotifyUri typeAndId =
    case head typeAndId of
        -- "track" ->
        -- "user" ->
        _ -> SpotifyUri
            { spotifyId = pack sId
            , resourceType = pack sType
            , getApiLink = SpotifyUrl
                { urlType = spotifyUrlTypeFromText (pack sId)
                , urlValue = pack ("http://open.spotify.com/" ++ sType ++ "/" ++ sId)
                }
            } where
                sType = head typeAndId
                sId = head $ tail typeAndId

instance ToJSON (SpotifyUri a) where
    toJSON p = toJSON $ pack ("spotify:" ++ unpack (resourceType p) ++ unpack (spotifyId p))
