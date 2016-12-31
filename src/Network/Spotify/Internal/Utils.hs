module Network.Spotify.Internal.Utils
    ( doOmitNothingFields
    , spotifyApiBaseUrl
    , wrap
    ) where

import           Data.Aeson       (defaultOptions)
import           Data.Aeson.Types (Options (omitNothingFields))
import           Servant.Client   (BaseUrl (..), Scheme (Https))

-- | Omit 'Nothing' fields when encoding/decoding JSON
doOmitNothingFields :: Options
doOmitNothingFields = defaultOptions { omitNothingFields = True }

-- | Base URL for v1 of the Spotify Web API
spotifyApiBaseUrl :: BaseUrl
spotifyApiBaseUrl = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "api.spotify.com"
    , baseUrlPort = 443
    , baseUrlPath = "/v1"
    }

wrap :: String -> (String -> a) -> [(a, String)]
wrap str convert = [(convert str, "")]
