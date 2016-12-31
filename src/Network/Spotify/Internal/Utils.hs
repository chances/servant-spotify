module Network.Spotify.Internal.Utils
    ( doOmitNothingFields
    , spotifyApiBaseUrl
    , wrap
    ) where

import           Data.Aeson       (defaultOptions)
import           Data.Aeson.Types (Options (omitNothingFields))
import           Servant.Client   (BaseUrl (..), Scheme (Https))

doOmitNothingFields :: Options
doOmitNothingFields = defaultOptions { omitNothingFields = True }

spotifyApiBaseUrl :: BaseUrl
spotifyApiBaseUrl = BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "api.spotify.com"
    , baseUrlPort = 443
    , baseUrlPath = "/v1"
    }

wrap :: String -> (String -> a) -> [(a, String)]
wrap str convert = [(convert str, "")]
