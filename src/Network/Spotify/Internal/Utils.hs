module Network.Spotify.Internal.Utils
    ( doOmitNothingFields
    , spotifyApiBaseUrl
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
