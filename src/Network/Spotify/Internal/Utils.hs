module Network.Spotify.Internal.Utils
    ( doOmitNothingFields
    ) where

import           Data.Aeson       (defaultOptions)
import           Data.Aeson.Types (Options (omitNothingFields))

doOmitNothingFields :: Options
doOmitNothingFields = defaultOptions { omitNothingFields = True }
