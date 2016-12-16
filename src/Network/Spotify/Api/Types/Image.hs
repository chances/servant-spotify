{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Spotify.Api.Types.Image where

import           Data.Aeson                     (FromJSON (parseJSON), ToJSON,
                                                 genericParseJSON,
                                                 genericToJSON, toJSON)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)

import           Network.Spotify.Internal.Utils

-- | Information about the followers of the user.
data Image = Image
    { url    :: Text -- | The source URL of the image.
    , width  :: Maybe Int -- | The image width in pixels. If unknown: null or
                          --   not returned.
    , height :: Maybe Int -- | The image height in pixels. If unknown: null or
                          --   not returned.
    } deriving (Generic)

instance FromJSON Image where
    parseJSON = genericParseJSON doOmitNothingFields
instance ToJSON Image where
    toJSON = genericToJSON doOmitNothingFields