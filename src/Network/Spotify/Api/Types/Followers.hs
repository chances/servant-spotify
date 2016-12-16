{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Spotify.Api.Types.Followers where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- | Information about the followers of the user.
data Followers = Followers
    { href  :: Maybe Text -- | A link to the Web API endpoint providing full
                          --   details of the followers; null if not available.
                          --   .
                          --   Please note that this will always be set to null,
                          --   as the Web API does not support it at the moment.
    , total :: Int -- | The total number of followers.
    } deriving (Generic)

instance FromJSON Followers
instance ToJSON Followers
