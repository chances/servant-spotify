{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Spotify.Api.Types.Paging
Description : Spotify Paging object for pages of data.
Stability   : experimental
-}
module Network.Spotify.Api.Types.Paging where

import           Data.Aeson   (FromJSON (parseJSON), ToJSON, defaultOptions,
                               genericParseJSON, genericToJSON, toJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- | A Spotify Paging object for pages of data.
data Paging a = Paging
    { href     :: Text -- ^ A link to the Web API endpoint returning the full
                       --   result of the request.
    , items    :: a -- ^ The requested data.
    , limit    :: Int -- ^ The maximum number of items in the response
                      --   (as set in the query or by default).
    , next     :: Maybe Text -- ^ URL to the next page of items. ('Nothing' if
                             --   none)
    , offset   :: Int -- ^ The offset of the items returned (as set in the
                      --   query or by default).
    , previous :: Maybe Text -- ^ URL to the previous page of items. ('Nothing'
                             --   if none)
    , total    :: Int -- ^ The total number of items available to return.
    } deriving (Generic)

instance FromJSON a => FromJSON (Paging a) where
    parseJSON = genericParseJSON defaultOptions
instance ToJSON a => ToJSON (Paging a) where
    toJSON = genericToJSON defaultOptions
