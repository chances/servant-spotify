{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Spotify.Api.Types.User where

import           Data.Aeson                           (FromJSON (parseJSON),
                                                       ToJSON, genericToJSON,
                                                       toJSON, withObject, (.:),
                                                       (.:?))
import           Data.ISO3166_CountryCodes            (CountryCode)
import           Data.Text                            (Text)
import           GHC.Generics                         (Generic)

import           Network.Spotify.Api.Types.Followers  (Followers)
import           Network.Spotify.Api.Types.Image      (Image)
import           Network.Spotify.Api.Types.SpotifyUri (SpotifyUri)
import           Network.Spotify.Api.Types.SpotifyUrl (SpotifyUrl)
import           Network.Spotify.Internal.Utils

-- | A Spotify user.
--   .
--   @note Certain values are Nothing depending on the scope granted to the
--   request the retreives the 'User'.
data User = User
    { birthdate     :: Maybe Text -- | The user's date-of-birth.
                                  --   .
                                  --   Only available when granted the
                                  --   'userReadBirthdate' 'Scope'.
    , country       :: Maybe CountryCode -- | The country of the user, as set
                                         --   in the user's account profile.
                                         --   .
                                         --   An ISO 3166-1 alpha-2 country
                                         --   code.
                                         --   .
                                         --   Only available when granted the
                                         --   'userReadPrivate' 'Scope'.
    , display_name  :: Maybe Text -- | The name displayed on the user's profile.
                                  --   .
                                  --   'Nothing' if not available.
    , email         :: Maybe Text -- | The user's email address, as entered by
                                  --   the user when creating their account.
                                  --   .
                                  --   Important! This email address is
                                  --   unverified.
                                  --   .
                                  --   Only available when granted the
                                  --   'userReadPrivate' 'Scope'.
    , external_urls :: SpotifyUrl -- | Known external URLs for this user.
    , followers     :: Followers -- | Information about the followers of the
                                 --   user.
    -- IDEA: Generalize href for reuse across library to get other resources
    , href          :: Text -- | A link to the Web API endpoint for this user.
    , id            :: Text -- | The Spotify user ID for the user.
    , images        :: [Image] -- | The user's profile image.
    , product       :: Maybe Text -- | The user's Spotify subscription level:
                                  --   "premium", "free", etc. (The subscription
                                  --   level "open" can be considered the same
                                  --   as "free".)
                                  --   .
                                  --   Only available when granted the
                                  --   'userReadPrivate' 'Scope'.
    , uri           :: SpotifyUri User -- | The 'SpotifyUri' for the user.
    } deriving (Generic)

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> User
        <$> o .:? "birthdate"
        <*> o .:? "country"
        <*> o .: "display_name"
        <*> o .:? "email"
        <*> o .: "external_urls"
        <*> o .: "followers"
        <*> o .: "href"
        <*> o .: "id"
        <*> o .: "images"
        <*> o .:? "product"
        <*> o .: "uri"
instance ToJSON User where
    toJSON = genericToJSON doOmitNothingFields

instance FromJSON CountryCode
instance ToJSON CountryCode
