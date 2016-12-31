{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Spotify.Api.Types.Scope
Description : OAuth scopes for the Spotify Web API
Stability   : experimental

Scopes let you specify exactly what types of data your application wants to
access, and the set of scopes you pass in your call determines what access
permissions the user is asked to grant.
-}
module Network.Spotify.Api.Types.Scope where

import           Data.Aeson (FromJSON (parseJSON), ToJSON, toJSON, withText)
import           Data.Text  (Text, pack, unpack, words)
import           Prelude    hiding (words)
import           Servant    (ToHttpApiData (..), toQueryParam)

-- | If no scope is specified, access is permitted only to publicly available
--   information: that is, only information normally visible to normal
--   logged-in users of the Spotify desktop, web, and mobile clients
--   (e.g. public playlists).
data Scope = Scope
    { getScopes :: [Text]
    }

instance Show Scope where
    show s = unwords $ map unpack (getScopes s)

instance ToHttpApiData Scope where
    toQueryParam = pack . show

instance FromJSON Scope where
    parseJSON = withText "Scope" $ \scopes -> do
        let parsedScope = scopeFromList (words scopes)
        return parsedScope

instance ToJSON Scope where
    toJSON = toJSON . show

-- | Create a 'Scope' from a textual list of scopes
scopeFromList :: [Text] -> Scope
scopeFromList scopes = Scope { getScopes = scopes }

-- * Available Spotify Web API Scopes

-- | Read access to user's private playlists.
playlistReadPrivate :: Text
playlistReadPrivate = "playlist-read-private"

-- | Include collaborative playlists when requesting a user's playlists.
playlistReadCollaborative :: Text
playlistReadCollaborative = "playlist-read-collaborative"

-- | Write access to a user's public playlists.
playlistModifyPublic :: Text
playlistModifyPublic = "playlist-modify-public"

-- | Write access to a user's private playlists.
playlistModifyPrivate :: Text
playlistModifyPrivate = "playlist-modify-private"

-- | Control playback of a Spotify track. This scope is currently only available
--   to Spotify native SDKs (for example, the iOS SDK and the Android SDK). The
--   user must have a Spotify Premium account.
streaming :: Text
streaming = "streaming"

-- | Write/delete access to the list of artists and other users that the user follows.
userFollowModify :: Text
userFollowModify = "user-follow-modify"

-- | Read access to the list of artists and other users that the user follows.
userFollowRead :: Text
userFollowRead = "user-follow-read"

-- | Read access to a user's 'Your Music' library.
userLibraryRead :: Text
userLibraryRead = "user-library-read"

-- | Write/delete access to a user's 'Your Music' library.
userLibraryModify :: Text
userLibraryModify = "user-library-modify"

-- | Read access to user's subscription details (type of user account).
userReadPrivate :: Text
userReadPrivate = "user-read-private"

-- | Read access to the user's birthdate.
userReadBirthdate :: Text
userReadBirthdate = "user-read-birthdate"

-- | Read access to user's email address.
userReadEmail :: Text
userReadEmail = "user-read-email"

-- | Read access to a user's top artists and tracks
userTopRead :: Text
userTopRead = "user-top-read"
