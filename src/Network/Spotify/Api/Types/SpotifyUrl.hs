{-# LANGUAGE OverloadedStrings #-}

module Network.Spotify.Api.Types.SpotifyUrl where

import           Data.Aeson          (FromJSON (parseJSON), ToJSON, object,
                                      toJSON, withObject, (.=))
import           Data.Aeson.Types    (Parser)
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text, pack, unpack)

-- | An external URL for some object.
data SpotifyUrl = SpotifyUrl
    { urlType  :: SpotifyUrlType -- | The type of the URL.
    , urlValue :: Text -- | An external, public URL for the object.
    }

instance FromJSON SpotifyUrl where
    parseJSON = withObject "external URL" $ \o -> do
        let keys = HM.keys o
        result <- if not (null keys)
            then do
                let key = head keys
                case HM.lookup key o of
                    Just value -> do
                        urlVal <- parseJSON value :: Parser Text
                        return $ Right SpotifyUrl
                            { urlType = spotifyUrlTypeFromText key
                            , urlValue = urlVal
                            }
                    Nothing ->
                      return $ Left ("no field '" ++ unpack key ++ "''")
            else return $ Left "no field"
        case result of
            Left err         -> fail err
            Right spotifyUrl -> return spotifyUrl

instance ToJSON SpotifyUrl where
    toJSON p = object [ type_ .= value ] where
        type_ = pack $ show (urlType p)
        value = urlValue p

-- | The type of an external URL for some object.
data SpotifyUrlType =
      Spotify -- | The Spotify URL for the object.
    | Other -- | Some other URL for the object.

instance Show SpotifyUrlType where
    show spotifyUrlType = case spotifyUrlType of
        Spotify -> "spotify"
        _       -> "other"

spotifyUrlTypeFromText :: Text -> SpotifyUrlType
spotifyUrlTypeFromText spotifyUrlType = case spotifyUrlType of
    "spotify" -> Spotify
    _         -> Other
