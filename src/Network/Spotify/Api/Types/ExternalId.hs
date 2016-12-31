{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.Spotify.Api.Types.ExternalId
Description : External ID for some Spotify Data.
Stability   : experimental
-}
module Network.Spotify.Api.Types.ExternalId where

import           Data.Aeson          (FromJSON (parseJSON), ToJSON, object,
                                      toJSON, withObject, (.=))
import           Data.Aeson.Types    (Pair, Parser, Value)
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text, pack, unpack)

-- | An external ID for some object.
data ExternalId = ExternalId
    { externalIds :: [ExternalIdEntry] -- ^ List of external identifiers
    }

-- | A single external identifier for some object.
data ExternalIdEntry = ExternalIdEntry
    { idType  :: ExternalIdType -- ^ The identifier type
    , idValue :: Text -- ^ An external identifier for the object.
    }

instance FromJSON ExternalId where
    parseJSON = withObject "external ID" $ \o -> do
        result <- if not (null o)
            then do
                parsedExternalIds <- parseExternalIds o
                return $ Right ExternalId
                    { externalIds = parsedExternalIds
                    }
            else return $ Left "no fields"
        case result of
            Left err         -> fail err
            Right exteranlId -> return exteranlId

parseExternalIds :: HM.HashMap Text Value -> Parser [ExternalIdEntry]
parseExternalIds obj =
    mapM parseExternalIdEntry (HM.toList obj) where
        parseExternalIdEntry :: (Text, Value) -> Parser ExternalIdEntry
        parseExternalIdEntry (key, val) = do
            parsedValue <- parseJSON val
            return ExternalIdEntry
                { idType = externalIdTypeFromText key
                , idValue = parsedValue
                }

instance ToJSON ExternalId where
    toJSON p = object $ colateExternalIds (externalIds p)

colateExternalIds :: [ExternalIdEntry] -> [Pair]
colateExternalIds entries =
    let headEntry = head entries
        type_ = pack $ show (idType headEntry)
        value = idValue headEntry
        entry = type_ .= value
        in entry : colateExternalIds (tail entries)

-- | The type of an external ID for some object.
data ExternalIdType =
      ISRC -- ^ International Standard Recording Code
    | EAN -- ^ International Article Number
    | UPC -- ^ Universal Product Code
    | Other Text -- ^ Some other exteranl ID for the object.

instance Show ExternalIdType where
    show externalIdType = case externalIdType of
        ISRC              -> "isrc"
        EAN               -> "ean"
        UPC               -> "upc"
        Other otherIdType -> unpack otherIdType

externalIdTypeFromText :: Text -> ExternalIdType
externalIdTypeFromText externalIdType = case externalIdType of
    "isrc"      -> ISRC
    "ean"       -> EAN
    "upc"       -> UPC
    otherIdType -> Other otherIdType
