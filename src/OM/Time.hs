{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Basic missing time utilities. -}
module OM.Time (
  Time(..),
) where


import Control.Lens ((&), (?~))
import Data.Aeson (ToJSON, FromJSON)
import Data.Binary (Binary, get, put)
import Data.Proxy (Proxy(Proxy))
import Data.Swagger (ToSchema, declareNamedSchema,
   NamedSchema(NamedSchema), description)
import Data.Time (UTCTime(UTCTime), Day(ModifiedJulianDay))
import OM.JSON (schemaFor)


{- | Wrapper around 'UTCTime', used mainly to provide a 'Binary' instance. -}
newtype Time = Time {
    unTime :: UTCTime
  }
  deriving (Eq, Ord, ToJSON, FromJSON)
instance Show Time where
  showsPrec n = showsPrec n . unTime
instance Binary Time where
  put (Time (UTCTime (ModifiedJulianDay day) tod)) = 
    put (day, toRational tod)
  get = do
    (day, tod) <- get
    return (Time (UTCTime (ModifiedJulianDay day) (fromRational tod)))
instance ToSchema Time where
  declareNamedSchema _proxy = do
    schema <- schemaFor (Proxy :: Proxy UTCTime)
    return . NamedSchema Nothing $ schema
      & description ?~ "ISO-8601 time."


