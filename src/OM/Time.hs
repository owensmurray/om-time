{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | Basic missing time utilities. -}
module OM.Time (
  Time(..),
) where


import Data.Aeson (ToJSON)
import Data.Binary (Binary, get, put)
import Data.Proxy (Proxy(Proxy))
import Data.Swagger (ToSchema, declareNamedSchema)
import Data.Time (UTCTime(UTCTime), Day(ModifiedJulianDay))


{- | Wrapper around 'UTCTime', used mainly to provide a 'Binary' instance. -}
newtype Time = Time {
    unTime :: UTCTime
  }
  deriving (Eq, Ord, ToJSON)
instance Show Time where
  showsPrec n = showsPrec n . unTime
instance Binary Time where
  put (Time (UTCTime (ModifiedJulianDay day) tod)) = 
    put (day, toRational tod)
  get = do
    (day, tod) <- get
    return (Time (UTCTime (ModifiedJulianDay day) (fromRational tod)))
instance ToSchema Time where
  declareNamedSchema _proxy = declareNamedSchema (Proxy :: Proxy UTCTime)

