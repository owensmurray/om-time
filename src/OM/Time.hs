{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Basic missing time utilities. -}
module OM.Time (
  MonadTimeSpec(..),
  Time(..),
) where

import Control.Lens ((&), (?~))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Binary (Binary, get, put)
import Data.Proxy (Proxy(Proxy))
import Data.Swagger (NamedSchema(NamedSchema), ToSchema,
  declareNamedSchema, description)
import Data.Time (Day(ModifiedJulianDay), UTCTime(UTCTime))
import OM.JSON (schemaFor)
import System.Clock (TimeSpec)
import qualified System.Clock as Clock


{- | Wrapper around 'UTCTime', used mainly to provide a 'Binary' instance. -}
newtype Time = Time {
    unTime :: UTCTime
  }
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
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


{- | A monad that can produce the current time as a TimeSpec. -}
class (Monad m) => MonadTimeSpec m where
  getTime :: m TimeSpec

{- | The IO instances uses 'Clock.getTime' 'Clock.MonotonicCoarse'. -}
instance MonadTimeSpec IO where
  getTime = Clock.getTime Clock.MonotonicCoarse

instance {-# OVERLAPPABLE #-}
    ( Monad (t m)
    , MonadTimeSpec m
    , MonadTrans t
    )
  =>
    MonadTimeSpec (t m)
  where
    getTime = lift getTime


