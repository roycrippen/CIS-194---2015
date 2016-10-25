{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Parser (
                encodePretty
              , decode
              , Transaction(..)
              , TId
              , FromJSON(..)
              , ToJSON(..)
              ) where

import           Control.Applicative      ((<$>), (<*>))
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Monoid              (mempty)
import           GHC.Generics

type TId = String

data Transaction = Transaction { from   :: String
                               , to     :: String
                               , amount :: Integer
                               , tid    :: TId
                               }
                   deriving (Generic, Show, Eq)

instance ToJSON Transaction where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Transaction
