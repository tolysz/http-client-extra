{-# Language OverloadedStrings, LambdaCase
           , ExistentialQuantification #-}

module Network.HTTP.ClientExtra
 ( -- RequestType (..)
   QueryE (..)
 , ToQueryE (..)
 , RequestHeadersE (..)
 , HH.RequestHeaders
 --, Part (..)
 , methodBSL
 , methodJSON
 , fromQueryE
 , fromQueryE'
 , EResp
 , Method
 ) where

import Network.HTTP.Client
 
import Network.HTTP.ClientExtra.Multipart()
import Network.HTTP.ClientExtra.Types
import Network.HTTP.Types.Method (Method(..))
import qualified Network.HTTP.Types.Header as HH
import qualified Network.HTTP.Types.Status as HS

import qualified Data.Aeson as DA

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow (..))

import Data.Monoid
import Data.Either
import Prelude

-- import Debug.Trace

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8


type EResp k = Either (BSL.ByteString, CookieJar, HH.ResponseHeaders, Int) (k, CookieJar, HH.ResponseHeaders, Int)

methodBSL :: (MonadIO m, ContentEncoder m b, MonadThrow m) => Manager -> Method -> Maybe CookieJar -> String -> QueryE -> RequestHeadersE -> b -> m (EResp BSL.ByteString)
methodBSL manager m j url extraQuery extraHeaders reqBody = do
   initReq <- parseUrlThrow url
   (bb,eh) <- buildBody reqBody
   let req = initReq
              { method = m
              , requestHeaders = unRequestHeaders $ eh <> extraHeaders
              , queryString = fromQueryE . (<> extraQuery) . toQueryE . queryString $ initReq
              , requestBody = bb
              , cookieJar=j
--              , checkStatus = \_ _ _ ->  Nothing -- cc (checkStatus initReq)
              }
   liftIO $ withResponse req manager $ \rb' -> do
          let cj = responseCookieJar rb'
              rh = responseHeaders rb'
          rb <- BSL.fromChunks <$> brConsume (responseBody rb')
          return $ case HS.statusCode (responseStatus rb') of
              200 -> Right (rb, cj, rh, 200)
              201 -> Right (rb, cj, rh, 201)
              202 -> Right (rb, cj, rh, 202)
              s   -> Left  (rb, cj, rh, s )
          --  Status ResponseHeaders CookieJar

methodJSON :: (MonadIO m, ContentEncoder m b, MonadThrow m, Functor m, DA.FromJSON a) => Manager -> Method -> Maybe CookieJar -> String -> QueryE -> RequestHeadersE -> b -> m (EResp a)
methodJSON a b c d e f g = methodBSL a b c d e f g >>= return . \case
     Left  err             -> Left err
     Right r@(a1,b1,c1,d1) -> case DA.eitherDecode a1 of
                    Left _err -> Left r
                    Right v1  -> Right (v1,b1,c1,d1)
