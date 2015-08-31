{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}

module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Http.Server
import           Control.Monad.IO.Class
import           Data.TZworld.Api
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL


main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    route [ ("location", locationHandler)
          ] <|>
       defaultHandler 


defaultHandler::Snap()
defaultHandler = sendBadRequest "The request did not include a location e.g. /location?lat=49.00&lon=-130.0"
  
locationHandler::Snap ()
locationHandler = do
  lonparam <-  getQueryParam "lon"
  latparam <-  getQueryParam "lat"
  case (latparam,lonparam) of
    (Nothing,Nothing) -> sendBadRequest "The latitude and longitude parameters are invalid"
    (Nothing,_)       -> sendBadRequest "The latitude parameter is invalid"
    (_,Nothing)       -> sendBadRequest "The longitude parameter is invalid"
    (Just la,Just lo) ->
      do
        tze <- liftIO $ handleLocation la lo
        case tze  of
          Left err    -> sendBadRequest err
          Right tzstr -> sendBackTZ tzstr
        return ()
  where 
        sendBackTZ tz = writeBS $ BL.toStrict $ encode tz

sendBadRequest::String -> Snap()
sendBadRequest s = do
          modifyResponse $ setResponseStatus 400 "Bad Request" 
          writeBS $ BL.toStrict $ encode $ Message s 

