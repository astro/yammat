--  yammat - Yet Another MateMAT
--  Copyright (C) 2015  Amedeo Molnár
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published
--  by the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
module Handler.Avatar where

import Import
import Handler.Common
import Data.Conduit.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Base64
import Data.Maybe (fromJust)
import qualified Crypto.Hash.SHA3 as SHA3
import Codec.Picture
import Codec.Picture.Metadata as PM hiding (delete)
import Codec.Picture.ScaleDCT

getAvatarR :: Handler Html
getAvatarR = do
  avatars <- runDB $ selectList [] [Asc AvatarIdent]
  defaultLayout $ do
    setTitleI MsgAvatars
    $(widgetFile "avatars")

getNewAvatarR :: Handler Html
getNewAvatarR = do
  (newAvatarWidget, enctype) <- generateFormPost
    $ renderBootstrap3 BootstrapBasicForm $ avatarNewForm
  defaultLayout $ do
    setTitleI MsgNewAvatar
    $(widgetFile "newAvatar")

postNewAvatarR :: Handler Html
postNewAvatarR = do
  ((res, _), _) <- runFormPost
    $ renderBootstrap3 BootstrapBasicForm avatarNewForm
  case res of
    FormSuccess na -> do
      raw <- runResourceT $ fileSource (avatarNewFile na) $$ sinkLbs
      tdata <- generateThumb $ B.concat $ L.toChunks raw
      runDB $ insert_ $ Avatar
        (avatarNewIdent na)
        (thumbContent tdata)
        (thumbHash tdata)
      setMessageI MsgAvatarUploadSuccessfull
      redirect HomeR
    _ -> do
      setMessageI MsgErrorOccured
      redirect NewAvatarR

avatarNewForm :: AForm Handler AvatarNew
avatarNewForm = AvatarNew
  <$> areq textField (bfs MsgAvatarIdent) Nothing
  <*> areq fileField (bfs MsgAvatarFile) Nothing
  <*  bootstrapSubmit (msgToBSSubmit MsgSubmit)

data AvatarNew = AvatarNew
  { avatarNewIdent :: Text
  , avatarNewFile :: FileInfo
  }

getModifyAvatarR :: AvatarId -> Handler Html
getModifyAvatarR aId = do
  ma <- runDB $ get aId
  case ma of
    Just avatar -> do
      (avatarModifyWidget, enctype) <- generateFormPost
        $ renderBootstrap3 BootstrapBasicForm
        $ avatarModForm avatar
      defaultLayout $ do
        setTitleI MsgModifyAvatar
        $(widgetFile "modifyAvatar")
    Nothing -> do
      setMessageI MsgAvatarUnknown
      redirect AvatarR

postModifyAvatarR :: AvatarId -> Handler Html
postModifyAvatarR aId = do
  ma <- runDB $ get aId
  case ma of
    Just avatar -> do
      ((res, _), _) <- runFormPost
        $ renderBootstrap3 BootstrapBasicForm
        $ avatarModForm avatar
      case res of
        FormSuccess md -> do
          updateAvatar aId md
          setMessageI MsgAvatarUpdateSuccessfull
          redirect AvatarR
        _ -> do
          setMessageI MsgErrorOccured
          redirect $ ModifyAvatarR aId
    Nothing -> do
      setMessageI MsgAvatarUnknown
      redirect HomeR

avatarModForm :: Avatar -> AForm Handler AvatarMod
avatarModForm a = AvatarMod
  <$> areq textField (bfs MsgAvatarIdent) (Just $ avatarIdent a)
  <*> aopt fileField (bfs MsgAvatarFileChange) Nothing
  <*  bootstrapSubmit (msgToBSSubmit MsgSubmit)

data AvatarMod = AvatarMod
  { avatarModIdent :: Text
  , avatarModFile :: Maybe FileInfo
  }

updateAvatar :: AvatarId -> AvatarMod -> Handler ()
updateAvatar aId (AvatarMod ident Nothing) =
  runDB $ update aId [AvatarIdent =. ident]
updateAvatar aId (AvatarMod ident (Just fi)) = do
  raw <- runResourceT $ fileSource fi $$ sinkLbs
  tdata <- generateThumb $ B.concat $ L.toChunks raw
  runDB $ update aId
    [ AvatarIdent =. ident
    , AvatarData =. thumbContent tdata
    , AvatarHash =. thumbHash tdata
    ]

data ThumbData = ThumbData
  { thumbContent :: ByteString
  , thumbHash    :: ByteString
  }

generateThumb :: ByteString -> Handler ThumbData
generateThumb raw = do
  let eimg = decodeImageWithMetadata raw
  case eimg of
    Left e -> error e
    Right (img, meta) ->
      return $ ThumbData
        { thumbContent = ava
        , thumbHash    = h
        }
      where
        w1 = fromIntegral $ fromJust $ PM.lookup Width meta :: Int
        h1 = fromIntegral $ fromJust $ PM.lookup Height meta :: Int
        h2 = 140 :: Int
        w2 = floor ((fromIntegral w1 :: Double) / (fromIntegral h1 :: Double) * (fromIntegral h2 :: Double)) :: Int
        scimg = scale (w2, h2) $ convertRGBA8 img
        ava = (B.concat . L.toChunks) $ encodePng scimg
        h   = encode (SHA3.hash 32 ava)

getGetAvatarR :: AvatarId -> Handler TypedContent
getGetAvatarR aId = do
  avatar <- runDB $ get404 aId
  setEtag $ decodeUtf8 $ avatarHash avatar
  return $ TypedContent typePng $ toContent $ avatarData avatar

getAvatarDeleteR :: AvatarId -> Handler Html
getAvatarDeleteR aId = do
  ma <- runDB $ get aId
  case ma of
    Just _ -> do
      c <- runDB $ selectList [UserAvatar ==. Just aId] []
      d <- runDB $ selectList [BeverageAvatar ==. Just aId] []
      e <- runDB $ selectList [SupplierAvatar ==. Just aId] []
      if null c && null d && null e
        then do
          runDB $ delete aId
          setMessageI MsgAvatarDeleted
          redirect HomeR
        else do
          setMessageI MsgAvatarInUseError
          redirect AvatarR
    Nothing -> do
      setMessageI MsgAvatarUnknown
      redirect AvatarR
