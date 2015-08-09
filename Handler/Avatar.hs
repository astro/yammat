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
import Data.Conduit.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Graphics.ImageMagick.MagickWand

getAvatarR :: Handler Html
getAvatarR = do
  avatars <- runDB $ selectList [] [Asc AvatarIdent]
  defaultLayout $ do
    $(widgetFile "avatars")

getNewAvatarR :: Handler Html
getNewAvatarR = do
  (newAvatarWidget, enctype) <- generateFormPost $ avatarNewForm
  defaultLayout $ do
    $(widgetFile "newAvatar")

postNewAvatarR :: Handler Html
postNewAvatarR = do
  ((res, _), _) <- runFormPost $ avatarNewForm
  case res of
    FormSuccess na -> do
      raw <- runResourceT $ fileSource (avatarNewFile na) $$ sinkLbs
      thumb <- generateThumb $ B.concat $ L.toChunks raw
      now <- liftIO $ getCurrentTime
      runDB $ insert_ $ Avatar (avatarNewIdent na) thumb now
      setMessageI MsgAvatarUploadSuccessfull
      redirect $ HomeR
    _ -> do
      setMessageI MsgErrorOccured
      redirect $ NewAvatarR

avatarNewForm :: Form AvatarNew
avatarNewForm = renderDivs $ AvatarNew
  <$> areq textField (fieldSettingsLabel MsgAvatarIdent) Nothing
  <*> areq fileField (fieldSettingsLabel MsgAvatarFile) Nothing

data AvatarNew = AvatarNew
  { avatarNewIdent :: Text
  , avatarNewFile :: FileInfo
  }

getModifyAvatarR :: AvatarId -> Handler Html
getModifyAvatarR aId = do
  ma <- runDB $ get aId
  case ma of
    Just avatar -> do
      (avatarModifyWidget, enctype) <- generateFormPost $ avatarModForm avatar
      defaultLayout $ do
        $(widgetFile "modifyAvatar")
    Nothing -> do
      setMessageI MsgAvatarUnknown
      redirect $ AvatarR

postModifyAvatarR :: AvatarId -> Handler Html
postModifyAvatarR aId = do
  ma <- runDB $ get aId
  case ma of
    Just avatar -> do
      ((res, _), _) <- runFormPost $ avatarModForm avatar
      case res of
        FormSuccess md -> do
          updateAvatar aId md
          setMessageI MsgAvatarUpdateSuccessfull
          redirect $ AvatarR
        _ -> do
          setMessageI MsgErrorOccured
          redirect $ ModifyAvatarR aId
    Nothing -> do
      setMessageI MsgAvatarUnknown
      redirect $ HomeR

avatarModForm :: Avatar -> Form AvatarMod
avatarModForm a = renderDivs $ AvatarMod
  <$> areq textField (fieldSettingsLabel MsgAvatarIdent) (Just $ avatarIdent a)
  <*> aopt fileField (fieldSettingsLabel MsgAvatarFileChange) Nothing

data AvatarMod = AvatarMod
  { avatarModIdent :: Text
  , avatarModFile :: Maybe FileInfo
  }

updateAvatar :: AvatarId -> AvatarMod -> Handler ()
updateAvatar aId (AvatarMod ident Nothing) = do
  runDB $ update aId [AvatarIdent =. ident]
updateAvatar aId (AvatarMod ident (Just fi)) = do
  raw <- runResourceT $ fileSource fi $$ sinkLbs
  thumb <- generateThumb $ B.concat $ L.toChunks raw
  runDB $ update aId
    [ AvatarIdent =. ident
    , AvatarData =. thumb
    ]

generateThumb :: ByteString -> Handler ByteString
generateThumb raw = do
  thumb <- liftIO $ withMagickWandGenesis $ do
    (_, w) <- magickWand
    readImageBlob w raw
    w1 <- getImageWidth w
    h1 <- getImageHeight w
    h2 <- return 140
    w2 <- return $ floor (((fromIntegral w1) / (fromIntegral h1)) * (fromIntegral h2) :: Double)
    resizeImage w w2 h2 lanczosFilter 1
    setImageCompressionQuality w 95
    setImageFormat w "png"
    getImageBlob w
  return thumb

getGetAvatarR :: AvatarId -> Handler TypedContent
getGetAvatarR aId = do
  avatar <- runDB $ get404 aId
  return $ TypedContent typePng $ toContent $ avatarData avatar

getAvatarDeleteR :: AvatarId -> Handler Html
getAvatarDeleteR aId = do
  ma <- runDB $ get aId
  case ma of
    Just _ -> do
      c <- runDB $ selectList [UserAvatar ==. Just aId] []
      d <- runDB $ selectList [BeverageAvatar ==. Just aId] []
      case null c && null d of
        True -> do
          runDB $ delete aId
          setMessageI MsgAvatarDeleted
          redirect $ HomeR
        False -> do
          setMessageI MsgAvatarInUseError
          redirect $ AvatarR
    Nothing -> do
      setMessageI MsgAvatarUnknown
      redirect $ AvatarR
