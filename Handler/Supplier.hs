module Handler.Supplier where

import Import
import Handler.Common
import Data.Maybe
import qualified Data.Text as T

getSupplierR :: Handler Html
getSupplierR = do
  sups <- runDB $ selectList [] [Asc SupplierIdent]
  defaultLayout $
    $(widgetFile "supplier")

getNewSupplierR :: Handler Html
getNewSupplierR = do
  (newSupplierWidget, enctype) <- generateFormPost
    $ renderBootstrap3 BootstrapBasicForm newSupplierForm
  defaultLayout $
    $(widgetFile "newSupplier")

postNewSupplierR :: Handler Html
postNewSupplierR = do
  ((res, _), _) <- runFormPost
    $ renderBootstrap3 BootstrapBasicForm newSupplierForm
  case res of
    FormSuccess sup -> do
      runDB $ insert_ sup
      setMessageI MsgSupplierCreated
      redirect SupplierR
    _ -> do
      setMessageI MsgSupplierNotCreated
      redirect SupplierR

newSupplierForm :: AForm Handler Supplier
newSupplierForm = Supplier
  <$> areq textField (bfs MsgName) Nothing
  <*> areq textareaField (bfs MsgAddress) Nothing
  <*> areq textField (bfs MsgTelNr) Nothing
  <*> areq emailField (bfs MsgEmail) Nothing
  <*> areq textField (bfs MsgCustomerId) Nothing
  <*> aopt (selectField avatars) (bfs MsgSelectAvatar) Nothing
  <*  bootstrapSubmit (msgToBSSubmit MsgSubmit)
  where
    avatars = do
      ents <- runDB $ selectList [] [Asc AvatarIdent]
      optionsPairs $ map (\ent -> ((avatarIdent $ entityVal ent), entityKey ent)) ents

data SupConf = SupConf
  { supConfIdent :: Text
  , supConfAddr :: Textarea
  , supConfTel :: Text
  , supConfEmail :: Text
  , supConfCustomerId :: Text
  , supConfAvatar :: Maybe AvatarId
  }

getModifySupplierR :: SupplierId -> Handler Html
getModifySupplierR sId = do
  mSup <- runDB $ get sId
  case mSup of
    Just sup -> do
      (modifySupplierWidget, enctype) <- generateFormPost
        $ renderBootstrap3 BootstrapBasicForm
        $ modifySupplierForm sup
      defaultLayout $
        $(widgetFile "modifySupplier")
    Nothing -> do
      setMessageI MsgSupplierUnknown
      redirect $ SupplierR

postModifySupplierR :: SupplierId -> Handler Html
postModifySupplierR sId = do
  mSup <- runDB $ get sId
  case mSup of
    Just sup -> do
      ((res, _), _) <- runFormPost
        $ renderBootstrap3 BootstrapBasicForm
        $ modifySupplierForm sup
      case res of
        FormSuccess msup -> do
          runDB $ update sId
            [ SupplierAddress =. supConfAddr msup
            , SupplierTel =. supConfTel msup
            , SupplierEmail =. supConfEmail msup
            , SupplierCustomerId =. supConfCustomerId msup
            , SupplierAvatar =. supConfAvatar msup
            ]
          setMessageI MsgSupplierEdited
          redirect SupplierR
        _ -> do
          setMessageI MsgSupplierNotEdited
          redirect SupplierR
    Nothing -> do
      setMessageI MsgSupplierUnknown
      redirect SupplierR

modifySupplierForm :: Supplier -> AForm Handler SupConf
modifySupplierForm sup = SupConf
  <$> areq textField (bfs MsgName) (Just $ supplierIdent sup)
  <*> areq textareaField (bfs MsgAddress) (Just $ supplierAddress sup)
  <*> areq textField (bfs MsgTelNr) (Just $ supplierTel sup)
  <*> areq textField (bfs MsgEmail) (Just $ supplierEmail sup)
  <*> areq textField (bfs MsgCustomerId) (Just $ supplierCustomerId sup)
  <*> aopt (selectField avatars) (bfs MsgSelectAvatar) (Just $ supplierAvatar sup)
  <*  bootstrapSubmit (msgToBSSubmit MsgSubmit)
  where
    master = getYesod
    avatars = do
      ents <- runDB $ selectList [] [Asc AvatarIdent]
      optionsPairs $ map (\ent -> ((avatarIdent $ entityVal ent), entityKey ent)) ents
