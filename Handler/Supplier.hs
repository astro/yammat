module Handler.Supplier where

import Import
import Data.Maybe

getSupplierR :: Handler Html
getSupplierR = do
  sups <- runDB $ selectList [] [Asc SupplierIdent]
  defaultLayout $
    $(widgetFile "supplier")

getNewSupplierR :: Handler Html
getNewSupplierR = do
  (newSupplierWidget, enctype) <- generateFormPost newSupplierForm
  defaultLayout $
    $(widgetFile "newSupplier")

postNewSupplierR :: Handler Html
postNewSupplierR = do
  ((res, _), _) <- runFormPost newSupplierForm
  case res of
    FormSuccess sup -> do
      runDB $ insert_ sup
      setMessageI MsgSupplierCreated
      redirect SupplierR
    _ -> do
      setMessageI MsgSupplierNotCreated
      redirect SupplierR

newSupplierForm :: Form Supplier
newSupplierForm = renderDivs $ Supplier
  <$> areq textField (fieldSettingsLabel MsgName) Nothing
  <*> areq textareaField (fieldSettingsLabel MsgAddress) Nothing
  <*> areq textField (fieldSettingsLabel MsgTelNr) Nothing
  <*> areq emailField (fieldSettingsLabel MsgEmail) Nothing
  <*> areq textField (fieldSettingsLabel MsgCustomerId) Nothing
  <*> aopt (selectField avatars) (fieldSettingsLabel MsgSelectAvatar) Nothing
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
      (modifySupplierWidget, enctype) <- generateFormPost $ modifySupplierForm sup
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
      ((res, _), _) <- runFormPost $ modifySupplierForm sup
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

modifySupplierForm :: Supplier -> Form SupConf
modifySupplierForm sup = renderDivs $ SupConf
  <$> areq textField (fieldSettingsLabel MsgName) (Just $ supplierIdent sup)
  <*> areq textareaField (fieldSettingsLabel MsgAddress) (Just $ supplierAddress sup)
  <*> areq textField (fieldSettingsLabel MsgTelNr) (Just $ supplierTel sup)
  <*> areq textField (fieldSettingsLabel MsgEmail) (Just $ supplierEmail sup)
  <*> areq textField (fieldSettingsLabel MsgCustomerId) (Just $ supplierCustomerId sup)
  <*> aopt (selectField avatars) (fieldSettingsLabel MsgSelectAvatar) (Just $ supplierAvatar sup)
  where
    avatars = do
      ents <- runDB $ selectList [] [Asc AvatarIdent]
      optionsPairs $ map (\ent -> ((avatarIdent $ entityVal ent), entityKey ent)) ents
