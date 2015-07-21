module Handler.Barcode where

import Import

getHomeBarcodeR :: Handler Html
getHomeBarcodeR = do
  eub <- handleSelectParam
  case eub of
    Just (Left uId) -> do
      redirect $ SelectR uId
    Just (Right _) -> do
      setMessageI MsgBarcodeNotUser
      redirect $ HomeR
    Nothing -> do
      redirect $ HomeR

getSelectBarcodeR :: UserId -> Handler Html
getSelectBarcodeR uId = do
  eub <- handleSelectParam
  case eub of
    Just (Right bId) -> do
      redirect $ BuyR uId bId
    Just (Left _) -> do
      setMessageI MsgBarcodeNotBev
      redirect $ SelectR uId
    Nothing ->
      redirect $ SelectR uId

getSelectCashBarcodeR :: Handler Html
getSelectCashBarcodeR = do
  eub <- handleSelectParam
  case eub of
    Just (Right bId) -> do
      redirect $ BuyCashR bId
    Just (Left _) -> do
      setMessageI MsgBarcodeNotBev
      redirect $ SelectCashR
    Nothing -> do
      redirect $ SelectCashR

handleSelectParam :: Handler (Maybe (Either UserId BeverageId))
handleSelectParam = do
  c <- lookupGetParam "barcode"
  case c of
    Just code -> do
      be <- runDB $ getBy $ UniqueBarcode code
      case be of
        Just (Entity _ bar) -> do
          case barcodeIsUser bar of
            True -> do
              case (barcodeUser bar, barcodeBev bar) of
                (Just uId, Nothing) ->
                  return $ Just $ Left uId
                _ ->
                  error "Malformed barcode"
            False -> do
              case (barcodeBev bar, barcodeUser bar) of
                (Just bId, Nothing) ->
                  return $ Just $ Right bId
                _ ->
                  error "Malformed barcode"
        Nothing -> do
          setMessageI MsgBarcodeUnknown
          return Nothing
    Nothing -> do
      setMessageI MsgProvideBarcode
      return Nothing
