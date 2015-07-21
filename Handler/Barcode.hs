module Handler.Barcode where

import Import
import Handler.Common
import qualified Data.Text as T

getHomeBarcodeR :: Handler Html
getHomeBarcodeR = do
  c <- lookupGetParam "barcode"
  case c of
    Just code -> do
      be <- runDB $ getBy $ UniqueBarcode code
      case be of
        Just (Entity _ bar) -> do
          case barcodeIsUser bar of
            True -> do
              case barcodeUser bar of
                Just uId -> do
                  redirect $ SelectR uId
                Nothing -> do
                  error "Malformed barcode"
            False -> do
              setMessageI MsgBarcodeNotUser
              redirect HomeR
        Nothing -> do
          setMessageI MsgBarcodeUnknown
          redirect HomeR
    Nothing -> do
      setMessageI MsgProvideBarcode
      redirect HomeR

getSelectBarcodeR :: UserId -> Handler Html
getSelectBarcodeR uId = do
  c <- lookupGetParam "barcode"
  case c of
    Just code -> do
      be <- runDB $ getBy $ UniqueBarcode code
      case be of
        Just (Entity _ bar) -> do
          case barcodeIsUser bar of
            False -> do
              case barcodeBev bar of
                Just bId -> do
                  redirect $ BuyR uId bId
                Nothing -> do
                  error "Malformed barcode"
            True -> do
              setMessageI MsgBarcodeNotBev
              redirect $ SelectR uId
        Nothing -> do
          setMessageI MsgBarcodeUnknown
          redirect $ SelectR uId
    Nothing -> do
      setMessageI MsgProvideBarcode
      redirect $ SelectR uId
