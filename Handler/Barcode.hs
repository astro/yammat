module Handler.Barcode where

import Import
import Handler.Common
import qualified Data.Text as T
import Text.Blaze.Internal
import Text.Shakespeare.Text

getHomeBarcodeR :: Handler Html
getHomeBarcodeR = return mempty

getSelectBarcodeR :: UserId -> Handler Html
getSelectBarcodeR uId = return mempty
