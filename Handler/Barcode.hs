module Handler.Barcode where

import Import
import Handler.Common
import qualified Data.Text as T
import Text.Blaze.Internal
import Text.Shakespeare.Text

postHomeBarcodeR :: Handler Html
postHomeBarcodeR = return mempty

postSelectBarcodeR :: UserId -> Handler Html
postSelectBarcodeR uId = return mempty
