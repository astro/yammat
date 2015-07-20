module Handler.Barcode where

import Import
import Handler.Common
import qualified Data.Text as T
import Text.Blaze.Internal
import Text.Shakespeare.Text

getBarcodeR :: UserId -> Text -> Handler Html
getBarcodeR uId barcode = undefined
