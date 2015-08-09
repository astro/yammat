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
