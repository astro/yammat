module Handler.SupplierActions where

import Import
import Handler.Common
import qualified Data.Text as T

getSupplierActionsR :: SupplierId -> Handler Html
getSupplierActionsR sId = do
  mSup <- runDB $ get sId
  case mSup of
    Just sup ->
      defaultLayout $
        $(widgetFile "supplierActions")
    Nothing -> do
      setMessageI MsgSupplierUnknown
      redirect SupplierR

data BevDigest = BevDigest
  { bdCrates :: Int
  , bdTotal :: Int
  , bdBev :: Beverage
  }

getSupplierDigestR :: SupplierId -> Handler Html
getSupplierDigestR sId = do
  mSup <- runDB $ get sId
  case mSup of
    Just sup -> do
      master <- getYesod
      bevs <- runDB $ selectList [BeverageSupplier ==. (Just sId)] [Asc BeverageIdent]
      digests <- return $ map genBevDigest bevs
      w <- return $ [whamlet|$newline always
        <p>
          #{supplierIdent sup}<br>
          #{unTextarea $ supplierAddress sup}<br>
          #{supplierTel sup}<br>
          #{supplierEmail sup}<br>
        <hr>
        <p>
          <b>
            _{MsgCustomerId}: #{supplierCustomerId sup}
        <table>
          <thead>
            <tr>
              <th>_{MsgArtNr}
              <th>_{MsgName}
              <th>_{MsgVolume}
              <th>_{MsgCrateCount}
              <th>_{MsgPricePerCrate}
              <th>_{MsgTotalValue}
          $forall dig <- digests
            $if bdCrates dig /= 0
              <tr>
                <td>#{fromMaybe "" $ beverageArtNr $ bdBev dig}
                <td>#{beverageIdent $ bdBev dig}
                <td>#{formatIntVolume $ beverageMl $ bdBev dig}
                <td>#{T.pack $ show $ bdCrates dig}
                <td style="text-align: right;">#{formatIntCurrency $ fromMaybe 0 $ beveragePricePerCrate $ bdBev dig} #{appCurrency $ appSettings master}
                <td style="text-align: right;">#{formatIntCurrency $ bdTotal dig} #{appCurrency $ appSettings master}
          <tr>
            <td colspan="3">_{MsgTotalCrates}
            <td>#{T.pack $ show $ sum $ map bdCrates digests}
            <td>_{MsgBuyValue}
            <td style="text-align: right;">#{formatIntCurrency $ sum $ map bdTotal digests} #{appCurrency $ appSettings master}
        |]
      tableLayout w
    Nothing -> do
      setMessageI MsgSupplierUnknown
      redirect SupplierR

-- tableLayout :: Widget -> WidgetT site0 IO ()
tableLayout widget = do
  cont <- widgetToPageContent $ do
    $(combineStylesheets 'StaticR
      [ css_bootstrap_css
      , css_main_css
      ])
    widget
  withUrlRenderer [hamlet|$newline never
    $doctype 5
    <html>
      <head>
        <meta charset="UTF-8">
        ^{pageHead cont}
      <body>
        ^{pageBody cont}
    |]

genBevDigest :: Entity Beverage -> BevDigest
genBevDigest bev =
  BevDigest amount (amount * (fromMaybe 0 $ beveragePricePerCrate $ entityVal bev)) (entityVal bev)
  where
    amount =
      if ((beverageMaxAmount (entityVal bev) - beverageAmount (entityVal bev)) `div` (fromMaybe 1 $ beveragePerCrate (entityVal bev))) < 0
        then
          0
        else
          ((beverageMaxAmount (entityVal bev) - beverageAmount (entityVal bev)) `div` (fromMaybe 1 $ beveragePerCrate (entityVal bev)))


getDeleteSupplierR :: SupplierId -> Handler Html
getDeleteSupplierR sId = do
  mSup <- runDB $ get sId
  case mSup of
    Just sup -> do
      a <- runDB $ selectList [BeverageSupplier ==. (Just sId)] []
      if null a
        then do
          runDB $ delete sId
          setMessageI MsgSupplierDeleted
          redirect SupplierR
        else do
          setMessageI MsgSupplierInUseError
          redirect SupplierR
    Nothing -> do
      setMessageI MsgSupplierUnknown
      redirect SupplierR
