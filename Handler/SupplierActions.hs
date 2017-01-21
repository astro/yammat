module Handler.SupplierActions where

import Import
import Handler.Common
import qualified Data.Text as T
import Text.Blaze

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
        <p>&nbsp;
        <table>
          <thead>
            <tr>
              <th>
                <span .transp>|
                _{MsgArtNr}
                <span .transp>|
              <th>
                _{MsgName}
                <span .transp>|
              <th>
                _{MsgVolume}
                <span .transp>|
              <th>
                _{MsgCrateCount}
                <span .transp>|
              <th>
                _{MsgPricePerCrate}
                <span .transp>|
              <th>
                _{MsgTotalValue}
                <span .transp>|
            <tr>
                <span .transp>---:
              <th>
                <span .transp>---:
              <th>
                <span .transp>---:
              <th>
                <span .transp>---:
              <th>
                <span .transp>---:
              <th>
                <span .transp>---:
            
          $forall dig <- digests
            $if bdCrates dig /= 0
              <tr>
                <td>
                  <span .transp>|
                  #{fromMaybe "" $ beverageArtNr $ bdBev dig}
                  <span .transp>|
                <td>#{beverageIdent $ bdBev dig}
                  <span .transp>|
                <td>#{formatIntVolume $ beverageMl $ bdBev dig}
                  <span .transp>|
                <td>#{T.pack $ show $ bdCrates dig}
                  <span .transp>|
                <td style="text-align: right;">#{formatIntCurrency $ fromMaybe 0 $ beveragePricePerCrate $ bdBev dig} #{appCurrency $ appSettings master}
                  <span .transp>|
                <td style="text-align: right;">#{formatIntCurrency $ bdTotal dig} #{appCurrency $ appSettings master}
                  <span .transp>|
          <tr>
            <td colspan="3">
              <span .transp>
                |&nbsp;
                |_
              {MsgTotalCrates}
              <span .transp>
                |&nbsp;
              <span .transp>|
            <td>#{T.pack $ show $ sum $ map bdCrates digests}
              <span .transp>|
            <td>_{MsgBuyValue}
              <span .transp>|
            <td style="text-align: right;">#{formatIntCurrency $ sum $ map bdTotal digests} #{appCurrency $ appSettings master}
              <span .transp>|
        |]
      tableLayout w
    Nothing -> do
      setMessageI MsgSupplierUnknown
      redirect SupplierR

-- tableLayout :: Widget -> WidgetT site0 IO ()
tableLayout :: WidgetT App IO () -> HandlerT App IO Markup
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
    Just _ -> do
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
