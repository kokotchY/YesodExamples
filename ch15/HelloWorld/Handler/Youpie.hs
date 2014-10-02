module Handler.Youpie where

import Import

getYoupieR :: String -> Handler Html
getYoupieR text = defaultLayout $(widgetFile "youpie")
