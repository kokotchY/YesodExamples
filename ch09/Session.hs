{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses #-}
import Control.Applicative ((<$>), (<*>))
import qualified Web.ClientSession as CS
import Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET POST
/set-message SetMessageR POST
|]

instance Yesod App where
    makeSessionBackend _ = do
        backend <- defaultClientSessionBackend 1 "keyfile.aes"
        return $ Just backend
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        mmsg <- getMessage
        withUrlRenderer [hamlet|
$doctype 5
<html>
    <head>
        <title>#{pageTitle pc}
        ^{pageHead pc}
    <body>
        $maybe msg <- mmsg
            <p>Your message was: #{msg}
        ^{pageBody pc}
|]


instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
    sess <- getSession
    defaultLayout [whamlet|
    <h1>#{show sess}
    <form method=post>
        <input type=text name=key>
        <input type=text name=val>
        <input type=submit>
    <hr>
    <form method=post action=@{SetMessageR}>
        My message is: #
        <input type=text name=message>
        <button>Go
|]

postHomeR :: Handler ()
postHomeR = do
    (key, mval) <- runInputPost $ (,) <$> ireq textField "key" <*> iopt textField "val"
    case mval of
        Nothing -> deleteSession key
        Just val -> setSession key val
    liftIO $ print (key, mval)
    redirect HomeR

postSetMessageR :: Handler ()
postSetMessageR = do
    msg <- runInputPost $ ireq textField "message"
    setMessage $ toHtml msg
    redirect HomeR

main :: IO ()
main = warp 3000 App
