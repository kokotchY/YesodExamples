{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
import Data.Default (def)
import Data.Text (Text)
import Network.HTTP.Conduit (Manager, conduitManagerSettings, newManager)
import Yesod
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail

data App = App {
    httpManager :: Manager
}

mkYesod "App" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
    approot = ApprootStatic "http://localhost:3000"

instance YesodAuth App where
    type AuthId App = Text
    getAuthId = return . Just . credsIdent

    loginDest _ = HomeR
    logoutDest _ = HomeR

    authPlugins _ = [
        authBrowserId def,
        authGoogleEmail
        ]

    authHttpManager = httpManager
    maybeAuthId = lookupSession "_ID"

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout [whamlet|
    <p>You current auth ID: #{show maid}
    $maybe _ <- maid
        <p>
            <a href=@{AuthR LogoutR}>Logout
    $nothing
        <p>
            <a href=@{AuthR LoginR}>Go to the login page
    |]

main :: IO ()
main = do
    man <- newManager conduitManagerSettings
    warp 3000 $ App man
