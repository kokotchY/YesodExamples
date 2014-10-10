{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
import Network.HTTP.Types (status200)
import Network.Wai (responseBuilder)
import Network.Wai.Handler.Warp (run)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import qualified Text.Blaze.Html5 as H
import Yesod.Core (Html, RenderRoute (..), Yesod, YesodDispatch (..), toWaiApp)
import Yesod.Core.Types (YesodRunnerEnv (..))

data App = App {
    welcomeMessage :: !Html
}

instance Yesod App

instance RenderRoute App where
    data Route App = HomeR
        deriving (Show, Read, Eq, Ord)

    renderRoute HomeR = ( [], [] )

instance YesodDispatch App where
    yesodDispatch (YesodRunnerEnv _logger site _sessionBackend) _req sendResponse =
        sendResponse $ responseBuilder
            status200
            [("Content-Type", "text/html")]
            (renderHtmlBuilder $ welcomeMessage site)

main :: IO ()
main = do
    let welcome = H.p "Welcome to Yesod!"
    waiApp <- toWaiApp App {
        welcomeMessage = welcome
    }
    run 3000 waiApp
