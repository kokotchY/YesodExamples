{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses #-}
import Yesod
import Data.Text

data PasswordFieldApp = PasswordFieldApp

mkYesod "PasswordFieldApp" [parseRoutes|
/ RootR GET
|]

instance Yesod PasswordFieldApp

instance RenderMessage PasswordFieldApp FormMessage where
    renderMessage _ _ = defaultFormMessage

passwordConfirmField :: Field Handler Text
passwordConfirmField = Field {
    fieldParse = \rawVals _fileVals ->
        case rawVals of
            [a, b]
                | a == b -> return $ Right $ Just a
                | otherwise -> return $ Left "Passwords don't match"
            [] -> return $ Right Nothing
            _ -> return $ Left "You must enter two values"
    , fieldView = \idAttr nameAttr otherAttr eResult isReq -> [whamlet|
    <input id=#{idAttr} name=#{nameAttr} *{otherAttr} type=password>
    <div>Confirm:
    <input id=#{idAttr}-confirm name=#{nameAttr} *{otherAttr} type=password>
|]
    , fieldEnctype = UrlEncoded
    }

getRootR :: Handler Html
getRootR = do
    ((res, widget), enctype) <- runFormGet $ renderDivs $
        areq passwordConfirmField "Password" Nothing
    defaultLayout [whamlet|
    <p>Result: #{show res}
    <form enctype=#{enctype}>
        ^{widget}
        <input type=submit value="Change password">
    |]

main :: IO ()
main = warp 3000 PasswordFieldApp
