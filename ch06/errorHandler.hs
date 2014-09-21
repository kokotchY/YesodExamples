{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}

import Yesod

data TestError = TestError

mkYesod "TestError" [parseRoutes|
/ RootR GET
/error ErrorR GET
/not-found NotFoundR GET
|]

instance Yesod TestError where
    errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
        setTitle "Request page not located"
        toWidget [hamlet|
<h1>Not Found
<p>We blabla.
|]
    errorHandler other = defaultErrorHandler other

getRootR :: Handler Html
getRootR = defaultLayout [whamlet|
<p>
    <a href=@{ErrorR}>Internal server error
    <a href=@{NotFoundR}>Not found
|]

getErrorR :: Handler ()
getErrorR = error "This is an error"

getNotFoundR :: Handler ()
getNotFoundR = notFound

main :: IO ()
main = warp 3000 TestError
