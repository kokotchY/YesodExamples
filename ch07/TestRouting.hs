{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
import Yesod
import qualified Data.Text as T
import Text.Blaze

data App = App

newtype Natural = Natural Int deriving (Show, Eq, Read)

instance PathPiece Natural where
    toPathPiece (Natural i) = T.pack $ show i
    fromPathPiece s =
        case reads $ T.unpack s of
            (i, ""):_
                | i < 1 -> Nothing
                | otherwise -> Just $ Natural i
            [] -> Nothing

data Page = Page T.Text T.Text [T.Text]
instance PathMultiPiece Page where
    toPathMultiPiece (Page x y z) = x : y : z
    fromPathMultiPiece (x:y:z) = Just $ Page x y z
    fromPathMultiPiece _ = Nothing

{-instance PathMultiPiece AppIdents-}

mkYesod "App" [parseRoutes|
/ RootR GET
/fib/#Natural FibR GET
/wiki/*Texts WikiR GET
/year/#Integer/month/#T.Text/day/#Int DateR
/redirect RedirectR GET
/notFound NotFoundR GET
/permissionDenied PermissionDeniedR GET
/invalidArgs InvalidArgsR GET
/sendFile SendFileR GET
|]
{-/json JsonR GET-}
{-/single/#AppIdent SingleR GET-}
{-/multiple/*AppIdents MultipleR GET-}
{-/static StaticR Static getStatic-}

instance Yesod App

getRedirectR :: Handler Html
getRedirectR = redirect RootR

getNotFoundR :: Handler Html
getNotFoundR = notFound

getPermissionDeniedR :: Handler Html
getPermissionDeniedR = permissionDenied "Nop"

getInvalidArgsR :: Handler Html
getInvalidArgsR = invalidArgs ["Youpie"]

getSendFileR :: Handler Html
getSendFileR = sendFile "image/png" "rack.png"

getRootR :: Handler Html
getRootR = do
    let listFib = [3, 5, -1]
    defaultLayout [whamlet|
<h1>Some examples
<h2>Fib:
<ul>
    $forall nb <- listFib
        <li>Fib 
            <a href=@{FibR (Natural nb)}>#{nb}
<h2>Wiki:
<ul>
    <li>Wiki 
<h2>Year:
<ul>
    <li>My birthday #
        <a href=@{DateR 1986 "May" 5}>05/05/1986
    <li>Today #
        <a href=@{DateR 2014 "Sep" 21}>21/09/2014
<h2>Redirect:
Simple redirect: <a href=@{RedirectR}>Link</a>
<h2>Some errors message:
<ul>
    <li>NotFound: #
        <a href=@{NotFoundR}>Link
    <li>PermissionDenied: #
        <a href=@{PermissionDeniedR}>Link
    <li>InvalidArgs: #
        <a href=@{InvalidArgsR}>Link
<h2>Send file: #
<a href=@{SendFileR}>File
|]

getSingleR = undefined
getMultipleR = undefined

getFibR :: Natural -> Handler Html
getFibR val = defaultLayout [whamlet|
<p>Fibonacci of #{getVal val} is #{calcFib val}
Next number: 
    <a href=@{FibR $ nextVal val}>#{T.pack $ show $ nextVal val}
|]

getWikiR :: [T.Text] -> Handler RepPlain
getWikiR = return . RepPlain . toContent . T.unwords

handleDateR :: Integer -> T.Text -> Int -> Handler RepPlain
handleDateR year month day =
    return $ RepPlain $ toContent $
        T.concat [month, " ", T.pack $ show day, ", ", T.pack $ show year]

{-getJsonR = undefined-}

getVal :: Natural -> T.Text
getVal (Natural a) = T.pack $ show a

nextVal :: Natural -> Natural
nextVal (Natural a) = Natural $ a + 1

calcFib :: Natural -> T.Text
calcFib (Natural a) = T.pack $ show $ fib a

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib a = fib (a-1) + fib (a-2)

main = warp 3000 App
