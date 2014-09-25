{-# LANGUAGE OverloadedStrings, RecordWildCards, QuasiQuotes, TemplateHaskell, TypeFamilies, ExistentialQuantification #-}
import Yesod
import Data.Text (Text)

data Person = Person {
    name :: Text,
    age :: Int
} deriving Show

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name,
          "age" .= age
        ]

data App = App

mkYesod "App" [parseRoutes|
/ RootR GET
/example/1 HomeR GET
/example/2 Home2R GET
/example/3 Home3R GET
/example/4 Home4R GET
/example/5 Home5R GET
|]

instance Yesod App

mimeType :: ContentType
mimeType = "text/haskell-show"

pages = [HomeR, Home2R, Home3R, Home4R, Home5R]

getRootR :: Handler Html
getRootR = do
    defaultLayout [whamlet|
<div>
    <ul>
        $forall page <- pages
            <li><a href=@{page}>Example #{show page}
    |]

data HaskellShow = forall a. Show a => HaskellShow a

instance ToContent HaskellShow where
    toContent (HaskellShow x) = toContent $ show x
instance ToTypedContent HaskellShow where
    toTypedContent = TypedContent mimeType . toContent
instance HasContentType HaskellShow where
    getContentType _ = mimeType

getHomeR :: Handler Value
getHomeR = returnJson $ Person "Bla" 21

getHome2R :: Handler TypedContent
getHome2R = selectRep $ do
    provideRep $ return [shamlet|
    <p>Hello, my name is #{name} and I am #{age} years old.
    |]
    provideJson personJson
    where
        person = Person "Bla" 21
        personJson@Person {..} = person

getHome3R :: Handler TypedContent
getHome3R =
    return $ TypedContent mimeType $ toContent $ show person
    where
        person = Person "Michael" 28

getHome4R :: Handler HaskellShow
getHome4R =
    return $ HaskellShow person
    where
        person = Person "Michael" 28

getHome5R :: Handler TypedContent
getHome5R = selectRep $ do
    provideRep $ return $ HaskellShow person
    provideJson person
    where
        person = Person "Michael" 28

main :: IO ()
main = warp 3000 App
