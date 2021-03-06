{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses #-}
import Yesod
import Control.Applicative
import Data.Text (Text)

data Input = Input

mkYesod "Input" [parseRoutes|
/ RootR GET
/input InputR GET
|]

instance Yesod Input

instance RenderMessage Input FormMessage where
    renderMessage _ _ = defaultFormMessage

data Person = Person {
    personName :: Text,
    personAge :: Int
} deriving (Show)

getRootR :: Handler Html
getRootR = defaultLayout [whamlet|
<form action=@{InputR}>
    <p>
        My name is #
        <input type=text name=name>
        \ and I am #
        <input type=text name=age>
        \ years old. #
        <input type=submit value="Introduce myself">
|]

getInputR :: Handler Html
getInputR = do
    person <- runInputGet $ Person
        <$> ireq textField "name"
        <*> ireq intField "age"
    defaultLayout [whamlet|<p>#{show person}|]

main :: IO ()
main = warp 3000 Input
