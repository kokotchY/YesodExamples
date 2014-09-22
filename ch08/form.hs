{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings, TypeFamilies #-}
import Yesod
import Yesod.Form.Jquery
import Data.Time (Day)
import Data.Text (Text)
import Control.Applicative ((<$>),(<*>))

data Synopsis = Synopsis

mkYesod "Synopsis" [parseRoutes|
/ RootR GET
/person PersonR POST
/car CarR POST
|]

instance Yesod Synopsis

instance RenderMessage Synopsis FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodJquery Synopsis

data Person = Person {
    personName :: Text,
    personBirthday :: Day,
    personFavoriteColor :: Maybe Text,
    personEmail :: Text,
    personWebsite :: Maybe Text
} deriving (Show)

data Car = Car {
    carModel :: Text,
    carYear :: Int
} deriving (Show)

carAForm :: AForm Handler Car
carAForm = Car
    <$> areq textField "Model" Nothing
    <*> areq intField "Year" Nothing

carForm :: Html -> MForm Handler (FormResult Car, Widget)
carForm = renderTable carAForm

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderDivs $ Person
    <$> areq textField "Name" (Just "Youpie")
    <*> areq (jqueryDayField def {
        jdsChangeYear = True,
        jdsYearRange = "1900:-5"
    }) "Birthday" Nothing
    <*> aopt textField "Favorite color" Nothing
    <*> areq emailField "Email address" Nothing
    <*> aopt urlField "Website" Nothing

getRootR :: Handler Html
getRootR = do
    (widget, enctype) <- generateFormPost personForm
    (widgetCar, enctypeCar) <- generateFormPost carForm
    defaultLayout [whamlet|
    <p>The widget generated contains only the contents of the form, not the form tag itself. So...
    <form method=post action=@{PersonR} enctype=#{enctype}>
        ^{widget}
        <p>It also doesn't include the submit button.
        <input type=submit>
    <hr>
    Another example of form
    <form method=post action=@{CarR} enctype=#{enctypeCar}>
        ^{widgetCar}
        <input type=submit>
    |]

postPersonR :: Handler Html
postPersonR = do
    ((result, widget), enctype) <- runFormPost personForm
    case result of
        FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
        _ -> defaultLayout [whamlet|
    <p>Invalid input, let's try again.
    <form method=post action=@{PersonR} enctype=#{enctype}>
        ^{widget}
        <input type=submit>
    |]

postCarR :: Handler Html
postCarR = do
    ((result, widget), enctype) <- runFormPost carForm
    case result of
        FormSuccess car -> defaultLayout [whamlet|<p>#{show car}|]
        _ -> defaultLayout [whamlet|<p>Error in formular|]

main :: IO ()
main = warp 3000 Synopsis
