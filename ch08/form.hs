{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings, TypeFamilies #-}
import Yesod
import Yesod.Form.Jquery
import Data.Time (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Data.Text (Text, pack)
import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((&&&))

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
    carYear :: Int,
    carColor :: Maybe Color
} deriving (Show)

data Color = Red | Blue | Gray | Black
    deriving (Show, Eq, Enum, Bounded)


carAForm :: Maybe Car -> AForm Handler Car
carAForm mcar = Car
    <$> areq textField "Model" (carModel <$> mcar)
    <*> areq carYearField "Year" (carYear <$> mcar)
    <*> aopt (radioFieldList colors) "Color" (carColor <$> mcar)
    where
        carYearField = checkM inPast $ checkBool (>= 1990) errorMessage intField
        inPast y = do
            thisYear <- liftIO getCurrentYear
            return $ if y <= thisYear
                then Right y
                else Left ("You have a time machine!" :: Text)
        getCurrentYear :: IO Int
        getCurrentYear = do
            now <- getCurrentTime
            let today = utctDay now
            let (year, _, _) = toGregorian today
            return $ fromInteger year
        errorMessage :: Text
        errorMessage = "Your car is too old, get a new one!"
        colors :: [(Text, Color)]
        colors = map (pack . show &&& id) $ [minBound..maxBound]

carForm :: Html -> MForm Handler (FormResult Car, Widget)
carForm = renderDivs $ carAForm Nothing

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
        _ -> defaultLayout [whamlet|
<p>Invalid input, let's try again.
<form method=post action=@{CarR} enctype=#{enctype}>
    ^{widget}
    <input type=submit>
|]

main :: IO ()
main = warp 3000 Synopsis
