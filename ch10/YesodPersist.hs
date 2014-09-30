{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts, TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}
import Yesod
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Applicative ((<$>),(<*>))
import Data.Text (Text, unpack)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    firstName String
    lastName String
    age Int
    deriving Show
|]

data PersistTest = PersistTest ConnectionPool

mkYesod "PersistTest" [parseRoutes|
/ HomeR GET
/person/#PersonId PersonR GET
/addPerson AddPersonR GET POST
/deletePerson/#PersonId DeletePersonR GET
|]

instance Yesod PersistTest

instance RenderMessage PersistTest FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist PersistTest where
    type YesodPersistBackend PersistTest = SqlBackend

    runDB action = do
        PersistTest pool <- getYesod
        runSqlPool action pool

getHomeR :: Handler Html
getHomeR = do
    people <- runDB $ selectList [] [Asc PersonAge]
    defaultLayout [whamlet|
    <a href=@{AddPersonR}>Add person
    <br />
    $if null people
        No people in database
    $else
        <table>
            <tr>
                <th>Id
                <th>First name
                <th>Last name
                <th>Age
                <th>Actions
            $forall Entity personid person <- people
                <tr>
                    <td>#{unSqlBackendKey $ unPersonKey personid}
                    <td>
                        <a href=@{PersonR personid}>#{personFirstName person}
                    <td>#{personLastName person}
                    <td>#{show $ personAge person}
                    <td>
                        <a href=@{DeletePersonR personid}>Delete
    |]

getPersonR :: PersonId -> Handler String
getPersonR personId = do
    person <- runDB $ get404 personId
    return $ show person


data PersonForm = PersonForm {
    personFormFirstName :: Text,
    personFormLastName :: Text,
    personFormAge :: Int
} deriving (Show)

personForm :: Html -> MForm Handler (FormResult PersonForm, Widget)
personForm = renderDivs $ PersonForm
    <$> areq textField "First name" Nothing
    <*> areq textField "Last name" Nothing
    <*> areq intField "Age" Nothing

getAddPersonR :: Handler Html
getAddPersonR = do
    (widget, enctype) <- generateFormPost personForm
    defaultLayout [whamlet|
        <p>Add a Person
        <form method=post action=@{AddPersonR} enctype=#{enctype}>
            ^{widget}
            <input type=submit value="Add">
    |]


savePerson (PersonForm firstName lastName age) = runDB $ do
    let person = Person (unpack firstName) (unpack lastName) age
    personId <- insert person
    return (personId, person)


postAddPersonR :: Handler Html
postAddPersonR = do
    ((result, widget), enctype) <- runFormPost personForm
    case result of
        FormSuccess personForm -> do
            (personId, person) <- savePerson personForm
            defaultLayout [whamlet|
                <p>This #
                    <a href=@{PersonR personId}>#{show personId}
                    \ have been added: #{show person}
                <a href=@{HomeR}>Home
            |]
        _ -> defaultLayout [whamlet|
            <p>Invalid input, let's try again.
            <form method=post action=@{AddPersonR} enctype=#{enctype}>
                ^{widget}
                <input type=submit value="Add">
            |]

getDeletePersonR :: PersonId -> Handler Html
getDeletePersonR personId = do
    person <- runDB $ get personId
    runDB $ delete personId
    defaultLayout [whamlet|
    <h1>Delete a user
    <p>The user #{show person} has been deleted
    <a href=@{HomeR}>Home
    |]

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test3.db" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ runMigration migrateAll
    warp 3000 $ PersistTest pool
