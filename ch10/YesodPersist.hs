{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts, TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}
import Yesod
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT, MonadLogger, monadLoggerLog)
import Control.Applicative (pure)

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
|]

instance Yesod PersistTest

instance YesodPersist PersistTest where
    type YesodPersistBackend PersistTest = SqlBackend

    runDB action = do
        PersistTest pool <- getYesod
        runSqlPool action pool

getHomeR :: Handler Html
getHomeR = do
    people <- runDB $ selectList [] [Asc PersonAge]
    defaultLayout [whamlet|
    <ul>
        $forall Entity personid person <- people
            <li>
                <a href=@{PersonR personid}>#{personFirstName person}
    |]

getPersonR :: PersonId -> Handler String
getPersonR personId = do
    person <- runDB $ get404 personId
    return $ show person

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runNoLoggingT $ withSqlitePool "test3.db" openConnectionCount $ \pool -> do
    runResourceT $ runStderrLoggingT $ flip runSqlPool pool $ do
        runMigration migrateAll
        insert $ Person "Michael" "Snoyman" 26
    warp 3000 $ PersistTest pool
