{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving #-}
import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.MySQL
import Database.Persist.TH
import Control.Monad.Logger (runNoLoggingT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

mysqlConnection :: ConnectInfo
mysqlConnection = defaultConnectInfo { connectUser = "persist", connectPassword = "persist", connectDatabase = "persist" }

main = runNoLoggingT $ withMySQLPool mysqlConnection 10 $ \pool -> do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        johnId <- insert $ Person "John Doe" $ Just 35
        janeId <- insert $ Person "Jane Doe" Nothing
        insert $ BlogPost "My first post" johnId
        insert $ BlogPost "One more for good measure" johnId
        oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
        liftIO $ print (oneJohnPost :: [Entity BlogPost])
        john <- get johnId
        liftIO $ print (john :: Maybe Person)

        delete janeId
        deleteWhere [BlogPostAuthorId ==. johnId]
