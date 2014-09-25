{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts #-}
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int
    deriving Show
Car
    color String
    make String
    model String
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    printMigration migrateAll
    runMigration migrateAll
    michaelId <- insert $ Person "Michael" 26
    michael <- get michaelId
    liftIO $ print michael
