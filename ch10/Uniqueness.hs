{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Data.Time
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    firstName String
    lastName String
    age Int
    PersonName firstName lastName
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll
    michaelId <- insert $ Person "Michael" "Snoyman" 26
    michael <- getBy $ PersonName "Michael" "Snoyman"
    liftIO $ print michael
