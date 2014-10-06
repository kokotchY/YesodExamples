{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, ViewPatterns #-}
import Control.Monad.Logger
import Data.Text (Text)
import Database.Persist.Sqlite
import Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Author
    name Text
Blog
    author AuthorId
    title Text
    content Html
|]

data App = App {
    persistConfig :: SqliteConf,
    connPool :: ConnectionPool
}

instance Yesod App
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

mkYesod "App" [parseRoutes|
/ HomeR GET
/blog/#BlogId BlogR GET
|]

getHomeR :: Handler Html
getHomeR = do
    blogs <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Blog posts"
        [whamlet|
            <ul>
                $forall Entity blogid blog <- blogs
                    <li>
                        <a href=@{BlogR blogid}>
                            #{blogTitle blog} by #{show $ blogAuthor blog}
        |]

getBlogR :: BlogId -> Handler Html
getBlogR _ = error "Todo"

main :: IO ()
main = do
    let conf = SqliteConf ":memory:" 1
    pool <- createPoolConfig conf
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        alice <- insert $ Author "Alice"
        bob <- insert $ Author "Bob"

        insert_ $ Blog alice "Alice's first post" "Hello World!"
        insert_ $ Blog bob "Bob's first post" "Hello World!"
        insert_ $ Blog alice "Alice's second post" "Goodbye World!"
    warp 3000 App {
        persistConfig = conf,
        connPool = pool
    }
