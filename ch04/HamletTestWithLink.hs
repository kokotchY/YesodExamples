{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
import Text.Hamlet (HtmlUrl, hamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Text (Text)

data Person = Person {
    name :: String,
    age :: Int
}
data MyRoute = Home

render :: MyRoute -> [(Text, Text)] -> Text
render Home _ = "/home"

footer :: HtmlUrl MyRoute
footer = [hamlet|
<footer>
    Return to #
    <a #youpie href=@{Home}>
        <div .bla>Homepage
    <input type=checked :isChecked:checked>
    <p :isRed:style="color:red">Some text that might be red
    .
|]

isChecked = True
isRed = True

main :: IO ()
main = putStrLn $ renderHtml $ [hamlet|
$doctype 5
<html>
    <head>
        <title>Hamlet is Awesome
    <body>
        <p>This is my page.
        $if isAdmin
            <p>Welcome to the admin section.
        $elseif isLoggedIn
            <p>You are not the administrator.
        $else
            <p>I don't know you.

        <h1>Test Maybe
        $maybe name <- maybeName
            <p>Your name is #{name}
        $nothing
            <p>I don't know your name.

        <h1>Test Maybe2
        $maybe name <- maybeName2
            <p>Your name is #{name}

        <h1>Test Maybe Person
        $maybe Person name age <- maybePerson
            <p>Your name is #{name} and your are #{age} year old.

        <h1>Forall person
        $if null people
            <p>No people.
        $else
            <ul>
                $forall Person name age <- people
                    <li>#{name} - #{age}

        <h1>Test case
        $case foo
            $of Left bar
                <p>It was left: #{bar}
            $of Right baz
                <p>It was right: #{baz}

        ^{footer}
|] render

isAdmin = False
isLoggedIn = False

maybeName :: Maybe String
maybeName = Just "Sebastien"

maybeName2 :: Maybe String
maybeName2 = Nothing

maybePerson :: Maybe Person
maybePerson = Just $ Person "Sebastien" 28

people :: [Person]
people = [Person "Seb" 28, Person "Quentin" 26]

foo :: Either String String
foo = Right "right case"
{-foo = Left "left case"-}
