#!/usr/bin/env stack
-- stack runghc
{-
Coder Radio Challenge for Episode #216
https://www.reddit.com/r/CoderRadio/comments/4wa09e/216_coding_challenge/

Run with Docker Compose: ./start.bash

Run without Docker: ./jarjar_quotes.hs

Requirements:
  - GHC
  - runhaskell
  - yesod

This program creates a server side web application which stores
text into a Sqlite database. A form on the Home page is for adding new
quotes. The Random page is for showing a random quote.

I created this mostly by reading through the Yesod book.
http://www.yesodweb.com/book

-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Yesod
import Text.Hamlet          (hamletFile)
import Data.Text            (Text)
import Control.Applicative  ((<$>), (<*>))
import Text.Blaze           (ToMarkup)

data JarJarQuotes = JarJarQuotes

-- Routes
mkYesod "JarJarQuotes" [parseRoutes|
/       HomeR   GET POST
/about  AboutR  GET
/random RandomR GET
|]

instance Yesod JarJarQuotes where
  -- Default layout for each page
  defaultLayout widget = do
    mmsg <- getMessage
    pc <- widgetToPageContent widget
    withUrlRenderer $(hamletFile "default-layout.hamlet")

instance RenderMessage JarJarQuotes FormMessage where
    renderMessage _ _ = defaultFormMessage

-- The Quote datatype to receive from QuoateForm
data Quote = Quote { quoteText :: Text } deriving Show

-- Quote form
quoteForm :: Html -> MForm Handler (FormResult Quote, Widget)
quoteForm = renderDivs $ Quote <$> areq textField "Quote" Nothing

quotesListWidget :: Widget
quotesListWidget = toWidget [hamlet|
  <p>
    TODO: List all quotes here.
  |]

addQuoteWidget :: (Text.Blaze.ToMarkup a) =>  Widget -> a -> Widget
addQuoteWidget w e = [whamlet|
<p>
  Add words of wisdom from Jar Jar Binks
  <form method=post action=@{HomeR} enctype=#{e}>
    ^{w}
    <button>Submit
|]

homeListWidget :: (Text.Blaze.ToMarkup a) =>  Widget -> a -> Widget
homeListWidget w e = addQuoteWidget w e >> quotesListWidget

-- Home page post handler
postHomeR :: Handler Html
postHomeR = do
  ((result, widget), enctype) <- runFormPost quoteForm
  case result of
    FormSuccess quote -> defaultLayout $ do
      [whamlet|<p>Added '#{quoteText quote}'|]
      addQuoteWidget widget enctype
      quotesListWidget
    _ -> defaultLayout $ do
      [whamlet|<p>Invalid input, let's try again.|]
      addQuoteWidget widget enctype
      quotesListWidget

-- Home page
getHomeR :: Handler Html
getHomeR = do
  (widget, enctype) <- generateFormPost quoteForm
  defaultLayout $ do
    addQuoteWidget widget enctype
    quotesListWidget

-- Random page
getRandomR :: Handler Html
getRandomR = defaultLayout [whamlet|TODO: Show a random quote here|]

-- About page
getAboutR :: Handler Html
getAboutR = defaultLayout [whamlet|Small web app for saving your favorite Jar Jar Binks quotes!|]

main :: IO ()
main = do
  putStrLn "View website by going to http://localhost:3000/ in your web browser"
  putStrLn "Starting web server on port 3000 ..."
  warp 3000 JarJarQuotes
