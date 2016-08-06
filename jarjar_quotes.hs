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

-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Yesod
import Text.Hamlet          (hamletFile)

data JarJarQuotes = JarJarQuotes

mkYesod "JarJarQuotes" [parseRoutes|
/ HomeR GET
/about AboutR GET
/random RandomR GET
|]

instance Yesod JarJarQuotes where
  defaultLayout widget = do
    mmsg <- getMessage
    pc <- widgetToPageContent widget
    withUrlRenderer $(hamletFile "default-layout.hamlet")

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
<p>
  TODO: Add form for adding quotes.<br />
  TODO: List all quotes here.
|]

getRandomR :: Handler Html
getRandomR = defaultLayout [whamlet|TODO: Show a random quote here|]

getAboutR :: Handler Html
getAboutR = defaultLayout [whamlet|Small web app for saving your favorite Jar Jar Binks quotes!|]

main :: IO ()
main = do
  putStrLn "View website by going to http://localhost:3000/ in your web browser"
  putStrLn "Starting web server on port 3000 ..."
  warp 3000 JarJarQuotes
