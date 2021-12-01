{-# LANGUAGE OverloadedStrings #-}

import qualified GHC.IO.Encoding as E
import Hakyll
import System.FilePath (takeBaseName, joinPath, splitPath)

myConfig :: Configuration
myConfig = defaultConfiguration 
    { providerDirectory = "source"
    , destinationDirectory = "public"
    }

myPostCtx :: Context String
myPostCtx = dateField "date" "%B %e, %Y" <> defaultContext

main :: IO ()
main = E.setLocaleEncoding E.utf8 >> genSite

genSite :: IO ()
genSite = hakyllWith myConfig $ do

    match "templates/*" $ compile templateBodyCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "*.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*.md" $ do
        route $ setExtension "html"
        compile $ do
            fp <- takeBaseName <$> getResourceFilePath
            let myCtx =
                    myPostCtx
                    <> constField "file" fp
                    <> defaultContext
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    myCtx
                >>= loadAndApplyTemplate "templates/default.html" myCtx
                >>= relativizeUrls

    match "posts/*.md" $ version "pdf" $ do
        route $ setExtension "pdf"
        compile $ do
            fp <- getResourceFilePath
            TmpFile tmpFile <- newTmpFile (takeBaseName fp ++ ".pdf")
            let siteDstDir = destinationDirectory myConfig
            let dstDir = joinPath (siteDstDir : init (tail $ splitPath fp))
            getResourceLBS
                >>= traverse (unixFilterLBS "pandoc"
                        [ "-o", tmpFile
                        , "--resource-path", dstDir
                        , "-s"
                        , "-V", "geometry:margin=1in"
                        ]) 
                >>= traverse (unixFilterLBS "cat" [tmpFile])

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*.md" .&&. hasNoVersion)
            let myCtx =
                    listField "posts" myPostCtx (return posts)
                    <> constField "title" "Blog"
                    <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" myCtx
                >>= loadAndApplyTemplate "templates/default.html" myCtx
                >>= relativizeUrls


