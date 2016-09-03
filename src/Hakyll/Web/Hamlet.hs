{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright   : (c) eliza, 2016
License     : MIT
Maintainer  : me@eliza.link

If you using option except default, read "Text.Hamlet.Runtime".

> import Hakyll
> import Hakyll.Web.Hamlet
> 
> main :: IO ()
> main = hakyll $ do
>     match "templates/*.hamlet" $ compile hamlTemplateCompiler
-}

module Hakyll.Web.Hamlet (
    hamlCompiler
  , hamlCompilerWith
  , hamlTemplateCompiler
  , hamlTemplateCompilerWith
  , renderHaml
  ) where

import Hakyll
import Data.Text(Text)
import Text.Hamlet.Runtime (HamletSettings, HamletData, parseHamletTemplate, renderHamletTemplate, HamletSettings, defaultHamletSettings)
import Data.Map (Map)
import Text.Blaze.Html.Renderer.String (renderHtml)

-- | Read complete file contents as a string using Hamlet, with the default options.
hamlCompiler :: Compiler (Item String)
hamlCompiler = 
  hamlCompilerWith defaultHamletSettings mempty

-- | Read complete file contents as a string using Hamlet, with the spplied options.
hamlCompilerWith ::  HamletSettings -> Map Text HamletData -> Compiler (Item String)
hamlCompilerWith hamletSettings hamletVariables =
  getResourceBody >>= renderHaml hamletSettings hamletVariables

-- | Read complete file contents as a template, with the default options.
hamlTemplateCompiler :: Compiler (Item Template)
hamlTemplateCompiler =
  hamlTemplateCompilerWith defaultHamletSettings mempty

-- | Read complete file contents as a template, with the spplied options.
hamlTemplateCompilerWith :: HamletSettings -> Map Text HamletData -> Compiler (Item Template)
hamlTemplateCompilerWith hamletSettings hamletVariables =
  cached "Hakyll.Web.Hamlet.hamlTemplateCompiler" $ do
  body <- getResourceBody
  html <- renderHaml hamletSettings hamletVariables body
  return $ readTemplate <$> html

-- | Read a string using hamlet.
renderHaml :: HamletSettings -> Map Text HamletData -> Item String -> Compiler (Item String)
renderHaml hamletSettings hamletVariables item = do
  html <- unsafeCompiler 
            . hamlToHtml hamletSettings hamletVariables 
            . itemBody $ item
  makeItem html

hamlToHtml :: HamletSettings -> Map Text HamletData -> String -> IO String
hamlToHtml hamletSettings hamletVariables body = do
    template <- parseHamletTemplate hamletSettings body
    html <- renderHamletTemplate template hamletVariables
    return $ renderHtml html

