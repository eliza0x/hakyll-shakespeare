{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Web.Hamlet (
    hamlCompiler
  , hamlCompilerWith
  , hamlTemplateCompiler
  , hamlTemplateCompilerWith
  , renderHtml
  ) where

import Hakyll
import Data.Text(Text)
import Text.Hamlet.Runtime (HamletSettings, HamletData, parseHamletTemplate, renderHamletTemplate, HamletSettings, defaultHamletSettings)
import Data.Map (Map)
import Text.Blaze.Html.Renderer.String (renderHtml)

hamlCompiler :: Compiler (Item String)
hamlCompiler = 
  hamlCompilerWith defaultHamletSettings mempty

hamlCompilerWith ::  HamletSettings -> Map Text HamletData -> Compiler (Item String)
hamlCompilerWith hamletSettings hamletVariables =
  getResourceBody >>= renderHaml hamletSettings hamletVariables

hamlTemplateCompiler :: Compiler (Item Template)
hamlTemplateCompiler =
  hamlTemplateCompilerWith defaultHamletSettings mempty

hamlTemplateCompilerWith :: HamletSettings -> Map Text HamletData -> Compiler (Item Template)
hamlTemplateCompilerWith hamletSettings hamletVariables =
  cached "Hakyll.Web.Hamlet.hamlTemplateCompiler" $ do
  body <- getResourceBody
  html <- renderHaml hamletSettings hamletVariables body
  return $ readTemplate <$> html

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
