{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Web.Hamlet (
    hamlCompiler
  , hamlCompilerWith
  , hamlTemplateCompiler
  , hamlTemplateCompilerWith
  , renderHtml
  ) where

import Data.Monoid (mempty)
import Hakyll
import Data.Text(Text)
import Text.Hamlet.Runtime
import qualified Data.Map as Map
import Text.Blaze.Html.Renderer.String (renderHtml)

hamlCompiler :: Compiler (Item String)
hamlCompiler = hamlCompilerWith defaultHamletSettings mempty

hamlCompilerWith ::  HamletSettings -> Map.Map Text HamletData -> Compiler (Item String)
hamlCompilerWith hamletSettings hamletVariables =
  getResourceBody >>= renderHaml hamletSettings hamletVariables

hamlTemplateCompiler :: Compiler (Item Template)
hamlTemplateCompiler =
  hamlTemplateCompilerWith defaultHamletSettings mempty

hamlTemplateCompilerWith :: HamletSettings -> Map.Map Text HamletData -> Compiler (Item Template)
hamlTemplateCompilerWith hamletSettings hamletVariables = do
  body <- getResourceBody
  html <- renderHaml hamletSettings hamletVariables body
  return $ readTemplate <$> html

renderHaml :: HamletSettings -> Map.Map Text HamletData -> Item String -> Compiler (Item String)
renderHaml hamletSettings hamletVariables item = do
  html <- unsafeCompiler . hamlToHtml hamletSettings hamletVariables 
            . itemBody $ item
  makeItem html

hamlToHtml :: HamletSettings -> Map.Map Text HamletData -> String -> IO String
hamlToHtml hamletSettings hamletVariables body = do
    template <- parseHamletTemplate hamletSettings body
    html <- renderHamletTemplate template hamletVariables
    return $ renderHtml html
