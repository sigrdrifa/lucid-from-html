{-# LANGUAGE OverloadedStrings #-}

-- | Supplemental to Html5 terms.
--   Some of them are obsolete or deprecated but still used.

module Lucid.Supplemental where

import           Lucid.Base
import           Data.Text (Text)

parentElements :: [String]
parentElements = ["tt"]

leafElements :: [String]
leafElements = []

attributeElements :: [String]
attributeElements =
  [ "aria-hidden"
  , "property"
  , "language"
  , "align"
  ]

ariaHidden_ :: Text -> Attribute
ariaHidden_ = makeAttribute "aria-hidden"

property_ :: Text -> Attribute
property_ = makeAttribute "property"

-- this attribute is deprecated
language_ :: Text -> Attribute
language_ = makeAttribute "language"

-- this attribute is obsolete!
align_ :: Text -> Attribute
align_ = makeAttribute "align"

-- this tag is deprecated
tt_ :: Term arg result => arg -> result
tt_ = term "tt"

