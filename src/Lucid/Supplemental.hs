{-# LANGUAGE OverloadedStrings #-}

-- | Supplemental to Html5 terms.
-- Some of them are obsolete or deprecated but still used in real pages.

module Lucid.Supplemental where

import           Lucid.Base
import           Data.Text (Text)

----- List of tags and attributes, don't forget to add here if
----- corresponding function is added

-- | parent tags
parentElements :: [String]
parentElements =
  [ "tt", "svg"]

-- | leaf tags
leafElements :: [String]
leafElements = []

-- | attributes
attributeElements :: [String]
attributeElements =
  [ "aria-hidden"
  , "property"
  , "language"
  , "align"
  ]
-- hack for svg
  ++ svgAttrs ++ svgCamelCaseAttrs

------ Parent elements ---------------------
---------------------------------------------

-- | @\<tt\>@ tag, deprecated
tt_ :: Term arg result => arg -> result
tt_ = term "tt"

-- here is hack for <svg> tag
-- | @\<svg\>@ tag.
svg_ :: TermRaw arg result => arg -> result
svg_ = termRaw "svg"

------ Leaf elements -----------------------
-------------------------------------------


------ Attributes --------------------------
--------------------------------------------

-- | The @aria-hidden@ attribute
ariaHidden_ :: Text -> Attribute
ariaHidden_ = makeAttribute "aria-hidden"

-- | The @property@ attribute
property_ :: Text -> Attribute
property_ = makeAttribute "property"

-- | The @language@ attribute, deprecated
language_ :: Text -> Attribute
language_ = makeAttribute "language"

-- | The @align@ attribute.  
-- This attribute is obsolete!
align_ :: Text -> Attribute
align_ = makeAttribute "align"


------------ Svg attributes, remove when fix !!!! -----

-- | The @id@ attribute for svg.
id_ :: Text -> Attribute
id_ = makeAttribute "id"

-- | The @version@ attribute for svg.
version_ :: Text -> Attribute
version_ = makeAttribute "version"

-- | The @x@ attribute.
x_ :: Text -> Attribute
x_ = makeAttribute "x"

-- | The @y@ attribute.
y_ :: Text -> Attribute
y_ = makeAttribute "y"

-- | The @xmlns:xlink@ attribute.
xmlnsXlink_ :: Text -> Attribute
xmlnsXlink_ = makeAttribute "xmlns:xlink"

-- | The @xml:space@ attribute.
xmlSpace_ :: Text -> Attribute
xmlSpace_ = makeAttribute "xml:space"

-- | The @enable-background@ attribute
enableBackground_ :: Text -> Attribute
enableBackground_ = makeAttribute "enable-background"

-- | The @viewBox@ attribute
viewBox_ :: Text -> Attribute
viewBox_ = makeAttribute "viewBox"

svgAttrs :: [String]
svgAttrs = 
  [ "id", "version", "x", "y", "xmlns:xlink", "xml:space"
  , "enable-background" ] 

svgCamelCaseAttrs :: [String]
svgCamelCaseAttrs = ["viewBox"]
