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
leafElements = 
  [ "relative-time", "time-until"
  , "time-ago", "local-time"
  ]

-- | attributes
attributeElements :: [String]
attributeElements =
  [ "aria-hidden", "color"
  , "aria-label", "autocapitalize"
  , "aria-expanded", "aria-haspopup"
  , "aria-labelledby", "aria-describedby"
  , "property"
  , "language"
  , "align"
  , "itemscope"
  , "itemtype"
  ]
-- hack for svg
  ++ svgAttrs ++ svgCamelCaseAttrs

------ Parent elements ---------------------
---------------------------------------------

-- | @\<tt\>@ tag, deprecated
tt_ :: Term arg result => arg -> result
tt_ = term "tt"

-- | @\<relative-time\>@ tag, from GitHub
relativeTime_ :: Monad m => [Attribute] -> HtmlT m ()
relativeTime_ = with (makeElementNoEnd "relative-time")

-- | @time-until@ element
timeUntil_ :: Monad m => [Attribute] -> HtmlT m ()
timeUntil_ = with (makeElementNoEnd "time-until")

-- | @time-ago@ element
timeAgo_ :: Monad m => [Attribute] -> HtmlT m ()
timeAgo_ = with (makeElementNoEnd "time-ago")

-- | @local-time@ element
localTime_ :: Monad m => [Attribute] -> HtmlT m ()
localTime_ = with (makeElementNoEnd "local-time")



-- here is hack for <svg> tag
-- | @\<svg\>@ tag.
svg_ :: TermRaw arg result => arg -> result
svg_ = termRaw "svg"

------ Leaf elements -----------------------
-------------------------------------------


------ Attributes --------------------------
--------------------------------------------

-- | The @aria-expanded@ attribute
ariaExpanded_ :: Text -> Attribute
ariaExpanded_ = makeAttribute "aria-expanded"

-- | The @aria-haspopup@ attribute
ariaHaspopup_ :: Text -> Attribute
ariaHaspopup_ = makeAttribute "aria-haspopup"

-- | The @aria-hidden@ attribute
ariaHidden_ :: Text -> Attribute
ariaHidden_ = makeAttribute "aria-hidden"

-- | The @aria-label@ attribute for svg.
ariaLabel_ :: Text -> Attribute
ariaLabel_ = makeAttribute "aria-label"

-- | The @aria-labelledby@ attribute for svg.
ariaLabelledby_ :: Text -> Attribute
ariaLabelledby_ = makeAttribute "aria-labelledby"

-- | The @aria-describedby@ attribute for svg.
ariaDescribedby_ :: Text -> Attribute
ariaDescribedby_ = makeAttribute "aria-describedby"

-- | The @autocapitalize@ attribute
autocapitalize_ :: Text -> Attribute
autocapitalize_ = makeAttribute "autocapitalize"

-- | The @color@ attribute
color_ :: Text -> Attribute
color_ = makeAttribute "color"

-- | The @itemscope@ attribute
itemscope_ :: Text -> Attribute
itemscope_ = makeAttribute "itemscope"

-- | The @itemtype@ attribute
itemtype_ :: Text -> Attribute
itemtype_ = makeAttribute "itemtype"

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
  [ "version", "x", "y", "xmlns:xlink", "xml:space"
  , "enable-background" ] 

svgCamelCaseAttrs :: [String]
svgCamelCaseAttrs = ["viewBox"]

{-
-- | Generates code for a list of HTML attributes
--
-- >>> genSvgAttribs ["xmlns:dc", "xmlns:sodipodi"]

import Lucid.Sanitize
:{
genAttr :: [String] -> IO ()
genAttr = 
  putStr . unlines . map unlines 
  . map (\str -> ["-- | The @" ++ str ++ "@ attribute"
                 , sanitize str ++ " :: Text -> Attribute"
                 , sanitize str ++ " = makeAttribute " ++ show str])


genTag1 :: [String] -> IO ()
genTag1 =
  putStr . unlines . map unlines 
  . map (\str -> ["-- | @" ++ str ++ "@ element"
                 , sanitize str ++ " :: Monad m => [Attribute] -> HtmlT m ()"
                 , sanitize str ++ " = with (makeElementNoEnd " ++ show str ++ ")"])

genSvgAttribs :: [String] -> IO ()
genSvgAttribs = 
  putStr . unlines . map unlines 
  . map (\str -> ["-- | The @" ++ str ++ "@ attribute for svg."
                 , sanitize str ++ " :: Text -> Attribute"
                 , sanitize str ++ " = makeAttribute " ++ show str])
:}

-}
