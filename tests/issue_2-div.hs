template :: Html ()
template = do
  div_ [ class_ "select-holder" ] $ do
    "\n              "
    toHtmlRaw  "<!-- google translate language select -->"
    "\n              "
    toHtmlRaw  "<!-- currency select -->"
    "\n"
  "\n"
