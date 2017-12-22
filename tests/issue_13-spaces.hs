template :: Html ()
template = do
    tt_ [ class_ "docutils literal" ] $ do
        span_ [ class_ "pre" ] $ "foreign"
        " "
        span_ [ class_ "pre" ] $ "export"
    " and "
    tt_ [ class_ "docutils literal" ] $ do
        span_ [ class_ "pre" ] $ "foreign"
        " "
        span_ [ class_ "pre" ] $ "import"
        " "
        span_ [ class_ "pre" ] $ "ccall"
        " "
        span_ [ class_ "pre" ] $ "\"wrapper\""
    "\n"
