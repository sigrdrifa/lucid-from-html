template :: Html ()
template = do
    div_ [ class_ "opacity" ] $ "\r\nthis string ends in \\r\\n\r\nThis also contains \t\\t tab-char\r\n"
