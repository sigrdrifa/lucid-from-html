template :: Html ()
template = do
    div_ [ class_ "icon" ] $ do
        "\n      "
        div_ [ class_ "svg svg-facebook" ] $ do
            "\n  "
            toHtmlRaw  "<!-- <svg version=\"1.1\" id=\"Layer_1\"> \n  </svg>\n  -->"
            "\n      "
        " Facebook\n"
    "\n"
