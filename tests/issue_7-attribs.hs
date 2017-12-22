template :: Html ()
template = do
    ul_ [ class_ "info-list list-unstyled" ] $ do
        "\n    "
        li_ $ a_ [ href_ "tel://#{@client.support_phone}" ] $ do
            i_ [ class_ "fa fa-phone skip-title-check", ariaHidden_ "true" ] $ ""
            " +91 2066447777 "
        "\n    "
        li_ $ a_ [ href_ "mailto:#{@client.support_email}" ] $ do
            i_ [ class_ "fa fa-envelope", ariaHidden_ "true" ] $ ""
            "info@foliageoutdoors.com"
        "\n"
    "\n"
