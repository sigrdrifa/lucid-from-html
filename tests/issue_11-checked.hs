template :: Html ()
template = do
    input_ [ type_ "checkbox", name_ "vehicle", value_ "Car", checked_ ]
    " Just checked"
    br_ []
    "\n"
    input_ [ type_ "checkbox", name_ "vehicle", value_ "Car", checked_ ]
    " Checked with parameter"
    br_ []
    "\n"
