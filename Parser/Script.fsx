#load "Parser.fs"
open Parser

let x = %"erik"

parse (mreturn %"hej") %"fisk"
parse item %"fisk"
parse (item >> item) %"fisk"

parse (item >> item >> (mreturn "FOO")) %"fisk"

parse (ch 'a') %"apa"
parse (ch 'b') %"apa"

parse ((ch 'a') >> (ch 'p')) %"apa"
parse ((ch 'a') >> (ch 'ö')) %"apa"

let go = (ch 'g') >> (ch 'o')
parse go %"go"

'r' + 'a'

let goo =
    (ch 'g') >>= fun c1 ->
    (ch 'o') >>= fun c2 ->
    mreturn (charListToStr [c1;c2])

parse goo %"goo"
parse goo %"ggo"


type Lang = A | B | C
let lang = (ch 'f') >> mreturn B
parse lang %"fff"
parse lang %"ggg"
















