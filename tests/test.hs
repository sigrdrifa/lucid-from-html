import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Lucid.Combinators (html5S)
import Lucid.Generate (Options(..), lucidFromHtml)
import System.FilePath (takeBaseName, replaceExtension)

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  let opts = Options { ignore_ = False, noTrimText_ = True }
      lucidFromHtml' = lucidFromHtml html5S opts "template" 
  htmlFiles <- findByExtension [".html"] "."
  return $ testGroup "HtmlToLucid golden tests"
    [ goldenVsString
        (takeBaseName htmlFile) -- test name
        lucidFile -- golden file path
        (LBS.pack . lucidFromHtml' <$> readFile htmlFile) -- action whose result is tested
    | htmlFile <- htmlFiles
    , let lucidFile = replaceExtension htmlFile ".hs"
    ]
