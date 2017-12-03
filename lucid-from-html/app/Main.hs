{-# LANGUAGE PackageImports #-}

module Main (
    main
)   where

import "base" Control.Monad (forM_, when)
import "base" Control.Applicative ((<$>))
import "base" System.Environment (getArgs)
import "base" Data.Maybe (listToMaybe, fromMaybe)
import "filepath" System.FilePath (dropExtension)
import qualified "containers" Data.Map as M
import "base" System.Console.GetOpt
import "base" System.Exit
import "base" System.IO

import Lucid.Generate
import Lucid.Combinators

-- | Main function
--
main :: IO ()
main = do
    args <- getOpt Permute options <$> getArgs
    let (o, n, errs) = args
    case () of
      _ | elem ArgHelp o  -> putStr help
        | not (null errs) -> do hPutStr stderr (concat errs)
                                hPutStrLn stderr "use -h for usage help"
                                exitFailure
        | otherwise       -> let v = getVariant o
                                 s = standalone' o
                                 i = ignore' o
                                 t = trim' o
                                 opts = Options i t
                             in do imports' v o
                                   main' v s opts n
  where
    -- No files given, work with stdin
    main' variant standalone opts [] = interact $
        lucidFromHtml variant standalone opts "template"

    -- Handle all files
    main' variant standalone opts files = forM_ files $ \file -> do
        body <- readFile file
        putStrLn $ lucidFromHtml variant standalone opts
                                 (dropExtension file) body

    -- Print imports if needed
    imports' variant opts = when (standalone' opts) $
        putStrLn $ unlines $ getImports variant

    -- Should we produce standalone code?
    standalone' opts = ArgStandalone `elem` opts

    -- Should we ignore errors?
    ignore' opts = ArgIgnoreErrors `elem` opts

    -- Should we trim whitespace from text?
    trim' opts = ArgNoTrimText `elem` opts

    -- Get the variant from the options
    getVariant opts = fromMaybe defaultHtmlVariant $ listToMaybe $
        flip concatMap opts $ \o -> case o of (ArgHtmlVariant x) -> [x]
                                              _ -> []

-- | Help information.
--
help :: String
help = unlines $
    [ "This is a tool to convert HTML code to LucidHtml code. It is still"
    , "experimental and the results might need to be edited manually."
    , ""
    , "USAGE"
    , ""
    , "  lucid-from-html [OPTIONS...] [FILES ...]"
    , ""
    , "When no files are given, it works as a filter."
    , ""
    , "EXAMPLE"
    , ""
    , "  lucid-from-html -v html4-strict index.html"
    , ""
    , "This converts the index.html file to Haskell code, writing to stdout."
    , ""
    , "OPTIONS"
    , usageInfo "" options
    , "VARIANTS"
    , ""
    ] ++
    map (("  " ++) . fst) (M.toList htmlVariants) ++
    [ ""
    , "By default, " ++ show defaultHtmlVariant ++ " is used."
    ]

-- | Options for the CLI program
--
data Arg = ArgHtmlVariant HtmlVariant
         | ArgStandalone
         | ArgIgnoreErrors
         | ArgNoTrimText
         | ArgHelp
         deriving (Show, Eq)

-- | A description of the options
--
options :: [OptDescr Arg]
options =
    [ Option "v" ["html-variant"] htmlVariantOption "HTML variant to use"
    , Option "s" ["standalone"] (NoArg ArgStandalone) "Produce standalone code"
    , Option "e" ["ignore-errors"] (NoArg ArgIgnoreErrors) "Ignore most errors"
    , Option "t" ["no-trim-text"]  (NoArg ArgNoTrimText) "Do not trim text"
    , Option "h" ["help"] (NoArg ArgHelp) "Show help"
    ]
  where
    htmlVariantOption = flip ReqArg "VARIANT" $ \name -> ArgHtmlVariant $
        fromMaybe (error $ "No HTML variant called " ++ name ++ " found.")
                  (M.lookup name htmlVariants)

-- | The default HTML variant
--
defaultHtmlVariant :: HtmlVariant
defaultHtmlVariant = html5
