module Main where

import Control.Applicative
import Control.Monad.Identity ( Identity )
import Data.Char ( digitToInt )
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import System.Environment ( getArgs )

import Safe ( readNote )

import Data.List.Split

import Text.Parsec ( ParsecT, alphaNum, digit, char, string
                   , anyChar, eof
                   , many1, sepBy1
                   , try, parse )



type Parser a = ParsecT String () Identity a



newtype Version = Version [Int]
    deriving (Eq, Ord)

instance Show Version where
    show (Version is) = intercalate "." (map show is)

pVersion :: Parser Version
pVersion = Version <$> (try (char '(' *> ints <* char ')') <|> ints)
    where
        ints :: Parser [Int]
        ints = int `sepBy1` char '.'

        int :: Parser Int
        int = do
            is <- map digitToInt <$> many1 digit
            return $ sum $ zipWith (*) (reverse is) (iterate (10*) 1)



data Package = Package { name :: String
                       , availableVersion :: Maybe Version
                       , installedVersions :: [Version]
                       }
    deriving (Eq, Ord, Show)

combinePkg :: [Package] -> [Package]
combinePkg =
    map grp .
    groupBy ((==) `on` name) .
    sortBy (comparing name)
    where
        grp :: [Package] -> Package
        grp [] = error "empty group"
        grp ps@(Package{name=n}:_) =
            Package n
                    (listToMaybe $ mapMaybe availableVersion ps)
                    (nub $ sort $ concatMap installedVersions ps)

showP :: Package -> String
showP (Package n Nothing  is) = n ++ ": a new version might be available. You have " ++ show is ++ "."
showP (Package n (Just a) is) = n ++ ": version " ++ show a ++ " is available. You have " ++ show is ++ "."

parseSkipFinish :: Parser f -> Parser s -> Parser a -> Parser [a]
parseSkipFinish f s a =  (f *> return [])
                     <|> ((:) <$> a <*> parseSkipFinish f s a)
                     <|> (s *> parseSkipFinish f s a)

skipOr :: Parser b -> Parser a -> Parser a
skipOr skip p = try p <|> (skip *> skipOr skip p)

pPackage :: Parser Package
pPackage = 
    Package <$> (string "* " *> many1 (alphaNum <|> char '-'))
            <*> pOne
            <*> skipOr anyChar (string "Installed versions: " *> pVersion `sepBy1` string ", ")
    where
        noneStr = "[ Not available from any configured repository ]"
        pOne    = skipOr anyChar $ string "Default available version: "
                  *> ( Nothing <$  string noneStr <|>
                       Just    <$> pVersion )

pPackages :: Parser [Package]
pPackages = parseSkipFinish eof anyChar pPackage <* eof


pGhcPkgList :: String -> Maybe Package
pGhcPkgList = helper . strip
    where
        helper "" = Nothing
        helper ('/':_) = Nothing
        helper xs =
            let
                parts   = splitOn "-" xs
                pkgname = init parts
                pkgvers = map (\ s -> readNote (show s) s) $ splitOn "." $ last parts
            in
                Just $ Package (intercalate "-" pkgname) Nothing [Version pkgvers]

strip :: String -> String
strip = reverse . stripL . reverse . stripL
    where
        stripL (ch:xs) | ch `elem` " ()" = stripL xs
        stripL xs = xs


main :: IO ()
main = do
    (a:b:_) <- getArgs
    cabalListInstalled <- readFile a
    ghcPkgList <- readFile b
    case parse pPackages "<stdin>" cabalListInstalled of
        Left err -> error $ show err
        Right ps -> do
            let
                fromCabalList = ps
                fromGhcPkgList = mapMaybe pGhcPkgList (lines ghcPkgList)
                allPackages = combinePkg $ fromCabalList ++ fromGhcPkgList
            -- putStrLn $ unlines $ map show fromCabalList
            -- putStrLn $ unlines $ map show fromGhcPkgList
            putStrLn $ unlines $ map showP $
                flip filter allPackages $ \ p ->
                    case availableVersion p of
                        Nothing -> False
                        Just av -> av > last (installedVersions p)
            putStrLn $ unlines $
                "A new version might be availble for the following packages too:" :
                map (("   " ++) . name) (filter (isNothing . availableVersion) allPackages)


