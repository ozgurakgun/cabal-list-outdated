module Main where

import Control.Applicative
import Data.List ( intercalate )
import Data.Char ( digitToInt )
import Control.Monad.Identity ( Identity )
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
                       , availableVersion :: Version
                       , installedVersions :: [Version]
                       }
    deriving (Eq, Ord)

instance Show Package where
    show (Package n a is) = n ++ ": version " ++ show a ++ " is available. You have " ++ show is ++ "."

parseSkipFinish :: Parser f -> Parser s -> Parser a -> Parser [a]
parseSkipFinish f s a =  (f *> return [])
                     <|> ((:) <$> a <*> parseSkipFinish f s a)
                     <|> (s *> parseSkipFinish f s a)

skipOr :: Parser b -> Parser a -> Parser a
skipOr skip p = try p <|> (skip *> skipOr skip p)

pPackage :: Parser Package
pPackage = Package <$> (string "* " *> many1 (alphaNum <|> char '-'))
                   <*> skipOr anyChar (string "Default available version: " *> pVersion)
                   <*> skipOr anyChar (string "Installed versions: " *> pVersion `sepBy1` string ", ")

pPackages :: Parser [Package]
pPackages = parseSkipFinish eof anyChar pPackage <* eof



main :: IO ()
main = interact $ \ s ->
        case parse pPackages "<stdin>" s of
            Left err -> error $ show err
            Right ps -> unlines $ map show
                                $ filter (\ p -> availableVersion p > last (installedVersions p) )
                                  ps