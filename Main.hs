module Main where

import Data.Maybe
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding
import Options.Applicative

data WcOptions = WcOptions {
    l :: Bool,
    c :: Bool,
    m :: Bool,
    w :: Bool,
    filename :: Maybe String
}

wc :: WcOptions -> IO ()
wc opts = do
    contents <- fromMaybe getContents $ readFile <$> filename opts
    when (none || l opts) $ print $ length $ lines contents
    when (none || w opts) $ print $ length $ words contents 
    when (none || m opts) $ print $ length contents
    when (none || c opts) $ print $ B.length $ encodeUtf8 $ T.pack contents
    where
        none = not $ l opts || c opts || m opts || w opts

main :: IO ()
main = execParser (info parser idm) >>= wc
    where
        parser = WcOptions <$> 
            switch (
                short 'l' <>
                long "lines" <>
                help "Write number of lines to stdout") <*> 
            switch (
                short 'c' <>
                long "bytes" <>
                help "Write number of bytes to stdout") <*> 
            switch (
                short 'm' <>
                long "chars" <>
                help "Write number of characters to stdout") <*> 
            switch (
                short 'w' <>
                long "words" <>
                help "write number of words to the stdout") <*> 
            optional (
                strArgument $ metavar "filename") <**> 
            helper
