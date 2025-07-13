module Main (main) where

import Control.Monad (forever)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Options.Applicative (
    Parser,
    ParserInfo,
    customExecParser,
    fullDesc,
    help,
    helper,
    info,
    metavar,
    optional,
    prefs,
    progDesc,
    showHelpOnEmpty,
    strArgument,
    (<**>),
 )
import System.IO (hSetBuffering, stdout, BufferMode (..))

newtype HloxInterpreterInput = HloxInterpreterInput
    { filePath :: Maybe FilePath
    }
    deriving (Eq, Show)

parseInputData :: Parser HloxInterpreterInput
parseInputData =
    HloxInterpreterInput
        <$> optional (strArgument (metavar "File" <> help "Input program"))

parseHloxInput :: ParserInfo HloxInterpreterInput
parseHloxInput = info (parseInputData <**> helper) (fullDesc <> progDesc "Hlox Interpreter")

process :: HloxInterpreterInput -> IO ()
process input = do
    maybe runPrompt runFile (filePath input)

runFile :: FilePath -> IO ()
runFile inputFile = do
    content <- TIO.readFile inputFile
    run content


runPrompt :: IO ()
runPrompt = do
    -- by default ouput is written to a buffer until there's
    -- a newline and then it is written to stdout, this disables
    -- the buffer
    hSetBuffering stdout NoBuffering

    forever $ do
        putStr "> "
        line <- TIO.getLine
        print line

run :: Text -> IO ()
run fileContent = undefined

main :: IO ()
main = process =<< customExecParser (prefs showHelpOnEmpty) parseHloxInput
