module Main (main) where

import Control.Monad (forever)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Interpreter (interpret)
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
import Parser (parse)
import Resolver (resolve)
import Scanner (scanTokens)
import System.IO (BufferMode (..), hSetBuffering, stdout)

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

    Control.Monad.forever $ do
        putStr "> "
        ln <- TIO.getLine
        print ln

run' :: Text -> IO (Either String ())
run' input = runExceptT $ do
    tokens <- result $ scanTokens input
    statements <- result $ parse tokens
    locals <- result $ resolve statements

    liftIO $ interpret locals statements

type LoxInterpreter = ExceptT String IO

result :: (Show e) => Either e a -> LoxInterpreter a
result = either (throwError . show) pure

run :: Text -> IO ()
run fileContent = do
    result' <- run' fileContent
    case result' of
        Left e -> print e
        Right _ -> pure ()

main :: IO ()
main = process =<< customExecParser (prefs showHelpOnEmpty) parseHloxInput
