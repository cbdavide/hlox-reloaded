import Test.Tasty ( defaultMain, testGroup )
import Test.Tasty.Hspec ( testSpecs )

import ScannerSpec ( scannerSpecs )
import ParserSpec ( parserSpecs )
import InterpreterSpec ( interpreterSpecs )

main :: IO ()
main = do
    specs <- concat <$> mapM testSpecs [scannerSpecs, parserSpecs, interpreterSpecs]
    defaultMain $ testGroup "All tests" [ testGroup "Specs" specs]
