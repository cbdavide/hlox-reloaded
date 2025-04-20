import Test.Tasty ( defaultMain, testGroup )
import Test.Tasty.Hspec ( testSpecs )

import ScannerSpec ( scannerSpecs )
import ParserSpec ( parserSpecs )

main :: IO ()
main = do
    specs <- concat <$> mapM testSpecs [scannerSpecs, parserSpecs]
    defaultMain $ testGroup "All tests" [ testGroup "Specs" specs]
