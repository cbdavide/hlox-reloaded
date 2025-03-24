import Test.Tasty ( defaultMain, testGroup )
import Test.Tasty.Hspec ( testSpecs )

import ScannerSpec ( scannerSpecs )

main :: IO ()
main = do
    specs <- concat <$> mapM testSpecs [scannerSpecs]
    defaultMain $ testGroup "All tests" [ testGroup "Specs" specs]
