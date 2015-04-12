import BasePrelude
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as TE
import qualified ListT as L
import ListT.Text
import qualified Data.ByteString as B
import Data.Functor.Identity


main = 
  hspec $ do
    context "utf8ByteString" $ do
      it "back and forth" $
        property $ \text ->
          TE.encodeUtf8 text & streamByteString 3 & utf8ByteString & L.toList & runIdentity & mconcat &
          (==) text

streamByteString :: Monad m => Int -> ByteString -> L.ListT m ByteString
streamByteString n b =
  B.splitAt n b & \(l, r) -> 
  if B.null l
    then mzero
    else L.cons l (streamByteString n r)
