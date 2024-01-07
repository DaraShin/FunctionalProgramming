import Data.Map (fromListWith, toList, Map, fromList, (!))
import Data.List (sortOn, insertBy, map)
import Data.Ord (comparing)
import Control.Monad (join)
import qualified Data.ByteString as BS (readFile, unpack, writeFile, pack, ByteString, concat)
import Data.Word (Word8)
import qualified Data.Binary as Bin (encode)
import Data.Bits (testBit)
import qualified Data.ByteString.Char8 as Char8
import Control.Monad.State (runState)


class (Eq a, Show a) => Bit a where
    zero    :: a
    one     :: a

instance Bit Bool where
    zero  = False
    one   = True


data HuffmanTree a
   = Leaf a Int  -- Leaf :: Char -> Int -> HuffmanTree
   | Node (HuffmanTree a) (HuffmanTree a) Int
   deriving (Show)

nodeValue :: HuffmanTree a -> Int
nodeValue (Leaf _ x) = x
nodeValue (Node _ _ x) = x


getFrequency :: Ord a => [a] -> [(a, Int)]
getFrequency = toList . fromListWith (+) . map (\c -> (c, 1))

buildHuffmanTree :: [(a, Int)] -> HuffmanTree a
buildHuffmanTree freqList = build $ map (uncurry Leaf) freqSorted  where
    freqSorted = sortOn snd freqList
    -- build :: [HuffmanTree] -> HuffmanTree
    build [t] = t
    build (t1 : t2 : ts) = build $ insertBy (comparing nodeValue) (joinTrees t1 t2) ts
    --joinTrees :: HuffmanTree -> HuffmanTree -> HuffmanTree 
    joinTrees t1 t2 = Node t1 t2 (nodeValue t1 + nodeValue t2)

huffmanTreeTraverse :: Bit b => HuffmanTree a -> [b] -> [(a, [b])] -> [(a, [b])]
huffmanTreeTraverse (Leaf c _) s codes  = (c, reverse s) : codes
huffmanTreeTraverse (Node t1 t2 _) s codes = huffmanTreeTraverse t2 (one : s) (huffmanTreeTraverse t1 (zero : s) codes)

getCodes :: (Ord a, Bit b) => HuffmanTree a -> Map a [b]
getCodes t = fromList (huffmanTreeTraverse t [] [])


encode :: (Ord a, Bit b) => [a] -> Map a [b] -> [b] 
encode str codes = join $ map (codes !) str 


decodeSymbol :: Bit b => [b] -> HuffmanTree a -> (a, [b])
decodeSymbol s (Leaf c _) = (c, s)
decodeSymbol (hs : ts) (Node t1 t2 c) = decodeSymbol ts tree where
    tree = if hs == zero then t1 else t2

decodeHelper :: Bit b => [b] -> HuffmanTree a -> [a] -> [a]
decodeHelper [] _ l = l
decodeHelper s t l = decodeHelper s' t (c:l)  where
    (c, s') = decodeSymbol s t

decode :: Bit b => [b] -> HuffmanTree a -> [a]
decode s t = reverse $ decodeHelper s t []


bitListToByte :: [Bool] -> Word8
bitListToByte bits = foldl (\word  bit -> 2 * word + (if bit then 1 else 0)) 0 (take 8 (bits ++ repeat False))

boolsToBytes :: [Bool] -> [Word8]
boolsToBytes [] = []
boolsToBytes bits = 
  let (byte, rest) = splitAt 8 bits
  in bitListToByte byte : boolsToBytes rest

boolsToByteString :: [Bool] -> BS.ByteString
boolsToByteString bits = BS.pack $ boolsToBytes bits

archive :: FilePath -> FilePath -> FilePath -> IO()
archive inputFile outputFile treeFile = do
    byteString <- BS.readFile inputFile
    let bytes           = BS.unpack byteString
        frequencyTable  = getFrequency bytes
        encodedStr      = encode bytes (getCodes $ buildHuffmanTree frequencyTable)
        padding         = (8 - (length encodedStr `mod` 8)) `mod` 8  -- number of zeros added to complement the last byte
        paddingBS       = Char8.pack (toEnum padding : "\n")
    writeFile treeFile (show frequencyTable)
    BS.writeFile outputFile  (BS.concat [paddingBS, boolsToByteString encodedStr])


word8ToBoolList :: Word8 -> [Bool]
word8ToBoolList w = [testBit w i | i <- [7,6..0]]

unarchive :: FilePath -> FilePath -> FilePath -> IO ()
unarchive archivedFile treeFile unarchivedFile = do
    byteString      <- BS.readFile archivedFile  
    frequncyList    <- readFile treeFile
    let (toDelete : _ : words) = BS.unpack byteString
        bitsPadded      = join $ map word8ToBoolList words
        bits            = take ((length bitsPadded) - (fromIntegral toDelete)) bitsPadded
        tree            = buildHuffmanTree (read frequncyList)
        decodedBytes    = decode bits tree
    BS.writeFile unarchivedFile $ BS.pack decodedBytes