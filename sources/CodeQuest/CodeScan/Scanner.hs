-- @+leo-ver=4-thin
-- @+node:gcross.20081228030258.25:@thin Scanner.hs
-- @@language haskell

{-# INCLUDE "codescan.h" #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module CodeQuest.CodeScan.Scanner where
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Data.Array.IArray
import Control.Exception

import CodeQuest.CodeScan.CodeGraph

data Connection = Connection
type PConnection = Ptr (Connection)

foreign import ccall safe "scan_4" c_scan_4 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_5" c_scan_5 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_6" c_scan_6 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_7" c_scan_7 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_8" c_scan_8 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_9" c_scan_9 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_10" c_scan_10 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_11" c_scan_11 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_12" c_scan_12 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_13" c_scan_13 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_14" c_scan_14 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_15" c_scan_15 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_16" c_scan_16 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_17" c_scan_17 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_18" c_scan_18 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_19" c_scan_19 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)
foreign import ccall safe "scan_20" c_scan_20 :: PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong)

c_scan_fns :: Array Int (PConnection -> CString -> CInt -> Ptr (Ptr CInt) -> IO (CULLong))
c_scan_fns = listArray (4,20) [c_scan_4,c_scan_5,c_scan_6,c_scan_7,c_scan_8,c_scan_9,c_scan_10,c_scan_11,c_scan_12,c_scan_13,c_scan_14,c_scan_15,c_scan_16,c_scan_17,c_scan_18,c_scan_19,c_scan_20]

foreign import ccall safe "initialize_connection" c_initialize_connection :: CString -> IO (PConnection)
foreign import ccall safe "finalize_connection" c_finalize_connection :: PConnection -> IO ()

withConnectionTo :: String -> (PConnection -> IO a) -> IO a
withConnectionTo connection_information f = bracket
    (withCString connection_information c_initialize_connection)
    c_finalize_connection
    f

withArrays :: Storable a => [[a]] -> (Ptr (Ptr a) -> IO b) -> IO b
withArrays lists f =
    let withSomeArrays list_ptrs remaining_lists =
            case remaining_lists of
              [] -> withArray (reverse list_ptrs) f
              head:tail -> withArray head (\arr -> withSomeArrays (arr:list_ptrs) tail)
    in withSomeArrays [] lists

scan :: PConnection -> String -> IO (Integer)
scan connection graph_name =
    let permutations = (map elems . permutationsOfGraphNamed) graph_name
        number_of_vertices = (length . head) permutations
        number_of_permutations = (length permutations) - 1
        permutationsAsCints :: [[CInt]]
        permutationsAsCints = map (map (fromInteger . toInteger)) (tail permutations)
        c_scan_fn = c_scan_fns ! number_of_vertices
    in do c_number_of_systems_examined <- withCString graph_name $ \c_graph_name ->
                                            withArrays permutationsAsCints $ \c_permutations ->
                                                c_scan_fn connection c_graph_name ((fromInteger . toInteger) number_of_permutations) c_permutations
          return $ toInteger c_number_of_systems_examined

-- @-node:gcross.20081228030258.25:@thin Scanner.hs
-- @-leo
