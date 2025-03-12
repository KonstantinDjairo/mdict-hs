{-# LANGUAGE ForeignFunctionInterface #-}

module MDict (
  MDict,
  MDictType(..),
  withMDict,
  lookupWord,
  getKeys,
  suggestWords
) where

import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Control.Exception (bracket, throwIO)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8')
import Data.Text (Text, unpack)

-- Safe handle type
newtype MDict = MDict (ForeignPtr ())

-- Dictionary type enum
data MDictType = MDX | MDD deriving (Show, Eq)

-- Foreign imports
foreign import ccall "mdict_init"
  c_mdict_init :: CString -> IO (Ptr ())

-- well, that's a typo in the c++ library, but im too lazy to correct it.
foreign import ccall "mdict_destory"
  c_mdict_destroy :: Ptr () -> IO ()

foreign import ccall "&mdict_destory"
  c_mdict_destroy_finalizer :: FunPtr (Ptr () -> IO ())

foreign import ccall "mdict_lookup"
  c_mdict_lookup :: Ptr () -> CString -> Ptr (Ptr CChar) -> IO ()

foreign import ccall "mdict_keylist"
  c_mdict_keylist :: Ptr () -> Ptr CULong -> IO (Ptr (Ptr CChar))

foreign import ccall "free_simple_key_list"
  c_free_simple_key_list :: Ptr (Ptr CChar) -> CULong -> IO ()

foreign import ccall "mdict_suggest"
  c_mdict_suggest :: Ptr () -> CString -> Ptr (Ptr (Ptr CChar)) -> CInt -> IO ()

foreign import ccall "mdict_filetype"
  c_mdict_filetype :: Ptr () -> IO CInt

-- Custom UTF-8 string decoder
peekUtf8CString :: CString -> IO String
peekUtf8CString cstr = do
  bs <- BS.packCString cstr
  case decodeUtf8' bs of
    Left _ -> return ""
    Right text -> return $ unpack text

-- Safe initialization/destruction
withMDict :: FilePath -> (MDict -> IO a) -> IO a
withMDict path action = 
  withCString path $ \cpath -> 
    bracket
      (do rawPtr <- c_mdict_init cpath
          when (rawPtr == nullPtr) $ 
            throwIO (userError "Failed to initialize MDict")
          fptr <- newForeignPtr c_mdict_destroy_finalizer rawPtr
          return (MDict fptr))
      (\_ -> return ())
      action

-- Safe lookup function
lookupWord :: MDict -> String -> IO (Maybe String)
lookupWord (MDict fptr) word = 
  withForeignPtr fptr $ \ptr ->
    withCString word $ \cword ->
      alloca $ \resultPtr -> do
        c_mdict_lookup ptr cword resultPtr
        result <- peek resultPtr
        if result == nullPtr
          then return Nothing
          else Just <$> peekUtf8CString result

-- Key list handling
getKeys :: MDict -> IO [String]
getKeys (MDict fptr) = 
  withForeignPtr fptr $ \ptr ->
    alloca $ \lenPtr -> do
      keysPtr <- c_mdict_keylist ptr lenPtr
      len <- peek lenPtr
      if keysPtr == nullPtr
        then return []
        else do
          keyPtrs <- peekArray (fromIntegral len) keysPtr
          keys <- mapM peekUtf8CString keyPtrs
          c_free_simple_key_list keysPtr (fromIntegral len)
          return keys

-- Suggestion handling
suggestWords :: MDict -> String -> Int -> IO [String]
suggestWords (MDict fptr) word maxSuggestions =
  withForeignPtr fptr $ \ptr ->
    withCString word $ \cword ->
      alloca $ \resultsPtr -> do
        c_mdict_suggest ptr cword resultsPtr (fromIntegral maxSuggestions)
        results <- peek resultsPtr
        if results == nullPtr
          then return []
          else do
            suggestions <- peekArray maxSuggestions results
            mapM peekUtf8CString suggestions
