{-# LANGUAGE ForeignFunctionInterface #-}

module MDict (
  MDict,
  MDictType(..),
  withMDict,
  lookupWord,
  getKeys,
  suggestWords,
  getFileType,
  listAllKeys
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
import Data.List (intercalate)

-- C-compatible struct representation
data SimpleKeyItem = SimpleKeyItem
  { recordStart :: CULong
  , keyWordPtr :: CString
  }

instance Storable SimpleKeyItem where
  sizeOf _ = 16  -- sizeof(simple_key_item) on 64-bit systems
  alignment _ = 8
  peek ptr = SimpleKeyItem
    <$> peekByteOff ptr 0  -- record_start at offset 0
    <*> peekByteOff ptr 8  -- key_word at offset 8
  poke ptr (SimpleKeyItem rs kw) = do
    pokeByteOff ptr 0 rs
    pokeByteOff ptr 8 kw

-- Dictionary handle
newtype MDict = MDict (ForeignPtr ())

-- Dictionary type enumeration
data MDictType = MDX | MDD deriving (Show, Eq, Enum)

-- Foreign function imports
foreign import ccall "mdict_init"
  c_mdict_init :: CString -> IO (Ptr ())

foreign import ccall "mdict_destory"
  c_mdict_destroy :: Ptr () -> IO ()

foreign import ccall "&mdict_destory"
  c_mdict_destroy_finalizer :: FunPtr (Ptr () -> IO ())

foreign import ccall "mdict_lookup"
  c_mdict_lookup :: Ptr () -> CString -> Ptr (Ptr CChar) -> IO ()

foreign import ccall "mdict_keylist"
  c_mdict_keylist :: Ptr () -> Ptr CULong -> IO (Ptr (Ptr SimpleKeyItem))

foreign import ccall "free_simple_key_list"
  c_free_simple_key_list :: Ptr (Ptr SimpleKeyItem) -> CULong -> IO CInt

foreign import ccall "mdict_suggest"
  c_mdict_suggest :: Ptr () -> CString -> Ptr (Ptr (Ptr CChar)) -> CInt -> IO CInt

foreign import ccall "mdict_filetype"
  c_mdict_filetype :: Ptr () -> IO CInt

-- Safe initialization/cleanup
withMDict :: FilePath -> (MDict -> IO a) -> IO a
withMDict path action = 
  withCString path $ \cpath -> 
    bracket
      (do rawPtr <- c_mdict_init cpath
          when (rawPtr == nullPtr) $ 
            throwIO (userError "Failed to initialize MDict")
          fp <- newForeignPtr c_mdict_destroy_finalizer rawPtr  -- Correct finalizer
          return (MDict fp))
      (\_ -> return ())
      action

-- Dictionary lookup with error handling
lookupWord :: MDict -> String -> IO (Either String String)
lookupWord (MDict fptr) word = 
  withForeignPtr fptr $ \ptr ->
    withCString word $ \cword ->
      alloca $ \resultPtr -> do
        c_mdict_lookup ptr cword resultPtr
        result <- peek resultPtr
        if result == nullPtr
          then return $ Left "Word not found"
          else do
            content <- peekUtf8CString result
            return $ Right content

-- only seem to work with MDD
listAllKeys :: MDict -> IO String
listAllKeys dict = do
  result <- getKeys dict
  case result of
    Left err -> return $ "Error: " ++ err
    Right keys -> return $ formatKeys keys
  where
    formatKeys :: [(String, CULong)] -> String
    formatKeys = unlines . map (\(k, o) -> k ++ " (" ++ show o ++ ")")

-- Key list retrieval with proper cleanup
getKeys :: MDict -> IO (Either String [(String, CULong)])
getKeys (MDict fptr) = 
  withForeignPtr fptr $ \ptr ->
    alloca $ \lenPtr -> do
      arrPtr <- c_mdict_keylist ptr lenPtr
      len <- peek lenPtr
      if arrPtr == nullPtr
        then return $ Left "Failed to retrieve key list"
        else do
          keyPtrs <- peekArray (fromIntegral len) arrPtr
          items <- mapM (handleKeyItem arrPtr len) keyPtrs
          c_free_simple_key_list arrPtr len >>= checkFreeResult
          return $ Right items
  where
    handleKeyItem arrPtr len ptr = do
      item <- peek ptr
      key <- peekUtf8CString (keyWordPtr item)
      return (key, recordStart item)
    
    checkFreeResult rc = when (rc /= 0) $
      throwIO $ userError "Failed to free key list memory"

-- Suggestion handling with error checking
suggestWords :: MDict -> String -> Int -> IO (Either String [String])
suggestWords (MDict fptr) word maxSuggestions =
  withForeignPtr fptr $ \ptr ->
    withCString word $ \cword ->
      alloca $ \resultsPtr -> do
        rc <- c_mdict_suggest ptr cword resultsPtr (fromIntegral maxSuggestions)
        if rc /= 0
          then return $ Left "Suggestion failed"
          else do
            suggestionsPtr <- peek resultsPtr
            if suggestionsPtr == nullPtr
              then return $ Right []
              else do
                suggestionPtrs <- peekArray maxSuggestions suggestionsPtr
                suggestions <- mapM peekUtf8CString suggestionPtrs
                return $ Right suggestions

-- File type detection
getFileType :: MDict -> IO (Either String MDictType)
getFileType (MDict fptr) = 
  withForeignPtr fptr $ \ptr -> do
    ft <- c_mdict_filetype ptr
    return $ case ft of
      0 -> Right MDX
      1 -> Right MDD
      _ -> Left "Unknown file type"

-- UTF-8 decoding with error recovery
peekUtf8CString :: CString -> IO String
peekUtf8CString cstr = do
  bs <- BS.packCString cstr
  case decodeUtf8' bs of
    Left _ -> return "<invalid utf-8>"
    Right text -> return $ unpack text
