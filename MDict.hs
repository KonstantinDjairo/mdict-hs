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
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Char8 as BSC
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


-- we need to use it somewhere, to clean memory
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
          fp <- newForeignPtr c_mdict_destroy_finalizer rawPtr
          return (MDict fp))
      (\_ -> return ())
      action

-- Dictionary lookup with raw bytes
lookupWord :: MDict -> String -> IO (Either String BS.ByteString)
lookupWord (MDict fptr) word = 
  withForeignPtr fptr $ \ptr ->
    withCString word $ \cword ->
      alloca $ \resultPtr -> do
        c_mdict_lookup ptr cword resultPtr
        result <- peek resultPtr
        if result == nullPtr
          then return $ Left "Word not found"
          else do
            len <- c_strlen result
            bs <- BSI.create (fromIntegral len) $ \p -> 
              BSI.memcpy p (castPtr result) (fromIntegral len)
            return $ Right bs

-- Key list handling
type KeyEntry = (BS.ByteString, CULong)

getKeys :: MDict -> IO (Either String [KeyEntry])
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
          freeResult <- c_free_simple_key_list arrPtr len
          if freeResult /= 0
            then return $ Left "Failed to free key list memory"
            else return $ Right items
  where
    handleKeyItem arrPtr len ptr = do
      item <- peek ptr
      key <- packKeyBytes (keyWordPtr item)
      return (key, recordStart item)
    
    packKeyBytes :: CString -> IO BS.ByteString
    packKeyBytes cstr = do
      len <- c_strlen cstr
      BSI.create (fromIntegral len) $ \p -> 
        BSI.memcpy p (castPtr cstr) (fromIntegral len)

-- Safe suggestion handling
suggestWords :: MDict -> String -> Int -> IO (Either String [BS.ByteString])
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
                results <- traverse readSuggestion suggestionPtrs
                return $ sequence results
  where
    readSuggestion :: Ptr CChar -> IO (Either String BS.ByteString)
    readSuggestion ptr = do
      if ptr == nullPtr
        then return $ Left "Null suggestion pointer"
        else do
          len <- c_strlen ptr
          bs <- BSI.create (fromIntegral len) $ \p -> 
            BSI.memcpy p (castPtr ptr) (fromIntegral len)
          return $ Right bs


-- File type detection
getFileType :: MDict -> IO (Either String MDictType)
getFileType (MDict fptr) = 
  withForeignPtr fptr $ \ptr -> do
    ft <- c_mdict_filetype ptr
    return $ case ft of
      0 -> Right MDX
      1 -> Right MDD
      _ -> Left "Unknown file type"

-- Helper for C string length
foreign import ccall unsafe "string.h strlen"
  c_strlen :: CString -> IO CSize

-- Formatted list output
listAllKeys :: MDict -> IO String
listAllKeys dict = do
  result <- getKeys dict
  case result of
    Left err -> return $ "Error: " ++ err
    Right keys -> return $ formatKeys keys
  where
    formatKeys :: [KeyEntry] -> String
    formatKeys entries = intercalate "\n" $ map formatEntry entries
    
    formatEntry :: KeyEntry -> String
    formatEntry (bs, offset) =
      BSC.unpack (BSC.take 50 bs) ++ "... (" ++ show offset ++ ")"
