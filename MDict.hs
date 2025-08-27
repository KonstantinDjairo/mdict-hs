{-# LANGUAGE ForeignFunctionInterface #-}

module MDict (
  MDict,
  MDictType(..),
  withMDict,
  lookupWord,
  locateWord,
  parseDefinition,
  getKeys,
  getFileType,
  listAllKeys
) where

import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castFunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Control.Exception (bracket, throwIO)
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Char8 as BSC
import Data.List (intercalate)
import Data.Word (Word64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE


-- C-compatible struct
data SimpleKeyItem = SimpleKeyItem
  { recordStart :: Word64
  , keyWordPtr  :: CString
  }

instance Storable SimpleKeyItem where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = SimpleKeyItem <$> peekByteOff ptr 0 <*> peekByteOff ptr 8
  poke ptr (SimpleKeyItem rs kw) = pokeByteOff ptr 0 rs >> pokeByteOff ptr 8 kw

-- Dictionary handle
newtype MDict = MDict (ForeignPtr ())

-- Dictionary type
data MDictType = MDX | MDD deriving (Show, Eq, Enum)

-- FFI imports
foreign import ccall "mdict_init"         c_mdict_init :: CString -> IO (Ptr ())
foreign import ccall "mdict_destory"      c_mdict_destroy :: Ptr () -> IO CInt
foreign import ccall "&mdict_destory"     raw_mdict_destroy_finalizer :: FunPtr (Ptr () -> IO CInt)
foreign import ccall "mdict_lookup"       c_mdict_lookup :: Ptr () -> CString -> Ptr (Ptr CChar) -> IO ()
foreign import ccall "mdict_locate"       c_mdict_locate :: Ptr () -> CString -> Ptr (Ptr CChar) -> CInt -> IO ()
foreign import ccall "mdict_parse_definition" c_mdict_parse_definition :: Ptr () -> CString -> Word64 -> Ptr (Ptr CChar) -> IO ()
foreign import ccall "mdict_keylist"      c_mdict_keylist :: Ptr () -> Ptr Word64 -> IO (Ptr (Ptr SimpleKeyItem))
foreign import ccall "free_simple_key_list" c_free_simple_key_list :: Ptr (Ptr SimpleKeyItem) -> Word64 -> IO CInt
foreign import ccall "mdict_filetype"     c_mdict_filetype :: Ptr () -> IO CInt
foreign import ccall "mdict_suggest"      c_mdict_suggest :: Ptr () -> CString -> Ptr (Ptr (Ptr CChar)) -> CInt -> IO ()
foreign import ccall "mdict_stem"         c_mdict_stem :: Ptr () -> CString -> Ptr (Ptr (Ptr CChar)) -> CInt -> IO ()
foreign import ccall unsafe "string.h strlen" c_strlen :: CString -> IO CSize

-- Cast finalizer to expected type for newForeignPtr
castFinalizer :: FunPtr (Ptr () -> IO CInt) -> FinalizerPtr a
castFinalizer = castFunPtr

-- Safe dictionary initialization
withMDict :: FilePath -> (MDict -> IO a) -> IO a
withMDict path action = withCString path $ \cpath ->
  bracket
    (do rawPtr <- c_mdict_init cpath
        when (rawPtr == nullPtr) $ throwIO (userError "Failed to initialize MDict")
        fp <- newForeignPtr (castFinalizer raw_mdict_destroy_finalizer) rawPtr
        return (MDict fp))
    (\_ -> return ())  -- ForeignPtr handles cleanup
    action

-- Lookup
-- | Lookup a word and return decoded UTF-8 Text
lookupWord :: MDict -> String -> IO (Either String T.Text)
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
            -- Decode as UTF-8
            case TE.decodeUtf8' bs of
              Left err  -> return $ Left ("UTF-8 decode error: " ++ show err)
              Right txt -> return $ Right txt

-- Locate
locateWord :: MDict -> String -> Int -> IO (Either String BS.ByteString)
locateWord (MDict fptr) word enc = withForeignPtr fptr $ \ptr ->
  withCString word $ \cword ->
    alloca $ \resultPtr -> do
      c_mdict_locate ptr cword resultPtr (fromIntegral enc)
      result <- peek resultPtr
      if result == nullPtr then return $ Left "Word not found"
      else do
        len <- c_strlen result
        bs <- BSI.create (fromIntegral len) $ \p -> BSI.memcpy p (castPtr result) (fromIntegral len)
        return $ Right bs

-- Parse definition
parseDefinition :: MDict -> String -> Word64 -> IO (Either String BS.ByteString)
parseDefinition (MDict fptr) word start = withForeignPtr fptr $ \ptr ->
  withCString word $ \cword ->
    alloca $ \resultPtr -> do
      c_mdict_parse_definition ptr cword start resultPtr
      result <- peek resultPtr
      if result == nullPtr then return $ Left "Definition not found"
      else do
        len <- c_strlen result
        bs <- BSI.create (fromIntegral len) $ \p -> BSI.memcpy p (castPtr result) (fromIntegral len)
        return $ Right bs

-- Get keys
type KeyEntry = (BS.ByteString, Word64)
getKeys :: MDict -> IO (Either String [KeyEntry])
getKeys (MDict fptr) = withForeignPtr fptr $ \ptr ->
  alloca $ \lenPtr -> do
    arrPtr <- c_mdict_keylist ptr lenPtr
    len <- peek lenPtr
    if arrPtr == nullPtr then return $ Left "Failed to retrieve key list"
    else do
      keyPtrs <- peekArray (fromIntegral len) arrPtr
      items <- mapM handleItem keyPtrs
      _ <- c_free_simple_key_list arrPtr len
      return $ Right items
  where
    handleItem ptr = do
      item <- peek ptr
      bs <- packCString (keyWordPtr item)
      return (bs, recordStart item)
    packCString cstr = do
      len <- c_strlen cstr
      BSI.create (fromIntegral len) $ \p -> BSI.memcpy p (castPtr cstr) (fromIntegral len)

-- File type
getFileType :: MDict -> IO (Either String MDictType)
getFileType (MDict fptr) = withForeignPtr fptr $ \ptr -> do
  ft <- c_mdict_filetype ptr
  return $ case ft of
    0 -> Right MDX
    1 -> Right MDD
    _ -> Left "Unknown file type"

-- List all keys
listAllKeys :: MDict -> IO String
listAllKeys dict = do
  ekeys <- getKeys dict
  case ekeys of
    Left err -> return $ "Error: " ++ err
    Right keys -> return $ unlines [BSC.unpack (BSC.take 50 bs) ++ "... (" ++ show off ++ ")" | (bs, off) <- keys]
