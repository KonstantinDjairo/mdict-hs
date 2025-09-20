-- Copyright © 2025 Hashirama Senju
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module MDict (
  MDict,
  isMDDFile,
  MDictType(..),
  withMDict,
  lookupWord,
  locateWord,
  parseDefinition,
  getKeys,
  getFileType,
  mimeDetect,
  destroyDict,
  listAllKeys,
  replaceMedia,
  lookupInCollection
) where

import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castFunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Control.Exception (bracket, throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC
import Data.List (intercalate)
import Data.Word (Word64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Char (toLower)
import System.FilePath (takeExtension)
import Text.HTML.Scalpel
import Data.Maybe (fromMaybe)
import Data.List (isInfixOf)
import Control.Monad
import System.FilePath (replaceExtension)
import Control.DeepSeq (NFData, deepseq)
import Wazahs

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

isMDDFile :: FilePath -> Bool
isMDDFile path = map toLower (takeExtension path) == ".mdd"


-- FFI imports
foreign import ccall "mdict_init"         c_mdict_init :: CString -> IO (Ptr ())
foreign import ccall "mdict_destory"      c_mdict_destroy :: Ptr () -> IO CInt
foreign import ccall "&mdict_destory"     raw_mdict_destroy_finalizer :: FunPtr (Ptr () -> IO CInt)
foreign import ccall "mdict_lookup"       c_mdict_lookup :: Ptr () -> CString -> Ptr (Ptr CChar) -> IO ()
foreign import ccall "mdict_atomic_lookup"  c_mdict_atomic_lookup :: CString -> CString -> IO CString
foreign import ccall "mdict_locate"       c_mdict_locate :: Ptr () -> CString -> Ptr (Ptr CChar) -> CInt -> IO ()
foreign import ccall "mdict_parse_definition" c_mdict_parse_definition :: Ptr () -> CString -> Word64 -> Ptr (Ptr CChar) -> IO ()
foreign import ccall "mdict_keylist"      c_mdict_keylist :: Ptr () -> Ptr Word64 -> IO (Ptr (Ptr SimpleKeyItem))
foreign import ccall "free_simple_key_list" c_free_simple_key_list :: Ptr (Ptr SimpleKeyItem) -> Word64 -> IO CInt
foreign import ccall "mdict_filetype"     c_mdict_filetype :: Ptr () -> IO CInt
foreign import ccall "mdict_suggest"      c_mdict_suggest :: Ptr () -> CString -> Ptr (Ptr (Ptr CChar)) -> CInt -> IO ()
foreign import ccall "mdict_stem"         c_mdict_stem :: Ptr () -> CString -> Ptr (Ptr (Ptr CChar)) -> CInt -> IO ()
foreign import ccall unsafe "string.h strlen" c_strlen :: CString -> IO CSize

foreign import ccall unsafe "c_mime_detect"
  c_mime_detect :: CString -> IO CString

-- Convert a Haskell String to CString, call the C function, get back a Haskell String
mimeDetect :: String -> IO String
mimeDetect filename =
  withCString filename $ \cstr -> do
    result <- c_mime_detect cstr
    peekCString result


-- Define a Haskell version of the enum
data MDictEncoding = MDBase64 | MDHex

-- Convert to CInt for FFI
encodeToCInt :: MDictEncoding -> CInt
encodeToCInt MDBase64 = 0
encodeToCInt MDHex    = 1


-- | Destroy a dictionary
destroyDict :: MDict -> IO ()
destroyDict (MDict fptr) = withForeignPtr fptr $ \ptr -> do
  _ <- c_mdict_destroy ptr
  return ()


-- Cast finalizer to expected type for newForeignPtr
castFinalizer :: FunPtr (Ptr () -> IO CInt) -> FinalizerPtr a
castFinalizer = castFunPtr

-- Safe dictionary initialization
withMDict :: NFData a => FilePath -> (MDict -> IO a) -> IO a
withMDict path action = withCString path $ \cpath -> do
    rawPtr <- c_mdict_init cpath
    when (rawPtr == nullPtr) $
        throwIO (userError "Failed to initialize MDict")

    fp <- newForeignPtr (castFinalizer raw_mdict_destroy_finalizer) rawPtr
    let dict = MDict fp

    result <- action dict
    result `deepseq` return result

lookupWordAtomic :: FilePath -> String -> IO String
lookupWordAtomic dictFile key =
  withCString dictFile $ \cfile ->
  withCString key $ \ckey -> do
      cstr <- c_mdict_atomic_lookup cfile ckey
      result <- peekCString cstr
      free cstr
      return result


-- Lookup
lookupWord :: MDict -> String -> String -> IO (Either String String)
lookupWord (MDict fptr) dictFile word =
  withForeignPtr fptr $ \ptr ->
    withCString word $ \cword ->
      alloca $ \resultPtr -> do
        let isMDD = isMDDFile dictFile
        if not isMDD
          then c_mdict_lookup ptr cword resultPtr
          else c_mdict_locate ptr cword resultPtr 0  -- force base64

        result <- peek resultPtr
        if result == nullPtr
          then return $ Left "Word not found"
          else do
            str <- peekCString (castPtr result)
            -- MIME detection can be applied here if desired
            mime <- mimeDetect word
            let finalStr =
                  if mime /= "application/octet-stream"
                    then "data:" ++ mime ++ ";base64," ++ str
                    else str
            return $ Right finalStr



-- Locate
locateWord :: MDict -> String -> Bool -> IO (Either String BS.ByteString)
locateWord (MDict fptr) word hexOutput = 
  withForeignPtr fptr $ \ptr ->
    withCString word $ \cword ->
      alloca $ \resultPtr -> do
        let enc = if hexOutput then 1 else 0  -- MDD_ENCODING_HEX or BASE64
        c_mdict_locate ptr cword resultPtr (fromIntegral enc)
        result <- peek resultPtr
        if result == nullPtr
          then return $ Left "No result"
          else do
            len <- c_strlen result
            bs <- BSI.create (fromIntegral len) $ \p ->
              BSI.memcpy p (castPtr result) (fromIntegral len)
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

-- Media related functions:

-- | Replace all local <img> sources with base64 content from .mdd
replaceMedia :: FilePath -> String -> IO String
replaceMedia mdxFile html = do
    let urlsRaw = scrapeStringLike html $ chroots "img" $ attr "src" anySelector
        urls = fromMaybe [] urlsRaw
        localPaths = filter (not . isInfixOf "://") urls
        mddFile = replaceExtension mdxFile ".mdd"
    foldM (replaceOne mddFile) html localPaths

-- | Replace a single <img> src with base64 content
replaceOne :: FilePath -> String -> String -> IO String
replaceOne mddFile html orig = do
    let lookupQuery = "\\" ++ map slashToBack orig
    base64 <- lookupMedia mddFile lookupQuery
    return $ replaceSrc orig base64 html

-- | Helper: convert '/' to '\'
slashToBack :: Char -> Char
slashToBack '/' = '\\'
slashToBack c   = c

-- | Replace the original src attribute with the base64 string
replaceSrc :: String -> String -> String -> String
replaceSrc orig base64 html =
    T.unpack $ T.replace (T.pack $ "src=\"" ++ orig ++ "\"")
                         (T.pack $ "src=\"" ++ base64 ++ "\"")
                         (T.pack html)


-- | Lookup the actual media in a .mdd using your MDict bindings
lookupMedia :: FilePath -> String -> IO String
lookupMedia mddFile query = do
    -- Open the MDD dictionary
    withMDict mddFile $ \mdd -> do
        result <- lookupWord mdd mddFile query
        case result of
            Left err -> do
                putStrLn $ "ERROR: " ++ err
                return ""  -- leave empty if missing
            Right base64 -> do
              --  putStrLn $ "DEBUG: Found media for " ++ query
                return base64


processDictionary :: FilePath -> String -> IO (Either String String)
processDictionary dictFile queryKey =
    withMDict dictFile $ \dict -> do
        result <- lookupWord dict dictFile queryKey
        case result of
            Left err -> return (Left err)
            Right html -> do
                htmlWithMedia <- replaceMedia dictFile html
                return (Right htmlWithMedia)

-- | fmap it over a collection of dictionaries
lookupInCollection :: FilePath -> String -> IO ()
lookupInCollection dir queryKey = do
    files <- listFiles dir
    forM_ files $ \file -> do
        putStrLn $ "Processing: " ++ file
        html <- lookupWordAtomic file queryKey -- needs to combine with replaceMedia
        putStrLn html
        putStrLn "-----"
