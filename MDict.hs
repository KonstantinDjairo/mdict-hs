{-# LANGUAGE ForeignFunctionInterface #-}

-- Copyright © 2025 Hashirama Senju

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
import Foreign.ForeignPtr
import Control.Exception (bracket, throwIO)
import Control.Monad (when)

-- Safe handle type
newtype MDict = MDict (ForeignPtr ())

-- Dictionary type enum
data MDictType = MDX | MDD deriving (Show, Eq)

-- Finalizer wrapper
foreign import ccall "wrapper"
  mkDestroyFinalizer :: (Ptr () -> IO ()) -> IO (FunPtr (Ptr () -> IO ()))

-- Foreign imports with corrected types
foreign import ccall "mdict_init"
  c_mdict_init :: CString -> IO (Ptr ())

foreign import ccall "mdict_destory"
  c_mdict_destroy :: Ptr () -> IO CInt  -- Original C returns int

foreign import ccall "mdict_lookup"
  c_mdict_lookup :: Ptr () -> CString -> Ptr (Ptr CChar) -> IO ()

foreign import ccall "mdict_keylist"
  c_mdict_keylist :: Ptr () -> Ptr CULong -> IO (Ptr (Ptr CChar))  -- Changed to Ptr CChar

foreign import ccall "free_simple_key_list"
  c_free_simple_key_list :: Ptr (Ptr CChar) -> CULong -> IO ()

foreign import ccall "mdict_suggest"
  c_mdict_suggest :: Ptr () -> CString -> Ptr (Ptr (Ptr CChar)) -> CInt -> IO ()

foreign import ccall "mdict_filetype"
  c_mdict_filetype :: Ptr () -> IO CInt

-- Safe initialization/destruction
withMDict :: FilePath -> (MDict -> IO a) -> IO a
withMDict path action = 
  withCString path $ \cpath -> 
    bracket
      (do rawPtr <- c_mdict_init cpath
          when (rawPtr == nullPtr) $ 
            throwIO (userError "Failed to initialize MDict")
          finalizer <- mkDestroyFinalizer $ \ptr -> do
            _ <- c_mdict_destroy ptr  -- Discard return code
            return ()
          newForeignPtr finalizer rawPtr)
      (\fptr -> return ())
      (action . MDict)

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
          else Just <$> peekCString result

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
          keys <- mapM peekCString keyPtrs
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
            mapM peekCString suggestions
