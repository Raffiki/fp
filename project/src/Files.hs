module Files where

import Control.Exception (IOException, handle, throw)
import Errors
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)

readFile' :: FilePath -> AppError -> IO (Either AppError String)
readFile' path err = handle (missingFile err) $ Right <$> readFile path

missingFile :: AppError -> IOException -> IO (Either AppError String)
missingFile err e
  | isDoesNotExistError e = return $ Left err
  | otherwise = throw e