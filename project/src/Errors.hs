module Errors (AppError (..)) where

data AppError = AppParseError String | MissingFileError String deriving (Show)