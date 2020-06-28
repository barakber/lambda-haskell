{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Aws.Lambda
import Aws.Lambda.Runtime
import GHC.Generics
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Either
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Types.Header
--
import Data.Default
import Control.Lens hiding (Context)
import Control.Monad.IO.Class
import Control.Monad.Free
import Data.Attoparsec
--


{-# LANGUAGE TemplateHaskell #-}
import Development.GitRev

panic :: String -> a
panic msg = error panicMsg
  where panicMsg =
          concat [ "[panic ", $(gitBranch), "@", $(gitHash)
                 , " (", $(gitCommitDate), ")"
                 , " (", $(gitCommitCount), " commits in HEAD)"
                 , dirty, "] ", msg ]
        dirty | $(gitDirty) = " (uncommitted files present)"
              | otherwise   = ""

main = panic "oh no!"

{-
main :: IO ()
main =  runLambda run
  where
   run ::  LambdaOptions -> IO (Either a LambdaResult)
   run opts = do
    result <- either (error . show) id $ handler <$> (decodeObj (eventObject opts)) <*> (decodeObj (contextObject opts))
    either error (pure . Right . StandaloneLambdaResult . encodeObj) result

type Request = Value

data Response = Response
  { statusCode :: Int
  , headers :: [(Text, Text)]
  , body :: Text
  , isBase64Encoded :: Bool
  } deriving (Generic, ToJSON)

handler :: Request -> Context -> IO (Either String Response)
handler e context = return
    $ Right
    $ Response 200 mempty (toStrict $ decodeUtf8 $ encodePretty e) False
    
-}
