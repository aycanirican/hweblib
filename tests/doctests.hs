module Main where
--------------------------------------------------------------------------------
import           System.FilePath.Glob (glob)
import           Test.DocTest
--------------------------------------------------------------------------------
main :: IO ()
main = glob "src/**/*.hs" >>= doctest . ([ "-isrc"
                                         , "-XOverloadedStrings"
                                         , "-XTupleSections"
                                         ] ++)
