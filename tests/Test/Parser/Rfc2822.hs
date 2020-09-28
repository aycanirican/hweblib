{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.Rfc2822 where

import Network.Parser.Rfc2822
  ( NameAddress (NameAddress, naAddr, naName),
    angleAddr,
    nameAddr,
  )
import Test.HUnit (Test (TestCase, TestList))
import Test.Parser.Parser (aP, ae)

tests = TestList $ fmap TestCase lst

lst = [ test_angle_addr
      , test_name_addr
      ]

test_angle_addr
  = ae "angle_addr"
    (Just "user1@ex.org")
    (aP angleAddr "<user1@ex.org>;")

test_name_addr
  = ae "name_addr"
    (Just NameAddress {naName = Just "John Doe", naAddr = "john.doe@example.org"})
    (aP nameAddr "John Doe <john.doe@example.org>;")
