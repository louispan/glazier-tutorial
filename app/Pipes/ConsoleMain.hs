{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.Decimal as D
import qualified Glazier.Tutorial.App as GTA
import qualified Glazier.Tutorial.Console as GTC
import qualified Glazier.Tutorial.StreamModel as GTS

main :: IO ()
main = void $ GTC.exampleApp
    200000
    start1
    tick1
    desired1
    threshold1
    interval1
    start2
    tick2
    desired2
    threshold2
    interval2
    (GTA.AppModel 5 "Hello, world!" (GTS.StreamModel Nothing Nothing [] Nothing))
 where
  interval1 = (500000, 1000000)
  start1 = D.Decimal 0 130
  desired1 = D.Decimal 0 130
  tick1 = D.Decimal 0 20
  threshold1 = D.Decimal 0 300
  interval2 = (1000000, 2000000)
  start2 = D.Decimal 0 26
  desired2 = D.Decimal 0 26
  tick2 = D.Decimal 1 5
  threshold2 = D.Decimal 0 2
