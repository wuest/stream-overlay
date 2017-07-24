{-# LANGUAGE TemplateHaskell #-}

module Server.Const where

import Prelude()
import Data.ByteString.Lazy as LBS
import Data.FileEmbed  ( embedFile )

overlayApp :: LBS.ByteString
overlayApp = LBS.fromStrict $(embedFile "static/overlay.html")

adminApp :: LBS.ByteString
adminApp = LBS.fromStrict $(embedFile "static/overlay.html")

overlayJS :: LBS.ByteString
overlayJS = LBS.fromStrict $(embedFile "static/overlay.js")

adminJS :: LBS.ByteString
adminJS = LBS.fromStrict $(embedFile "static/overlay.js")

overlayStyle :: LBS.ByteString
overlayStyle = LBS.fromStrict $(embedFile "static/default.css")

overlayMask :: LBS.ByteString
overlayMask = LBS.fromStrict $(embedFile "static/default_mask.svg")
