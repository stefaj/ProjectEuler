
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax

main = $(return $(return $ InfixE
    (Just $
        (ConE $ mkName "VarE") `AppE`
        ((VarE $ mkName "mkName") `AppE` (LitE $ StringL "putStrLn")))
    (ConE $ mkName "AppE")
    (Just $
        (ConE $ mkName "LitE") `AppE`
        ((ConE $ mkName "StringL") `AppE` (LitE $ StringL "Yo dawg")))))