{-# LANGUAGE LambdaCase #-}
module Main where
import Language.Haskell.Exts
import System.Environment
import System.IO

deJSFFI :: Decl -> [Decl]
deJSFFI (ForImp l JavaScript _ _ fname ty) =
  [TypeSig l [fname] ty
  ,PatBind l (PVar fname)
   (UnGuardedRhs $ (App (Var (UnQual (Ident "error"))) $ Lit $ String $ "JS FFI call from plain ghc: " ++ prettyPrint fname)) Nothing]
deJSFFI d = [d]

main :: IO ()
main = do
  _ : inp : out : _ <- getArgs
  hPrint stderr =<< getEnvironment
  parseFileWithExts [EnableExtension  ForeignFunctionInterface] inp >>= \case
    ParseOk (Module l mname prag mwarn mexp imps decls) -> do
      writeFile out $ prettyPrint $
        Module l mname prag mwarn mexp imps (deJSFFI =<< decls)
    _ -> writeFile out =<< readFile inp

