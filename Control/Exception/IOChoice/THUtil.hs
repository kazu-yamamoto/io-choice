{-# LANGUAGE TemplateHaskell, CPP #-}
module Control.Exception.IOChoice.THUtil (newChoice) where
import Language.Haskell.TH
import Control.Exception (IOException)

newChoice :: ExpQ -> ExpQ -> [Name] -> ExpQ
newChoice catches handler typs = do
  ma <- newName "ma"
  mb <- newName "mb"
  let hs = map (mkHandler handler mb) (''IOException : typs)
  lamE [varP ma, varP mb] $ [| $catches $(varE ma) $(listE hs) |]

mkHandler :: ExpQ -> Name -> Name -> ExpQ
mkHandler handler act eName = do
  let exc = checkSupported eName
  [| $handler $ \_e  -> let _ = _e :: $exc in $(varE act) |]

checkSupported :: Name -> TypeQ
checkSupported exc = do
  info <- reify exc
  case info of
    TyConI dec       -> do
      case dec of
        DataD _ name [] _ _ -> conT name
        NewtypeD _ name [] _ _ -> conT name
        TySynD name [] _ -> conT name
        DataInstD _ name args _ _ -> foldl1 appT (conT name:map return args)
        NewtypeInstD _ name args _ _ -> foldl1 appT (conT name:map return args)
#if __GLASGOW_HASKELL__ >= 707
        TySynInstD name (TySynEqn _ t) -> foldl1 appT (conT name:[return t])
#else
        TySynInstD name args _ -> foldl1 appT (conT name:map return args)
#endif
        _ -> error $ "Exception type must not have any type argument: " ++ show exc
    PrimTyConI n _ _ -> conT n
    _ -> error $ "Type name required, but got: " ++ show exc
