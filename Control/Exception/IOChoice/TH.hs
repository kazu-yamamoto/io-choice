{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Control.Exception.IOChoice.TH (newIOChoice, newIOChoiceLifted) where
import qualified Control.Exception as Exc
import qualified Control.Exception.Lifted as LExc
import Language.Haskell.TH

newIOChoice :: [Name] -> ExpQ
newIOChoice =  newChoice [| Exc.catches |] [| Exc.Handler |]

newIOChoiceLifted :: [Name] -> ExpQ
newIOChoiceLifted = newChoice [| LExc.catches |] [| LExc.Handler |]

newChoice :: ExpQ -> ExpQ -> [Name] -> ExpQ
newChoice catches handler typs = do
  ma <- newName "ma"
  mb <- newName "mb"
  let hs = map (mkHandler handler mb) typs
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
        TySynInstD name args _ -> foldl1 appT (conT name:map return args)
        _ -> error $ "Exception type must not have any type argument: " ++ show exc
    PrimTyConI n _ _ -> conT n
    _ -> error $ "Type name required, but got: " ++ show exc
