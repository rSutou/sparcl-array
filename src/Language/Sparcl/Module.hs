{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TypeApplications #-}
module Language.Sparcl.Module where

import qualified Data.Map                        as M
import qualified Data.Set                        as S

import           Data.Function                   (on)
import           Data.Ratio                      ((%))

import           System.Directory                as Dir (createDirectoryIfMissing,
                                                         doesFileExist)
import qualified System.FilePath                 as FP (takeDirectory, (<.>),
                                                        (</>))

import           Control.Monad                   (forM, when, guard, (>=>), unless)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Control.Monad.State             as St

import           Language.Sparcl.Pretty          hiding ((<$>))

import           Language.Sparcl.Class
import           Language.Sparcl.Core.Syntax
import           Language.Sparcl.Desugar
import           Language.Sparcl.Exception
import           Language.Sparcl.Multiplicity
import           Language.Sparcl.Renaming
import           Language.Sparcl.Surface.Syntax  (Assoc (..), Prec (..))
import           Language.Sparcl.Typing.TCMonad
import           Language.Sparcl.Typing.Type
import           Language.Sparcl.Typing.Typing
import           Language.Sparcl.Value

import           Language.Sparcl.CodeGen.Haskell (targetFilePath, toDocTop)

import           Language.Sparcl.DebugPrint
import           Language.Sparcl.Surface.Parsing
-- import Control.Exception (Exception, throw)

import           Data.Array.MArray
import           Data.Vector as V (generateM, (!), freeze, thaw, length, imapM_, unsafeFreeze)
import           Data.Vector.Mutable as MV
import qualified Control.Monad.Reader as R
import Debug.Trace (traceIO, trace)
import GHC.RTS.Flags (ProfFlags(retainerSelector))
import qualified Language.Haskell.TH.PprLib as D

data KeyName
data KeyOp
data KeyType
data KeyCon
data KeySyn
data KeySearchPath
data KeyValue
data KeyLoadPath

type MonadModule v m =
  (MonadIO m,
   MonadCatch m,
   Has   KeyLoadPath   FilePath m,
   Local KeyTC         TypingContext m,
   Has   KeyDebugLevel Int   m,
   Local KeyName       NameTable m,
   Local KeyOp         OpTable   m,
   Local KeyType       TypeTable m,
   Local KeyCon        CTypeTable m,
   Local KeySyn        SynTable m,
   Has   KeySearchPath [FilePath] m,
   Local KeyValue      (M.Map Name v) m)

data ModuleInfo v = ModuleInfo {
  miModuleName :: !ModuleName,
  miNameTable  :: !NameTable,
  miOpTable    :: !OpTable,
  miTypeTable  :: !TypeTable,
  miConTable   :: !CTypeTable,
  miSynTable   :: !SynTable,
  miValueTable :: !(M.Map Name v),
  miHsFile     :: !FilePath
  }

-- for caching.
type ModuleTable v = M.Map ModuleName (ModuleInfo v)

type M v m a = MonadModule v m => St.StateT (ModuleTable v) m a




-- data ModuleInfo = ModuleInfo {
--   miModuleName :: ModuleName,
--   miTables     :: Tables
--   }

-- data InterpInfo = InterpInfo {
--   iiSearchPath  :: [FilePath],
--   iiTables      :: Tables,
--   iiTInfo       :: TInfo,
--   iiModuleTable :: IORef ModuleTable
--   }


-- -- type M = ReaderT InterpInfo (StateT ModuleTable IO)
-- type M = ReaderT InterpInfo IO

-- data Tables = Tables
--               { tDefinedNames :: [QName],
--                 tOpTable      :: OpTable,
--                 tTypeTable    :: TypeTable,
--                 tSynTable     :: SynTable,
--                 tValueTable   :: ValueTable
--               }

-- mergeTables :: Tables -> Tables -> Tables
-- mergeTables t1 t2 =
--   setDefinedNames  (tDefinedNames t1 ++ ) $
--   setOpTable       (M.union $ tOpTable t1) $
--   setTypeTable     (M.union $ tTypeTable t1) $
--   setSynTable      (M.union $ tSynTable t1) $
--   setValueTable    (M.union $ tValueTable t1) $
--   t2

-- setDefinedNames :: ([QName] -> [QName]) -> Tables -> Tables
-- setDefinedNames f t = t { tDefinedNames = f (tDefinedNames t) }

-- setOpTable :: (OpTable -> OpTable) -> Tables -> Tables
-- setOpTable f t = t { tOpTable = f (tOpTable t) }

-- setTypeTable :: (TypeTable -> TypeTable) -> Tables -> Tables
-- setTypeTable f t = t { tTypeTable = f (tTypeTable t) }

-- setSynTable :: (SynTable -> SynTable) -> Tables -> Tables
-- setSynTable f t = t { tSynTable = f (tSynTable t) }

-- setValueTable :: (ValueTable -> ValueTable) -> Tables -> Tables
-- setValueTable f t = t { tValueTable = f (tValueTable t) }

-- iiSetTable :: (Tables -> Tables) -> InterpInfo -> InterpInfo
-- iiSetTable f t = t { iiTables = f (iiTables t) }

-- miSetTables :: (Tables -> Tables) -> ModuleInfo -> ModuleInfo
-- miSetTables f t = t { miTables = f (miTables t) }

-- instance HasOpTable M where
--   getOpTable     = asks $ tOpTable . iiTables
--   localOpTable f = local $ iiSetTable $ setOpTable f

-- instance HasTypeTable M where
--   getTypeTable = asks $ tTypeTable . iiTables
--   localTypeTable f = local $ iiSetTable $ setTypeTable f

-- instance HasDefinedNames M where
--   getDefinedNames = asks $ tDefinedNames . iiTables
--   localDefinedNames f = local $ iiSetTable $ setDefinedNames f

-- instance HasSynTable M where
--   getSynTable = asks $ tSynTable . iiTables
--   localSynTable f = local $ iiSetTable $ setSynTable f

-- instance HasValueTable M where
--   getValueTable = asks $ tValueTable . iiTables
--   localValueTable f = local $ iiSetTable $ setValueTable f


-- instance HasTInfo M where
--   getTInfo = asks iiTInfo

-- class HasModuleTable m where
--   getModuleTable :: m ModuleTable
-- --  localModuleTable :: (ModuleTable -> ModuleTable) -> m r -> m r

-- instance HasModuleTable M where
--   getModuleTable = do
--     ref <- asks iiModuleTable
--     liftIO $ readIORef ref

--   -- localModuleTable f m = do
--   --   ref <- asks iiModuleTable
--   --   old <- liftIO $ readIORef ref
--   --   liftIO $ writeIORef ref (f old)
--   --   res <- m
--   --   liftIO $ writeIORef ref old
--   --   return res

-- instance HasSearchPath M where
--   getSearchPath = asks iiSearchPath
--   localSearchPath f =
--     local $ \ii -> ii { iiSearchPath = f (iiSearchPath ii) }

-- type HasTables m = (HasDefinedNames m,
--                     HasOpTable m,
--                     HasTypeTable m,
--                     HasSynTable m,
--                     HasValueTable m)

-- class HasModuleTable m => ModifyModuleTable m where
--   modifyModuleTable :: (ModuleTable -> ModuleTable) -> m ()

-- instance ModifyModuleTable M where
--   modifyModuleTable f = do
--     ref <- asks iiModuleTable
--     old <- liftIO $ readIORef ref
--     liftIO $ writeIORef ref (f old)





-- runMTest :: [FilePath] -> M a -> IO a
-- runMTest searchPath m = do
--   tinfo <- initTInfo
--   let ii = initInterpInfo tinfo searchPath
--   evalStateT (runReaderT (withImport baseModuleInfo m) ii) M.empty


runM :: MonadModule v m => M v m a -> m a
runM m = St.evalStateT m M.empty

-- runM :: [FilePath] -> TInfo -> M a -> IO a
-- runM searchPath tinfo m = do
--   ii <- initInterpInfo tinfo searchPath
--   -- (res, _) <- runStateT (runReaderT (withImport baseModuleInfo m) ii) M.empty
--   runReaderT (withImport baseModuleInfo m) ii
-- --  return res

-- initTables :: Tables
-- initTables = Tables [] M.empty M.empty M.empty M.empty

-- initInterpInfo :: TInfo -> [FilePath] -> IO InterpInfo
-- initInterpInfo tinfo searchPath = do
--   ref <- newIORef M.empty
--   return $ InterpInfo { iiSearchPath = searchPath,
--                         iiTables = initTables,
--                         iiTInfo      = tinfo,
--                         iiModuleTable = ref }

baseModuleInfo :: ModuleInfo Value
baseModuleInfo = ModuleInfo {
  miModuleName = baseModule,
  miNameTable  = M.fromListWith S.union $
                 [ (Bare n, S.fromList [(mn ,n)]) | Original mn n _ <- names ]
                 ++ [ (Qual mn n, S.fromList [(mn, n)]) | Original mn n _ <- names ],
  miOpTable    = opTable,
  miConTable   = conTable,
  miTypeTable  = typeTable,
  miSynTable   = synTable,
  miValueTable = valueTable,
  miHsFile     = "Language.Sparcl.Base"
  }
  where
    eqInt = base "eqInt"
    leInt = base "leInt"
    ltInt = base "ltInt"
    eqChar = base "eqChar"
    leChar = base "leChar"
    ltChar = base "ltChar"

    eqRational = base "eqRational"
    leRational = base "leRational"
    ltRational = base "ltRational"

    name_generate = base "generate"
    -- nonlinearReadIArray = base "nonlinearReadIArray"
    readIArray = base "readIArray"
    lengthIArray = base "lengthIArray"

    freezeMArray = base "freezeMArray"

    slice2 = base "slice2"
    sliceAt = base "sliceAt"
    lengthMArray = base "lengthMArray"

    runRevM = base "runRevM"
    srunM = base "runM"

    -- readMArray = base "readMArray"
    -- writeMArray = base "writeMArray"

    pinM = base "pinM"


    pureRevM = base "pureRevM"
    bindRevM = base "bindRevM"

    pureM = base "pureM"
    bindM = base "bindM"

    unliftM = base "unliftM"
    liftM = base "liftM"
    
    
    get = base "get"
    
    pinH = base "pinH"
    pureReader = base "pureReader"
    bindReader = base "bindReader"
    
    liftRS = base "liftR2S"

    -- bindRev2 = base "bindRev2"

    -- forceDeRev = base "forceDeRev"

    unInt  (VLit (LitInt n)) = n
    unInt  _                 = cannotHappen $ text "Not an integer"
    unChar (VLit (LitChar n)) = n
    unChar _                  = cannotHappen $ text "Not a character"
    unRat (VLit (LitRational n)) = n
    unRat _                      = cannotHappen $ text "Not a rational"

    unMArr (VMArr sa offset size) = (sa, offset, size)
    unMArr _                = cannotHappen $ text "Not a mutable array"
    -- unIArr (VIArr iov) = iov
    -- unIArr _           = cannotHappen $ text "Not a immutable array"
    unUnit (VCon c []) | c == nameTuple 0 = ()
    unUnit _                              = cannotHappen $ text "Not a unit"
    unBool (VCon c []) | c == conTrue  = True
                       | c == conFalse = False
    unBool _                           = cannotHappen $ text "Not a boolean"
    unPair (VCon c [e1, e2]) | c == nameTuple 2 = (e1, e2)
    unPair _                                    = cannotHappen $ text "Not a pair"
    unPair3 (VCon c [e1, e2, e3]) | c == nameTuple 3 = (e1, e2, e3)
    unPair3 _                                        = cannotHappen $ text "Not a tuple3"
    unRes (VRes f b) = (f, b)
    unRes _          = cannotHappen $ text "Not a reversivele value"

    applyVFun (VFun f) v = f v
    applyVFun _ _ = cannotHappen $ text "Not a function"

    conTable = M.fromList [
      conTrue  |-> ConTy [] [] [] [] boolTy,
      conFalse |-> ConTy [] [] [] [] boolTy,
      base "U" |->
        let a = BoundTv (Local $ User "a")
        in ConTy [a] [] [] [(TyVar a, omega)] (TyCon (base "Un") [TyVar a]),
      base "MkMany" |->
        let [a, p] = map (BoundTv . Local . User) ["a", "p"]
        in ConTy [p, a] [] [] [(TyVar a, TyVar p)] (TyCon (base "Many") [TyVar p, TyVar a])
      ]

    typeTable = M.fromList [
          base "+" |-> intTy -@ (intTy -@ intTy),
          base "-" |-> intTy -@ (intTy -@ intTy),
          base "*" |-> intTy -@ (intTy -@ intTy),
          base "%" |-> intTy -@ (intTy -@ rationalTy),

          -- operators on rationals
          base "+%" |-> rationalTy -@ (rationalTy -@ rationalTy),
          base "-%" |-> rationalTy -@ (rationalTy -@ rationalTy),
          base "*%" |-> rationalTy -@ (rationalTy -@ rationalTy),
          base "/%" |-> rationalTy -@ (rationalTy -@ rationalTy),

          -- In future, we should use type classes.
          eqInt  |-> intTy -@ intTy -@ boolTy,
          leInt  |-> intTy -@ intTy -@ boolTy,
          ltInt  |-> intTy -@ intTy -@ boolTy,

          eqChar |-> charTy -@ charTy -@ boolTy,
          leChar |-> charTy -@ charTy -@ boolTy,
          ltChar |-> charTy -@ charTy -@ boolTy,

          eqRational |-> rationalTy -@ rationalTy -@ boolTy,
          leRational |-> rationalTy -@ rationalTy -@ boolTy,
          ltRational |-> rationalTy -@ rationalTy -@ boolTy,

          nameTyInt  |-> typeKi,
          nameTyBool |-> typeKi,
          nameTyChar |-> typeKi,
          nameTyRational |-> typeKi,
          base "Un" |-> typeKi `arrKi` typeKi,


          nameTyHeapState |-> typeKi,
          nameTyRevMonad |-> typeKi `arrKi` typeKi,
          nameTyMonad |-> typeKi `arrKi` typeKi,
          
          nameTyRState |-> typeKi `arrKi` typeKi,
          nameTyState |-> typeKi `arrKi` typeKi,
          nameTyReader |-> typeKi `arrKi` typeKi,
          
          nameTyMArray |-> typeKi `arrKi` typeKi,
          nameTyIArray |-> typeKi `arrKi` typeKi,

          name_generate |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            let pname = BoundTv $ Local $ User "p" in
            let pvar = TyVar pname in
            let qname = BoundTv $ Local $ User "q" in
            let qvar = TyVar qname in
            let rname = BoundTv $ Local $ User "r" in
            let rvar = TyVar rname in
            TyForAll [aname, pname, qname, rname]
            $ TyQual []
              $ tyarr pvar avar (tyarr qvar avar boolTy) *->
                intTy *-> tyarr rvar intTy avar *->
                revTy unitTy -@ revTy (iarrayBodyTy avar),

          -- nonlinearReadIArray |->
          --   let aname = BoundTv $ Local $ User "a" in
          --   let avar = TyVar aname in
          --   TyForAll [aname]
          --   $ TyQual []
          --     $ intTy *-> iarrayBodyTy avar *-> avar,
          readIArray |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            let pname = BoundTv $ Local $ User "p" in
            let pvar = TyVar pname in
            let qname = BoundTv $ Local $ User "q" in
            let qvar = TyVar qname in
            TyForAll [aname, pname, qname]
            $ TyQual []
              $ tyarr pvar avar (tyarr qvar avar boolTy) *->
                intTy *-> revTy (iarrayBodyTy avar) *-@ revTy (tupleTy [avar, iarrayBodyTy avar]),


          lengthIArray |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            TyForAll [aname]
            $ TyQual []
              $ revTy (iarrayBodyTy avar) -@ revTy (tupleTy [intTy, iarrayBodyTy avar]),

          freezeMArray |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            TyForAll [aname]
            $ TyQual []
              $ revTy (marrayBodyTy avar) -@ revMonadTy (iarrayBodyTy avar),

          slice2 |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            TyForAll [aname]
            $ TyQual []
              $ intTy *-> revTy (marrayBodyTy avar) -@ revTy (tupleTy [marrayBodyTy avar, marrayBodyTy avar]),
          sliceAt |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            TyForAll [aname]
            $ TyQual []
              $ intTy *-> revTy (marrayBodyTy avar) -@ revMonadTy (tupleTy [marrayBodyTy avar, avar, marrayBodyTy avar]),
          lengthMArray |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            TyForAll [aname]
            $ TyQual []
              $ revTy (marrayBodyTy avar) -@ revTy (tupleTy [intTy, marrayBodyTy avar]),

          runRevM |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            TyForAll [aname]
            $ TyQual []
              $ revMonadTy avar -@ revTy avar,
          srunM |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            TyForAll [aname]
            $ TyQual []
              $ monadTy avar *-> avar,

          -- readMArray |->
          --   let aname = BoundTv $ Local $ User "a" in
          --   let avar = TyVar aname in
          --   TyForAll [aname]
          --   $ TyQual []
          --     $ intTy *-> marrayBodyTy avar *-> monadTy avar,
          -- writeMArray |->
          --   let aname = BoundTv $ Local $ User "a" in
          --   let avar = TyVar aname in
          --   TyForAll [aname]
          --   $ TyQual []
          --     $ intTy *-> avar *-> marrayBodyTy avar *-> monadTy unitTy,

          pinM |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            let bname = BoundTv $ Local $ User "b" in
            let bvar = TyVar bname in
            TyForAll [aname, bname]
            $ TyQual []
              $ revTy avar -@ (avar *-> revMonadTy bvar) -@ revMonadTy (tupleTy [avar, bvar]),

          pureRevM |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            TyForAll [aname]
            $ TyQual []
              $ revTy avar -@ revMonadTy avar,
          bindRevM |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            let bname = BoundTv $ Local $ User "b" in
            let bvar = TyVar bname in
            TyForAll [aname, bname]
            $ TyQual []
              $ revMonadTy avar -@ (revTy avar -@ revMonadTy bvar) -@ revMonadTy bvar,

          pureM |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            TyForAll [aname]
            $ TyQual []
              $ avar *-> monadTy avar,
          bindM |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            let bname = BoundTv $ Local $ User "b" in
            let bvar = TyVar bname in
            TyForAll [aname, bname]
            $ TyQual []
              $ monadTy avar *-> (avar *-> monadTy bvar) *-> monadTy bvar,

          unliftM |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            let bname = BoundTv $ Local $ User "b" in
            let bvar = TyVar bname in
            TyForAll [aname, bname]
            $ TyQual []
              $ (revTy avar *-@ revMonadTy bvar) *-> tupleTy [avar *-> monadTy bvar, bvar *-> monadTy avar],
          liftM |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            let bname = BoundTv $ Local $ User "b" in
            let bvar = TyVar bname in
            TyForAll [aname, bname]
            $ TyQual []
              $ (avar *-> monadTy bvar) *-> (bvar *-> monadTy avar) *-> (revTy avar *-@ revMonadTy bvar),
              
          get |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            TyForAll [aname]
            $ TyQual []
              $ intTy *-> marrayBodyTy avar *-> readerTy avar,
              
          pinH |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            let bname = BoundTv $ Local $ User "b" in
            let bvar = TyVar bname in
            TyForAll [aname, bname]
            $ TyQual []
              $ revTy avar -@ (avar *-> readerTy (revTy bvar)) -@ rstateTy (tupleTy [avar, bvar]),
              
          pureReader |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            TyForAll [aname]
            $ TyQual []
              $ avar *-> readerTy avar,
          bindReader |->
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            let bname = BoundTv $ Local $ User "b" in
            let bvar = TyVar bname in
            TyForAll [aname, bname]
            $ TyQual []
              $ readerTy avar *-> (avar *-> readerTy bvar) *-> readerTy bvar,
              
          liftRS |-> 
            let aname = BoundTv $ Local $ User "a" in
            let avar = TyVar aname in
            TyForAll [aname]
            $ TyQual []
              $ readerTy avar *-> stateTy avar

          -- bindRev2 |->
          --   let aname = BoundTv $ Local $ User "a" in
          --   let avar = TyVar aname in
          --   let bname = BoundTv $ Local $ User "b" in
          --   let bvar = TyVar bname in
          --   let cname = BoundTv $ Local $ User "c" in
          --   let cvar = TyVar cname in
          --   TyForAll [aname, bname, cname]
          --   $ TyQual []
          --     $ revTy (tupleTy [avar, bvar]) *-@ (revTy avar *-@ revTy bvar *-@ revMonadTy cvar) *-@ revMonadTy cvar,



          -- back door
          -- forceDeRev |->
          --   let aname = BoundTv $ Local $ User "a" in
          --   let avar = TyVar aname in
          --   TyForAll [aname] (TyQual [] $ revTy avar -@ avar)



          ]

    -- synTable = M.empty
    synTable = M.fromList [
      -- nameTyRState |-> 
      --   let aname = BoundTv $ Local $ User "a" in
      --   let avar = TyVar aname in 
      --     ([aname], revMonadTy avar),
          
      -- nameTyState |-> 
      --   let aname = BoundTv $ Local $ User "a" in
      --   let avar = TyVar aname in 
      --     ([aname], monadTy avar),
          
      nameTyRevMonad |-> 
        let aname = BoundTv $ Local $ User "a" in
        let avar = TyVar aname in 
          ([aname], rstateTy avar),
          
      nameTyMonad |-> 
        let aname = BoundTv $ Local $ User "a" in
        let avar = TyVar aname in 
          ([aname], stateTy avar)
      
      ]

    opTable = M.fromList [
      base "+" |-> (Prec 60, L),
      base "-" |-> (Prec 60, L),
      base "*" |-> (Prec 70, L),
      base "%" |-> (Prec 70, L),

      base "+%" |-> (Prec 60, L),
      base "-%" |-> (Prec 60, L),
      base "*%" |-> (Prec 70, L),
      base "/%" |-> (Prec 70, L)
      ]


    valueTable = M.fromList [
          base "+" |-> intOp (+),
          base "-" |-> intOp (-),
          base "*" |-> intOp (*),
          base "%" |-> VFun (\(VLit (LitInt n)) -> return $ VFun $ \(VLit (LitInt m)) -> return $ VLit (LitRational (fromIntegral n % fromIntegral m))),

          base "+%" |-> ratOp (+),
          base "-%" |-> ratOp (-),
          base "*%" |-> ratOp (*),
          base "/%" |-> ratOp (/),

          eqInt  |-> VFun (\n -> return $ VFun $ \m -> return $ fromBool $ ((==) `on` unInt ) n m),
          leInt  |-> VFun (\n -> return $ VFun $ \m -> return $ fromBool $ ((<=) `on` unInt ) n m),
          ltInt  |-> VFun (\n -> return $ VFun $ \m -> return $ fromBool $ ((<)  `on` unInt ) n m),
          eqChar |-> VFun (\c -> return $ VFun $ \d -> return $ fromBool $ ((==) `on` unChar) c d),
          leChar |-> VFun (\c -> return $ VFun $ \d -> return $ fromBool $ ((<=) `on` unChar) c d),
          ltChar |-> VFun (\c -> return $ VFun $ \d -> return $ fromBool $ ((<)  `on` unChar) c d),

          eqRational |-> VFun (\n -> return $ VFun $ \m -> return $ fromBool $ ((==) `on` unRat) n m),
          leRational |-> VFun (\n -> return $ VFun $ \m -> return $ fromBool $ ((<=) `on` unRat) n m),
          ltRational |-> VFun (\n -> return $ VFun $ \m -> return $ fromBool $ ((<)  `on` unRat) n m),



          name_generate |->
            VFun (\eq -> return $ VFun $ \n -> return $ VFun $ \vg -> return $ VFun $ \vu -> do
                let VFun eq2 = eq
                let n' = unInt n
                let VFun g = vg
                let (fu, bu) = unRes vu
                let f' hp = do
                      () <- unUnit <$> fu hp
                      imv <- V.generateM n' (g . VLit . LitInt)
                      return $ VIArr imv
                let b' v = do
                      let VIArr imv = v
                      V.imapM_ (\i a1 -> do
                                          VFun eq1 <- eq2 a1
                                          r <- g (VLit $ LitInt i) >>= eq1
                                          unless (unBool r) $ rtError $ text "element isn't match in generate(bwd)"
                                          return ()
                                          ) imv
                      bu unitVal
                return $ VRes f' b'),

          -- nonlinearReadIArray |->
          --   VFun (\n -> return $ VFun $ \va -> do
          --             let n' = unInt n
          --             let VIArr imv = va
          --             return $ imv V.! n'),
          readIArray |->
            VFun (\veq -> return $ VFun $ \n -> return $ VFun $ \va -> do
                      let VFun eq2 = veq
                      let n' = unInt n
                      let (farr, barr) = unRes va
                      let f' hp = do
                            varr <- farr hp
                            let VIArr imv = varr
                            let size = V.length imv
                            unless (0 <= n' && n' < size) $ rtError $ text "index out of range: in readIArray(fwd)"
                            return $ VCon (nameTuple 2) [imv V.! n', varr]
                      let b' v = do
                            let (ve, varr) = unPair v
                            let VIArr imv = varr
                            let size = V.length imv
                            unless (0 <= n' && n' < size) $ rtError $ text "index out of range: in readIArray(fwd)"
                            VFun eq1 <- eq2 ve
                            b <- unBool <$> (eq1 $ imv V.! n')
                            unless b $ rtError $ text "equality error in readIArray(bwd)"
                            barr varr

                      return $ VRes f' b'),
          lengthIArray |->
            VFun (\vrarr -> do
              let (fa, ba) = unRes vrarr
              let f' hp = do
                    varr <- fa hp
                    let VIArr imv = varr
                    let size = V.length imv
                    return $ VCon (nameTuple 2) [VLit $ LitInt size, varr]
              let b' v = do
                    let (v1, v2) = unPair v
                    let VIArr imv = v2
                    let size' = unInt v1
                    guard $ size' == V.length imv
                    ba v2
              return $ VRes f' b'),

          freezeMArray |->
            VFun (\vrma -> return $ VFun $ \vrhs -> do
              let (fma, bma) = unRes vrma
              let (fhs, bhs) = unRes vrhs
              let f' hp = do
                    varr <- fma hp
                    let (hsa, _, size) = unMArr varr
                    VHSt hs <- fhs hp
                    let SArr pa = lookUpHeapState hsa hs
                    unless (MV.length pa == size) $ rtError $ text "length error in freezeMArray"
                    let newhs = removeHeapState hsa hs
                    imv <- liftIO $ V.freeze pa
                    -- imv <- liftIO $ V.unsafeFreeze pa
                    return $ VCon (nameTuple 2) [VIArr imv, VHSt newhs]
              let b' v = do
                    let (VIArr imv, VHSt s) = unPair v
                    let size = V.length imv
                    marr <- liftIO $ V.thaw imv
                    let (newhs, newa) = extendHeapState (SArr marr) s
                    hp1 <- bma (VMArr newa 0 size)
                    hp2 <- bhs (VHSt newhs)
                    return $ unionHeap hp1 hp2
              return $ VRes f' b'),

          slice2 |->
            VFun (\n -> return $ VFun $ \va -> do
              let n' = unInt n
              guard $ 0 <= n'
              let (fa, ba) = unRes va
              let f' hp = do
                    (hsa, off, size) <- unMArr <$> fa hp
                    guard (n' <= size)
                    let sl1 = VMArr hsa off n'
                    let sl2 = VMArr hsa (off + n') (size - n')
                    return $ VCon (nameTuple 2) [sl1, sl2]
              let b' v = do
                    let (sl1,sl2) = unPair v
                    let (hsa1, off1, size1) = unMArr sl1
                    let (hsa2, off2, size2) = unMArr sl2
                    guard (hsa1 == hsa2 && n' == size1 && 0 <= size2 && off1 + n' == off2)
                    ba $ VMArr hsa1 off1 (size1 + size2)
              return $ VRes f' b'),
          sliceAt |->
            VFun (\n -> return $ VFun $ \va -> return $ VFun $ \vrhs -> do
              let n' = unInt n
              guard $ 0 <= n'
              let (fa, ba) = unRes va
              let (fhs, bhs) = unRes vrhs
              let f' hp = do
                    (hsa, off, size) <- unMArr <$> fa hp
                    unless (n' < size)
                      $ rtError $ text "index is out of range in sliceAt(fwd)"
                    VHSt hs <- fhs hp
                    let sl1 = VMArr hsa off n'
                    let (SArr iov) = lookUpHeapState hsa hs
                    el2 <- liftIO $ MV.read iov (off + n')
                    let sl3 = VMArr hsa (off + n' + 1) (size - n' - 1)
                    return $ VCon (nameTuple 2) [VCon (nameTuple 3) [sl1, el2, sl3], VHSt hs]
              let b' v = do
                    -- rtError $ ppr v 
                    let (vslices, VHSt hs) = unPair v
                    let (sl1, el2, sl3) = unPair3 vslices
                    let (hsa1, off1, size1) = unMArr sl1
                    let (hsa3, off3, size3) = unMArr sl3
                    unless (hsa1 == hsa3 && n' == size1 && 0 <= size3 && off1 + size1 + 1 == off3)
                           $ rtError $ text "index error in sliceAt(bwd)"
                    let SArr iov = lookUpHeapState hsa1 hs
                    liftIO $ MV.write iov (off1 + n') el2
                    hp1 <- ba $ VMArr hsa1 off1 (size1 + size3 + 1)
                    hp2 <- bhs $ VHSt hs
                    return $ unionHeap hp1 hp2
              return $ VRes f' b'),
          lengthMArray |->
            VFun (\vrarr -> do
              let (fa, ba) = unRes vrarr
              let f' hp = do
                    varr <- fa hp
                    let (_, _, size) = unMArr varr
                    return $ VCon (nameTuple 2) [VLit $ LitInt size, varr]
              let b' v = do
                    let (v1,v2) = unPair v
                    let (_, _, size) = unMArr v2
                    let size' = unInt v1
                    guard $ size == size'
                    ba v2
              return $ VRes f' b'),

          runRevM |->
            VFun (\vf -> do
                let VFun f = vf
                (f0, b0) <- unRes <$> f (VRes (\_ -> return $ VHSt emptyHeapState) (\(VHSt s) -> do guard $ isEmptyHeapState s; return emptyHeap))
                let f' = f0 >=> (\x -> do { let {(r,VHSt h) = unPair x}; guard (isEmptyHeapState h); pure r})
                let b' = (\r -> pure $ VCon (nameTuple 2) [r, VHSt emptyHeapState]) >=> b0
                pure $ VRes f' b'
                ),
          srunM |->
            VFun (\vf -> do
                let VFun f = vf
                (va, VHSt newhs) <- unPair <$> f (VHSt emptyHeapState)
                guard $ isEmptyHeapState newhs
                return va),

          -- readMArray |-> 
          --   VFun (\n -> return $ VFun $ \vma -> return $ VFun $ \vhs -> do
          --     let n' = unInt n
          --     let (hsa, off, size) = unMArr vma
          --     guard $ 0 <= n' && n' < size
          --     let VHSt hs = vhs
          --     let SArr iov = lookUpHeapState hsa hs
          --     el <- liftIO $ MV.read iov (off + n')
          --     return $ VCon (nameTuple 2) [el, vhs]),
          -- writeMArray |-> 
          --   VFun (\n -> return $ VFun $ \va -> return $ VFun $ \vma -> return $ VFun $ \vhs -> do
          --     let n' = unInt n
          --     let (hsa, off, size) = unMArr vma
          --     guard $ 0 <= n' && n' < size
          --     let VHSt hs = vhs
          --     let SArr iov = lookUpHeapState hsa hs
          --     () <- liftIO $ MV.write iov (off + n') va
          --     return $ VCon (nameTuple 2) [unitVal, vhs]),

          pinM |-> 
            VFun (\vra -> return $ VFun $ \vf -> return $ VFun $ \vrhs -> do
              let (fa, ba) = unRes vra
              let VFun f = vf
              let f' hp = do
                    va <- fa hp
                    VFun frevmb <- f va
                    (fbh, _) <- unRes <$> frevmb vrhs
                    v <- fbh hp
                    let (vb, vhs') = unPair $ v
                    -- let (VMArr h o s) = vhs'
                    -- rtError $ ppr o
                    return $ VCon (nameTuple 2) [VCon (nameTuple 2) [va, vb], vhs']
              let b' v = do
                    let (vab, VHSt hs) = unPair v
                    let (va, vb) = unPair vab
                    VFun frevmb <- f va
                    VRes _ bbh <- frevmb vrhs
                    hp0 <- bbh $ VCon (nameTuple 2) [vb, VHSt hs]
                    hp2 <- ba va
                    return $ unionHeap hp0 hp2
              return $ VRes f' b'),

          pureRevM |->
            VFun (\vra -> return $ VFun $ \vrhs -> do
              let (fa, ba) = unRes vra
              let (fhs, bhs) = unRes vrhs
              let f' hp = do
                    va <- fa hp
                    vhs <- fhs hp
                    return $ VCon (nameTuple 2) [va, vhs]
              let b' v = do
                    let (va, VHSt hs) = unPair v
                    hp1 <- ba va
                    hp2 <- bhs $ VHSt hs
                    return $ unionHeap hp1 hp2
              return $ VRes f' b'),
          bindRevM |-> VFun (\vrevma -> return $ VFun $ \vfab -> return $ VFun $ \vrhs -> do
              let VFun frevma = vrevma
              let VFun fab = vfab
              (fahs, bahs) <- unRes <$> frevma vrhs
              newAddrs 2 $ \as -> do
                let [a1, a2] = as
                VFun fb <- fab (VRes (lookupHeap a1) (return . singletonHeap a1))
                VRes f0 b0 <- fb (VRes (lookupHeap a2) (return . singletonHeap a2))
                let f' hp = do
                      (va, vhs) <- unPair <$> fahs hp
                      f0 (extendHeap a2 vhs (extendHeap a1 va hp))
                let b' v = do
                      hp' <- b0 v
                      va <- lookupHeap a1 hp'
                      VHSt hs <- lookupHeap a2 hp'
                      let hp1 = removesHeap as hp'
                      hp2 <- bahs $ VCon (nameTuple 2) [va, VHSt hs]
                      return $ unionHeap hp1 hp2
                return $ VRes f' b'),
          
          pureM |->
            VFun (\va -> return $ VFun $ \(VHSt hs) -> return $ VCon (nameTuple 2) [va, VHSt hs]),

          bindM |-> 
            VFun (\vma -> return $ VFun $ \vf -> return $ VFun $ \vhs -> do
              let VFun ma = vma
              let VFun f = vf
              (va, vhs') <- unPair <$> ma vhs
              VFun mb <- f va
              mb vhs'),

          unliftM |->
            VFun (\vrmf -> do
              let VFun rmf = vrmf
              newAddrs 2 $ \as -> do
                let [a1, a2] = as
                VFun rmb <- rmf (VRes (lookupHeap a1) (return . singletonHeap a1))
                VRes f0 b0 <- rmb (VRes (lookupHeap a2) (return . singletonHeap a2))
                let fw' = VFun $ \va -> return $ VFun $ \vhs -> f0 (extendHeap a2 vhs (singletonHeap a1 va))
                let bw' = VFun $ \vb -> return $ VFun $ \vhs -> do
                      hp <- b0 $ VCon (nameTuple 2) [vb, vhs]
                      va <- lookupHeap a1 hp
                      VHSt hs' <- lookupHeap a2 hp
                      return $ VCon (nameTuple 2) [va, VHSt hs']
                return $ VCon (nameTuple 2 ) [fw', bw']),

          liftM |->
            VFun (\vamb -> return $ VFun $ \vbma -> do
              let VFun amb = vamb
              let VFun bma = vbma
              return $ VFun $ \vra -> return $ VFun $ \vrhs -> do
                let (fa, ba) = unRes vra
                let (fhs, bhs) = unRes vrhs
                let f' hp = do 
                      VFun mb <- fa hp >>= amb
                      fhs hp >>= mb
                let b' v = do
                      let (vb, vhs) = unPair v
                      VFun ma <- bma vb
                      (va, vhs') <- unPair <$> ma vhs
                      hp1 <- ba va
                      hp2 <- bhs vhs'
                      return $ unionHeap hp1 hp2
                return $ VRes f' b'),
          
          
          get |->
            VFun (\n -> return $ VFun $ \va -> return $ VFun $ \vh -> do
                      let n' = unInt n
                      let (hsa, off, size) = unMArr va
                      let VHSt hs = vh
                      unless (n' < size)
                        $ rtError $ text "index is out of range in get"
                      let (SArr iov) = lookUpHeapState hsa hs
                      el <- liftIO $ MV.read iov (off + n')
                      return $ el),
          
          pinH |-> 
            VFun (\vra -> return $ VFun $ \vf -> return $ VFun $ \vrh -> do
              let (fa, ba) = unRes vra
              let VFun f = vf
              let (fh, bh) = unRes vrh
              let f' hp = do
                    va <- fa hp
                    vh <- fh hp
                    VFun freaderb <- f va
                    (fb, _) <- unRes <$> freaderb vh
                    vb <- fb hp
                    -- let (VMArr h o s) = vhs'
                    -- rtError $ ppr o
                    return $ VCon (nameTuple 2) [VCon (nameTuple 2) [va, vb], vh]
              let b' v = do
                    let (vab, vh) = unPair v
                    let (va, vb) = unPair vab
                    VFun freaderb <- f va
                    VRes _ bb <- freaderb vh
                    hp0 <- bb vb
                    hp1 <- ba va
                    hp2 <- bh vh
                    return $ unionHeap hp0 $ unionHeap hp1 hp2
              return $ VRes f' b'),
              
          
          pureReader |->
            VFun (\va -> return $ VFun $ \_ -> return $ va),

          bindReader |-> 
            VFun (\vreadera -> return $ VFun $ \vf -> return $ VFun $ \vh -> do
              let VFun ma = vreadera
              let VFun f = vf
              va <- ma vh
              VFun mb <- f va
              mb vh),
              
          liftRS |->
            VFun (\vreadera -> return $ VFun $ \vh -> do
                let VFun fha = vreadera
                va <- fha vh 
                return $ VCon (nameTuple 2) [va, vh])
            

          -- bindRev2 |->
          --   VFun (\vrab -> return $ VFun $ \vf2 -> return $ VFun $ \vrhs -> do
          --     let (fab, bab) = unRes vrab
          --     let VFun f2 = vf2
          --     newAddrs 2 $ \as -> do
          --       let [a1, a2] = as
          --       VFun f1 <- f2 (VRes (lookupHeap a1) (return . singletonHeap a1))
          --       VFun frmc <- f1 (VRes (lookupHeap a2) (return . singletonHeap a2))
          --       VRes f0 b0 <- f1 vrhs
          --       let f' hp = do
          --             (va, vb) <- unPair <$> fab hp
          --             f0 (extendHeap a2 vb (extendHeap a1 va hp))
          --       let b' v = do
          --             hp' <- b0 v
          --             va <- lookupHeap a1 hp'
          --             vb <- lookupHeap a2 hp'
          --             let hp1 = removesHeap as hp'
          --             hp2 <- bab $ VCon (nameTuple 2) [va, vb]
          --             return $ unionHeap hp1 hp2
          --       return $ VRes f' b')

          -- forceDeRev |->
          --   VFun (\vr ->
          --     let (f, b) = unRes vr in
          --     f emptyHeap)
          ]

    names = M.keys typeTable ++ M.keys conTable

    fromBool True  = VCon conTrue  []
    fromBool False = VCon conFalse []

    intOp f = VFun $ \(VLit (LitInt n)) -> return $ VFun $ \(VLit (LitInt m)) -> return (VLit (LitInt (f n m)))
    ratOp f = VFun $ \(VLit (LitRational n)) -> return $ VFun $ \(VLit (LitRational m)) -> return (VLit (LitRational (f n m)))

    rationalTy = TyCon (base "Rational") []
    intTy = TyCon (base "Int") []

    nameTyRevMonad = base "RevM"
    nameTyMonad = base "M"
    nameTyRState = base "RState"
    nameTyState = base "State"
    nameTyReader = base "Reader"
    
    nameTyHeapState = base "H"
    nameTyMArray = base "MArray"
    nameTyIArray = base "IArray"
    -- revMonadTy avar = (revTy $ TyCon nameTyHeapState []) -@ (revTy $ tupleTy [avar, TyCon nameTyHeapState []])
    
    revMonadTy avar = TyCon nameTyRevMonad [avar]
    monadTy avar = TyCon nameTyMonad [avar]
    
    
    rstateTy avar = TyCon nameTyRState [avar]
    stateTy avar = TyCon nameTyState [avar]
    readerTy avar = TyCon nameTyReader [avar]
    
    marrayBodyTy avar = TyCon nameTyMArray [avar]
    iarrayBodyTy avar = TyCon nameTyIArray [avar]
    unitTy = tupleTy []
    unitVal = VCon (nameTuple 0) []


    base n = nameInBase (User n)
    a |-> b = (a, b)
    infix 0 |->








-- withOpTable :: HasOpTable m => OpTable -> m r -> m r
-- withOpTable newOpTable m = do
--   -- opTable <- asks iiOpTable
--   -- local (\ii -> ii { iiOpTable = M.union newOpTable opTable }) m
--   localOpTable (M.union newOpTable) m

-- withTypeTable :: HasTypeTable m => TypeTable -> m r -> m r
-- withTypeTable newTbl m = do
--   localTypeTable (M.union newTbl) m
--   -- tbl <- asks iiTypeTable
--   -- local (\ii -> ii { iiTypeTable = M.union newTbl tbl }) m

-- withSynTable :: HasSynTable m => SynTable -> m r -> m r
-- withSynTable newTbl m = do
--   localSynTable (M.union newTbl) m
--   -- tbl <- asks iiSynTable
--   -- local (\ii -> ii { iiSynTable = M.union newTbl tbl }) m

-- withValueTable :: HasValueTable m => ValueTable -> m r -> m r
-- withValueTable newTbl m = do
--   localValueTable (M.union newTbl) m
--   -- tbl <- asks iiValueTable
--   -- local (\ii -> ii { iiValueTable = M.union newTbl tbl }) m

-- withDefinedNames :: HasDefinedNames m => [Name] -> m r -> m r
-- withDefinedNames newTbl m = do
--   localDefinedNames (newTbl ++) m
--   -- tbl <- asks iiDefinedNames
--   -- local (\ii -> ii { iiDefinedNames = newTbl ++ tbl}) m


-- withImport :: ModuleInfo -> M r -> M r
-- withImport :: HasTables m => ModuleInfo -> m r -> m r
withImport :: MonadModule v m => ModuleInfo v -> m r -> m r
withImport mo m =
  local (key @KeyName) (M.unionWith S.union $ miNameTable mo) $
  local (key @KeyOp) (M.union $ miOpTable mo) $
  local (key @KeyType) (M.union $ miTypeTable mo) $
  local (key @KeyCon) (M.union $ miConTable mo) $
  local (key @KeySyn) (M.union $ miSynTable mo) $
  local (key @KeyValue) (M.union $ miValueTable mo) m
  -- let t = miTables mod
  -- withOpTable (tOpTable t) $
  --   withTypeTable (tTypeTable t) $
  --     withSynTable (tSynTable t) $
  --       withDefinedNames (tDefinedNames t) $
  --         withValueTable (tValueTable t) m


withImports :: MonadModule v m => [ModuleInfo v] -> m r -> m r
withImports ms comp =
  Prelude.foldr withImport comp ms

withNoTables :: MonadModule v m => m r -> m r
withNoTables m =
  local (key @KeyName) (const M.empty) $
  local (key @KeyOp) (const M.empty) $
  local (key @KeyType) (const M.empty) $
  local (key @KeyCon) (const M.empty) $
  local (key @KeySyn) (const M.empty) $
  local (key @KeyValue) (const M.empty) m

ext :: String
ext = "sparcl"

moduleNameToFilePath :: ModuleName -> FilePath
moduleNameToFilePath (ModuleName mo) = go mo
  where
    go = go2' id

    go2' ds [] = ds "" FP.<.> ext
    go2' ds (c:cs)
      | c == '.'  = ds "" FP.</> go2' id cs
      | otherwise = go2' (ds . (c:)) cs




--  (foldr1 (FP.</>) mn) FP.<.> ext

restrictNames :: [Name] -> ModuleInfo v -> ModuleInfo v
restrictNames ns mi =
  mi { miNameTable  = M.mapMaybe conv (miNameTable mi),
       miOpTable    = restrict (miOpTable mi),
       miTypeTable  = restrict (miTypeTable mi),
       miConTable   = restrict (miConTable mi),
       miSynTable   = restrict (miSynTable mi),
       miValueTable = restrict (miValueTable mi)
        }
  where
    ns' = S.fromList ns

    restrict :: M.Map Name a -> M.Map Name a
    restrict x = M.restrictKeys x ns'

    mnsI = S.fromList [ (mn, n) | Original mn n _ <- ns ]

    conv mns =
      let res = S.intersection mns mnsI
      in if S.null res then
           Nothing
         else
           Just res

searchModule :: (MonadIO m, MonadModule v m) => ModuleName -> m FilePath
searchModule mo = do
  dirs <- ask (key @KeySearchPath)
  let file = moduleNameToFilePath mo
  let searchFiles = [ dir FP.</> file | dir <- dirs ]
  fs <- liftIO $ mapM Dir.doesFileExist searchFiles
  case map fst $ filter snd $ zip searchFiles fs of
    fp:_ -> return fp
    []   -> do
      vlevel <- ask (key @KeyDebugLevel)
      staticError $ text "Cannot find module:" <+> ppr mo <> reportSearchFiles vlevel searchFiles
  where
    reportSearchFiles vlevel sf
      | vlevel < 2 = mempty
      | otherwise  =
        line <> text "Files searched:" <+> align (vcat (map ppr sf))


importNames :: MonadModule v m => ModuleName -> [Loc SurfaceName] -> ModuleInfo v -> m (ModuleInfo v)
importNames mn ns m = do
  onames <- forM ns $ \(Loc loc n) ->
    case n of
      Bare bn -> return (Original mn bn (Bare bn))
      _       -> staticError $ nest 2 $
                 vcat [ ppr loc ,
                        text "Qualified names in the import list:" <+> ppr n ]

  return $ restrictNames onames m

exportNames :: MonadModule v m => [Loc SurfaceName] -> ModuleInfo v -> m (ModuleInfo v)
exportNames ns m = do
  -- In general, ns can contain names that come from other modules.
  -- Then, exporting is done by filtering all the available names.

  nameTbl <- M.union (miNameTable m)  <$> ask (key @KeyName)
  opTbl   <- M.union (miOpTable m)    <$> ask (key @KeyOp)
  typeTbl <- M.union (miTypeTable m)  <$> ask (key @KeyType)
  conTbl  <- M.union (miConTable m)   <$> ask (key @KeyCon)
  synTbl  <- M.union (miSynTable m)   <$> ask (key @KeySyn)
  valTbl  <- M.union (miValueTable m) <$> ask (key @KeyValue)

  onames <- forM ns $ \(Loc loc n) ->
    case S.toList <$> M.lookup n nameTbl of
      Just [(mn, bn)] -> return (Original mn bn n)
      Just qs  -> staticError $ nest 2 $
        vcat [ ppr loc,
               text "Ambiguous name in the export list:" <+> ppr n,
               text "candidates are:",
               vcat (map ppr qs) ]
      Nothing  -> staticError $ nest 2 $
        vcat [ppr loc,
              text "Unbound name in the export list:" <+> ppr n]

  return $ restrictNames onames $ m { miNameTable = nameTbl,
                                      miOpTable = opTbl,
                                      miTypeTable = typeTbl,
                                      miConTable  = conTbl,
                                      miSynTable = synTbl,
                                      miValueTable = valTbl }



-- readModule :: FilePath -> M v m ModuleInfo
-- readModule fp = do
--   -- Clear cache.
--   modifyModuleTable (const $ M.empty)
--   -- reset emvironments.
--   localDefinedNames (const []) $
--     localOpTable (const $ M.empty) $
--       localTypeTable (const $ M.empty) $
--         localSynTable (const $ M.empty) $
--           withImport baseModuleInfo $
--             readModuleWork fp

-- readModule :: FilePath -> (M.Map Name v -> Bind Name -> [(Name, v)]) -> M v m (ModuleInfo v)
-- readModule :: FilePath -> (M.Map Name v -> Bind Name -> IO [(Name, v)]) -> M v m (ModuleInfo v)
readModule :: FilePath -> (M.Map Name v -> Bind Name -> M v m [(Name, v)]) -> M v m (ModuleInfo v)
readModule fp interp = do
  debugPrint 1 $ text "Parsing" <+> ppr fp <+> text "..."
  s <- liftIO $ readFile fp
  Module currentModule exports imports decls <- either (staticError . text) return $ parseModule fp s

  debugPrint 1 $ text "Parsing Ok."
  debugPrint 2 $ ppr decls

  ms <- forM imports $ \(Import m is) -> do
    md <- interpModuleWork m interp
    case is of
      Nothing -> return md
      Just ns ->
        importNames m ns md -- restrictNames (map (qualifyName m) ns) md) imports

  withImports ms $ do
    nameTable <- ask (key @KeyName)
    opTable   <- ask (key @KeyOp)

    debugPrint 1 $ text "Renaming ..."
    debugPrint 2 $ group $
      text "w.r.t." </>
      vcat [ nest 2 (text "opTable:" <> line <> align (pprMap opTable)),
             nest 2 (text "nameMap:" <> line <> align (pprMap (M.map S.toList nameTable))) ]

    -- (decls', newDefinedNames, newOpTable, newDataTable, newSynTable) <-
    --        liftIO $ runDesugar mod definedNames opTable (desugarTopDecls decls)

    (renamedDecls, tyDecls, synDecls, newNames, newOpTable) <-
      liftIO $ either nameError return $ runRenaming nameTable opTable $ renameTopDecls currentModule decls

    -- debugPrint $ "Desugaring Ok."
    -- debugPrint $ show (D.group $ D.nest 2 $ D.text "Desugared syntax:" D.</> D.align (ppr decls'))

    -- debugPrint $ "Type checking ..."
    -- debugPrint $ show (D.text "under ty env" D.<+> pprMap tyEnv)

    debugPrint 1 $ text "Renaming Ok."
    debugPrint 2 $ ppr renamedDecls

    tyEnv  <- ask (key @KeyType)
    conEnv <- ask (key @KeyCon)
    synEnv <- ask (key @KeySyn)

    debugPrint 1 $ text "Type checking ..."
    debugPrint 2 $ text "under ty env" </> pprMap tyEnv

    (typedDecls, nts, dataDecls', typeDecls', newCTypeTable, newSynTable) <-
      runTCWith conEnv tyEnv synEnv $ inferTopDecls renamedDecls tyDecls synDecls

    debugPrint 1 $ text "Type checking Ok."
    debugPrint 1 $ text "Desugaring ..."
    bind <- runTC $ runDesugar $ desugarTopDecls typedDecls

    debugPrint 1 $ text "Desugaring Ok."
    debugPrint 2 $ text "Desugared:" <> line <> align (vcat (map (\(x,_,e) -> ppr (x,e)) bind))


    loadPath <- ask (key @KeyLoadPath)
    let hsFile = loadPath FP.</> targetFilePath currentModule

    liftIO $ do let dir = FP.takeDirectory hsFile
                Dir.createDirectoryIfMissing True dir
                writeFile hsFile $
                  show $ toDocTop currentModule exports imports dataDecls' typeDecls' bind

    -- for de

    valEnv <- ask (key @KeyValue)
    -- let newValueEnv = interp valEnv bind
    newValueEnv <- interp valEnv bind
    -- let newValueEnvIO = interp valEnv bind

    let newNameTable =
          let mns = [ (mn, n) | Original mn n _ <- S.toList newNames ]
          in M.fromList $
             [ (Bare n, S.singleton (mn, n)) | (mn, n) <- mns ]
             ++
             [ (Qual mn n, S.singleton (mn, n)) | (mn, n) <- mns ]

    let newMod = ModuleInfo {
          miModuleName = currentModule,
          miOpTable   = newOpTable,
          miNameTable = newNameTable,
          miSynTable  = newSynTable,
          miTypeTable = M.fromList nts,
          miConTable  = newCTypeTable,
          miValueTable = M.fromList newValueEnv,
          miHsFile     = hsFile
          }

    newMod' <- case exports of
      Just es -> exportNames es newMod
      Nothing -> return newMod

    St.modify (M.insert currentModule newMod')
    return newMod'


    -- withOpTable newOpTable $
    --   withTypeTable newDataTable $
    --     withSynTable newSynTable $ do
    --       modTbl <- getModuleTable
    --       tyEnv  <- getTypeTable

    --       debugPrint "Type checking..."
    --       debugPrint $ show (D.text "under ty env" D.<+> pprMap tyEnv)

    --       tinfo <- getTInfo
    --       synEnv <- getSynTable
    --       liftIO $ setEnvs tinfo tyEnv synEnv

    --       nts <- liftIO $ runTC tinfo $ inferDecls decls'


    --       -- let env = foldr M.union M.empty $ map miValueTable ms
    --       env <- getValueTable
    --       let env' = runEval (evalUDecls env decls')

    --       let newMod = ModuleInfo {
    --             miModuleName = mod,
    --             miOpTable   = newOpTable,
    --             miNameTable = newNameTable,
    --             miSynTable  = newSynTable,
    --             miTypeTable = foldr (uncurry M.insert) newDataType nts,
    --             miValueTable = env'
    --             }

    --       modifyModuleTable (const $ M.insert mod newMod modTbl)

    --       case exports of
    --         Just es ->
    --           return $ restrictNames (map (qualifyName mod) es) newMod
    --         _ ->
    --           return newMod

   where
     nameError (l, d) =
       staticError (nest 2 (ppr l </> d))

--      qualifyName = undefined
     -- qualifyName cm (BName n) = QName cm n
     -- qualifyName _  (QName cm n) = QName cm n


-- interpModuleWork :: ModuleName -> (M.Map Name v -> Bind Name -> [(Name,v)]) -> M v m (ModuleInfo v)
-- interpModuleWork :: ModuleName -> (M.Map Name v -> Bind Name -> IO [(Name,v)]) -> M v m (ModuleInfo v)
interpModuleWork :: ModuleName -> (M.Map Name v -> Bind Name -> M v m [(Name,v)]) -> M v m (ModuleInfo v)
interpModuleWork mo interp = do
  modTable <- St.get
  case M.lookup mo modTable of
    Just modData -> return modData
    Nothing      -> do
      fp <- searchModule mo
      m <- readModule fp interp
      when (miModuleName m /= mo) $
        staticError $ text "The file" <+> ppr fp <+> text "must define module" <+> ppr mo
      return m






