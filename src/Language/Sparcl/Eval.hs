{-# LANGUAGE RecursiveDo #-}

module Language.Sparcl.Eval where

import           Language.Sparcl.Core.Syntax
import           Language.Sparcl.Exception
import           Language.Sparcl.Value

import           Control.Monad               ((>=>), unless, zipWithM, when, filterM)
import           Control.Monad.Except
import           Data.Maybe                  (fromMaybe)
-- import Control.Monad.State

-- import qualified Control.Monad.Fail as Fail

-- import Control.Monad.State

-- import qualified Control.Monad.Fail as Fail
import           Control.Monad.Reader        (MonadReader (local), ask, ReaderT (runReaderT))
-- import           Debug.Trace                 (trace)
import           Language.Sparcl.Pretty      hiding ((<$>))

import           GHC.IO (unsafeInterleaveIO)
import           Control.Monad.IO.Class

-- lookupEnvR :: QName -> Env -> Eval Value
-- lookupEnvR n env = case M.lookup n env of
--   Nothing -> throwError $ "Undefined value: " ++ show n
--   Just  v -> return v


evalUBind :: Env -> Bind Name -> Eval Env
evalUBind env ds = do
    rec ev  <- mapM (\(n,_,e) -> do
                        lvl <- ask 
                        -- v <- evalU env' e
                        v <- liftIO $ unsafeInterleaveIO $ runEvalWith lvl (evalU env' e)
                        return (n,v)) ds
        let env' = extendsEnv ev env
    return env'

evalU :: Env -> Exp Name -> Eval Value
evalU env expr = case expr of
  Lit l -> return $ VLit l
  Var n ->
    lookupEnv n env
  App e1 e2 -> do
    v1 <- evalU env e1
    case v1 of
      VFun f -> do
        v2 <- evalU env e2
        f v2
      _ ->
        rtError $ text "the first component of application must be a function, but we got " <> ppr v1 <> text " from " <> ppr e1
  Abs n e ->
    return $ VFun (\v -> evalU (extendEnv n v env) e)

  Con q es ->
    VCon q <$> mapM (evalU env) es

  Case e0 pes -> do
    v0 <- evalU env e0
    evalCase env v0 pes

  Lift ef eb -> do
    VFun vf <- evalU env ef
    VFun vb <- evalU env eb
    return $ VFun $ \(VRes f b) -> return $ VRes (f >=> vf) (vb >=> b)
    -- VBang (VFun vf) <- evalU env ef
    -- VBang (VFun vb) <- evalU env eb
    -- let vf' = vf . VBang
    -- let vb' = vb . VBang
    -- return $ VBang $ VFun $ \(VRes f b) ->
    --                           return $ VRes (f >=> vf') (vb' >=> b)

  Let ds e -> do
    env' <- evalUBind env ds
    evalU env' e

  Unlift e -> do
    -- VBang (VFun f) <- evalU env e
    VFun f <- evalU env e
    newAddr $ \a -> do
      VRes f0 b0 <- f (VRes (lookupHeap a)
                            (return . singletonHeap a))
      let f0' v = f0 (singletonHeap a v)
      let b0' v = do hp <- b0 v
                     lookupHeap a hp
      -- let f0' (VBang v) = f0 (singletonHeap a v)
      --     f0' _         = error "expecting !"
      -- let b0' (VBang v) = do hp <- b0 v
      --                        lookupHeap a hp
      --     b0' _         = error "expecting !"
      let c = nameTuple 2
      -- return $ VCon c [VBang (VFun f0'), VBang (VFun b0')]
      return $ VCon c [VFun f0', VFun b0']


  RCon q es -> do
    vs <- mapM (evalU env) es
    return $ VRes (\heap -> do
                      us <- mapM (\v -> runFwd v heap) vs
                      return $ VCon q us)
                  (\v' ->
                     case v' of
                       VCon q' us' | q == q' && length us' == length es -> do
                                       envs <- zipWithM runBwd vs us'
                                       return $ foldr unionHeap emptyHeap envs
                       _ ->
                         rtError $ text "out of the range:" <+> ppr v' <+> text "for" <+> ppr expr)

  RCase e0 pes -> do
    VRes f0 b0 <- evalU env e0
    pes' <- mapM (\(p,e,e') -> do
                     -- VBang (VFun ch) <- evalU env e'
                     VFun ch <- evalU env e'
                     let ch' v = do
                           res <- ch v
                           case res of
                             VCon q [] | q == conTrue -> return True
                             _                        -> return False
                     return (p, e, ch', e')) pes
    lvl <- ask
    return $ VRes (\hp -> local (const lvl) $ evalCaseF env hp f0 pes')
                  (\v  -> local (const lvl) $ evalCaseB env v b0 pes')


  RPin e1 e2 -> do
    VRes f1 b1 <- evalU env e1
    VFun h     <- evalU env e2
    let c = nameTuple 2
    return $ VRes (\hp -> do
                      a <- f1 hp
                      VRes f2 _ <- h a -- (VBang a)
                      b <- f2 hp
                      return $ VCon c [a, b])
                  (\case
                      VCon c' [a,b] | c' == c -> do
                                        VRes _ b2 <- h a -- (VBang a)
                                        hp2 <- b2 b
                                        hp1 <- b1 a
                                        return $ unionHeap hp1 hp2
                      _ -> rtError $ text "Expected a pair"
                  )

  -- RCaseM e0 pes -> do
  --   VRes f0 b0 <- evalU env e0
  --   pes' <- mapM (\(p,e,e') -> do
  --                    -- VBang (VFun ch) <- evalU env e'
  --                    VFun ch <- evalU env e'
  --                    let ch' v = do
  --                          case v of
  --                           VCon qp [vb,vhs] | qp == nameTuple 2 -> do
  --                             VFun chm <- ch vb
  --                             resPair <- chm vhs
  --                             case resPair of
  --                               VCon qp [VCon q [], _] | qp == nameTuple 2 && q == conTrue -> return True
  --                               _ -> return False
  --                           _ -> return False
  --                    return (p, e, ch', e')) pes
  --   lvl <- ask
  --   return $ VRes (\hp -> local (const lvl) $ evalCaseFM env hp f0 fhs pes')
  --                 (\v  -> local (const lvl) $ evalCaseBM env v b0 bhs pes')





evalCase :: Env -> Value -> [ (Pat Name, Exp Name) ] -> Eval Value
evalCase _   v [] = rtError $ text "pattern match error" <+> ppr v
evalCase env v ((p, e):pes) =
  case findMatch v p of
    Just binds -> evalU (extendsEnv binds env) e
    _          -> evalCase env v pes

findMatch :: Value -> Pat Name -> Maybe [ (Name, Value) ]
findMatch v (PVar n) = return [(n, v)]
findMatch (VCon q vs) (PCon q' ps) | q == q' && length vs == length ps =
                                       concat <$> zipWithM findMatch vs ps
findMatch _ _ = Nothing

evalCaseF :: Env -> Heap -> (Heap -> Eval Value) -> [ (Pat Name, Exp Name, Value -> Eval Bool, Exp Name) ] -> Eval Value
evalCaseF env hp f0 alts = do
  v0 <- f0 hp
  go v0 [] alts
  where
    go :: Value -> [(Exp Name, Value -> Eval Bool)] -> [ (Pat Name, Exp Name, Value -> Eval Bool, Exp Name) ] -> Eval Value
    go v0  _       [] = rtError $ text $ "pattern match failure (fwd): " ++ prettyShow v0
    go v0 checker ((p,e,ch,chExp) : pes) =
      case findMatch v0 p of
        Nothing ->
          go v0 ((chExp, ch):checker) pes
        Just binds ->
          newAddrs (length binds) $ \as -> do
             let hbinds = zipWith (\a (_, v) -> (a, v)) as binds
             let binds' = zipWith (\a (x, _) ->
                                     (x, VRes (lookupHeap a) (return . singletonHeap a))) as binds
             VRes f _ <- evalU (extendsEnv binds' env) e
             res <- f (foldr (uncurry extendHeap) hp hbinds)
             checkAssert (chExp, ch) checker res

    checkAssert :: (Exp Name, Value -> Eval Bool) -> [(Exp Name, Value -> Eval Bool)] -> Value -> Eval Value
    checkAssert ch checkers res = do
      let expectedResults = True : map (const False) checkers 
      let checks = zip (ch:checkers) expectedResults

      failedChecks <- filterM (\((_cExp, c), r) -> (/= r) <$> c res) checks 

      () <- case failedChecks of 
              [] -> pure () 
              _  -> do 
                rtError $ align $ text "Assertion failed (fwd) for value: " <> ppr res 
                          <> nest 2 (linebreak <> vsep 
                              [ sep [text "-", align $ vsep [ppr cExp, hsep [text "should return" , ppr expected]]]
                              | ((cExp, _), expected) <- failedChecks ])
      pure res 


evalCaseB :: Env -> Value -> (Value -> Eval Heap) -> [ (Pat Name, Exp Name, Value -> Eval Bool, a) ] -> Eval Heap
evalCaseB env vres b0 alts = do
  (v, hp) <- go [] alts
  hp' <- {- trace (show $ text "hp = " <> pprHeap hp <> comma <+> text "v = " <> ppr v) $ -} b0 v
  return $ unionHeap hp hp'
  where
    mkAssert :: Pat Name -> (Pat Name, Value -> Bool)
    mkAssert p = (p, \v -> case findMatch v p of
                     Just _ -> True
                     _      -> False) 

    checkAllFalse :: Applicative m => Value -> [(Pat Name, Value -> Bool)] -> (Pat Name -> m ()) -> m () 
    checkAllFalse _ [] _whenTrue = pure () 
    checkAllFalse v ((p, ch) : checkers) whenTrue = 
      if ch v then whenTrue p 
      else         checkAllFalse v checkers whenTrue 

    go _ [] = rtError $ text "pattern match failure (bwd)"
    go checker ((p,e,ch,_):pes) = do
      -- flg <- ch (VBang vres)
      flg <- ch vres
      if flg
        then do
          let xs = freeVarsP p
          newAddrs (length xs) $ \as -> do
            let binds' = zipWith (\x a ->
                                    (x, VRes (lookupHeap a) (return . singletonHeap a))) xs as
            VRes _ b <- {- trace ("Evaluating bodies") $ -} evalU (extendsEnv binds' env) e
            hpBr <- {- trace ("vres = " ++ show (ppr vres)) $ -} b vres
            v0 <- {- trace ("hpBr = " ++ show (pprHeap hpBr)) $ -} fillPat p <$> zipWithM (\x a -> (x,) <$> lookupHeap a hpBr) xs as
            () <- checkAllFalse v0 checker $ \pTrue -> 
              rtError $ align $ text "Assertion failed (bwd): the following pattern and value should not match." <> 
                nest 2 (linebreak <> vsep [ hsep [ text "pattern:" , ppr pTrue ],
                                            hsep [ text "value:  " , ppr v0 ] ])
            return (v0, removesHeap as hpBr)
        else go (mkAssert p:checker) pes

    fillPat :: Pat Name -> [ (Name, Value) ] -> Value
    fillPat (PVar n) bs =
      fromMaybe (error "Shouldn't happen") (lookup n bs)

    fillPat (PCon c ps) bs =
      VCon c (map (flip fillPat bs) ps)

evalCaseFM :: Env -> Heap -> (Heap -> Eval Value) -> (Heap -> Eval Value) -> [ (Pat Name, Exp Name, Value -> Eval Bool, Exp Name) ] -> Eval Value
evalCaseFM env hp f0 fh alts = do
  v0 <- f0 hp
  vh <- fh hp
  go v0 vh [] alts
  where
    go :: Value -> Value -> [(Exp Name, Value -> Eval Bool)] -> [ (Pat Name, Exp Name, Value -> Eval Bool, Exp Name) ] -> Eval Value
    go v0  _      _  [] = rtError $ text $ "pattern match failure (fwd): " ++ prettyShow v0
    go v0 vh checker ((p,e,ch,chExp) : pes) =
      case findMatch v0 p of
        Nothing ->
          go v0 vh ((chExp, ch):checker) pes
        Just binds ->
          newAddr $ \ah -> do
            newAddrs (length binds) $ \as -> do
              let hbinds = zipWith (\a (_, v) -> (a, v)) as binds
              let binds' = zipWith (\a (x, _) ->
                                      (x, VRes (lookupHeap a) (return . singletonHeap a))) as binds
              VFun f1  <- evalU (extendsEnv binds' env) e
              VRes f _ <- f1 (VRes (lookupHeap ah) (return . singletonHeap ah))
              res <- f (foldr (uncurry extendHeap) hp hbinds)
              checkAssert (chExp, ch) checker res

    checkAssert :: (Exp Name, Value -> Eval Bool) -> [(Exp Name, Value -> Eval Bool)] -> Value -> Eval Value
    checkAssert ch checkers res = do
      let expectedResults = True : map (const False) checkers 
      let checks = zip (ch:checkers) expectedResults

      failedChecks <- filterM (\((_cExp, c), r) -> (/= r) <$> c res) checks 

      () <- case failedChecks of 
              [] -> pure () 
              _  -> do 
                rtError $ align $ text "Assertion failed (fwd) for value: " <> ppr res 
                          <> nest 2 (linebreak <> vsep 
                              [ sep [text "-", align $ vsep [ppr cExp, hsep [text "should return" , ppr expected]]]
                              | ((cExp, _), expected) <- failedChecks ])
      pure res 


evalCaseBM :: Env -> Value -> (Value -> Eval Heap) -> (Value -> Eval Heap) -> [ (Pat Name, Exp Name, Value -> Eval Bool, a) ] -> Eval Heap
evalCaseBM env vres b0 bh alts = do
  (v, vh, hp) <- go [] alts
  hp' <- {- trace (show $ text "hp = " <> pprHeap hp <> comma <+> text "v = " <> ppr v) $ -} b0 v
  hp'' <- bh vh
  return $ unionHeap hp'' $ unionHeap hp hp'
  where
    mkAssert :: Pat Name -> (Pat Name, Value -> Bool)
    mkAssert p = (p, \v -> case findMatch v p of
                     Just _ -> True
                     _      -> False) 

    checkAllFalse :: Applicative m => Value -> [(Pat Name, Value -> Bool)] -> (Pat Name -> m ()) -> m () 
    checkAllFalse _ [] _whenTrue = pure () 
    checkAllFalse v ((p, ch) : checkers) whenTrue = 
      if ch v then whenTrue p 
      else         checkAllFalse v checkers whenTrue 

    go _ [] = rtError $ text "pattern match failure (bwd)"
    go checker ((p,e,ch,_):pes) = do
      -- flg <- ch (VBang vres)
      flg <- ch vres
      if flg
        then do
          let xs = freeVarsP p
          newAddr $ \ah -> do
            let hbinds' = (,ah)
            newAddrs (length xs) $ \as -> do
              let binds' = zipWith (\x a ->
                                      (x, VRes (lookupHeap a) (return . singletonHeap a))) xs as
              VFun f <- {- trace ("Evaluating bodies") $ -} evalU (extendsEnv binds' env) e
              VRes _ b <- f $ VRes (lookupHeap ah) (return . singletonHeap ah)
              hpBr <- {- trace ("vres = " ++ show (ppr vres)) $ -} b vres
              v0 <- {- trace ("hpBr = " ++ show (pprHeap hpBr)) $ -} fillPat p <$> zipWithM (\x a -> (x,) <$> lookupHeap a hpBr) xs as
              vh <- lookupHeap ah hpBr
              () <- checkAllFalse v0 checker $ \pTrue -> 
                rtError $ align $ text "Assertion failed (bwd): the following pattern and value should not match." <> 
                  nest 2 (linebreak <> vsep [ hsep [ text "pattern:" , ppr pTrue ],
                                              hsep [ text "value:  " , ppr v0 ] ])
              return (v0, vh, removeHeap ah (removesHeap as hpBr))
        else go (mkAssert p:checker) pes

    fillPat :: Pat Name -> [ (Name, Value) ] -> Value
    fillPat (PVar n) bs =
      fromMaybe (error "Shouldn't happen") (lookup n bs)

    fillPat (PCon c ps) bs =
      VCon c (map (flip fillPat bs) ps)

runFwd :: Value -> Heap -> Eval Value
runFwd (VRes f _) = f
runFwd _          = \_ -> rtError $ text "expected a reversible comp."

runBwd :: Value -> Value -> Eval Heap
runBwd (VRes _ b) = b
runBwd _          = \_ -> rtError $ text "expected a reversible comp."


