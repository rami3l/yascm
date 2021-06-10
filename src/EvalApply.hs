{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module EvalApply
  ( eval,
    evalList,
  )
where

import Control.Monad.Trans.Except (except, throwE)
import Data.Text.Format (format)
import Relude hiding (Text)
import qualified System.Exit as Exit
import Text.RawString.QQ (r)
import Types as T
  ( Env,
    Exp (..),
    ScmErr (..),
    fromOuter,
    insertValue,
    isDefined,
    lookup,
    scmNil,
    setValue,
    toConsCell,
  )

handleLambda :: Exp -> [Exp] -> IORef Env -> ExceptT ScmErr IO Exp
handleLambda exp' xs envBox = do
  func <- eval exp' envBox
  args <- xs & mapM (`eval` envBox)
  apply func args

evalList :: [Exp] -> IORef Env -> ExceptT ScmErr IO Exp
evalList xs envBox = case nonEmpty xs of
  Nothing -> return scmNil
  Just xs' -> do
    init xs' & mapM_ (`eval` envBox)
    eval (last xs') envBox

eval :: Exp -> IORef Env -> ExceptT ScmErr IO Exp
-- Self-evaluating types.
eval i@(ScmInt _) _ = return i
eval d@(ScmDouble _) _ = return d
eval str@(ScmStr _) _ = return str
eval (ScmSym s) envBox = case s of
  -- Booleans and other unchangeable constants.
  -- (No, we should not learn Python 2, where the booleans
  -- are part of the prelude!)
  "#t" -> return $ ScmBool True
  "#f" -> return $ ScmBool False
  -- The empty list alias.
  "nil" -> return scmNil
  -- Variable evaluation by name.
  _ ->
    s & (`T.lookup` envBox)
      & maybeToExceptT
        (ScmErr $ format [r|eval: Symbol "{}" undefined.|] [s])
-- Function calls and keywords.
eval (ScmList l) envBox = case l of
  [] -> throwE $ ScmErr "eval: got empty List"
  -- Inline anonymous function invocation.
  -- eg. ((lambda (x) (+ x 2)) 3) ;; => 5
  (lambda@(ScmList _) : xs) -> handleLambda lambda xs envBox
  -- Quote.
  (ScmSym "quote") : xs -> case xs of
    [ScmList l'] -> return $ toConsCell l'
    [quotee] -> return quotee
    _ -> throwE $ ScmErr "quote: nothing to quote"
  (ScmSym "lambda") : xs -> do
    -- ! Here we want to clone a pointer, not to clone an Env.
    -- body := (ScmList(ScmList(vars) : defs))
    ScmClosure (ScmList xs) <$> lift (newIORef (fromOuter envBox))
  -- Definition.
  (ScmSym "define") : xs -> case xs of
    -- Simple definition.
    -- eg. (define f (lambda (x y) *defns*))
    [ScmSym sym, def] -> do
      evalDef <- eval def envBox
      lift $ insertValue sym evalDef envBox
      return scmNil
    -- Syntax sugar for function definition.
    -- eg. (define (f x y) *defns*)
    -- ->  (define f (lambda (x y) *defns*))
    (ScmList (func@(ScmSym _) : args)) : defs ->
      let explicitLambda = ScmList $ [ScmSym "lambda", ScmList args] ++ defs
          desugared = ScmList [ScmSym "define", func, explicitLambda]
       in eval desugared envBox
    _ -> throwE $ ScmErr "define: nothing to define"
  -- Variable reset.
  (ScmSym "set!") : xs -> case xs of
    -- We can only reset a value that is already defined.
    [ScmSym sym, def] -> do
      isDefined' <- lift $ isDefined sym envBox
      if isDefined'
        then do
          evalDef <- eval def envBox
          lift $ setValue sym evalDef envBox
          return scmNil
        else throwE $ ScmErr "set!: can only set a already defined value"
    _ -> throwE $ ScmErr "set!: nothing to set"
  -- Conditional expression.
  (ScmSym "if") : t -> case t of
    [cond, then', else'] -> do
      evalCond <- eval cond envBox
      case evalCond of
        ScmBool True -> eval then' envBox
        ScmBool False -> eval else' envBox
        _ -> throwE $ ScmErr "if: expected Boolean"
    _ -> throwE $ ScmErr "if: ill-formed"
  (ScmSym "cond") : t ->
    let evalTail [ScmList [ScmSym "else", then']] = eval then' envBox
        evalTail ((ScmList [cond, then']) : xs) = do
          evalCond <- eval cond envBox
          case evalCond of
            ScmBool True -> eval then' envBox
            ScmBool False -> evalTail xs
            _ -> throwE $ ScmErr "cond: expected Boolean"
        evalTail _ = throwE $ ScmErr "cond: ill-formed"
     in evalTail t
  -- Begin, exit and display expressions.
  (ScmSym "begin") : t -> evalList t envBox
  (ScmSym "exit") : t ->
    let exit [] = lift Exit.exitSuccess
        exit [ScmInt 0] = lift Exit.exitSuccess
        exit [ScmInt x] | x > 0 = lift . Exit.exitWith . Exit.ExitFailure . fromIntegral $ x
        exit [ScmDouble d] = exit [ScmInt $ truncate d]
        exit _ = throwE $ ScmErr "exit: invalid exit code"
     in exit t
  (ScmSym "display") : t ->
    let printElem x = do
          val <- eval x envBox
          lift $ print val
          return $ ScmList []
     in case t of
          [] -> throwE $ ScmErr "display: nothing to display"
          xs -> xs & mapM printElem >> return scmNil
  (ScmSym "newline") : t -> case t of
    [] -> lift $ putStrLn "" >> return scmNil
    _ -> throwE $ ScmErr "newline: expected no arguments"
  -- Function call by name.
  func@(ScmSym _) : t -> handleLambda func t envBox
  _ -> throwE $ ScmErr "eval: unexpected compound Exp"
eval _ _ = throwE $ ScmErr "eval: unexpected Exp"

apply :: Exp -> [Exp] -> ExceptT ScmErr IO Exp
-- func can only be Primitive or Closure
apply (ScmPrimitive prim) args = except $ prim args
apply (ScmClosure body' envBox) args = do
  let (ScmList (ScmList vars : defs)) = body'
  env' <- readIORef envBox & lift
  localEnv <- newIORef env' & lift
  zip vars args
    & mapM_ (\(ScmSym i, arg) -> insertValue i arg localEnv)
    & lift
  evalList defs localEnv
apply _ _ = throwE $ ScmErr "apply: unexpected Exp"
