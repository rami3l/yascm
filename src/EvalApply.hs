{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module EvalApply
  ( eval,
    evalList,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, except, throwE)
import Control.Monad.Trans.Maybe (maybeToExceptT)
import Data.Function ((&))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Text.Format (format)
import qualified System.Exit as Exit
import Text.RawString.QQ (r)
import Types
  ( Env,
    Exp (..),
    ScmErr (..),
    fromOuter,
    insertValue,
    lookup,
    scmNil,
    setValue,
  )

handleLambda :: Exp -> [Exp] -> IORef Env -> ExceptT ScmErr IO Exp
handleLambda exp' xs envBox = do
  func <- eval exp' envBox
  args <- xs & mapM (`eval` envBox)
  apply func args

evalList :: [Exp] -> IORef Env -> ExceptT ScmErr IO Exp
evalList xs envBox = do
  init xs & mapM_ (`eval` envBox)
  eval (last xs) envBox

eval :: Exp -> IORef Env -> ExceptT ScmErr IO Exp
eval i@(ScmInt _) _ = return i
eval d@(ScmDouble _) _ = return d
eval str@(ScmStr _) _ = return str
eval (ScmSym s) envBox =
  s & (`Types.lookup` envBox)
    & maybeToExceptT
      (ScmErr $ format [r|eval: Symbol "{}" undefined.|] [s])
eval (ScmList []) _ = throwE $ ScmErr "eval: got empty List"
eval (ScmList (lambda@(ScmList _) : xs)) envBox = handleLambda lambda xs envBox
eval (ScmList ((ScmSym "quote") : xs)) _ = case xs of
  [sth] -> return sth
  _ -> throwE $ ScmErr "quote: nothing to quote"
eval (ScmList ((ScmSym "lambda") : xs)) envBox = do
  -- ! Here we want to clone a pointer, not to clone an Env.
  closEnv <- lift $ newIORef (fromOuter envBox)
  -- body := (ScmList(ScmList(vars) : defs))
  return $ ScmClosure (ScmList xs) closEnv
eval (ScmList ((ScmSym "define") : xs)) envBox = case xs of
  [ScmSym sym, def] -> do
    evalDef <- eval def envBox
    lift $ insertValue sym evalDef envBox
    return scmNil
  -- syntax sugar for func definition
  (ScmList (func@(ScmSym _) : args)) : defs ->
    let desugared =
          ScmList
            [ ScmSym "define",
              func,
              ScmList $ [ScmSym "lambda", ScmList args] ++ defs
            ]
     in eval desugared envBox
  _ -> throwE $ ScmErr "define: nothing to define"
eval (ScmList ((ScmSym "set!") : xs)) envBox = case xs of
  [ScmSym sym, def] -> do
    evalDef <- eval def envBox
    lift $ setValue sym evalDef envBox
    return scmNil
  _ -> throwE $ ScmErr "set!: nothing to set"
eval (ScmList [ScmSym "if", cond, then', else']) envBox = do
  evalCond <- eval cond envBox
  case evalCond of
    ScmBool True -> eval then' envBox
    ScmBool False -> eval else' envBox
    _ -> throwE $ ScmErr "if: expected Boolean"
eval (ScmList ((ScmSym "if") : _)) _ = throwE $ ScmErr "if: ill-formed"
eval (ScmList ((ScmSym "cond") : t)) envBox =
  let evalTail [ScmList [ScmSym "else", then']] = eval then' envBox
      evalTail ((ScmList [cond, then']) : xs) = do
        evalCond <- eval cond envBox
        case evalCond of
          ScmBool True -> eval then' envBox
          ScmBool False -> evalTail xs
          _ -> throwE $ ScmErr "cond: expected Boolean"
      evalTail _ = throwE $ ScmErr "cond: ill-formed"
   in evalTail t
eval (ScmList ((ScmSym "begin") : t)) envBox = evalList t envBox
eval (ScmList ((ScmSym "exit") : t)) _ =
  let exit [] = lift Exit.exitSuccess
      exit [ScmInt 0] = lift Exit.exitSuccess
      exit [ScmInt x] | x > 0 = lift $ Exit.exitWith (Exit.ExitFailure x)
      exit [ScmDouble d] = exit [ScmInt $ truncate d]
      exit _ = throwE $ ScmErr "exit: invalid exit code"
   in exit t
eval (ScmList ((ScmSym "display") : t)) envBox =
  let printElem x = do
        val <- eval x envBox
        lift $ Prelude.print val
        return $ ScmList []
   in case t of
        [] -> throwE $ ScmErr "display: nothing to display"
        xs -> xs & mapM printElem >> return scmNil
eval (ScmList ((ScmSym "newline") : t)) _ = case t of
  [] -> lift $ putStrLn "" >> return scmNil
  _ -> throwE $ ScmErr "newline: expected no arguments"
eval (ScmList (func@(ScmSym _) : t)) envBox = handleLambda func t envBox
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
