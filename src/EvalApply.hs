module EvalApply
    ( eval
    , evalList
    ) where
import           Data.IORef
import           Data.Function
import           Types
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import qualified System.Exit                   as Exit

handleLambda :: Exp -> [Exp] -> IORef Env -> ExceptT ScmErr IO Exp
handleLambda exp' xs envBox = do
    func <- eval exp' envBox
    args <- xs `forM` (`eval` envBox)
    apply func args

evalList :: [Exp] -> IORef Env -> ExceptT ScmErr IO Exp
evalList xs envBox = do
    init xs `forM_` (`eval` envBox)
    eval (last xs) envBox

eval :: Exp -> IORef Env -> ExceptT ScmErr IO Exp
eval n@(  Number _) _      = return n

eval str@(String _) _      = return str

eval (    Symbol s) envBox = s & (`Types.lookup` envBox) & maybeToExceptT
    (ScmErr $ "eval: Symbol \"" ++ s ++ "\" undefined.")


eval (List []) _ = throwE $ ScmErr "eval: got empty List"

eval (List (lambda@(List _) : xs)) envBox = handleLambda lambda xs envBox

eval (List ((Symbol "quote") : xs)) _ = case xs of
    [sth] -> return sth
    _     -> throwE $ ScmErr "quote: nothing to quote"

eval (List ((Symbol "lambda") : xs)) envBox = do
    -- ! Here we want to clone a pointer, not to clone an Env.
    closEnv <- lift $ newIORef (fromOuter envBox)
    -- body := (List (List (vars) : defs))
    let closure = ScmClosure (List xs) closEnv
    return $ Closure closure

eval (List ((Symbol "define") : xs)) envBox = case xs of
    [Symbol sym, def] -> do
        evalDef <- eval def envBox
        lift $ insertValue sym evalDef envBox
        return Empty

    -- syntax sugar for func definition
    (List (func@(Symbol _) : args)) : defs ->
        let desugared =
                List
                    [ Symbol "define"
                    , func
                    , List $ [Symbol "lambda", List args] ++ defs
                    ]
        in  eval desugared envBox

    _ -> throwE $ ScmErr "define: nothing to define"

eval (List ((Symbol "set!") : xs)) envBox = case xs of
    [Symbol sym, def] -> do
        evalDef <- eval def envBox
        lift $ setValue sym evalDef envBox
        return Empty
    _ -> throwE $ ScmErr "set!: nothing to set"

eval (List [Symbol "if", cond, then', else']) envBox = do
    evalCond <- eval cond envBox
    case evalCond of
        Boolean True  -> eval then' envBox
        Boolean False -> eval else' envBox
        _             -> throwE $ ScmErr "if: expected Boolean"

eval (List ((Symbol "if") : _)) _ = throwE $ ScmErr "if: ill-formed"

eval (List ((Symbol "cond") : t)) envBox =
    let evalTail [List [Symbol "else", then']] = eval then' envBox
        evalTail ((List [cond, then']) : xs  ) = do
            evalCond <- eval cond envBox
            case evalCond of
                Boolean True  -> eval then' envBox
                Boolean False -> evalTail xs
                _             -> throwE $ ScmErr "cond: expected Boolean"
        evalTail _ = throwE $ ScmErr "cond: ill-formed"
    in  evalTail t

eval (List ((Symbol "begin") : t)) envBox = evalList t envBox

eval (List ((Symbol "exit" ) : t)) _      = case t of
    []         -> lift Exit.exitSuccess
    [Number 0] -> lift Exit.exitSuccess
    [Number x] -> lift $ Exit.exitWith (Exit.ExitFailure $ truncate x)
    _          -> throwE $ ScmErr "exit: invalid exit code"

eval (List ((Symbol "display") : t)) envBox =
    let printElem x = do
            val <- eval x envBox
            lift $ print val
            return Empty
    in  case t of
            [] -> throwE $ ScmErr "display: nothing to display"
            xs -> do
                _ <- xs `forM` printElem
                return Empty

eval (List ((Symbol "newline") : t)) _ = case t of
    [] -> do
        lift $ putStrLn ""
        return Empty
    _ -> throwE $ ScmErr "newline: expected no arguments"

eval (List (func@(Symbol _) : t)) envBox = handleLambda func t envBox

eval _ _ = throwE $ ScmErr "eval: unexpected Exp"

apply :: Exp -> [Exp] -> ExceptT ScmErr IO Exp
-- func can only be Primitive or Closure
apply (Primitive (ScmPrimitive prim      )) args = except $ prim args

apply (Closure   (ScmClosure body' envBox)) args = do
    let (List (List vars : defs)) = body'
    env'     <- lift $ readIORef envBox
    localEnv <- lift $ newIORef env'
    zip vars args
        `forM_` (\(Symbol i, arg) -> insertValue i arg localEnv)
        &       lift
    evalList defs localEnv

apply _ _ = throwE $ ScmErr "apply: unexpected Exp"
