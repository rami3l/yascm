module EvalApply
    ( eval
    , evalList
    ) where
import           Data.IORef
import           Types
import           Control.Monad
import qualified System.Exit                   as Exit

handleLambda :: Exp -> [Exp] -> IORef Env -> IO (Either ScmErr Exp)
handleLambda exp' xs envBox = do
    Right func <- eval exp' envBox
    ms         <- forM xs (`eval` envBox)
    Right args <- return $ sequenceA ms
    apply func args

evalList :: [Exp] -> IORef Env -> IO (Either ScmErr Exp)
evalList xs envBox = do
    forM_ (init xs) (`eval` envBox)
    eval (last xs) envBox

eval :: Exp -> IORef Env -> IO (Either ScmErr Exp)
eval n@(  Number _) _      = return $ Right n

eval str@(String _) _      = return $ Right str

eval (    Symbol s) envBox = do
    mDef <- Types.lookup s envBox
    return $ maybe
        (Left $ ScmErr $ "eval: Symbol \"" ++ s ++ "\" undefined.")
        Right
        mDef

eval (List []) _ = return $ Left $ ScmErr "eval: got empty List"

eval (List (lambda@(List _) : xs)) envBox = handleLambda lambda xs envBox

eval (List ((Symbol "quote") : xs)) _ = case xs of
    [sth] -> return $ Right sth
    _     -> return $ Left $ ScmErr "quote: nothing to quote"

eval (List ((Symbol "lambda") : xs)) envBox = do
    -- ! Here we want to clone a pointer, not to clone an Env.
    closEnv <- newIORef $ fromOuter envBox
    return
        $ let t       = List xs
              closure = ScmClosure t closEnv
          in  Right $ Closure closure

eval (List ((Symbol "define") : xs)) envBox = case xs of
    [Symbol sym, def] -> do
        Right evalDef <- eval def envBox
        insertValue sym evalDef envBox
        return $ Right Empty

    -- syntax sugar for func definition
    (List (func@(Symbol _) : args)) : defs ->
        let desugared =
                List
                    [ Symbol "define"
                    , func
                    , List $ [Symbol "lambda", List args] ++ defs
                    ]
        in  eval desugared envBox

    _ -> return $ Left $ ScmErr "define: nothing to define"

eval (List ((Symbol "set!") : xs)) envBox = case xs of
    [Symbol sym, def] -> do
        Right evalDef <- eval def envBox
        setValue sym evalDef envBox
        return $ Right Empty
    _ -> return $ Left $ ScmErr "set!: nothing to set"

eval (List [Symbol "if", cond, then', else']) envBox = do
    mEvalCond <- eval cond envBox
    case mEvalCond of
        Left  e               -> return $ Left e
        Right (Boolean True ) -> eval then' envBox
        Right (Boolean False) -> eval else' envBox
        Right _               -> return $ Left $ ScmErr "if: expected Boolean"

eval (List ((Symbol "if") : _)) _ = return $ Left $ ScmErr "if: ill-formed"

eval (List ((Symbol "cond") : t)) envBox =
    let
        evalTail [List [Symbol "else", then']] = eval then' envBox
        evalTail ((List [cond, then']) : xs  ) = do
            mEvalCond <- eval cond envBox
            case mEvalCond of
                Left  e               -> return $ Left e
                Right (Boolean True ) -> eval then' envBox
                Right (Boolean False) -> evalTail xs
                Right _ -> return $ Left $ ScmErr "cond: expected Boolean"
        evalTail _ = return $ Left $ ScmErr "cond: ill-formed"
    in
        evalTail t

eval (List ((Symbol "begin") : t)) envBox = evalList t envBox

eval (List ((Symbol "exit" ) : t)) _      = case t of
    []         -> Exit.exitSuccess
    [Number 0] -> Exit.exitSuccess
    [Number x] -> Exit.exitWith $ Exit.ExitFailure $ truncate x
    _          -> return $ Left $ ScmErr "exit: invalid exit code"

eval (List ((Symbol "display") : t)) envBox =
    let printElem x = do
            Right val <- eval x envBox
            print val
            return $ Right Empty
    in  case t of
            [] -> return (Left $ ScmErr "display: nothing to display")
            xs -> do
                res     <- forM xs printElem
                Right _ <- return $ sequenceA res
                return $ Right Empty

eval (List ((Symbol "newline") : t)) _ = case t of
    [] -> do
        putStrLn ""
        return $ Right Empty
    _ -> return $ Left $ ScmErr "newline: expected no arguments"

eval (List (func@(Symbol _) : t)) envBox = handleLambda func t envBox

eval _ _ = return $ Left $ ScmErr "eval: unexpected Exp"

apply :: Exp -> [Exp] -> IO (Either ScmErr Exp)
-- func can only be Primitive or Closure
apply (Primitive (ScmPrimitive prim      )) args = return $ prim args

apply (Closure   (ScmClosure body' envBox)) args = do
    let (List (List vars : defs)) = body'
    env'     <- readIORef envBox
    localEnv <- newIORef env'
    forM_ (zip vars args) (\(Symbol i, arg) -> insertValue i arg localEnv)
    evalList defs localEnv

apply _ _ = undefined
