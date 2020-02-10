module EvalApply
    ( eval
    )
where
import           Data.IORef
import           Types
import           Control.Monad.State

handleLambda :: Exp -> [Exp] -> IORef Env -> IO (Either ScmErr Exp)
handleLambda exp xs envBox = do
    mFunc <- eval exp envBox
    case mFunc of
        Left  e    -> return (Left e)
        Right func -> do
            lmargs <- forM xs (\x -> eval x envBox)
            let margs = sequenceA lmargs
            case margs of
                Left  e    -> return (Left e)
                Right args -> apply func args

evalList :: [Exp] -> IORef Env -> IO (Either ScmErr Exp)
evalList xs envBox = do
    forM_ (init xs) (\i -> eval i envBox)
    eval (last xs) envBox

eval :: Exp -> IORef Env -> IO (Either ScmErr Exp)
eval n@(Number _) _      = return (Right n)

eval (  Symbol s) envBox = do
    env <- readIORef envBox
    case Types.lookup s env of
        Just def -> return (Right def)
        Nothing ->
            return (Left $ ScmErr $ "eval: Symbol \"" ++ s ++ "\" undefined.")


eval (List []) _ = return (Left $ ScmErr $ "eval: got empty List")

eval (List (lambda@(List _) : xs)) envBox = handleLambda lambda xs envBox

eval (List ((Symbol "quote") : xs)) _ = case xs of
    [sth] -> return (Right sth)
    _     -> return (Left $ ScmErr $ "quote: nothing to quote")

eval (List ((Symbol "lambda") : xs)) envBox = do
    env     <- readIORef envBox
    closEnv <- newIORef (fromOuter env)
    return
        $ let t       = List xs
              closure = ScmClosure t closEnv
          in  Right (Closure closure)

eval (List ((Symbol "define") : xs)) envBox = case xs of
    [(Symbol sym), def] -> do
        mevalDef <- eval def envBox
        case mevalDef of
            Left  e       -> return (Left e)
            Right evalDef -> do
                modifyIORef' envBox (insertValue sym evalDef)
                return (Right Empty)

    -- syntax sugar for func definition
    (List (func@(Symbol _) : args)) : defs ->
        let
            desugared =
                (List
                    [ (Symbol "define")
                    , func
                    , (List $ [(Symbol "lambda"), List args] ++ defs)
                    ]
                )
        in  eval desugared envBox

    _ -> return (Left $ ScmErr $ "define: nothing to define")

eval (List ((Symbol "set!") : xs)) envBox = case xs of
    [(Symbol sym), def] -> do
        mevalDef <- eval def envBox
        case mevalDef of
            Left  e       -> return (Left e)
            Right evalDef -> do
                modifyIORef' envBox (setValue sym evalDef)
                return (Right Empty)
    _ -> return (Left $ ScmErr $ "set!: nothing to set")

eval (List [(Symbol "if"), cond, then_, else_]) envBox = do
    mevalCond <- eval cond envBox
    case mevalCond of
        Left  e               -> return (Left e)
        Right (Boolean True ) -> eval then_ envBox
        Right (Boolean False) -> eval else_ envBox
        Right _               -> return (Left $ ScmErr $ "if: expected Boolean")

eval (List ((Symbol "if") : _)) _ = return (Left $ ScmErr $ "if: ill-formed")

eval (List ((Symbol "cond") : t)) envBox =
    let
        evalTail [List [(Symbol "else"), then_]] = eval then_ envBox
        evalTail ((List [cond, then_]) : xs    ) = do
            mevalCond <- eval cond envBox
            case mevalCond of
                Left  e               -> return (Left e)
                Right (Boolean True ) -> eval then_ envBox
                Right (Boolean False) -> evalTail xs
                Right _ -> return (Left $ ScmErr $ "cond: expected Boolean")
        evalTail _ = return (Left $ ScmErr $ "cond: ill-formed")
    in
        evalTail t

eval (List ((Symbol "begin") : t)) envBox = evalList t envBox

eval (List (func@(Symbol _) : t)) envBox = handleLambda func t envBox

eval _ _      = return (Left $ ScmErr "eval: unexpected Exp")

apply :: Exp -> [Exp] -> IO (Either ScmErr Exp)
-- func can only be Primitive or Closure
apply (Primitive (ScmPrimitive prim     )) args = return (prim args)
apply (Closure   (ScmClosure body envBox)) args = do
    let (List (List (vars) : defs)) = body
    env      <- readIORef envBox
    localEnv <- newIORef env
    forM_ (zip vars args)
          (\((Symbol i), arg) -> modifyIORef' localEnv (insertValue i arg))
    evalList defs envBox

apply _ _ = undefined
