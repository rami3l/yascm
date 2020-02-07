module EvalApply
    ( eval
    )
where
import qualified Data.Map                      as Map
import           Types
import           Data.Either
import           Control.Monad.State

handleLambda :: Exp -> [Exp] -> State Env (Either ScmErr Exp)
handleLambda exp xs = state $ \env ->
    let (mFunc, env') = runState (eval exp) env
    in  case mFunc of
            Left e -> (Left e, env)
            Right func ->
                let (args, finalEnv) = foldl seedGen ([], env') xs
                          where
                            seedGen (ys, env) x =
                                let (Right y, env') = runState (eval x) env
                                in  (ys ++ [y], env')
                                -- ! Dangerous! ys ++ [y] is slow!
                                -- * Maybe we can use a DiffList
                in  (apply func args, finalEnv)

eval :: Exp -> State Env (Either ScmErr Exp)
eval (Number n) = return (Right (Number n))

eval (Symbol s) = state $ \env -> case Types.lookup s env of
    Just def -> (Right def, env)
    Nothing  -> (Left $ ScmErr $ "eval: Symbol \"" ++ s ++ "\" undefined.", env)

eval (List []) = return (Left $ ScmErr $ "eval: got empty List")

eval (List ((List lambda) : xs)) = handleLambda (List lambda) xs

eval (List ((Symbol "quote") : xs)) = case xs of
    [sth] -> return (Right sth)
    _     -> return (Left $ ScmErr $ "quote: nothing to quote")

eval (List ((Symbol "lambda") : xs)) = state $ \env ->
    let t       = List xs
        closure = ScmClosure t $ fromOuter env
    in  (Right $ Closure closure, env)

eval (List ((Symbol "define") : xs)) = state $ \env -> case xs of
    [(Symbol sym), def] ->
        let (mevalDef, env') = runState (eval def) env
        in  case mevalDef of
                Left e -> (Left e, env)
                Right evalDef ->
                    let (Env d o) = env'
                        env''     = Env (Map.insert sym evalDef d) o
                    in  (Right Empty, env'')
    _ -> (Left $ ScmErr $ "define: nothing to define", env)

eval (List ((Symbol "set!") : xs)) = state $ \env -> case xs of
    [(Symbol sym), def] ->
        let (mevalDef, env') = runState (eval def) env
        in  case mevalDef of
                Left e -> (Left e, env')
                Right evalDef ->
                    let (Env d o) = env'
                        env''     = setValue sym def env'
                    in  (Right Empty, env'')
    _ -> (Left $ ScmErr $ "set!: nothing to set", env)

eval (List [(Symbol "if"), cond, then_, else_]) = do
    mevalCond <- eval cond
    case mevalCond of
        Left  e               -> return (Left e)
        Right (Boolean True ) -> eval then_
        Right (Boolean False) -> eval else_
        Right _               -> return (Left $ ScmErr $ "if: expected Boolean")

eval (List ((Symbol "if") : xs)) = return (Left $ ScmErr $ "if: ill-formed")

eval (List ((Symbol "cond") : t)) =
    let
        evalTail [List [(Symbol "else"), then_]] =
            state $ \env -> runState (eval then_) env
        evalTail ((List [cond, then_]) : xs) = state $ \env ->
            let (mevalCond, env') = runState (eval cond) env
            in  case mevalCond of
                    Left  e               -> (Left e, env')
                    Right (Boolean True ) -> runState (eval then_) env'
                    Right (Boolean False) -> runState (evalTail xs) env'
                    Right _ -> (Left $ ScmErr $ "cond: expected Boolean", env)
        evalTail _ = state $ \env -> (Left $ ScmErr $ "cond: ill-formed", env)
    in
        evalTail t

eval (List ((Symbol "begin") : t)) = state
    $ \env -> foldl seedGen (Right Empty, env) t
    where seedGen (_, env) x = runState (eval x) env

eval (List ((Symbol f) : t)) = handleLambda (Symbol f) t

eval _                       = return (Left $ ScmErr "eval: unexpected Exp")

apply :: Exp -> [Exp] -> Either ScmErr Exp
-- func can only be Primitive or Closure
apply (Primitive (ScmPrimitive prim)) args = prim args
apply (Closure (ScmClosure body env)) args =
    let
        (List (List (vars) : defs)) = body
        (Env d o                  ) = env
        d'                          = foldl seedGen d (zip vars args)
            where seedGen seed ((Symbol var), arg) = Map.insert var arg seed
        localEnv = Env d' o
        (res, localEnv') =
            runState ((forM (init defs) eval) >> (eval (last defs))) localEnv
    in
        res

