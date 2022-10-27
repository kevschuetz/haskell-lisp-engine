module Lisp (lisp) where

import           Data.Char
import           Data.List
import           Debug.Trace
import           Parser
import System.IO
import System.Directory

-- data type definitions --------------------------------------------------------------------------

data LispData =
      Nil
    | I Integer
    | B Bool
    | S String
    | P LispData LispData
    | F { params :: [String], body :: LispData }
     deriving (Eq)

--- String representation for LispData ------------------------------------------------------------

instance Show LispData where
    show Nil         = "nil"
    show (B v)       = map toLower (show v)
    show (I v)       = show v
    show (S s)       = show s
    show (F params body)  = "(Fun " ++ show params ++ show body ++ ")"
    show l@(P _ _)  = "(" ++  showP l ++ ")"
      where
         showP (P e Nil) = show e                                 -- last element element
         showP (P f r@(P _ _)) = (show f) ++ " " ++ (showP r)     -- list with tail list
         showP (P f s) = (show f) ++ " . " ++ (show s)            -- a pair of two elems
         showP _       = "NA"


--- eval LispExpr ---------------------------------------------------------------------------------

eval :: [(String, LispData)] -> LispData -> Maybe LispData

-- eval nil 
eval bds Nil =
  trace "++ Evaluating nil" (
    Just Nil
  )

-- eval values
eval _   b@(B _)                =
  trace ("++ Evaluating boolean: " ++ (show b))
    (Just b)
eval _   i@(I _)                =
  trace ("++ Evaluating integer: " ++ (show i))
     Just i

-- eval variables
eval bds v@(S n)                =
  trace ("++ Evaluating symbol: " ++ (show v))
    lookupBdg bds n

-- eval lambda
eval bds l@(P (S "lambda") (P args (P body Nil)))  =
  trace ("++ Evaluating lambda: " ++ (show l)) (
    case names args of
       Just ns -> Just (F ns body)
       Nothing -> Nothing
  )
  where
      names :: LispData -> Maybe [String]
      names Nil                    = Just []
      names (P (S n) rest)         =
        case (names rest) of
          Just ns -> Just (n:ns)
          Nothing -> Nothing
      names _                      = Nothing
eval bds (P (S "lambda") _)                      = Nothing

-- eval eval special form
eval bds e@(P (S "eval") (P d Nil)) =
  trace ("++ Evaluating eval: " ++ (show e)) (
    case (eval bds d) of 
      Just r   -> eval bds r
      Nothing  -> Nothing
  )
eval bds e@(P (S "eval") _) = 
    trace ("++ Evaluating invalid eval: " ++ (show e))
      Nothing 

-- eval arithmetic binary function applications
eval bds e@(P (S op) (P left (P right Nil)))  | elem op ["+", "-", "*", "/", "%"]    =
  trace ("++ Evaluating arithmetic: " ++ (show e)) (
    let
      lr = eval bds left
      rr = eval bds right
    in
      case (lr, rr) of
        (Just (I li), Just (I ri)) ->
          case op of
            "+" -> Just (I (li + ri))
            "-" -> Just (I (li - ri))
            "*" -> Just (I (li * ri))
            "/" -> Just (I (li `div` ri))
            "%" -> Just (I (li `mod` ri))
            _   -> Nothing -- will not happen
        (_, _)                     -> Nothing
  )

-- eval minus unary function applications (e.g., (- 3) )
  -- TODO
eval bds e@(P (S "-") (P right Nil)) =
  trace ("++ Evaluating unary arithmetic " ++ (show e))(
    let 
      argument = eval bds right
    in
      case (argument) of
        (Just (I a)) ->
          Just (I (0 - a))
        (_)                     -> Nothing
  )


-- eval equality function applications (operators "==" and "!=")
  -- TODO
eval bds e@(P (S op) (P left (P right Nil))) | elem op["==", "!="] =
  trace ("++ Evaluating equality function application " ++ (show e))(
    let
      lr = eval bds left
      rr = eval bds right
    in
      case (lr, rr) of
        (Just (I li), Just (I ri)) ->
          case op of
            "==" -> Just (B (li == ri))
            "!=" -> Just (B (li /= ri))
            _   -> Nothing -- will not happen
        (_, _)                     -> Nothing
  )

-- eval relational function applications (operators "<", ">", "<=", and ">=")
  -- TODO
eval bds e@(P (S op) (P left (P right Nil))) | elem op["<", ">", "<=", ">="] =
  trace ("++ Evaluating relational function application " ++ (show e))(
    let
      lr = eval bds left
      rr = eval bds right
    in
      case (lr, rr) of
        (Just (I li), Just (I ri)) ->
          case op of
            "<" -> Just (B (li < ri))
            ">" -> Just (B (li > ri))
            "<=" -> Just (B (li <= ri))
            ">=" -> Just (B (li >= ri))
            _   -> Nothing -- will not happen
        (_, _)                     -> Nothing
  )

-- eval Boolean binary function applications (operators "&&" and "||")
  -- TODO
eval bds e@(P (S op) (P left (P right Nil))) | elem op["&&", "||"] =
  trace ("++ Evaluating binary function application " ++ (show e))(
    let
      lr = eval bds left
      rr = eval bds right
    in
      case (lr, rr) of
        (Just (B lb), Just (B rb)) ->
          case op of
            "&&" -> Just (B (lb && rb))
            "||" -> Just (B (lb || rb))
            _   -> Nothing -- will not happen
        (_, _)                     -> Nothing
  )

-- eval Boolean unary Boolean not function application (e.g., (! true))
  -- TODO
eval bds e@(P (S "!") (P right Nil)) =
  trace ("++ Evaluating unary Boolean not function " ++ (show e))(
    let 
      argument = eval bds right
    in
      case (argument) of
        (Just (B b)) -> Just (B (not b))
        (_) -> Nothing
  )

-- eval built in function application cons (e.g., (cons 1 nil) )
  -- TODO
eval bds e@(P (S "cons") (P left (P right Nil))) =
  trace ("++ Evaluating built in function application cons " ++ (show e))(
    let
      lr = eval bds left
      rr = eval bds right
    in
      case (lr, rr) of
        (Just (I li), Just Nil) -> Just (P (I li) Nil)
        (Just (I li), Just (I ri)) -> Just (P (I li) (I ri))
        (Just (I li), Just (P l r)) -> Just (P (I li) (P l r))
        (_, _)                     -> Nothing 
  )


-- eval built in function application car (e.g., (car (cons 1 nil)) )
  -- TODO
eval bds e@(P (S "car") (P left right)) =
  trace ("++ Evaluating built in function application car " ++ (show e))(
    let
      lr = eval bds left
      rr = eval bds right
    in
      case (lr, rr) of
        (Just (P l r), _) -> Just l
        (Just (I i), _) -> Just (I i)
        (_, _)                     -> Nothing
  )

-- eval built in function application cdr (e.g., (cdr (cons 1 nil)) )
  -- TODO
eval bds e@(P (S "cdr") (P left right)) =
  trace ("++ Evaluating built in function application cdr " ++ (show e))(
    let
      lr = eval bds left
      rr = eval bds right
    in
      case (lr, rr) of
        (Just (P l r), _) -> Just r
        (_, _)                     -> Nothing
  )


-- eval quote special form
  -- TODO
eval bds e@(P (S "quote") arg) =
  trace ("++ Evaluating quote special form " ++ (show e))(
    case (arg) of
      (P l r) -> Just l
      (_) -> Nothing
  )

-- eval if special form
  -- TODO
eval bds e@(P (S "if") (P cond (P eThen (P eElse Nil)))) =
  trace ("++ Evaluating if special form " ++ (show cond) ++ " | " ++ (show eThen) ++ " | " ++ (show eElse))(
    let
      condEval = eval bds cond
    in
      case (condEval) of
        (Just (B b)) -> if b then eval bds eThen else eval bds eElse
        (_) -> Nothing
  )


-- eval of binary operator with other than two arguments fails
eval bds e@(P (S op) _) | elem op ["+", "-", "*", "/", "%", "<", ">", "<=", ">=", "&&", "||", "==", "!=", "cons"]  =
  trace ("++ Evaluating invalid binary operation: " ++ (show e))
    Nothing

-- eval of unary operator with other than one arguments fails
eval bds e@(P (S op) _) | elem op ["-", "!", "car", "cdr", "quote"]  =
  trace ("++ Evaluating invalid unary operation: " ++ (show e))
    Nothing

-- eval function applications
  -- TODO
eval bds e@(P func a) =
  trace ("++ Evaluating function application " ++ (show func) ++ " | " ++ (show a) ++ " | ")(
      let
        fl = eval bds func
      in
        case (fl) of
          (Just (F args body)) -> trace ((show fl) ++ " | " ++ (show args) ++ " | " ++ (show body)) (
            case (extendBds bds args a) of
              Just newBindings -> eval newBindings body
              (_) -> Nothing) -- bind evaluation of actual arguments to formal parameters, call function body
          (_) -> Nothing
  )

-- evaluating any other operation is invalid
eval bds e                      =
  trace ("++ Evaluating invalid expression: " ++ (show e))
    Nothing


-- Bindings ---------------------------------------------------------------------------------------

-- extendBds
extendBds :: [(String, LispData)] -> [String] -> LispData -> Maybe [(String, LispData)]
extendBds bds [] Nil           = Just bds
extendBds bds [] _             = Nothing -- should not happen
extendBds bds (n:ns) (P a as)  =
  case eval bds a of
    Just v    -> extendBds ((n, v):bds) ns as
    _         -> Nothing
extendBds bds _  _             = Nothing -- should not happen

-- lookup binding

lookupBdg :: [(String, LispData)] -> String -> Maybe LispData
lookupBdg bds variable =
  case find (\(n, e) -> n == variable) bds of
    Just (n, v) -> Just v
    _           -> Nothing

--- Parse LispData --------------------------------------------------------------------------------

parseLispData :: Parser LispData
parseLispData = parseNil `orElse` parseInt `orElse` parseBool `orElse` parseSymb `orElse` parseList
  where
    parseNil = token "nil" >> return Nil
    parseBool = ((token "true") >> return (B True)) `orElse`
              ((token "false") >> return (B False))
    parseInt = (filterBy word isNumber) >>= \numbString -> return (I (read numbString :: Integer))
           where
               isNumber ('+':[])      = False
               isNumber ('-':[])      = False
               isNumber word@(f:rest) = (all isDigit word) || ((f == '-' || f == '+') && (all isDigit rest))
               isNumber _             = False -- will not happen
    parseSymb =   do
                  w <- word
                  return (S w)
    parseList = do
                open
                elems <- multiple parseLispData
                close
                return (mkPairs elems)
                where mkPairs []  = Nil
                      mkPairs (e:es) = P e (mkPairs es)

--------------------------------------------------------------------------------------------------

-- repl -----------------------------------------------------------------------

{--
   Implementation of a read-eval-print loop (REPL)
   Allows:
     -- evaluating lisp expressions
     -- defining global bindings with (define <variable> <expression>)
     -- loading files with defines
--}
lisp :: IO ()
lisp =
    do
    putStrLn "===== Micro Lisp ===================================="
    putStrLn "Use commands:"
    putStrLn "  <expr> - Evaluate lisp expressions "
    putStrLn "  (define <variable> <expr>) - Define global bindings "
    putStrLn "  :load <filename> - Load defines from file"
    putStrLn "  :defs  - Show global definitions"
    putStrLn "  :quit - Quit "
    putStrLn "====================================================="
    replRec []
    return ()

replRec :: [(String, LispData)] -> IO ()
replRec globals =
    do
    putStr "LISP> "
    line <- getLine
    case tokenize (map toLower line) of
        (":load" : fileName : _) ->
          do {
            fileExists <- doesFileExist fileName ;
            if fileExists then
                do {
                  fHandle <- openFile fileName ReadMode ;
                  source <- hGetContents fHandle ;
                  case execDefines globals (tokenize source) of
                          Just extGlobals ->
                            do {
                              putStrLn "DEFINED: " ;
                              printOutGlobals (reverse (extGlobals \\ globals)) ;
                              hClose fHandle ;
                              replRec extGlobals
                            }
                          Nothing ->
                              do {
                                putStrLn ("INVALID FILE LOAD: " ++ fileName) ;
                                hClose fHandle ;
                                replRec globals
                              }
                }
            else
              do {
                    putStrLn ("INVALID FILE NAME: " ++ fileName) ;
                    replRec globals
                  }
          }

        (":load" : _) ->
            do {
              putStrLn ("INVALID LOAD COMMAND: " ++ line) ;
              replRec globals
            }
        (":defs" : _) ->
            do {
              printOutGlobals (reverse globals) ;
              replRec globals
            }

        (":quit" : _) ->
            do {
              putStrLn "Thanks for using Micro Lisp" ;
              return ()
            }

        ((':':_) : _) ->
            do {
              putStrLn ("INVALID COMMAND: " ++ line) ;
              replRec globals
            }

        tokens ->
            do
              let expr = parse parseLispData tokens
              putStrLn ("PARSED: " ++ show expr)
              case expr of
                  (Success (P (S "define") (P (S name) (P xpr Nil))), _) ->
                     case eval globals xpr of
                        Just val ->
                           do
                           let extGlobals = (name, val):globals
                           putStrLn ("BINDING: " ++ show (name, val))
                           replRec extGlobals
                        Nothing ->
                           do
                           putStrLn ("ERROR EVAL: " ++ show xpr)
                           replRec globals
                  (Success (P (S "define") _), _) ->
                     do
                     putStrLn ("INVALID DEFINE: " ++ line)
                     replRec globals
                  (Success xpr, _) ->
                     do
                     let mbVal = eval globals xpr
                     print (if mbVal == Nothing then "ERROR EVAL: " ++ show xpr else show mbVal)
                     replRec globals
                  _ ->
                     do
                     putStrLn ("INVALID EXPRESSION - NO PARSE: " ++ line)
                     replRec globals

execDefines :: [(String, LispData)] -> [String] -> Maybe [(String, LispData)]
execDefines globals tokens =
  case (parse (multiple parseLispData) tokens) of
    (Success defs, _) -> Just ((createBds defs) ++ globals)
    _                 -> Just globals
  where
    createBds [] = []
    createBds (d:ds) =
      case d of
        (P (S "define") (P (S name) (P xpr Nil))) ->
          case eval globals xpr of
            Just val -> (name, val):(createBds ds)
            Nothing  -> (createBds ds)
        _  -> (createBds ds)

printOutGlobals :: [(String, LispData)] -> IO ()
printOutGlobals [] = return ()
printOutGlobals (global:rest) =
        do
        print global
        printOutGlobals rest


