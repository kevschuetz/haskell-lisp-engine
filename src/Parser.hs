{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Data.Char
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

data Result a = Success a | Failure String
  deriving Show
{-- 
  Data type definition for parser functions. 
  Retrieve the parser function by accessor parse. 
--}
data Parser a = Parser { parse :: ([String] -> (Result a, [String])) }

-- Functor and Monad impl -----------------------------------------------------

{-- Implementation of Functor type class for Parser --}
instance Functor Parser where 
  fmap = liftM
  
instance Applicative Parser where
  pure x = Parser { parse = (\tkns -> (Success x, tkns)) }
  (<*>) = ap
  
{-- Implementation of Monad type class for Parser. 
    The bind operator (>>=) is used ot abort parsing the first parse failed.--}
instance Monad Parser where 
    return = pure
    p >>= f = 
        Parser { parse = (\tkns -> 
                              case parse p tkns of 
                                  (Failure s, _)    -> (Failure s, tkns)
                                  (Success a, rest1) -> (parse (f a)) rest1) }

{-- Parser of any token. --}   
anyToken :: Parser String
anyToken = 
   Parser { parse = 
              (\tkns ->
              case tkns of 
                  (tkn:rest) -> (Success tkn, rest)
                  []         -> (Failure "empty input", [])) }

-- mapping and filtering ------------------------------------------------------
           
{-- Filtering a parser by making it fail if test is not successful --}
filterBy :: Parser a -> (a -> Bool) -> Parser a
filterBy p pred = 
    Parser (\tkns -> 
           case parse p tkns of 
              (Failure s, rest) -> (Failure s, tkns) 
              (Success x, rest)  -> if pred x then (Success x, rest)
                               else (Failure ("Filter failed "), tkns))
              
{-- Parser for a specific token tkn --}                               
token :: String -> Parser String
token tkn =  anyToken `filterBy` (== tkn) 

{-- Parser for opening bracket token --}
open :: Parser String
open = token "("

{-- Parser for closing bracket token --}
close :: Parser String
close = token ")"

{-- Parser for word token, i.e., all except opening and closing brackets --}
word :: Parser String    
word = anyToken `filterBy` isWord
            where isWord tkn = tkn /= "(" && tkn /= ")" 


-- further combinators --------------------------------------------------------

-- multiple -----------

{-- Parser for accepting 0 to n parse of parser p --}
multiple :: Show a => Parser a -> Parser [a]
multiple p = 
    Parser (\tkns -> 
               let 
                  r@(mbP, pRest) = parse p tkns 
               in 
                  case mbP of 
                      Failure m  -> (Success [], pRest) 
                      Success pR   -> 
                         case parse (multiple p) pRest of 
                            (Success results, rest)  -> (Success (pR:results), rest)
                            (Failure m, rest)       -> (Failure m, rest))


-- orElse ------------

{-- Parser for accepting either a parse of parser p or parser q --}
orElse :: Parser a -> Parser a -> Parser a
orElse p q = 
    Parser (\tkns -> 
                let 
                   (pR, pRest) = parse p tkns
                in 
                   case pR of 
                      Failure _  -> parse q tkns
                      Success _   -> (pR, pRest))


---Scanner --------------------------------------------------------------------

skipWhiteSpaces :: String -> String
skipWhiteSpaces (c:cs) | isSpace c   = skipWhiteSpaces cs
skipWhiteSpaces cs                   = cs


{-- this is a function to split a string by either spaces or '(' or ')'
    whereas brackets will become tokens  --}
tokenize :: String -> [String]
tokenize "" = []
tokenize text =
   case nextToken text of
      ("", _)      ->  []
      (tkn, rest)  ->  tkn : (tokenize rest)
   where
      nextToken text =
           case (skipWhiteSpaces text) of
               ('(':rest)    -> ("(", rest)
               (')':rest)    -> (")", rest)
               text1         -> nextWordAndRest text1


nextWordAndRest :: String -> (String, String)
nextWordAndRest text =
    nextWordAndRest' (skipWhiteSpaces text) ""
    where
        nextWordAndRest' :: String -> String -> (String, String)
        nextWordAndRest' (s:rest)   tkn | isSpace s   = (reverse tkn, rest)
        nextWordAndRest' rest@('(':_) tkn             = (reverse tkn, rest)
        nextWordAndRest' rest@(')':_) tkn             = (reverse tkn, rest)
        nextWordAndRest' []         tkn               = (reverse tkn, [])
        nextWordAndRest' (c:rest)   tkn               = nextWordAndRest' rest (c:tkn)

-------------------------------------------------------------------------------
                      
