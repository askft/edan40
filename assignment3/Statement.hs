module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement = Assignment String Expr.T
               | Skip
               | Begin [Statement]
               | If Expr.T Statement Statement
               | While Expr.T Statement
               | Read String
               | Write Expr.T
               | Comment String
               deriving Show

-- old
--assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
--buildAss (v, e) = Assignment v e

bldAssign    (v, e) = Assignment v e
bldSkip           _ = Skip
bldBegin         ss = Begin ss
bldIf ((e, s1), s2) = If e s1 s2
bldWhile     (e, s) = While e s
bldRead           v = Read v
bldWrite          e = Write e
bldComment        s = Comment s

smtAssign = word #- accept ":=" # Expr.parse #- require ";"      >-> bldAssign
smtSkip   = accept "skip"   # require ";"                        >-> bldSkip
smtBegin  = accept "begin" -# iter parse #- require "end"        >-> bldBegin
smtIf     = accept "if"    -# Expr.parse # require "then" -# parse #
                              require "else" -# parse            >-> bldIf
smtWhile  = accept "while" -# Expr.parse #- require "do" # parse >-> bldWhile
smtRead   = accept "read"  -# word #- require ";"                >-> bldRead
smtWrite  = accept "write" -# Expr.parse #- require ";"          >-> bldWrite
smtComment = accept "--"   -# line #- require "\n"               >-> bldComment

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment v e : ss)
      d i = exec ss (Dictionary.insert (v, Expr.value e d) d) i
exec (Skip : ss) d i = exec ss d i
exec (Begin s : ss) d i = exec (s ++ ss) d i
exec (If e s1 s2 : ss) d i =
    if   Expr.value e d > 0 
    then exec (s1 : ss) d i
    else exec (s2 : ss) d i
exec (While e s : ss) d i =
    if   Expr.value e d > 0
    then exec (s : (While e s) : ss) d i
    else exec ss d i
exec (Read v : ss) d i = exec ss (Dictionary.insert (v, head i) d) (tail i)
exec (Write e : ss) d i = Expr.value e d : exec ss d i
exec (Comment _ : ss) d i = exec ss d i

indent n = replicate n '\t'

shw :: Int -> Statement -> String
shw n (Assignment v e) = indent n ++ v ++ " := " ++ Expr.toString e ++ ";\n"
shw n Skip             = indent n ++ "skip;\n"
shw n (Begin ss)       = indent n ++ "begin\n" ++ concatMap (shw (n + 1)) ss ++
                         indent n ++ "end\n"
shw n (If e s1 s2)     = indent n ++ "if " ++ Expr.toString e ++ " then\n" ++
                         shw (n + 1) s1 ++ indent n ++ "else\n" ++
                         shw (n + 1) s2
shw n (While e s)      = indent n ++ "while " ++ Expr.toString e ++ " do\n" ++
                         shw (n + 1) s
shw n (Read v)         = indent n ++ "read " ++ v ++ ";\n"
shw n (Write e)        = indent n ++ "write " ++ Expr.toString e ++ ";\n"
shw n (Comment s)      = indent n ++ "-- " ++ s ++ "\n"

instance Parse Statement where
  parse = smtAssign ! smtSkip ! smtBegin ! smtIf
        ! smtWhile ! smtRead ! smtWrite ! smtComment
  toString = shw 0

