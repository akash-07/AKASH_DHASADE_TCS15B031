--Expr is Data type for Core language Expressions
data Expr a = EVar Name 
			| ENum Int 
			| EConstr Int Int
			| EAp (Expr a) (Expr a )
			| ELet IsRec [(a,Expr a)] (Expr a)
			| ECase (Expr a) --[Alter a]
			| ELam [a] (Expr a)
			deriving Show

type Name = String

-- Core Expressions are of type Expr Name where Name is String

type CoreExpr = Expr Name

-- A Boolen variable to check whether the let is recursive or not

type IsRec = Bool
recursive :: IsRec
nonRecursive :: IsRec
recursive = True
nonRecursive = False

-- bindersOf is to get the list of all the binding variables
bindersOf :: [(a,b)] -> [a]
bindersOf defns = [name | (name,rhs) <- defns]

-- rhsOf gets the list of expressions to which variables are bound
rhssOf :: [(a,b)] -> [b]
rhssOf defns = [rhs | (name,rhs) <- defns]

--Alternatives are tuples consisiting of tag,list of var and an expr to evaluate
type Alter a = [(Int,[a],Expr a)]
type CoreAlt = Alter Name

--Boolean valued Fuction to check whether the expressions are atomic
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True 
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

-- Program is list of superCombinator definitions
type Program a = [ScDefn a]
type CoreProgram = Program Name

--ScDefn are a consist of Name, a list of bound variables and then an expr
type ScDefn a = (Name,[a],Expr a)
type CoreScDefn = ScDefn Name

-- I didn't understand what this is being used for!
preludeDefs :: CoreProgram
preludeDefs = [	("I",["x"],EVar"x"),
				("K",["x","y"],EVar "x"),
				("K1",["x","y"],EVar "y"),
				("S",["f","g","x"],EAp (EAp (EVar "f") (EVar "x"))	(EAp (EVar "g") (EVar "x"))),
				("compose",["f","g","x"],EAp (EVar "f") (EAp (EVar "g")(EVar "x"))),
				("twice",["f"],EAp (EAp (EVar "compose")(EVar "f")) (EVar "f")) ]
