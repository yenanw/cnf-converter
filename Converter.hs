{-# LANGUAGE LambdaCase #-}

module Converter where

import qualified Formula.Abs                       as F
import           Formula.Par                       (myLexer, pFormula)
import           System.Directory.Internal.Prelude (exitFailure)

data Formula = And Formula Formula
             | Or Formula Formula
             | Imply Formula Formula
             | Neg Formula
             | Lit String
  deriving (Eq)

instance Show Formula where
  show :: Formula -> String
  show = formulaStr

{-
  Assuming the precendence of:
    1. Negation
    2. And
    3. Or
    4. Implication (though we add extra parenthesis in this case to prevent any ambiguities)
-}
formulaStr :: Formula -> String
formulaStr = \case
  Lit s         -> s
  p@(Neg f)     -> "~" ++ formulaStr' p f
  p@(And f g)   -> formulaStr' p f ++ " & " ++ formulaStr' p g
  p@(Or f g)    -> formulaStr' p f ++ " | " ++ formulaStr' p g
  p@(Imply f g) -> formulaStr' p f ++ " -> " ++ formulaStr' p g
  where
    formulaStr' :: Formula -> Formula -> String
    formulaStr' _ (Lit s)              = s
    formulaStr' (Neg _) f@(Neg _)      = formulaStr f
    formulaStr' (Neg _) f              = formulaStr'' f
    formulaStr' (And _ _) f@(Neg _)    = formulaStr f
    formulaStr' (And _ _) f@(And _ _)  = formulaStr f
    formulaStr' (And _ _) f            = formulaStr'' f
    formulaStr' (Or _ _) f@(Imply _ _) = formulaStr'' f
    formulaStr' (Or _ _) f             = formulaStr f
    formulaStr' (Imply _ _) f@(Neg _)  = formulaStr f
    formulaStr' (Imply _ _) f          = formulaStr'' f
    formulaStr' (Lit _) _              = error "Literals cannot nest formula."

    formulaStr'' :: Formula -> String
    formulaStr'' f = "(" ++ formulaStr f ++ ")"


toFormula :: F.Formula -> Formula
toFormula (F.FExp f) = toFormula' f
  where
    toFormula' = \case
      F.ELit (F.LNeg (F.Proposition p))  -> Neg (Lit p)
      F.ELit (F.LChar (F.Proposition p)) -> Lit p
      F.ENeg f                           -> Neg (toFormula' f)
      F.EAnd f g                         -> And (toFormula' f) (toFormula' g)
      F.EOr f g                          -> Or (toFormula' f) (toFormula' g)
      F.EImply f g                       -> Imply (toFormula' f) (toFormula' g)

-- Convert a propositional logic formula to its negation normal form
toNnf :: Formula -> Formula
toNnf = \case
  And f g         -> And (toNnf f) (toNnf g)
  Or f g          -> Or (toNnf f) (toNnf g)
  Imply f g       -> toNnf (Or (Neg f) g)        -- P -> Q = ~P | Q
  f@(Neg (Lit _)) -> f
  Neg (And f g)   -> toNnf (Or (Neg f) (Neg g))  -- De Morgan's
  Neg (Or f g)    -> toNnf (And (Neg f) (Neg g)) -- De Morgan's
  Neg (Neg f)     -> toNnf f                     -- double negation elimination
  Neg f           -> toNnf $ Neg (toNnf f)
  formula         -> formula


toCnf :: Formula -> Formula
toCnf f = toCnf' (toNnf f)
  where
    toCnf' :: Formula -> Formula
    toCnf' = \case
      (Or (And f g) h) -> toCnf' $ And (toCnf' $ Or f h) (toCnf' $ Or g h) -- distribute ORs
      (Or h (And f g)) -> toCnf' $ And (toCnf' $ Or h f) (toCnf' $ Or h g)
      (And f g)        -> And (toCnf' f) (toCnf' g)
      formula          -> formula


convert :: String -> IO()
convert s = do
  case pFormula (myLexer s) of
    Left err -> do
      putStrLn err
      exitFailure
    Right f -> do
      print (toCnf $ toFormula f)


