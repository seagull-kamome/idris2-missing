||| Rational number - Ported from Haskell base
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Data.Rational

import Data.So
import Data.Maybe

import Data.Integral.Gcd

%default total

-- --------------------------------------------------------------------------

export
record Rational where
  constructor MkRational
  num : Integer
  den : Integer  -- FIXME: denominator is always positive or zero
  {auto denIsPositive : So (den >= 0)}


-- --------------------------------------------------------------------------


export numerator : Rational -> Integer
numerator x = x.num
export denominator : Rational -> Integer
denominator x = x.den

export infinity : Rational
infinity = MkRational 1 0

export notANumber : Rational
notANumber = MkRational 0 0


export reduce : (num:Integer) -> (den:Integer) -> {auto ok:So (den >= 0)} -> Rational
reduce _ 0 = notANumber
reduce x y = let
  d = gcd x y
  in MkRational {denIsPositive=believe_me Oh} (x `div` d) (y `div` d)

infixr 9 %:

export
(%:) : (num:Integer) -> (den:Integer) -> Rational
(%:) num den with (choose (den >= 0))
  (%:) num den | Left _ = reduce num den
  (%:) num den | Right _ = reduce {ok=believe_me Oh} (negate num) (negate den) -- FIXME

-- --------------------------------------------------------------------------

public export Show Rational where show x = show x.num ++ " %: " ++ show x.den

public export Eq Rational where x == y = x.num == y.num && x.den == y.den
public export
Ord Rational where
  compare x y = let x' = x.num * y.den
                    y' = y.num * x.den
                  in compare x' y'

public export Cast Integer Rational where cast x = MkRational x 1
Cast ty Integer => Cast ty Rational where cast x = MkRational (cast x) 1


public export
Num Rational where
  x + y with (choose $ x.den * y.den >= 0)
    x + y | Left _ = reduce (x.num * y.den + y.num * x.den) (x.den * y.den)
    x + y | Right _ = ?lhs_rational_add_right
  x * y with (choose $ x.den * y.den >= 0)
    x * y | Left _ = reduce (x.num * y.num) (x.den * y.den)
    x * y | Right _ = ?lhs_rational_mult_right
  fromInteger x = MkRational x 1

public export
Neg Rational where
  negate x = MkRational {denIsPositive=x.denIsPositive} (negate x.num) x.den
  x - y with (choose $ x.den * y.den >= 0)
    x - y | Left _ = reduce (x.num * y.den - x.num * x.den) (x.den * y.den)
    x - y | Right _ = notANumber

public export
Abs Rational where
  abs x = MkRational {denIsPositive=x.denIsPositive} (abs x.num) x.den
--     signum x = MkRational (signum x.num) 1


public export
Fractional Rational where
  x / y with (choose $ y.num * x.den /= 0)
    x / y | Left _ = (x.num * y.den) %: (y.num * x.den)
    x / y | Right _ = notANumber
  recip x with (choose $ x.num >= 0)
    recip x | Left _ = MkRational x.den x.num
    recip x | Right _ with (choose $ negate x.num >= 0)
      recip x | Right _ | Left _ = MkRational (negate x.den) (negate x.num)
      recip x | Right _ | Right _ = ?lhs_rational_recip_impossible


-- --------------------------------------------------------------------------

export floor : Rational -> Maybe Integer
floor x = toMaybe (x.den /= 0) $ x.num `div` x.den



