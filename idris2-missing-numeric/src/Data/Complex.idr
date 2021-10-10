||| Complex number.
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Complex

import Generics.Derive
%language ElabReflection

-- --------------------------------------------------------------------------

public export
record Complex t where
  constructor MkComplex
  real : t
  imag : t


export
Show t => Show (Complex t) where
  show x = show x.real ++ " + " ++ show x.imag ++ "i"

export
Functor Complex where
  map f x = MkComplex (f x.real) (f x.imag)

%runElab derive "Complex" [Generic, Meta, Eq]

export
(Neg t, Num t) => Num (Complex t) where
  x + y = MkComplex (x.real + y.real) (x.imag + y.imag)
  x * y = MkComplex (x.real * y.real - x.imag * y.imag)
                    (x.real * y.imag + x.imag * y.real)
  fromInteger x = MkComplex (fromInteger x) (fromInteger 0)


export
Neg t => Neg (Complex t) where
  negate x = MkComplex (negate x.real) (negate x.imag)
  x - y    = MkComplex (x.real - y.real) (x.imag - y.imag)


export
isReal : (Num t, Eq t) => Complex t -> Bool
isReal x = x.imag == fromInteger 0


export
isImage : (Num t, Eq t) => Complex t -> Bool



export
plus_commutative : (Num t, Neg t)
    => {x, y:Complex t}
    -> {comm_t: {a,b:t} -> (a + b) = (b + a)}
    -> (x + y) = (y + x)
plus_commutative = rewrite (comm_t {a=x.real} {b=y.real})
                in rewrite (comm_t {a=x.imag} {b=y.imag})
                in Refl


plus_assosiative : (Num t, Neg t)
    => {x, y:Complex t}
    -> {assoc_t: {a,b,c:t} -> (a + b) + c = a + (b + c)}
    -> (x + y) + z = x + (y + z)
plus_assosiative = rewrite (assoc_t {a=x.real} {b=y.real} {c=z.real})
                in rewrite (assoc_t {a=x.imag} {b=y.imag} {c=z.imag})
                in Refl



mult_commutative : (Num t, Neg t)
    => {x, y:Complex t}
    -> {mult_comm_t: {a,b:t} -> a * b = b * a}
    -> {plus_comm_t: {a,b:t} -> a + b = b + a}
    -> (x * y) = (y * x)
mult_commutative = rewrite (mult_comm_t {a=x.real} {b=y.real})
                in rewrite (mult_comm_t {a=x.imag} {b=y.imag})
                in rewrite (mult_comm_t {a=x.real} {b=y.imag})
                in rewrite (mult_comm_t {a=x.imag} {b=y.real})
                in rewrite (plus_comm_t {a=(y.imag * x.real)} {b=(y.real * x.imag)})
                in Refl


distributive : (Num t, Neg t)
    => {x, y:Complex t}
    -> {dist_t: {a,b,c:t} -> a * (b + c) = a * b + a * c}
    -> {plus_commu_t: {a,b:t} -> a + b = b + a}
    -> x * (y + z) = x * y + x * z
distributive = rewrite (dist_t {a=x.real} {b=y.real} {c=z.real})
            in rewrite (dist_t {a=x.imag} {b=y.imag} {c=z.imag})
            in rewrite (dist_t {a=x.real} {b=y.imag} {c=z.imag})
            in rewrite (dist_t {a=x.imag} {b=y.real} {c=z.real})
            in ?lhs_distributive



-- --------------------------------------------------------------------------

export
conjugate : Neg t => Complex t -> Complex t
conjugate x = MkComplex x.real (negate x.imag)


conjugate_distributive : (Num t, Neg t)
    => {x, y: Complex t}
    -> {pr: {a,b:t} -> negate (a + b) = negate a + negate b}
    -> conjugate (x + y) = conjugate x + conjugate y
conjugate_distributive = rewrite (pr {a =x.imag} {b=y.imag}) in Refl





-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
