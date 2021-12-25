||| Foreign interface to Javascript
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Foreign.Javascript.Types

import Data.Maybe

%default total

-- --------------------------------------------------------------------------

export data JSMaybe : Type -> Type where [external]
export data JSAny : Type where [external]  -- Or, shoukd I use (JSValue "") instead?

public export JSTypes : Type
JSTypes = String

public export %inline
jsTyUndefined, jsTyBoolean, jsTyNumber, jsTyBigInt, jsTyString, jsTySymbol, jsTyFunction, jsTyObject : JSTypes
jsTyUndefined = "undefifned"
jsTyBoolean   = "boolean"
jsTyNumber    = "number"
jsTyBigInt    = "bigint"
jsTyString    = "string"
jsTySymbol    = "symbol"
jsTyFunction  = "function"
jsTyObject    = "object"

export data JSVal : JSTypes -> Type where [external]
public export %inline
JSUndefined, JSBoolean, JSNumber, JSBigInt, JSString, JSSymbol, JSFunction, JSMaybeObject : Type
JSUndefined = JSVal jsTyUndefined
JSBoolean   = JSVal jsTyBoolean
JSNumber    = JSVal jsTyNumber
JSBigInt    = JSVal jsTyBigInt
JSString    = JSVal jsTyString
JSSymbol    = JSVal jsTySymbol
JSFunction  = JSVal jsTyFunction
JSMaybeObject = JSVal jsTyObject -- null or valid object


export data JSObj : String -> Type where [external]



public export %inline
Cast a JSAny => Cast a (JSMaybe JSAny) where
  cast x = believe_me $ the JSAny $ cast x

public export %inline Cast (JSVal a) (JSMaybe (JSVal a)) where cast = believe_me
public export %inline Cast (JSVal a) JSAny where cast = believe_me

public export %foreign "javascript:lambda:x => x?1:0"
jsUnsafeBooleanToInt : (0 _:Cast a (JSMaybe JSAny)) => a -> Int

public export %inline
Cast JSBoolean Bool where
  cast x with (jsUnsafeBooleanToInt x)
    _ | 0 = False
    _ | _ = True


-- --------------------------------------------------------------------------
namespace SpecialValue

  %foreign "javascript:lambda:_ => undefined"
  %inline unsafeJsUndefined : {0 ty:Type} -> ty


  %foreign "javascript:lambda:_ => null"
  %inline unsafeJsNull : (0 ty:Type) -> ty

  %foreign "javascript:lambda:_ => true"
  %inline unsafeJsTrue : (0 ty:Type) => ty
  %foreign "javascript:lambda:_ => false"
  %inline unsafeHsFalse : (0 ty:Type) => ty


  namespace JSAnyValue

    public export
    jsNull : JSAny
    jsNull = unsafeJsNull {ty=JSAny}

    export %inline jsTrue : () => JSAny
    export %inline jsFalse : () => JSAny


  namespace JSMaybe

    export jsUndefined : JSMaybe JSAny
    jsUndefined = unsafeJsUndefined

    %foreign "javascript:lambda:x => ((x === undefined)?1:0)"
    prim__jsIsUndefined : JSMaybe JSAny -> Int

    export jsIsUndefined : JSMaybe ty -> Bool
    jsIsUndefined x = if prim__jsIsUndefined (believe_me x) == 0 then False else True

    public export %inline jsNull, jsTrue, jsFalse : JSMaybe JSAny
    jsNull = believe_me $ the JSAny jsNull
    jsTrue = believe_me $ the JSAny jsTrue
    jsFalse = believe_me $ the JSAny jsFalse


    %foreign "javascript:lambda:(_,x) => (x === null)"
    prim__jsIsNull : JSMaybe JSAny -> JSAny
    public export jsIsNull : Cast a (JSMaybe JSAny) => a -> JSBoolean
    jsIsNull x = believe_me $ prim__jsIsNull $ cast x



    namespace JSAnyValue
      public export %inline jsJust : JSAny -> JSMaybe JSAny
      jsJust = believe_me

    namespace JSValue
      public export %inline jsJust : JSVal t -> JSMaybe (JSVal t)
      jsJust = believe_me


  namespace JSVal
    public export jsUndefined : JSUndefined
    jsUndefined = believe_me $ the (JSMaybe JSAny) jsUndefined

    public export jsNull : JSMaybeObject
    jsNull = believe_me $ the JSAny jsNull



    public export jsTrue, jsFalse : JSBoolean
    jsTrue = believe_me $ the JSAny jsTrue
    jsFalse = believe_me $ the JSAny jsFalse




-- --------------------------------------------------------------------------

%foreign "javascript:lambda:(_, x) => x"
public export %inline unsafeToJSAny : {0 ty:Type} -> ty -> JSAny

public export %inline Cast Int JSAny where cast = unsafeToJSAny
public export %inline Cast Integer JSAny where cast = unsafeToJSAny
public export %inline Cast Double JSAny where cast = unsafeToJSAny
public export %inline Cast String JSAny where cast = unsafeToJSAny
public export %inline
Cast Bool JSAny where
  cast True = jsTrue
  cast False = jsFalse

public export %inline FromDouble JSAny where fromDouble = unsafeToJSAny
public export %inline FromString JSAny where fromString = unsafeToJSAny


public export %inline Cast Bool JSBoolean where cast = believe_me . unsafeToJSAny

public export %inline FromDouble JSNumber where fromDouble = believe_me .  unsafeToJSAny
public export %inline FromString JSString where fromString = believe_me .  unsafeToJSAny


{-
  you can just use (maybe jsAny cast) instead.
public export %inline Cast (Maybe JSAny) (JSMaybe JSAny) where
public export %inline Cast (Maybe (JSVal ty) (JSMaybe (JSVal ty)) where
-}
-- From JSMaybe
public export %inline
Cast (JSMaybe ty) (Maybe ty) where
  cast x =
    if jsIsUndefined x
      then Nothing
      else Just (believe_me x)


-- --------------------------------------------------------------------------

%foreign "javascript:lambda:(x, y) => (x == y)"
prim_jsEqual : (0 _:Cast a (JSMaybe JSAny)) => a -> a -> JSAny
%foreign "javascript:lambda:(x, y) => (x === y)"
prim_jsSame : (0 _:Cast a (JSMaybe JSAny)) => a -> a -> JSAny

public export %inline
jsEqual, jsSame: (0 _:Cast a (JSMaybe JSAny)) => a -> a -> JSBoolean
jsEqual x y = believe_me $ prim_jsEqual x y
jsSame x y = believe_me $ prim_jsSame x y




%foreign "javascript:lambda:(x, y) => (x === y)?1:0"
prim__jsEq : JSMaybe JSAny -> JSMaybe JSAny -> Int

public export Eq (JSMaybe a) where (==) x y = if prim__jsEq (believe_me x) (believe_me y) == 0 then False else True
public export Eq JSAny       where (==) x y = if prim__jsEq (believe_me x) (believe_me y) == 0 then False else True
public export Eq (JSVal ty)  where (==) x y = if prim__jsEq (believe_me x) (believe_me y) == 0 then False else True
public export Eq (JSObj ty)  where (==) x y = if prim__jsEq (believe_me x) (believe_me y) == 0 then False else True



-- --------------------------------------------------------------------------
-- Numeric operations
--

%foreign "javascript:lambda:(x,y) => x + y"
prim__jsPlus : JSAny -> JSAny -> JSAny
%foreign "javascript:lambda:(x,y) => x * y"
prim__jsMult : JSAny -> JSAny -> JSAny

public export
Num JSNumber where
  x + y = believe_me $ prim__jsPlus (believe_me x) (believe_me y)
  x * y = believe_me $ prim__jsMult (believe_me x) (believe_me y)
  fromInteger = believe_me . unsafeToJSAny

public export
Num JSBigInt where
  x + y = believe_me $ prim__jsPlus (believe_me x) (believe_me y)
  x * y = believe_me $ prim__jsMult (believe_me x) (believe_me y)
  fromInteger = believe_me . unsafeToJSAny


-- --------------------------------------------------------------------------
--

%foreign "javascript:lambda:(_, _, x) => typeof x"
public export %inline jsTypeOf : (0 _:Cast ty (JSMaybe JSAny)) => ty -> JSTypes


public export
toJSVal : Cast a (JSMaybe JSAny) => a -> (ty:JSTypes ** JSVal ty)
toJSVal x = (jsTypeOf x ** believe_me x)




-- --------------------------------------------------------------------------
--
--

public export %inline
JSObject, JSArray, JSMap: Type
JSObject = JSObj "Object"
JSArray  = JSObj "Array"
JSMap    = JSObj "Map"


export
toJSObject : Cast a (JSMaybe JSAny) => a -> (JSObject -> Maybe b) -> Maybe b
toJSObject x f with (toJSVal x)
  _ | ("object" ** x') =
    if jsIsNull x == jsTrue
       then f $ believe_me x'
       else Nothing
  _ | _ = Nothing


%foreign "javascript:lambda:(x,y) => y[x]"
prim_jsProp : String -> JSAny -> JSMaybe JSAny

export
jsProp : String -> JSObj x -> JSMaybe JSAny
jsProp x y = prim_jsProp x (believe_me y)







-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
