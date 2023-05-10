# types-for-typst
A small library for building and checking types in typst.

# Documention

## Check function
- `t_assert(type, value)` panics if the given value is not of the specified type. Returns nothing.
- `t_check(type, value)` checks if the given value if of the specified type and returns `true` if it is
- `t_require(type, value)` panics if the given value is not of the specified type. Returns the value if no panic.

## Primitive Types
- `TBoolean` primtive type
- `TInteger` primtive type
- `TFloat` primtive type
- `TLength` primtive type
- `TAngle` primtive type
- `TRatio` primtive type
- `TRelativeLength` primtive type
- `TFraction` primtive type
- `TColor` primtive type
- `TSymbol` primtive type
- `TString` primtive type
- `TContent` primtive type
- `TFunction` primtive type

## Other predifined Types
- `TAny` allows everything
- `TNone` none literal
- `TNumber` or combination of `TInteger` and `TFloat`
- `TLit(lit)` literal type. Allowes only the given literal as type.
- `TMap(values)` generic map from string to `value`
- `TDict(..fields)` generic dictionary with the given fields. Only named fields are allowed
- `TArray(values)` generic array with zero or more values of one type
- `TTuple(..elemn)` generic tuple of exact size. Only positional fields are allowed

## Other functions
- `t_or(..types)` constructs a union type of all given types
- `optional(type)` used to make a dictionary field optional

# Example
```typ
#import "types_for_typst.typ": *

#t_assert(TInteger, 7)
#t_assert(TNumber, 4.2)
#t_assert(TArray(TString), ())
#t_assert(TArray(TString), ("Test",))

#t_check(TArray(TNumber), (4.7, 1, "Test"))
#t_check(TArray(TString), ("Test", 8))
#t_check(TDict(test: TNumber, name: TString, sym: optional(TSymbol)), (test: "d", name: [OK], extra: 42))

#t_assert(TTuple(TNumber, TString, TDict(r: TRatio, num: TNumber)), (42, "LUL", (r: 50%, num: 8)))
#t_check(TMap(t_or(TString, TContent)), (l: "OK", b: [sdf], c: 8))
#t_assert(t_or(TLit("mon"), TLit("two"), TLit("three"), TLit(42)), 42)
```

