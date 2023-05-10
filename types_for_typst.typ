#let t_assert(ty, val) = {
  let err = (ty.check)(val)
  if err.len() > 0 {
    panic("Value does not match '" + ty.name + "'", err, val)
  }
}

#let t_check(ty, val) = {
  let err = (ty.check)(val)
  err.len() == 0
}

#let t_require(ty, val) = {
  let err = (ty.check)(val)
  if err.len() > 0 {
    panic("Value does not match '" + ty.name + "'", err, val)
  }
  val
}

#let t_type_def(t_name, t_check) = ( 
  name: t_name,
  check: value => if not t_check(value) {
    ("Value does not match '" + t_name + "'",)
  } else {
    ()
  }
)
#let _primitive_def(primitive_name) = t_type_def(primitive_name, v => type(v) == primitive_name)

#let _surround(str) = if str.starts-with("(") and str.ends-with(")") { 
  str
} else {
  "(" + str + ")"
}

#let t_or(..tys) = {
  let n = _surround(tys.pos().map(t => t.name).join(" | "))
  (
    name: n,
    check: value => {
      let res = tys.pos().fold((check: false, reason: ("Value does not match '" + n + "'",)), (acc, ty) => {
        let err = (ty.check)(value)
        (check: acc.check or err.len() == 0, reason: acc.reason + err.map(r => " -> " + r))
      })
      if not res.check {
        res.reason
      } else {
        ()
      }
    }
  )
}

#let TAny            = t_type_def("any", _v => true)

#let TBoolean        = _primitive_def("boolean")
#let TInteger        = _primitive_def("integer")
#let TFloat          = _primitive_def("float")
#let TLength         = _primitive_def("length")
#let TAngle          = _primitive_def("angle")
#let TRatio          = _primitive_def("ratio")
#let TRelativeLength = _primitive_def("relative length")
#let TFraction       = _primitive_def("fraction")
#let TColor          = _primitive_def("boolean")
#let TSymbol         = _primitive_def("symbol")
#let TString         = _primitive_def("string")
#let TContent        = _primitive_def("content")
#let TFunction       = _primitive_def("function")

#let TNumber   = t_or(TInteger, TFloat)
#let TLit(lit) = t_type_def("literal(" + str(lit) + ")", v => v == lit)
#let TNone     = t_type_def("none", v => v == none)


#let TArray(values) = (
  name: "array" + _surround(values.name),
  check: value => {
    if type(value) != "array" {
      return ("Value is no array",)
    }
    let errs = value.enumerate()
      .map(e => (e.at(0), (values.check)(e.at(1))))
      .filter(e => e.at(1).len() > 0)
      .map(e => ("Value at index " + str(e.at(0)) + " does not match '" + values.name + "'",) + e.at(1).map(err => " -> " + err))
    if errs.len() == 0 {
      ()
    } else {
      errs.join()
    }
  }
)

#let optional(ty) = (ty: ty, optional: true)

#let TDict(..fields) = {
  let fields = fields.named().pairs().map(f => {let d = (:); d.insert(f.at(0), if type(f.at(1)) == "dictionary" and f.at(1).len() == 2 and "ty" in f.at(1) and "optional" in f.at(1) {f.at(1)} else {(ty: f.at(1), optional: false)}); d}).join()
  let n = "dict(" + fields.pairs().map(entry => entry.at(0) + if entry.at(1).optional {"?"} + ": " + entry.at(1).ty.name).join(", ") + ")"
  (
    name: n,
    check: value => {
      if type(value) != "dictionary" {
        return ("Value is not a dictionary",)
      }

      // check missing and wrong fields
      let errs = fields.pairs().map(e => {
        let name = e.at(0)
        let e = e.at(1)
        if e.optional and not name in value {
          (name: name, errs: ())
        } else if not name in value { 
          (name: name, errs: ("Missing required field '" + name + "' in dict with type '" + e.ty.name + "'",)) 
        } else {
          (
            name: name,
            errs: (e.ty.check)(value.at(name)),
            ty_name: e.ty.name
          )
        }
      })
      .filter(e => e.errs.len() > 0)
      .map(e => if e.len() == 3 {
        ("Value at key '" + e.name + "' does not match '" + e.ty_name + "'",) + e.errs.map(err => " -> " + err)
      } else {
        e.errs
      })
      
      // check extra fields
      let extra_err = value.keys().map(k => if not k in fields {"Value has extra field '" + k + "' in dict"}).filter(e => e != none)

      if errs.len() > 0 {
        errs.join() + extra_err
      } else {
        extra_err
      }
    }
  )
}

#let TTuple(..elems) = {
  let n = "tuple" + _surround(elems.pos().map(t => t.name).join(", "))
  (
    name: n,
    check: value => {
      if type(value) != "array" {
        return ("Value is not a tuple",)
      }
      let elems = elems.pos()
      let errs = elems.enumerate().map(e => {
        if e.at(0) >= value.len() {
          (e.at(0), ("Missing element at index " + str(e.at(0)) + " in tuple with type '" + e.at(1).name + "'",)) 
        } else {
          let v = value.at(e.at(0))
          (
            e.at(0),
            (e.at(1).check)(v),
            e.at(1).name
          )
        }
      })
      .filter(e => e.at(1).len() > 0)
      .map(e => if e.len() == 3 {
        ("Value at index " + str(e.at(0)) + " does not match '" + e.at(2) + "'",) + e.at(1).map(err => " -> " + err)
      } else {
        e.at(1)
      })

      let extra_err = if value.len() > elems.len() { ("Value has " + str(value.len() - elems.len()) + " extra elements in tuple",) } else {()}
      
      if errs.len() > 0 {
        errs.join() + extra_err
      } else {
        extra_err
      }
    }
  )
}

#let TMap(values) = {
  let n = "map" + _surround(values.name)
  (
    name: n,
    check: value => {
      if type(value) != "dictionary" {
        return ("Value is not a map",)
      }

      let errs = value.pairs()
        .map(e => (e.at(0), (values.check)(e.at(1))))
        .filter(e => e.at(1).len() > 0)
        .map(e => ("Value at key '" + e.at(0) + "' does not match '" + values.name + "'",) + e.at(1).map(e => " -> " + e))
      
      if errs.len() > 0 {
        errs.join()
      } else {
        ()
      }
    }
  )
}

