let x = %eval
let node = %raw("0")
let node = %raw(@attr "0")
let node = %raw(@attrStructureLvl (@attrOnExpr "0"))

let node = %raw(@attrStructureLvl @attrStructureLvl2 @attrStructureLvl3 (@attrOnExpr "0"))
let node = %raw(
  @attrStructureLvl @attrStructureLvl2 @attrStructureLvl3
  (
    @attrOnExpr @attrOnExpr2 @attrOnExpr3 @attrOnExpr4 @attrOnExpr5
    "unsafe raw js"
  )
)
let f: (int, int) => int = %raw((a, b) => "{return a + b}");

let f = (x, y) => {
  %debugger
  x + y
}

switch %external(__DEV__) {
| Some(_) => Js.log("dev mode")
| None => Js.log("production mode")
}

let node = @attr %raw("0")
