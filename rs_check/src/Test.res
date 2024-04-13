// Importing and using IPurs from PursModule.res
open Types

let usePurs = () => {
  let pursInstance = IPurs.AnonymousInterface0.make(~name="John Doe", ~org="ReScript Association", ~rank=1.0)

  let name = IPurs.AnonymousInterface0.get_name(pursInstance)
  let org = IPurs.AnonymousInterface0.get_org(pursInstance)
  let rank = IPurs.AnonymousInterface0.get_rank(pursInstance)

  Js.log("Purs instance:")
  Js.log("Name: " ++ name)
  Js.log("Org: " ++ org)
  Js.log("Rank: " ++ Js.Float.toString(rank))
}
