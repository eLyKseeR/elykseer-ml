open Elykseer__Lxr

let counting (t : Tracer.tracer) =
  let n = ref 0 in
  let count f = fun m -> incr n; f m in
  ({ t with logWarning = count t.logWarning; logError = count t.logError },
   fun () -> !n)
