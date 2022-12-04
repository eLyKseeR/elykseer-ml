(* seen on https://thealmarty.com/2019/01/02/using-unfolds-to-zip-and-more-in-ocaml/ 
   also mentions the banana and *isms paper: https://maartenfokkinga.github.io/utwente/mmf91m.pdf
   which is definitely worth an hour of our lifes
*)

(* Define the unfold function.*)
let rec unfold pred gen b1 b2 =
  if pred b1 b2 then [] else
    (match gen b1 b2 with (a, (b1', b2')) ->
       a :: unfold pred gen b1' b2')

(* Define the zip function.*)
let zip _as _bs = unfold
  (* Define pred *)
  (fun x y -> ((List.length x = 0) || (List.length y = 0))) 
  (* Define gen *)
  (fun x y -> ( (List.hd x, List.hd y), (List.tl x, List.tl y)))
  _as _bs
