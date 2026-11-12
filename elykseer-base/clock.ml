(* the clock for the retry scheduler in theories/Distribution.v;
   steady, so a change of the system time cannot stretch a sleep *)

let now_ms () = Mlcpp_chrono.Chrono.Clock.Steady.(to_ns (now ())) / 1_000_000

let sleep_until due =
  let now = now_ms () in
  if due > now then Unix.sleepf (float_of_int (due - now) /. 1000.);
  now_ms ()
