let arm () =
  let s =
    Option.fold ~none:120 ~some:int_of_string
      (Sys.getenv_opt "MOONPOOL_TEST_TIMEOUT")
  in
  ignore (Unix.alarm s : int)
