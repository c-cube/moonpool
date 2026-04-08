let copy_file src dst =
  let ic = open_in src in
  let oc = open_out dst in
  let buf = Bytes.create 1024 in
  (try
     while true do
       let n = input ic buf 0 (Bytes.length buf) in
       if n = 0 then raise End_of_file;
       output oc buf 0 n
     done
   with End_of_file -> ());
  close_in ic;
  close_out oc

let () =
  let version = Sys.argv.(1) in
  let major =
    try Scanf.sscanf version "%d.%s" (fun maj _ -> maj) with _ -> 0
  in
  if major >= 6 then
    copy_file "types_.ml.6" "types_.ml"
  else
    copy_file "types_.ml.5" "types_.ml"
