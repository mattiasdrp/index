module Int63 = Optint.Int63

type int63 = Int63.t

module Unix = struct
  include Unix

  module FDM = Map.Make (struct
    type t = Unix.file_descr

    let compare = compare
  end)

  let pp_flags ppf f =
    let s =
      match f with
      | O_RDONLY -> "Read Only"
      | O_WRONLY -> "Write Only"
      | O_RDWR -> "Read Write"
      | O_CREAT -> "Create"
      | O_CLOEXEC -> "Close on exec"
      | O_EXCL -> "Fail if exist"
      | O_NONBLOCK | O_APPEND | O_TRUNC | O_NOCTTY | O_DSYNC | O_SYNC | O_RSYNC
      | O_SHARE_DELETE | O_KEEPEXEC ->
          assert false
    in
    Format.fprintf ppf "%s" s

  let pp_sep ppf () = Format.fprintf ppf ""

  let pp_dir ppf dir =
    let ls = "ls " ^ dir ^ " > tmp_ls" in
    (match Unix.system ls with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED _ -> failwith "ls failed"
    | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
        failwith "`ls` command was interrupted");
    let lines = ref [] in
    let ic = open_in "tmp_ls" in
    let lines =
      (try
         while true do
           lines := input_line ic :: !lines
         done
       with End_of_file -> close_in ic);
      !lines
    in
    Format.fprintf ppf "@[<v 2>LS %s: [%a@]@,]@]" dir
      Format.(pp_print_list ~pp_sep (fun ppf -> Format.fprintf ppf "@,%s"))
      lines

  let add, get, all, rename =
    let fdl = ref FDM.empty in
    let cpt = ref 0 in
    ( (fun fd s ->
        incr cpt;
        fdl := FDM.add fd (s, !cpt) !fdl;
        !cpt),
      (fun fd ->
        let s = FDM.find fd !fdl in
        fdl := FDM.remove fd !fdl;
        s),
      (fun ppf () ->
        FDM.iter
          (fun _ (s, c) ->
            Format.fprintf ppf "@,%s(%d)" (Filename.basename s) c)
          !fdl),
      fun s1 s2 ->
        fdl := FDM.map (fun (v, cpt) -> ((if v = s1 then s2 else v), cpt)) !fdl;
        Unix.rename s1 s2 )

  let pp_sep ppf () = Format.fprintf ppf ";@,"

  let openfile s l p =
    let fd = Unix.openfile s l p in
    let dir = Filename.dirname s in
    let cpt = add fd s in
    Format.eprintf
      "@[<v 2>>>>>>>>> Open: %s(%d)@,@[<v 2>Opened: [%a@]@,]@,%a@,>>>>>>>>@."
      (Filename.basename s) cpt (* Format.(pp_print_list ~pp_sep pp_flags) l*)
      all () pp_dir dir;
    fd

  let close fd =
    let s, c = get fd in
    let dir = Filename.dirname s in
    let s = Filename.basename s in
    Unix.close fd;
    Format.eprintf
      "@[< v 2><<<<<<<< Closing %s(%d)@,@[<v 2>Opened: [%a@]@,]@,%a@,<<<<<<<<@."
      s c all () pp_dir dir
end
