

include Mlsem_system.Config
let type_narrowing = ref true
let allow_implicit_downcast = ref true

let save_all, restore_all =
  let vr = ref !value_restriction
  and tn = ref !type_narrowing
  and aic = ref !allow_implicit_downcast
  and io = ref !infer_overload
  and nep = ref !no_empty_param
  and nai = ref !no_abstract_inter
  in
  let save_all () =
    vr := !value_restriction ;
    tn := !type_narrowing ;
    aic := !allow_implicit_downcast ;
    io := !infer_overload ;
    nep := !no_empty_param ;
    nai := !no_abstract_inter ;
  and restore_all () =
    value_restriction := !vr ;
    type_narrowing := !tn ;
    allow_implicit_downcast := !aic ;
    infer_overload := !io ;
    no_empty_param := !nep ;
    no_abstract_inter := !nai ;
  in
  save_all, restore_all
