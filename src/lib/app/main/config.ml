
include Mlsem_system.Config
include Mlsem_lang.Config

type narrowing =
  NoNarrowing | DirectNarrowing | PartitionNarrowing | BothNarrowing
let type_narrowing = ref BothNarrowing
let allow_implicit_downcast = ref true

let save_all, restore_all =
  let vr = ref !value_restriction
  and tn = ref !type_narrowing
  and aic = ref !allow_implicit_downcast
  and io = ref !infer_overload
  and nf = ref !normalization_fun
  and snf = ref !subst_normalization_fun
  and vty = ref !void_ty
  and aeo = ref !app_eval_order
  and teo = ref !tuple_eval_order
  and reo = ref !record_eval_order
  and ceo = ref !cons_eval_order
  and cceo = ref (Hashtbl.to_seq ccustom_eval_order)
  in
  let save_all () =
    vr := !value_restriction ;
    tn := !type_narrowing ;
    aic := !allow_implicit_downcast ;
    io := !infer_overload ;
    nf := !normalization_fun ;
    snf := !subst_normalization_fun ;
    vty := !void_ty ;
    aeo := !app_eval_order ;
    teo := !tuple_eval_order ;
    reo := !record_eval_order ;
    ceo := !cons_eval_order ;
    cceo := Hashtbl.to_seq ccustom_eval_order ;
  and restore_all () =
    value_restriction := !vr ;
    type_narrowing := !tn ;
    allow_implicit_downcast := !aic ;
    infer_overload := !io ;
    normalization_fun := !nf ;
    subst_normalization_fun := !snf ;
    void_ty := !vty ;
    app_eval_order := !aeo ;
    tuple_eval_order := !teo ;
    record_eval_order := !reo ;
    cons_eval_order := !ceo ;
    Hashtbl.clear ccustom_eval_order ;
    Hashtbl.add_seq ccustom_eval_order !cceo
  in
  save_all, restore_all
