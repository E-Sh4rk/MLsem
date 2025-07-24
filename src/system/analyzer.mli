open Annot

type severity = Message | Notice | Warning
type msg = { eid: Eid.t ; severity: severity ; title: string ; descr: string option }

val analyze : Ast.t -> Annot.t -> msg list
