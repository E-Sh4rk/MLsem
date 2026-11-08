(* Without arguments this is the language server the editor starts; it runs
   all mlsem work in a child process started with [--engine]. *)
let () =
  if Array.mem "--engine" Sys.argv then
    Mlsem_lsp.Engine.run ()
  else
    Mlsem_lsp.Server.run ()
