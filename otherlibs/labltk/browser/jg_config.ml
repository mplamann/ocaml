(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

let init () =
  let font =
    let font =
      Option.get Widget.default_toplevel name:"variableFont" class:"Font" in
    if font = "" then "variable" else font
  in
  List.iter ["Button"; "Label"; "Menu"; "Menubutton"; "Radiobutton"]
    fun:(fun cl -> Option.add path:("*" ^ cl ^ ".font") font);
  Option.add path:"*Button.padY" "0" priority:`StartupFile;
  Option.add path:"*Text.highlightThickness" "0" priority:`StartupFile;
  Option.add path:"*interface.background" "gray85" priority:`StartupFile;
  let foreground =
    Option.get Widget.default_toplevel
      name:"disabledForeground" class:"Foreground" in
  if foreground = "" then
    Option.add path:"*disabledForeground" "black"
