(*
Module:ldap 
   raddb client lenses
    ldap.conf
Author: coyote 

*)


module Ldap =
  autoload xfm
(********************************************************

**********************************************************)
(* Group: Comments and empty lines *)

(* View: indent *)
let indent = Util.indent

(* View: eol *)
let eol = Util.eol

(* View: opt_eol *)
let opt_eol = del /[ \t]*\n?/ " "

(* View: sep_spc *)
let sep_spc = Sep.space

(* View: sep_equal *)
let sep_equal = Sep.equal

(*  View: sep_spc_equal *)
let sep_spc_equal = Sep.space_equal

(* View: comment
Map comments in "#comment" nodes *)
let comment = Util.comment_generic /[ \t]*[#!][ \t]*/ "# "

(* View: comment_eol
Map comments at eol *)
let comment_eol = Util.comment_generic /[ \t]*[#!][ \t]*/ " # "

(* View: comment_or_eol
A <comment_eol> or <eol> *)
let comment_or_eol = comment_eol | (del /[ \t]*[#!]?\n/ "\n")

(* View: empty
Map empty lines *)
let empty   = Util.empty

(* View: sto_email_addr *)
let sto_email_addr = store Rx.email_addr

(* Variable: word *)
let word = Rx.word

(* Variable: sep_quo *)
let sep_quo = del "\"" "\""

(* Variable: word_slash *)
let word_slash = word | "/"

(* View: sto_word *)
let sto_word = store word

(* View: str_word*)
let sto_command = store Aliases.command

(* View: sto_str_word *) 
(* let sto_str_word = store sto_command*)

(* View: sto_num *)
let sto_num = store Rx.relinteger

(* View: sto_to_eol *)
let sto_to_eol = store /[^#! \t\n][^#!\n]*[^#! \t\n]|[^#! \t\n]/

(* View: field *)
let field (kw:regexp) (sto:lens) = indent . Build.key_value_line_comment kw sep_spc sto comment_eol

(* View: flag
A single word *)
let flag (kw:regexp) = [ indent . key kw . comment_or_eol ]

(* View: lens_block
A generic block with a title lens.
The definition is very similar to Build.block_newlines
but uses a different type of <comment>. *)
let lens_block (title:lens) (sto:lens) =
   [ indent . title
   . Build.block_newlines sto comment . eol ]

(* View: ipv4 and mask*)
let sto_ipv4 = store /[0-9.-]|[0-9.-]+\/[0-9]+/

(* View: block
A simple block with just a block title *)
let block (kw:regexp) (sto:lens) = lens_block (key kw) sto

(* View: named_block
A block with a block title and name *)
let named_block (kw:string) (sto:lens) = lens_block (key kw . sep_spc . sto_ipv4) sto

(* View: named_block_arg_title
A title lens for named_block_arg *)
let named_block_arg_title (kw:string) (name:string) (arg:string) =
                            key kw . sep_spc
                          . [ label name . sto_word ]
                          . sep_spc
                          . [ label arg . sto_word ]

(* View: named_block_arg
A block with a block title, a name and an argument *)
let named_block_arg (kw:string) (name:string) (arg:string) (sto:lens) =
                           lens_block (named_block_arg_title kw name arg) sto


(*********************************************************

**********************************************************)

(*View:*)
let str_word     = /[a-zA-Z0-9_.,\{\}\(\)%\$\/:=-]+/
let sto_str_word = store str_word
let sep_str_word = sep_quo . sto_str_word .sep_quo
let ldap_opt_line = [ indent . label "option"
                               . [ label "name" . sto_word ]
                               . sep_spc_equal
                               . [ label "value" . (sep_str_word | sto_str_word)]
                               . comment_or_eol ]


(*View: eap_opt_tnc_field *)
let ldap_conf = block "ldap"  ( ldap_opt_line | empty | comment )


(**********************************************************

************************************************************)


(* View: lns
     The keepalived lens
*)
let lns = ( empty | comment | ldap_conf )*

(* Variable: filter *)
(*let filter = incl "/etc/raddb/modules/ldap"
    . Util.stdexcl*)
let filter = incl "/etc/raddb/modules/ldap"

let xfm = transform lns filter
