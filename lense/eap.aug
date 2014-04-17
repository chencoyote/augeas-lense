(*
Module: eap
   raddb client lenses
   raddb/eap.conf
Author: coyote 

*)


module Eap =
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

(* Variable: word_slash *)
let word_slash = word | "/"

(* View: sto_word *)
let sto_word = store word

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


(********************************************************

*********************************************************)

(* View: prefixlen
A prefix for IP addresses *)
let prefixlen = [ label "prefixlen" . Util.del_str "/" . sto_num ]

(* View: ipaddr
An IP address or range with an optional mask *)
let ipaddr = label "ipaddr" . store /[0-9.-]+/ . prefixlen?




(*********************************************************

**********************************************************)
(* View: eap_opt_line *)
let eap_opt_line = [ indent . label "option"
                               . [ label "name" . sto_word ]
                               . sep_spc_equal
                               . [ label "value" .sto_word ]
                               . comment_or_eol ]

(*View:*)
let sto_path_word = store /[\/A-Za-z0-9_.-]+/
let sep_slh           = del "\"" "\""
let tnc_opt_line = [ indent . label "option"
                               . [ label "name" . sto_word ]
                               . sep_spc_equal
                               . [ label "value" . sep_slh . sto_path_word 
				 . sep_slh ]
                               . comment_or_eol ]

(*View: eap_opt_tnc_field *)
let eap_opt_tnc_field = block "tnc" tnc_opt_line

(*View:*)                      
let md5_opt_line = [ indent . label "option"
                               . [ label "name" . sto_word ]
                               . sep_spc_equal
                               . [ label "value" .sto_word ]
                               . comment_or_eol ]

(*View: eap_opt_field *)
let eap_opt_md5_field = block "md5" md5_opt_line

(*View:*)                      
let leap_opt_line = [ indent . label "option"
                               . [ label "name" . sto_word ]
                               . sep_spc_equal
                               . [ label "value" .sto_word ]
                               . comment_or_eol ]

(*View: eap_opt_field *)
let eap_opt_leap_field = block "leap" leap_opt_line

(*View:*)
let gtc_opt_line = [ indent . label "option"
                               . [ label "name" . sto_word ]
                               . sep_spc_equal
                               . [ label "value" .sto_word ]
                               . comment_or_eol ]


(*View: eap_opt_field *)
let eap_opt_gtc_field = block "gtc" gtc_opt_line


(*View:*)
let msch_opt_line = [ indent . label "option"
                               . [ label "name" . sto_word ]
                               . sep_spc_equal
                               . [ label "value" .sto_word ]
                               . comment_or_eol ]


(*View: eap_opt_field *)
let eap_opt_msch_field = block "mschapv2" msch_opt_line



(* View: eap_conf*)
let eap_conf = block "eap" (  eap_opt_line 
			        |eap_opt_tnc_field
                                |eap_opt_md5_field
			        |eap_opt_leap_field
			        |eap_opt_gtc_field 
			        |eap_opt_msch_field )
(* View: lvs_conf
contains subblocks of Virtual server group(s) and Virtual server(s) *)
let conf = eap_conf






(**********************************************************

************************************************************)


(* View: lns
     The keepalived lens
*)
let lns = ( empty | comment | conf )*

(* Variable: filter *)
(*let filter = incl "/etc/raddb/eap.conf"
    . Util.stdexcl*)
let filter = incl "/etc/raddb/eap.conf"

let xfm = transform lns filter
