  $ ocamlc -c -bin-annot a.mli
  $ ocamlc -c -bin-annot b.mli
  $ ocamlc -c -bin-annot c.mli
  $ ocamlc -i c.mli
  module type B = B.B
  module type B1 = B.B1
  module A : sig type t = B.A.t type b = B.A.b end

  $ odoc compile a.cmti
  $ odoc compile b.cmti
  $ odoc compile -I . c.cmti
 
  $ odoc_print --short c.odoc 
  include : module type of struct include A end (sig =  (sig=module type {B}1 = A.B
                                                             include : {B}1 (sig =  (sig=
                                                               module {A}1 = A.{A}1
                                                               ))
                                                               module type {B1}2 = A.B1
                                                               include : {B1}2 (sig =  (sig=
                                                                 module {A}3 = A.A
                                                                 ))
                                                                 ))
                                                               include : module type of struct include B end (sig =  (sig=
                                                                 module type B = B.B
                                                                 include : B (sig =  (sig=
                                                                   module {A}1 = B.{A}1
                                                                   ))
                                                                   module type B1 = B.B1
                                                                   include : B1 (sig =  (sig=
                                                                     module {A}4 = B.A
                                                                     ))
                                                                     ))
                                                                   module A : sig
                                                                     include : module type of struct include {A}4 end (sig =  (sig=
                                                                      include : module type of struct include B.{A}1 end (sig =  (sig=
                                                                      type t = B.A.t
                                                                      ))
                                                                      type b = B.A.b
                                                                      ))
                                                                      
                                                                      end
                                                                     
