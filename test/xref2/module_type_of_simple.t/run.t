Module type of should work 

  $ ocamlc -i  a.mli
  module type S =
    sig module X : sig type t end module type Y = sig type t = X.t end end
  module T : S
  module X2 : sig type t = int type u end
  module type T = sig module type Y = sig type t = X2.t end end
  module type S2 =
    sig module X : sig type t end module Y : sig type t = X.t end end
  module T2 :
    sig
      module X : sig type t = int type u = X2.u end
      module Y : sig type t = X.t end
    end
  $ ocamlc -c -bin-annot a.mli
  $ odoc compile a.cmti
  $ odoc_print -r T2 --short a.odoc
  module T2 : S2 with module X = = X2 : sig
    module X = X2
    module Y : sig
      type t = X.t
      
        end
      
    end
