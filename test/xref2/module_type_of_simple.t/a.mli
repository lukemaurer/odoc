(* Simple module type of *)

module type S = sig
    module X : sig
        type t
    end

    module type Y = module type of struct include X end
end

module T : S

module X2 : sig
    type t = int
    type u
end

module type T = module type of T with module X := X2

module type S2 = sig
    module X : sig type t end
    module Y : module type of struct include X end
end

module T2 : S2 with module X = X2
