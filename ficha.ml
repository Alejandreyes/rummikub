module Ficha = struct
type palo = C | R | P | T |Comodin
	type ficha = {n:int ;p: palo}

	let palo ficha= match ficha with
	| (_,p) -> p
	let valor ficha = match ficha with
	| (v,_) -> v

	let compara carta1 carta2 = match carta1 with
	| (n,_) -> (match carta2 with
	| (m,_) -> if (n>m) then (-1) else (if (m>n) then 1 else 0))
	
end