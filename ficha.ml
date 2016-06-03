(*module type FichaI =
sig
	type palo
	type ficha
	val valor: ficha -> int
	val palo:  ficha -> palo
	val compara: ficha -> ficha -> int
end
module Ficha:FichaI = struct*)
module Ficha = struct
	type palo = C | D | P | T |Comodin
	type ficha =  {valor:int; palo: palo} 
	let valor ficha= ficha.valor
	let palo ficha= ficha.palo 
	let compara ficha1 ficha2 = if ((ficha1.valor)>(ficha2.valor)) then (-1) else (if ((ficha2.valor)>(ficha1.valor)) then 1 else 0)
end