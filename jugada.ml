open Ficha
open List
(*module type JugadaI =
sig
	type jugada
	type ficha 
	type palo
	val esValida: jugada -> bool
	val esComodin:  ficha -> bool
	val checaSerie: jugada -> int -> palo list -> bool
	val esSerie : jugada -> bool
	val checaEscalera: jugada -> int -> palo -> bool
	val esEscalera: jugada -> bool
end
module Jugada:JugadaI = struct*)
module Jugada = struct
	type ficha = Ficha.ficha 
	type palo = Ficha.palo
	type jugada = ficha list  
	
	
	let esComodin ficha= if ((ficha.valor == 0) && (ficha.palo == Comodin)) then true else false  
	(*funcion auxiliar de esSerie*)
	let rec checaSerie lista numero palosExistentes = match lista with
	| [] -> true
	| (x::xs) -> if (((valor x) == numero && (not (mem (palo x) palosExistentes))) || (esComodin x)) then (checaSerie xs numero ((palo x)::palosExistentes)) else false  
	(*Metodo que revisa si una lista es una serie (secuencia de fichas con numeros iguales) es valida  *)
	let esSerie lista =if (length lista < 3) then false else (checaSerie lista (valor (hd lista)) [])
	let rec checaEscalera lista numero p = match lista with
	| [] -> true
	| (x::xs) -> if ((valor x) == numero && ((palo x) == p) || (esComodin x)) then (checaEscalera xs (numero + 1) p) else false
	(*Metodo que revisa si una lisa es una escalera (secuencia consecutiva con numeros con el mismo palo) es valida*)
	let esEscalera lista= if (length lista < 3) then false else (checaEscalera lista (valor (hd lista)) (palo (hd lista)))
	let esValida lista= match lista with
	| [] -> false
	| _ -> ((esEscalera lista) || (esSerie lista))

end