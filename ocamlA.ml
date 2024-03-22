(*Funçao para a primeira formula de Schroder, começa por definir um contador inicializado em 0 cujo o valor pode ser alterado utilizando o "ref" e uma funcao recursiva que recebe o valor "a"*)
let _c = ref 0
let rec st_formula a =
(*cada vez que a funcao st_formula for utilizada vai atualizar o contador incrementando em um o mesmo*)
_c := !_c +1;
(* se o a=0 vai devolver 1, se a=1 vai devolver 2, se for outro valor vai fazer a formula seguinte*)
match a with
    0 -> 1
   | 1 -> 2
   | n -> ((3 * (st_formula (a-1))) + (som a))
and som a = (*criar uma funcao para resolver a parte do somatorio*)
   let rec som_aux (a, k) = (*funcao que recebe o "a" e o "k" pedidos no somatorio para resolver o mesmo*)
     	if(k > (a-2) || (a-2) = 0) then 0 (*para parar a funcao (uma vez que o k aumenta até ao valor de a mas nunca o ultrapassa) ou para quando o a=2*) 
	else (st_formula k) * (st_formula (a-k-1)) + (som_aux (a, k+1)) (*parte da formula que resolve o somatorio*)
in som_aux (a, 1) (*inicializar a funçao com o k=1 como pede o somatorio para resolver a funcao corretamente*)

(*Funçao para a segunda formula de Schroder, começa por definir um contador inicializado em 0 cujo o valor pode ser alterado utilizando o "ref" e uma funcao recursiva que recebe o valor "a"*)
let c_ = ref 0
let rec nd_formula a=
(*cada vez que a funcao nd_formula for utilizada vai atualizar o contador incrementando em um o mesmo*)
c_ := !c_+1;
(* se o a=0 vai devolver 1, se a=1 vai devolver 2, se for outro valor vai fazer a formula seguinte*)
match a with
	 0->1
	| 1->2
	| n-> ((((6*a)-3) * nd_formula (a-1)) / (a+1)) - (((a-2)*(nd_formula(a-2)))/(a+1))

let espaco = 10000;; (*alocar memoria para 10000 valores*)
let tabela = Hashtbl.create espaco;; (*criar tabela utilizando a memoria alocada previamente*)

(*Funçao para a segunda formula de Schroder, começa por definir uma funcao recursiva que recebe o valor "b", é a mesma formula que a nd_formula mas esta utiliza o biblioteca zarith para permitir calcular numeros maiores*)
let rec nd_formula_z b =
(* se o a=0 vai devolver 1, se a=1 vai devolver 2, se for outro valor vai fazer a formula seguinte*)
match b with
	 0->(Z.one)
	| 1->(Z.of_int 2)
	| n->(match (Hashtbl.find_opt tabela b) with
            |Some resultado->resultado (*formula foi dividida em 9 parter (p1-p9) para facilitar o calculo e corrigir eventuais erros de programacao mais facilmente*)
            |None -> let p1 = Z.mul (Z.of_int 6) (Z.of_int b) in (*multiplica 6 e "b", (6*b)*)
                    let p2= Z.sub p1 (Z.of_int 3) in (*subtrai o resultado anterior com 3, ((6*b)-3)*)
                    let p3= Z.mul p2 (nd_formula_z (b-1)) in (*multiplica o resultado anterior com o resultado de (nd_formula_z (b-1)),   ((6*b)-3)*nd_formula_z (b-1))*)
                    let p4= Z.add (Z.of_int b)(Z.of_int 1) in (*soma o valor de "b" com 1*)
                    let p5= Z.div p3 p4 in (*divide o resultado de p3 com o resultado anterior, ((6*b)-3)*nd_formula_z (b-1)/(b+1))*)
                    let p6= Z.sub (Z.of_int b) (Z.of_int 2) in (*subtrai o valor de "b" com 2*)
                    let p7= Z.mul p6 (nd_formula_z (b-2)) in (*multiplica o resultado anterior com "nd_formula_z (b-2)", ((b-2)*nd_formula_z (b-2))*)
                    let p8= Z.div p7 p4 in (*divide o resultado anterior com a soma de "b" e 1, ((b-2)*nd_formula_z (b-2))/(b+1)*)
                    let p9= Z.sub p5 p8 in (*subtrai o valor de p5 com o de p8, representando a formula final, ((6*b)-3)*nd_formula_z (b-1)/(b+1))-((b-2)*nd_formula_z (b-2))/(b+1)*)
            Hashtbl.add tabela b p9; (*adiciona à tabela*)
            p9;)

(*Funcao main para testar o programa e as funcoes criadas acima*)
let main =
let (a,b) = Scanf.scanf"%d %d" (fun a b -> (a,b)) in (*pede os valores "a" e "b" ao utilizador caso estes sejam negativos ou superiores a 10000, é devolvida uma mensagem a dizer "valor invalido"*)
if (a>=0 && a<=20 && b>=0 && b<=10000) then
let (x) = st_formula (a) in (*executar a primeira formula utilizando o valor de "a" recebido previamente*)
let (y) = nd_formula (a) in (*executar a segunda formula utilizando o valor de "a" recebido previamente*)
let (z) = nd_formula_z (b) in (*executar a segunda formula utilizando o valor de "b" recebido previamente*)
Printf.printf"%d %d\n" x _c; (*é o resultado de Sa pela primeira definicao da sequencia de Schroder e o numero de vezes que a funcao recursiva é chamada*) 
Printf.printf"%d %d\n" y c_; (*é o resultado de Sa pela segunda definicao da sequencia de Schroder e o numero de vezes que a funcao recursiva é chamada*)
Printf.printf"%s\n" (Z.to_string z); (*é o resultado de Sa pela segunda definicao da sequencia de Schroder mas utilizando a biblioteca zarith*) 
else Printf.printf("Valor Invalido \n")
