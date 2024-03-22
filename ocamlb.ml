let calc_max_profit n m table =
  ( Inicializa um array dp com tamanho n+1 e preenche com zeros. )
  let dp = Array.make (n+1) 0 in
  
  ( Itera sobre cada tamanho possível. )
  for i = 1 to n do
    ( Define uma função auxiliar calc_max_profit_aux que será usada para calcular
       o lucro máximo para um tamanho específico i. )
    let rec calc_max_profit_aux j max_profit =
      ( Se já tivermos considerado todos os itens ou tivermos usado todo o espaço,
         retornamos o lucro máximo. )
      if j = 0 then max_profit
      else
        ( Pega o tamanho e o preço do item atual. )
        let size, price = table.(j) in
        
        ( Verifica se podemos incluir o item atual no espaço restante. )
        if size = i then
          ( Calcula o lucro se incluirmos o item atual. )
          let profit = dp.(i-size) + price in
          ( Chama a função recursivamente para verificar se podemos obter um lucro
             ainda maior se considerarmos o próximo item. )
          calc_max_profit_aux (j-1) (max max_profit profit)
        else
          ( Se não pudermos incluir o item atual, apenas verificamos o próximo item. )
          calc_max_profit_aux (j-1) max_profit
    in
    
    ( Calcula o lucro máximo possível para o tamanho atual i. )
    dp.(i) - calc_max_profit_aux m 0
  done;
  
  ( Retorna o lucro máximo possível para o tamanho total n. )
  dp.(n)
let () =
  ( Lê os valores de n e m da entrada padrão. )
  let n = read_int () in
  let m = read_int () in
  
  ( Verifica se os valores de n e m são válidos. )
  assert (0  m && m = n && n = 10000);
  
  ( Cria uma matriz table com tamanho m+1 para armazenar os preços e tamanhos
     dos itens disponíveis. )
  let table = Array.make (m+1) (0,0) in
  
  ( Lê os valores de tamanho e preço de cada item da entrada padrão e os armazena
     na matriz table. )
  for i = 1 to m do
    let size, price = Scanf.scanf %d %dn (fun x y - (x,y)) in
    assert (0  size && size = 10000 && 0 = price && price = 10000);
    table.(i) - (size, price);
  done;
  
  ( Chama a função calc_max_profit para calcular o lucro máximo possível e o
     armazena na variável max_profit. )
  let max_profit = calc_max_profit n m table in
  
  ( Imprime o lucro máximo possível na saída padrão. )
  Printf.printf %dn max_profit