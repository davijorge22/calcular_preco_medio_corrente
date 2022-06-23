
# Script que calcula o preco medio dos ativos a medida que as operacoes sao
# realizadas

# Libraries ---------------------------------------------------------------
library(magrittr)
library(dplyr)
library(tibble)

# imporetando base de dados ficticia para exemplificar -----
df_exemplo <- readxl::read_excel('data-examples/example.xlsx') %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(data = as.Date(data),
                c)

# Funcao que calcula o preco medio acumulado dos ativos no tempo das operacoes ----

fct_calc_pm <- function(ativos, compra_venda, quantidade, preco, data, cliente){

  #transformando os imputs em um data frame
  dados <- tibble::tibble(ativo = ativos,
                          c_v = compra_venda,
                          quantidade, 
                          pm = preco, 
                          data, 
                          cliente) %>%
    dplyr::group_by(cliente, ativo) %>% 
    dplyr::arrange(data, .by_group = T)           # organizando as colunas em ordem crescente
  
  #separando os clientes para o loop
  clientes <- dados %>% 
    dplyr::pull(cliente) %>% 
    unique()
  
  
  #core da funcao, onde todo o processo e realizado
  fct_preco_medio <- function(compra_venda, qtd, preco, data){
    if(compra_venda == "V" & qtd >= 0){
      qtd = qtd*-1
    }else{qtd = qtd}
    
    if(compra_venda == "C" & length(preco_medio$c_v) == 1){
      #para o primeiro dado, repetir preco e quantidade
      pm = as.numeric(preco)
      qtd_custodia = as.numeric(qtd)
      
    }else if(compra_venda == "C" & length(preco_medio$c_v) > 1){
      # para os demais dados do mesmo ativo e cliente, adionar as çlinhas com o calculo do preco
      # medio
      
      pm = ((dplyr::last(preco_medio$pm)*dplyr::last(preco_medio$qtd_custodia))+preco*qtd)/
        (dplyr::last(preco_medio$qtd_custodia)+qtd)
      qtd_custodia = as.numeric(dplyr::last(preco_medio$qtd_custodia)+qtd)
      
    }else if(compra_venda == "V"){
      # se for venda apenas repita os precos medio do ultimo dado
      pm = as.numeric(last(preco_medio$pm))
      qtd_custodia = as.numeric(dplyr::last(preco_medio$qtd_custodia + qtd))
      
    }
    
    #coloca os resultados obtidos em um data frame
    resultados = data.frame(c_v = compra_venda, qtd, qtd_custodia, pm, data)
    
    #combina os dados no data frame pai
    preco_medio = rbind(preco_medio, resultados)
    
    return(preco_medio)
    
  }
  
  # data frme mae, onde os dados serão compilados
  resultado = data.frame(ativo = 0, c_v = 0, qtd = 0, qtd_custodia =  0, pm = 0, conta_bolsa = 0, data = as.Date("1900-01-01"))
  
  #loop que calcula o preco media e tempo real para o ativo e ccliente especifico
  for (k in 1:length(clientes)){
    
    tickers <- dados %>% 
      dplyr::filter(cliente == clientes[k]) %>% 
      dplyr::pull(ativo) %>% 
      unique()
    
    for (j in 1:length(tickers)){
      preco_medio = data.frame(c_v = 0, qtd = 0, qtd_custodia =  0, pm = 0, data = as.Date("1900-01-01"))
      
      dados_1 <- dados %>% 
        dplyr::filter(ativo == tickers[j] & cliente == clientes[k])
      
      for (i in 1:length(dados_1$ativo)) {
        
        preco_medio = fct_preco_medio(dados_1$c_v[i],
                                      dados_1$quantidade[i],
                                      dados_1$pm[i],
                                      dados_1$data[i])
        
      }
      
      # data frame gerado que compila os dados de todos os clientes e seus ativos
      resultado = rbind(resultado, preco_medio %>%
                          dplyr::mutate(ativo = tickers[j],
                                        conta_bolsa = clientes[k],
                                        pm = round(pm, 2))) %>% 
        dplyr::arrange(ativo) %>% 
        dplyr::filter(pm != 0)
      
    }
    
  }
  return(resultado)
  
}


# Resultado
teste <- fct_calc_pm(ativos = df_exemplo$ativos, 
                     compra_venda = df_exemplo$c_v,
                     quantidade = df_exemplo$quantidade,
                     preco = df_exemplo$preco,
                     data = df_exemplo$data,
                     cliente = df_exemplo$cliente) %>% 
  tibble::as_tibble() 
  dplyr::mutate(data = janitor::excel_numeric_to_date(data))
