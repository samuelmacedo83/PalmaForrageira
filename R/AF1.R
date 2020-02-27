#' Area do cladodio
#'
#' @description Esta funcao estima a area do cladodio da palma forrageira recorrendo a dados morfologicos e regressao.
#' @usage AF1(Matriz)
#' @param Matriz    :Matriz com o conjunto de dados. Deve-se ter organizado nas colunas respectivamente o valor do comprimento (cm) e largura (cm)do cladodio
#' @return A funcao retorna a producao (Kg) por planta estimada por regressao multipla.
#' @references Referencias artigo agriambi.
#' @examples
#' data(Dados2)
#' Cor=Dados2
#' AnaliseTrilha(Dados2,6)
#'


AF1=function(Teste){
Teste=as.matrix(Teste,ncol=2)

if (nrow(Teste)==1){
  CF=Teste[,1];LF=Teste[,2]
  ARq= CF*LF * 0.693

  print(paste("Area foliar (cm²) estimada de: ", ARq))
  Resultado=ARq
}


if (nrow(Teste)>1){
  CF=Teste[,1];LF=Teste[,2]
  Predito=ARq= CF*LF * 0.693

  Predito=matrix(Predito,ncol=1)


  Soma=sum(Predito)
  Media=mean(Predito)
  sd=sd(Predito)
  cv=100*sd/Media

  resumo=c(Soma=Soma,Media=Media,DesvioPadrao=sd,CoeficienteVariacao=cv)
  Resultado=list(Predito=Predito,Resumo=resumo)
}

return(Resultado)

}
