#' Produtividade (interface grafica)
#'
#' @description Esta funcao estima a produtividade da palma forrageira recorrendo a dados morfologicos e redes neurais artificiais.
#' @usage Produtividade1(Matriz)
#' @param Matriz    :Matriz com o conjunto de dados. Deve-se ter organizado nas colunas respectivamente o valor medio na planta do comprimento (cm), largura (cm), espessura (mm) das raques, altura da planta (m), numero de raques (un) e area total do cladodio (m²)
#' @return A funcao retorna a producao (Kg) por planta estimada por redes neurais artificiais.
#' @references Referencias artigo agriambi.
#' @examples
#' data(Dados2)
#' Cor=Dados2
#' AnaliseTrilha(Dados2,6)
#'



Produtividade1=function(Teste){
  require(RSNNS)
  Teste=Teste[,1:6]
  Teste=as.matrix(Teste)
  Teste=matrix(Teste,ncol=6)
  # DadosMatN <- paste( "C:/_PalmaForrageira/", "DadosMatN.R",sep="")
  # rede <- paste( "C:/_PalmaForrageira/", "rede.R",sep="")

  load("extdata/DadosMatN.R")
  load("extdata/rede.R")
  get=getNormParameters(DadosEN)

  Norma=rbind(get$colMaxima,get$colMinima)[,-7]


  ####################################################################################################
  Normatiza=function(DadosEntrada, DadosBase, LimiteInferior, LimiteSuperior){
    valMax = apply(DadosBase,2,max)
    valMin = apply(DadosBase,2,min)
    valMax2=DadosEntrada
    valMin2=DadosEntrada
    Normatizado=DadosEntrada
    for (i in 1:ncol(DadosEntrada)){
      for (j in 1:nrow(DadosEntrada)){
        valMax2[j, i] = valMax[i]
        valMin2[j, i] = valMin[i]
      }}
    Normatizado=(LimiteSuperior - LimiteInferior)*(DadosEntrada- valMax2)/(valMax2- valMin2)
    Normatiza=Normatizado+LimiteSuperior
    return(Normatiza)
  }
  ####################################################################################



  DN=DadosEN
  Norma2=NULL
  Norma2$colMaxima=getNormParameters(DN)$colMaxima[7]
  Norma2$colMinima=getNormParameters(DN)$colMinima[7]
  Norma2$type=getNormParameters(DN)$type


  TesteN=Normatiza(Teste,Norma,0,1)
  pred=predict(rede2,TesteN)

  Predito=matrix(denormalizeData(pred,Norma2),ncol=1)


  if(nrow(Teste)>1){
    Soma=sum(Predito)
    Media=mean(Predito)
    sd=sd(Predito)
    cv=100*sd/Media

    resumo=c(Soma=Soma,Media=Media,DesvioPadrao=sd,CoeficienteVariacao=cv)
    Resultado=list(Predito=Predito,Resumo=resumo)
    return(Resultado)
  }

  if(nrow(Teste)==1){
    return(Predito)
  }

}
