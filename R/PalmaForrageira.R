
#' PalmaForrageira (interface grafica)
#'
#' @description Esta funcao ....
#' @usage PalmaForrageira()
#' @return A funcao retorna a producao (Kg) por planta estimada por redes neurais artificiais.
#' @references Referencias artigo agriambi.
#' @examples
#' data(Dados2)
#' Cor=Dados2
#' AnaliseTrilha(Dados2,6)
#'

#' @export
PalmaForrageira=function(){


require(rpanel)
#require(PalmaForrageira)
panel=rp.control("Palma Forrageira ",size = c(700, 400),Pars= c(CRq=29.91,	14.89,	8.94,	1.05,	34,	2.09),Resultado="Resultado")

# image.file <- paste( "C:/_PalmaForrageira/", "fig.gif",sep="")
image.file <- "extdata/fig.gif"
rp.image(panel, image.file)

FuncaoMenu=function(panel){
  if(panel$menu=="Modelo 1"){
    rp.button(panel,title = "-------> Digitar <-------", action = digitar, pos = "bottom")
    rp.button(panel,title = "-------> Carregar arquivo <-------", action = carregar, pos = "bottom")

    panel
  }


  if(panel$menu=='Modelo -> 1'){
    rp.button(panel,title = "-------> Digitar <-------", action = digitarAF, pos = "bottom")
    rp.button(panel,title = "-------> Carregar arquivo <-------", action = carregarAF, pos = "bottom")

    panel
  }
}

rp.menu(panel,menu,labels=list(list('Adubacao',"Plantio", "Cobertura"),
                               list('Estimativa da area do cladodio',"Modelo -> 1"),
                               list('Estimativa da produtividade',"Modelo 1"),
                               list("Espacamento e Estande","Linha simples", "Linhas duplas", "Linhas triplas")),action=FuncaoMenu)







############################################################################################
################################
######3 Produtividade


digitar=function(panel){
  browser()
  panel=rp.control("Palma Forrageira ",size = c(700, 400),Pars= c(CRq=29.91,	14.89,	8.94,	1.05,	34,	2.09),Resultado="Resultado")

  #image.file <- paste( "C:/_PalmaForrageira/", "fig.gif",sep="")
  image.file <- "extdata/fig.gif"
  rp.image(panel, image.file)
  rp.menu(panel,menu,labels=list(list('Adubacao',"Plantio", "Cobertura"),
                                 list('Estimativa da area do cladodio',"AC(cm²) = CC x LC x 0,693","Ajuste por Redes Neurais artificias"),
                                 list('Estimativa da produtividade',"Modelo 1"),
                                 list("Espacamento e Estande","Linha simples", "Linhas duplas", "Linhas triplas")),action=FuncaoMenu)

  rp.textentry(panel,Pars,action=ProdutividadeDig,labels=c("CRq (cm)", "LRq (cm)", "Erq (mm)", "ALT (m)", "NRQ (un)" ,"ATC (m²)"),pos="bottom")
  rp.button(panel,title = "-------> Estimar <-------", action = ProdutividadeDig, pos = "bottom")
  panel

}
ProdutividadeDig=function(panel){


  mat=matrix(as.numeric(panel$Pars),ncol=6)
  colnames(mat)=c("CRq (cm)", "LRq (cm)", "Erq (mm)", "ALT (m)", "NRQ (un)" ,"ATC (m²)")
  print(mat)
  print(paste("Produtividade estimada (kg/planta)= ",Produtividade1(mat)))
  panel
}


carregar=function(panel){
  file=file.choose()
entrada=read.table(file,h=T)
print(Produtividade1(entrada))
}


############################################################################################
################################
###### Area foliar

digitarAF=function(panel){
  panel=rp.control("Palma Forrageira ",size = c(700, 400),Pars= c(CRq=29.91,	LRq=14.89),Resultado="Resultado")

  #image.file <- paste( "C:/_PalmaForrageira/", "fig.gif",sep="")
  image.file <- "extdata/fig.gif"
  rp.image(panel, image.file)
  rp.menu(panel,menu,labels=list(list('Adubacao',"Plantio", "Cobertura"),
                                 list('Estimativa da area do cladodio',"AC(cm²) = CC x LC x 0,693","Ajuste por Redes Neurais artificias"),
                                 list('Estimativa da produtividade',"Modelo 1"),
                                 list("Espacamento e Estande","Linha simples", "Linhas duplas", "Linhas triplas")),action=FuncaoMenu)

  rp.textentry(panel,Pars,action=AF_Dig,labels=c("CRq (cm)", "LRq (cm)"),pos="bottom")
  rp.button(panel,title = "-------> Estimar <-------", action = AF_Dig, pos = "bottom")
  panel

}

AF_Dig=function(panel){
  browser()
    mat=matrix(as.numeric(panel$Pars),ncol=2)
  colnames(mat)=c("CRq (cm)", "LRq (cm)")
  print(mat)
  AF1(mat)
  panel
}

carregarAF=function(panel){
  file=file.choose()
  entrada=read.table(file,h=T)
  print("Area foliar (cm²) estimada de: ")
  print(AF1(entrada))
}

}
