area <- function(comprimento_m, largura_m) {
  largura_m * comprimento_m
}

espacamento <- function(comprimento_m, largura_m, tipo){
  area <- area(comprimento_m, largura_m)

  if(tipo == "Simples") return(area / 0.5)
  if(tipo == "Fileiras Duplas") return(area)
  if(tipo == "Três Fileiras") return(area / 0.41)
  if(tipo == "Quatro Fileiras") return(area / 0.375)
}

#cladodios <-  c(10,	8,	9,	9,	10,	11,	12,	13)


produtividade_palmal <- function(cladodios, comprimento_m, largura_m, tipo){

  produtividade_planta <- 8.9039 + 0.9765 * mean(cladodios)
  produtividade_real <- produtividade_planta * espacamento(comprimento_m, largura_m, tipo) / 1000
  produtividade_real
}

fornecimento <- function(qtd_animais, demanda_animal,
                         cladodios,
                         comprimento_m, largura_m,
                         tipo){
  produtividade_palmal <- produtividade_palmal(cladodios, comprimento_m, largura_m, tipo)
  consumo_diario <- qtd_animais * demanda_animal / 1000
  dias <- produtividade_palmal / consumo_diario
  meses <- dias / 30
  data.frame(Dias = dias, Meses = meses)
  }



