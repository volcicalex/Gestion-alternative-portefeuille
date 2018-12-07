library(readxl)

data1 <- data.frame(read_excel("Selected_Actif_85-05.xlsx"))
data2 <- data.frame(read_excel("data_final_facteurs_fusionne_2018.xlsx"))

findLigneVide<-function(data){
  nb_lignes <- dim(data)[1]
  countEmpty <- data.frame(index=1:1087, nb_ligne=0)
  index_action = 1
  nb_mois = 0
  for (i in seq(1:nb_lignes)) {
    current_action = data[i, 1]
    if(index_action != current_action){
      countEmpty[index_action, 2] = nb_mois
      nb_mois = 0
      index_action = current_action
    }
    nb_mois = nb_mois + 1
  }
  countEmpty[index_action, 2] = nb_mois
  return(countEmpty)
}

findNan <- function(i) {
  
}

ligneVide1 <- data.frame(findLigneVide(data1))
sortedLigne1 <- ligneVide1[order(ligneVide1$nb_ligne, decreasing=T),]
first100 <- sortedLigne1[1:100,]

ligneVide2 <- data.frame(findLigneVide(data2))
sortedLigne2 <- ligneVide2[order(ligneVide2$nb_ligne, decreasing=T),]
first1012 <- sortedLigne2[1:101,]