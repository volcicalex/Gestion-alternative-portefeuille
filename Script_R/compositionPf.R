# On recupere les donnees
library(readxl)

actifs <- data.frame(read_excel("../Excel/Selected_Actif_85-05.xlsx"))
stock_numbers <- unique(actifs$stock_number)

#Rentabilite sur la periode en fonction des rentabilites mensuelles
calcul_rentab <- function(liste){
  r = 1
  l = length(liste)
  for (k in c(1:l)){
    r = r*(1 + liste[k])
  }
  return(r-1)
}


#On calcule la rentabilite de chaque actif sur une annee donnee pendant une periode
rentab <- function(annee_periode, mois_periode, taille_periode){
  dico_rentab=data.frame(actif=stock_numbers, rentab = rep(0,100))
  for (i in c(1:100)){
    numero_actif = dico_rentab$actif[i]
    actif_k = actifs[actifs$stock_number == numero_actif & actifs$year==annee_periode & actifs$month >= mois_periode & actifs$month < mois_periode+taille_periode,]
    tab_rentab = actif_k["return_rf"] + actif_k["RiskFreeReturn"]
    rentab = calcul_rentab(tab_rentab[, 1])
    dico_rentab$rentab[i] = rentab
  }
  return(dico_rentab)
}

#Cette fonction permet de calculer la composition des portefeuilles P1 a P10 
#On lui donne en entree la date de composition de notre portefeuille
#Et la periode de prevision pour le portefeuille

association_actif_pf <- function(annee_periode, debut_periode, taille_periode){
  rentabilites = rentab(annee_periode, debut_periode - taille_periode , taille_periode)
  sorted_rentabilites = rentabilites[order(rentabilites[,2],decreasing=F),]
  portefeuille = data.frame(pf=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10"), actifs = rep(0, 10))
  actif_portfeuille = data.frame(portefeuille = rep(0,100))
  rownames(actif_portfeuille) = stock_numbers
  for (num_pf in seq(1:10)) {
    actif_pf = rep(0, 10)
    for (k in seq(1:10)) {
      actif_pf[k] = sorted_rentabilites$actif[k + 10*(num_pf-1)]
      numero_actif = toString(actif_pf[k])
      actif_portfeuille[numero_actif, "portefeuille"] = num_pf
    }
    portefeuille$actifs[num_pf] = toString(actif_pf)
  }
  return(actif_portfeuille)
}


composition_portefeuille <- function(annee_periode, debut_periode, taille_periode){
  rentabilites = rentab(annee_periode, debut_periode - taille_periode , taille_periode)
  sorted_rentabilites = rentabilites[order(rentabilites[,2],decreasing=F),]
  portefeuille = data.frame(pf=c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10"), actifs = rep(0, 10))
  for (num_pf in seq(1:10)) {
    actif_pf = rep(0, 10)
    for (k in seq(1:10)) {
      actif_pf[k] = sorted_rentabilites$actif[k + 10*(num_pf-1)]
    }
    portefeuille$actifs[num_pf] = toString(actif_pf)
  }
  return(portefeuille)
}

annees <- c()
moiss <- c()
rows <- c()
for (annee in c(1986:2004)){
  for (mois in c(1:12)){
    annees <- rbind(annees, annee)
    moiss <- rbind(moiss, mois)
    rows <- rbind(rows, paste(as.character(mois), "/", as.character(annee), sep = ""))
  }
}

#Calcul les rentas des portefeuilles tout les mois
#On prend en entrée la durée de la periode
rentab_portefeuille <- function(debut_periode, taille_periode){
  nb_lignes <- length(annees)
  rentab_frame <- data.frame(row.names = rows, annee=annees, mois=moiss, P1=rep(0, nb_lignes), P2=rep(0, nb_lignes), P3=rep(0, nb_lignes), 
                            P4=rep(0, nb_lignes), P5=rep(0, nb_lignes), P6=rep(0, nb_lignes), P7=rep(0, nb_lignes), P8=rep(0, nb_lignes), 
                            P9=rep(0, nb_lignes), P10=rep(0, nb_lignes), P10P1=rep(0, nb_lignes), marche=rep(0, nb_lignes), rf=rep(0, nb_lignes))
  rentaTransac_frame <- data.frame(row.names = rows, annee=annees, mois=moiss, P1=rep(0, nb_lignes), P2=rep(0, nb_lignes), P3=rep(0, nb_lignes), 
                             P4=rep(0, nb_lignes), P5=rep(0, nb_lignes), P6=rep(0, nb_lignes), P7=rep(0, nb_lignes), P8=rep(0, nb_lignes), 
                             P9=rep(0, nb_lignes), P10=rep(0, nb_lignes), P10P1=rep(0, nb_lignes), marche=rep(0, nb_lignes), rf=rep(0, nb_lignes))
  beta_frame <- data.frame(row.names = rows, annee=annees, mois=moiss, P1=rep(0, nb_lignes), P2=rep(0, nb_lignes), P3=rep(0, nb_lignes), 
                           P4=rep(0, nb_lignes), P5=rep(0, nb_lignes), P6=rep(0, nb_lignes), P7=rep(0, nb_lignes), P8=rep(0, nb_lignes), 
                           P9=rep(0, nb_lignes), P10=rep(0, nb_lignes), P10P1=rep(0, nb_lignes))
  betaSMB_frame <- data.frame(row.names = rows, annee=annees, mois=moiss, P1=rep(0, nb_lignes), P2=rep(0, nb_lignes), P3=rep(0, nb_lignes), 
                           P4=rep(0, nb_lignes), P5=rep(0, nb_lignes), P6=rep(0, nb_lignes), P7=rep(0, nb_lignes), P8=rep(0, nb_lignes), 
                           P9=rep(0, nb_lignes), P10=rep(0, nb_lignes), P10P1=rep(0, nb_lignes), SMB=rep(0, nb_lignes))
  betaHML_frame <- data.frame(row.names = rows, annee=annees, mois=moiss, P1=rep(0, nb_lignes), P2=rep(0, nb_lignes), P3=rep(0, nb_lignes), 
                           P4=rep(0, nb_lignes), P5=rep(0, nb_lignes), P6=rep(0, nb_lignes), P7=rep(0, nb_lignes), P8=rep(0, nb_lignes), 
                           P9=rep(0, nb_lignes), P10=rep(0, nb_lignes), P10P1=rep(0, nb_lignes), HML=rep(0, nb_lignes))
  betaMOM_frame <- data.frame(row.names = rows, annee=annees, mois=moiss, P1=rep(0, nb_lignes), P2=rep(0, nb_lignes), P3=rep(0, nb_lignes), 
                           P4=rep(0, nb_lignes), P5=rep(0, nb_lignes), P6=rep(0, nb_lignes), P7=rep(0, nb_lignes), P8=rep(0, nb_lignes), 
                           P9=rep(0, nb_lignes), P10=rep(0, nb_lignes), P10P1=rep(0, nb_lignes), MOM=rep(0, nb_lignes))
  for (annee in c(1986: 2004)){
    compo <- composition_portefeuille(annee, debut_periode, taille_periode)
    for (indice_P in seq(1:10)){
      P <- as.integer(unlist(strsplit(compo[indice_P,2], split=", ")))
      for (mois in seq(1:12)){
        renta_k <- 0
        rentaTransac_k <- 0
        beta_k <- 0
        betaSMB_k <- 0
        betaHML_k <- 0
        betaMOM_k <- 0
        for (action in P){
          tab_k <- actifs[actifs$stock_number==action & actifs$year==annee & actifs$month==mois,]
          renta_k <- renta_k + sum(tab_k["return_rf"] + tab_k["RiskFreeReturn"])
          beta_k <- beta_k + sum(tab_k["beta"])
          betaSMB_k <- betaSMB_k + sum(tab_k["betaSMB"])
          betaHML_k <- betaHML_k + sum(tab_k["betaHML"])
          betaMOM_k <- betaMOM_k + sum(tab_k["betaMOM"])
        }
        rentab_frame[rentab_frame$annee==annee & rentab_frame$mois==mois,indice_P + 2]<-renta_k/10
        rentab_frame[rentab_frame$annee==annee & rentab_frame$mois==mois,14]<-sum(tab_k["Marketretrun"])
        rentab_frame[rentab_frame$annee==annee & rentab_frame$mois==mois,15]<-sum(tab_k["RiskFreeReturn"])
        
        rentaTransac_frame[rentaTransac_frame$annee==annee & rentaTransac_frame$mois==mois,indice_P + 2]<-renta_k/10 - 0.001*abs(renta_k/10)
        rentaTransac_frame[rentaTransac_frame$annee==annee & rentaTransac_frame$mois==mois,14]<-sum(tab_k["Marketretrun"])
        rentaTransac_frame[rentaTransac_frame$annee==annee & rentaTransac_frame$mois==mois,15]<-sum(tab_k["RiskFreeReturn"])
        
        beta_frame[beta_frame$annee==annee & beta_frame$mois==mois,indice_P + 2]<-beta_k/10
        
        betaSMB_frame[betaSMB_frame$annee==annee & betaSMB_frame$mois==mois,indice_P + 2]<-betaSMB_k/10
        betaSMB_frame[betaSMB_frame$annee==annee & betaSMB_frame$mois==mois,14]<-sum(tab_k["SmallMinusBig"])
        
        betaHML_frame[betaHML_frame$annee==annee & betaHML_frame$mois==mois,indice_P + 2]<-betaHML_k/10
        betaHML_frame[betaHML_frame$annee==annee & betaHML_frame$mois==mois,14]<-sum(tab_k["HighMinusLow"])
        
        betaMOM_frame[betaMOM_frame$annee==annee & betaMOM_frame$mois==mois,indice_P + 2]<-betaMOM_k/10
        betaMOM_frame[betaMOM_frame$annee==annee & betaMOM_frame$mois==mois,14]<-sum(tab_k["MomentumFactor"])
      }
    }
  }
  rentab_frame$P10P1 <- rentab_frame$P10 - rentab_frame$P1
  beta_frame$P10P1 <- beta_frame$P10 - beta_frame$P1
  betaSMB_frame$P10P1 <- betaSMB_frame$P10 - betaSMB_frame$P1
  betaHML_frame$P10P1 <- betaHML_frame$P10 - betaHML_frame$P1
  betaMOM_frame$P10P1 <- betaMOM_frame$P10 - betaMOM_frame$P1
  rentaTransac_frame$P10P1 <- rentaTransac_frame$P10 - rentaTransac_frame$P1
  
  write.csv2(rentaTransac_frame, "../Excel/rentaTransac85-05.csv")
  write.csv2(rentab_frame, "../Excel/renta85-05.csv")
  write.csv2(beta_frame, "../Excel/beta85-05.csv")
  write.csv2(betaSMB_frame, "../Excel/betaSMB85-05.csv")
  write.csv2(betaHML_frame, "../Excel/betaHML85-05.csv")
  write.csv2(betaMOM_frame, "../Excel/betaMOM85-05.csv")
}
rentab_frame <- rentab_portefeuille(7,6)

# Creation du fichier associant chaque actif a un portefeuille en tout temps
# duree_etude correspond a la duree de l'etude en nombre d'annees (nombre entier)
# duree_prec est le nombre de mois precedents a prendre en compte pour constituer le pf

portefeuille_annuel <- function(duree_etude, duree_prec){
  constitution_annuelle <- data.frame(matrix(NA,ncol=101,nrow=20))
  rownames(constitution_annuelle) <- c(1985:2004)
  colnames(constitution_annuelle) <-  c("Mois", stock_numbers)
  constitution_annuelle$Mois <- rep(" Juillet - Juin ", 20)
  constitution_annuelle$Mois[20] <- " Juillet - Decembre "
  for (annee in c(1985:2004)) {
    print(annee)
    actif_portfeuille = association_actif_pf(annee, 7, duree_prec)
    constitution_annuelle[toString(annee),2:101] = actif_portfeuille$portefeuille
  }
  return(constitution_annuelle)
}









rentab_actif <- rentab(1985, 1, 6)
print(rentab_actif)
portefeuilles <- composition_portefeuille(1986, 7, 6)
print(portefeuilles)
test <- portefeuille_annuel(20, 6)
print(test)
write.csv2(test, row.names = TRUE,col.names = TRUE, file="../Excel/composition.csv")

