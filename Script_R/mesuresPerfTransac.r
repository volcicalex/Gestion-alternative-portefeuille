library(readxl)
csv2 <- read.csv2(file = "../Excel/betaSMB85-05.csv", header = TRUE)
data_betaSMB <- as.data.frame(csv2)
csv3 <- read.csv2(file = "../Excel/betaHML85-05.csv", header = TRUE)
data_betaHML <- as.data.frame(csv3)
csv4 <- read.csv2(file = "../Excel/betaMOM85-05.csv", header = TRUE)
data_betaMOM <- as.data.frame(csv4)
csv5 <- read.csv2(file = "../Excel/rentaTransac85-05.csv", header = TRUE)
data_renta<- as.data.frame(csv5)

ratioSharpe <- function(vect_return, vect_risk_free_return){
  return((mean(vect_return)-mean(vect_risk_free_return))/sd(vect_return))
}

ratioSharpe(data_renta$P1, data_renta$rf)
ratioSharpe(data_renta$P10, data_renta$rf)
ratioSharpe(data_renta$P10P1, data_renta$rf)

t.test((data_renta$P1-data_renta$rf)/sd(data_renta$P1), mu=0)
t.test((data_renta$P10-data_renta$rf)/sd(data_renta$P10), mu=0)
t.test((data_renta$P10P1-data_renta$rf)/sd(data_renta$P10P1), mu=0)

ratioTreynor <- function(vect_return, vect_risk_free_return,  vect_marche){
  rmRf = vect_marche - vect_risk_free_return
  reg8_Rm  <- lm(vect_return - vect_risk_free_return ~ rmRf)
  beta_Rm <- reg8_Rm$coefficients[2]
  t.test((data_renta$P1-data_renta$rf)/beta_Rm, mu=0)
  #return((mean(vect_return)-mean(vect_risk_free_return))/beta_Rm)
}

ratioTreynor(data_renta$P1, data_renta$rf, data_renta$marche)
ratioTreynor(data_renta$P10, data_renta$rf, data_renta$marche)
ratioTreynor(data_renta$P10P1, data_renta$rf, data_renta$marche)

#t.test((data_renta$P1-data_renta$rf)/mean(data_beta$P1), mu=0)
#t.test((data_renta$P10-data_renta$rf)/mean(data_beta$P10), mu=0)
#t.test((data_renta$P10P1-data_renta$rf)/mean(data_beta$P10P1), mu=0)

alphaJensen <- function(vect_return, vect_risk_free_return, vect_market){
  rmRf = vect_market - vect_risk_free_return
  reg8_Rm  <- lm(vect_return - vect_risk_free_return ~ rmRf)
  alpha <- reg8_Rm$coefficients[1]
  beta_Rm <- reg8_Rm$coefficients[2]
  print("ratio")
  ratio <- alpha / beta_Rm
  print(ratio)
  print("test")
  t.test((vect_return-vect_risk_free_return)-beta_Rm*(mean(rmRf)), mu=0)
}

alphaJensen(data_renta$P1, data_renta$rf, data_renta$marche)
alphaJensen(data_renta$P10, data_renta$rf, data_renta$marche)
alphaJensen(data_renta$P10P1, data_renta$rf, data_renta$marche)

t.test((data_renta$P1-data_renta$rf)-mean(data_beta$P1)*(mean(data_renta$marche)-mean(data_renta$rf)), mu=0)
t.test((data_renta$P10-data_renta$rf)-mean(data_beta$P10)*(mean(data_renta$marche)-mean(data_renta$rf)), mu=0)
t.test((data_renta$P10P1-data_renta$rf)-mean(data_beta$P10P1)*(mean(data_renta$marche)-mean(data_renta$rf)), mu=0)

alphaJensenNormalise <- function(vect_return, vect_risk_free_return, vect_beta, vect_market){
  return(alphaJensen(vect_return, vect_risk_free_return, vect_beta, vect_market)/mean(vect_beta))
}

alphaJensenNormalise(data_renta$P1, data_renta$rf, data_beta$P1, data_renta$marche)
alphaJensenNormalise(data_renta$P10, data_renta$rf, data_beta$P10, data_renta$marche)
alphaJensenNormalise(data_renta$P10P1, data_renta$rf, data_beta$P10P1, data_renta$marche)

alphaFF <- function(vect_return, vect_risk_free_return, vect_market, vect_smb, vect_hml){
  rmRf = vect_market - vect_risk_free_return
  reg8_Rm  <- lm(vect_return - vect_risk_free_return ~ rmRf + vect_smb + vect_hml)
  print(reg8_Rm)
  alpha_ff <- reg8_Rm$coefficients[1]
  beta_rm <- reg8_Rm$coefficients[2]
  beta_smb <- reg8_Rm$coefficients[3]
  beta_hml <- reg8_Rm$coefficients[4]
  print("alpfa_ff")
  print(alpha_ff)
  t.test((vect_return - vect_risk_free_return)-beta_rm*(vect_market-vect_risk_free_return)-beta_smb*vect_smb-beta_hml*vect_hml, mu=0)
  #return(alphaJensen(vect_return, vect_risk_free_return, vect_beta, vect_market)-beta_smb*smb-beta_hml*hml)
}

alphaFF(data_renta$P1, data_renta$rf, data_renta$marche, data_betaSMB$SMB, data_betaHML$HML)
alphaFF(data_renta$P10, data_renta$rf, data_renta$marche, data_betaSMB$SMB, data_betaHML$HML)
alphaFF(data_renta$P10P1, data_renta$rf, data_renta$marche, data_betaSMB$SMB, data_betaHML$HML)

#t.test((data_renta$P1-data_renta$rf)-mean(data_beta$P1)*(mean(data_renta$marche)-mean(data_renta$rf))-mean(data_betaSMB$P1)*mean(data_betaSMB$SMB)-mean(data_betaHML$P1)*mean(data_betaHML$HML), mu=0)
#t.test((data_renta$P10-data_renta$rf)-mean(data_beta$P10)*(mean(data_renta$marche)-mean(data_renta$rf))-mean(data_betaSMB$P10)*mean(data_betaSMB$SMB)-mean(data_betaHML$P10)*mean(data_betaHML$HML), mu=0)
#t.test((data_renta$P10P1-data_renta$rf)-mean(data_beta$P10P1)*(mean(data_renta$marche)-mean(data_renta$rf))-mean(data_betaSMB$P10P1)*mean(data_betaSMB$SMB)-mean(data_betaHML$P10P1)*mean(data_betaHML$HML), mu=0)

alphaCahart <- function(vect_return, vect_risk_free_return, vect_market, vect_smb, vect_hml, vect_umd){
  rmRf = vect_market - vect_risk_free_return
  reg8_Rm  <- lm(vect_return - vect_risk_free_return ~ rmRf + vect_smb + vect_hml + vect_umd)
  print(reg8_Rm)
  alpha_cahart <- reg8_Rm$coefficients[1]
  beta_rm <- reg8_Rm$coefficients[2]
  beta_smb <- reg8_Rm$coefficients[3]
  beta_hml <- reg8_Rm$coefficients[4]
  beta_umd <- reg8_Rm$coefficients[5]
  print("alpfa_cahart")
  print(alpha_cahart)
  t.test((vect_return - vect_risk_free_return)-beta_rm*(vect_market-vect_risk_free_return)-beta_smb*vect_smb-beta_hml*vect_hml -beta_umd*vect_umd, mu=0)
  
  #return(alphaFF(vect_return, vect_risk_free_return, vect_beta, vect_market, vect_beta_smb, vect_smb, vect_beta_hml, vect_hml)-beta_umd*umd)
}

alphaCahart(data_renta$P1, data_renta$rf, data_renta$marche,  data_betaSMB$SMB, data_betaHML$HML,  data_betaMOM$MOM)
alphaCahart(data_renta$P10, data_renta$rf, data_renta$marche,data_betaSMB$SMB,  data_betaHML$HML,  data_betaMOM$MOM)
alphaCahart(data_renta$P10P1, data_renta$rf,  data_renta$marche, data_betaSMB$SMB, data_betaHML$HML, data_betaMOM$MOM)

#t.test((data_renta$P1-data_renta$rf)-mean(data_beta$P1)*(mean(data_renta$marche)-mean(data_renta$rf))-mean(data_betaSMB$P1)*mean(data_betaSMB$SMB)-mean(data_betaHML$P1)*mean(data_betaHML$HML)-mean(data_betaMOM$P1)*mean(data_betaMOM$MOM), mu=0)
#t.test((data_renta$P10-data_renta$rf)-mean(data_beta$P10)*(mean(data_renta$marche)-mean(data_renta$rf))-mean(data_betaSMB$P10)*mean(data_betaSMB$SMB)-mean(data_betaHML$P10)*mean(data_betaHML$HML)-mean(data_betaMOM$P10)*mean(data_betaMOM$MOM), mu=0)
#t.test((data_renta$P10P1-data_renta$rf)-mean(data_beta$P10P1)*(mean(data_renta$marche)-mean(data_renta$rf))-mean(data_betaSMB$P10P1)*mean(data_betaSMB$SMB)-mean(data_betaHML$P10P1)*mean(data_betaHML$HML)-mean(data_betaMOM$P10P1)*mean(data_betaMOM$MOM), mu=0)

