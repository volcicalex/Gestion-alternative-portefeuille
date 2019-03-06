library(readxl)

csv1 <- read.csv2(file = "../Excel/renta85-05.csv", header = TRUE)
data_renta <- as.data.frame(csv1)
csv2 <- read.csv2(file = "../Excel/beta85-05.csv", header = TRUE)
data_beta <- as.data.frame(csv2)
csv3 <- read.csv2(file = "../Excel/betaSMB85-05.csv", header = TRUE)
data_betaSMB <- as.data.frame(csv3)
csv4 <- read.csv2(file = "../Excel/betaHML85-05.csv", header = TRUE)
data_betaHML <- as.data.frame(csv4)
csv5 <- read.csv2(file = "../Excel/betaMOM85-05.csv", header = TRUE)
data_betaMOM <- as.data.frame(csv5)

ratioSharpe <- function(vect_return, vect_risk_free_return){
  return((mean(vect_return)-mean(vect_risk_free_return))/sd(vect_return))
}

ratioSharpe(data_renta$P1, data_renta$Rf)
ratioSharpe(data_renta$P10, data_renta$Rf)
#ratioSharpe(data_renta$P10-P1, data_renta$Rf)

ratioTreynor <- function(vect_return, vect_risk_free_return, vect_beta){
  return((mean(vect_return)-mean(vect_risk_free_return))/mean(vect_beta))
}

alphaJensen <- function(vect_return, vect_risk_free_return, vect_beta, vect_market){
  rp <- mean(vect_return)
  rf <- mean(vect_risk_free_return)
  beta <- mean(vect_beta)
  rm <- mean(vect_market)
  return((rp-rf)-beta*(rm-rf))
}

alphaFF <- function(vect_return, vect_risk_free_return, vect_beta, vect_market, vect_beta_smb, vect_smb, vect_beta_hml, vect_hml){
  beta_smb <- mean(vect_beta_smb)
  smb <- mean(vect_smb)
  beta_hml <- mean(vect_beta_hml)
  hml <- mean(vect_hml)
  return(alphaJensen(vect_return, vect_risk_free_return, vect_beta, vect_market)-beta_smb*smb-beta_hml*hml)
}

alphaFF(data_renta$P1, data_renta$rf, data_beta$P1, data_renta$marche, data_betaSMB$P1, data_betaSMB$SMB, data_betaHML$P1, data_betaHML$HML)
alphaFF(data_renta$P10, data_renta$rf, data_beta$P10, data_renta$marche, data_betaSMB$P10, data_betaSMB$SMB, data_betaHML$P10, data_betaHML$HML)
alphaFF(data_renta$P10P1, data_renta$rf, data_beta$P10P1, data_renta$marche, data_betaSMB$P10P1, data_betaSMB$SMB, data_betaHML$P10P1, data_betaHML$HML)

t.test((data_renta$P1-data_renta$rf)-data_beta$P1*(data_renta$marche-data_renta$rf)-data_betaSMB$P1*data_betaSMB$SMB-data_betaHML$P1*data_betaHML$HML, mu=0)
t.test((data_renta$P10-data_renta$rf)-data_beta$P1*(data_renta$marche-data_renta$rf)-data_betaSMB$P10*data_betaSMB$SMB-data_betaHML$P10*data_betaHML$HML, mu=0)
t.test((data_renta$P10P1-data_renta$rf)-data_beta$P1*(data_renta$marche-data_renta$rf)-data_betaSMB$P10P1*data_betaSMB$SMB-data_betaHML$P10P1*data_betaHML$HML, mu=0)


alphaCahart <- function(vect_return, vect_risk_free_return, vect_beta, vect_market, vect_beta_smb, vect_smb, vect_beta_hml, vect_hml, vect_beta_umd, vect_umd){
  beta_umd <- mean(vect_beta_umd)
  umd <- mean(vect_umd)
  return(alphaFF(vect_return, vect_risk_free_return, vect_beta, vect_market, vect_beta_smb, vect_smb, vect_beta_hml, vect_hml)-beta_umd*umd)
}

alphaCahart(data_renta$P1, data_renta$rf, data_beta$P1, data_renta$marche, data_betaSMB$P1, data_betaSMB$SMB, data_betaHML$P1, data_betaHML$HML, data_betaMOM$P1, data_betaMOM$MOM)
alphaCahart(data_renta$P10, data_renta$rf, data_beta$P10, data_renta$marche, data_betaSMB$P10, data_betaSMB$SMB, data_betaHML$P10, data_betaHML$HML, data_betaMOM$P1, data_betaMOM$MOM)
alphaCahart(data_renta$P10P1, data_renta$rf, data_beta$P10P1, data_renta$marche, data_betaSMB$P10P1, data_betaSMB$SMB, data_betaHML$P10P1, data_betaHML$HML, data_betaMOM$P1, data_betaMOM$MOM)

t.test((data_renta$P1-data_renta$rf)-data_beta$P1*(data_renta$marche-data_renta$rf)-data_betaSMB$P1*data_betaSMB$SMB-data_betaHML$P1*data_betaHML$HML-data_betaMOM$P1*data_betaMOM$MOM, mu=0)
t.test((data_renta$P10-data_renta$rf)-data_beta$P1*(data_renta$marche-data_renta$rf)-data_betaSMB$P10*data_betaSMB$SMB-data_betaHML$P10*data_betaHML$HML-data_betaMOM$P1*data_betaMOM$MOM, mu=0)
t.test((data_renta$P10P1-data_renta$rf)-data_beta$P1*(data_renta$marche-data_renta$rf)-data_betaSMB$P10P1*data_betaSMB$SMB-data_betaHML$P10P1*data_betaHML$HML-data_betaMOM$P1*data_betaMOM$MOM, mu=0)

