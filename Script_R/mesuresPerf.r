library(readxl)

csv1 <- read.csv2(file = "../Excel/renta85-05.csv", header = TRUE)
data_renta <- as.data.frame(csv1)
csv2 <- read.csv2(file = "../Excel/beta85-05.csv", header = TRUE)
data_beta <- as.data.frame(csv2)
csv3 <- read.csv2(file = "../Excel/betaSMB85-05.csv", header = TRUE)
data_beta <- as.data.frame(csv3)
csv4 <- read.csv2(file = "../Excel/betaHML85-05.csv", header = TRUE)
data_beta <- as.data.frame(csv4)
csv5 <- read.csv2(file = "../Excel/betaMOM85-05.csv", header = TRUE)
data_beta <- as.data.frame(csv5)

ratioSharpe <- function(vect_return, vect_risk_free_return){
  return((mean(vect_return)-mean(vect_risk_free_return))/sd(vect_return))
}

pValeurRatioSharpe <- function(vect_return, vect_risk_free_return){
  t.test((vect_return - vect_risk_free_return)/sd(vect_return), mu = 0) 
}

ratioSharpe(data_renta$P1, data_renta$rf)
ratioSharpe(data_renta$P10, data_renta$rf)
ratioSharpe(data_renta$P10P1, data_renta$rf)

t.test((data_renta$P1-data_renta$rf)/sd(data_renta$P1), mu=0)
t.test((data_renta$P10-data_renta$rf)/sd(data_renta$P10), mu=0)
t.test((data_renta$P10P1-data_renta$rf)/sd(data_renta$P10P1), mu=0)

ratioTreynor <- function(vect_return, vect_risk_free_return, vect_beta){
  return((mean(vect_return)-mean(vect_risk_free_return))/mean(vect_beta))
}

ratioTreynor(data_renta$P1, data_renta$rf, data_beta$P1)
ratioTreynor(data_renta$P10, data_renta$rf, data_beta$P10)
ratioTreynor(data_renta$P10P1, data_renta$rf, data_beta$P10P1)

t.test((data_renta$P1-data_renta$rf)/mean(data_beta$P1), mu=0)
t.test((data_renta$P10-data_renta$rf)/mean(data_beta$P10), mu=0)
t.test((data_renta$P10P1-data_renta$rf)/mean(data_beta$P10P1), mu=0)

alphaJensen <- function(vect_return, vect_risk_free_return, vect_beta, vect_market){
  rp <- mean(vect_return)
  rf <- mean(vect_risk_free_return)
  beta <- mean(vect_beta)
  rm <- mean(vect_market)
  return((rp-rf)-beta*(rm-rf))
}

alphaJensen(data_renta$P1, data_renta$rf, data_beta$P1, data_renta$marche)
alphaJensen(data_renta$P10, data_renta$rf, data_beta$P10, data_renta$marche)
alphaJensen(data_renta$P10P1, data_renta$rf, data_beta$P10P1, data_renta$marche)

t.test((data_renta$P1-data_renta$rf)-data_beta$P1*(data_renta$marche-data_renta$rf), mu=0)
t.test((data_renta$P10-data_renta$rf)-data_beta$P10*(data_renta$marche-data_renta$rf), mu=0)
t.test((data_renta$P10P1-data_renta$rf)-data_beta$P10P1*(data_renta$marche-data_renta$rf), mu=0)

alphaFF <- function(vect_return, vect_risk_free_return, vect_beta, vect_market, vect_beta_smb, vect_smb, vect_beta_hml, vect_hml){
  beta_smb <- mean(vect_beta_smb)
  smb <- mean(vect_smb)
  beta_hml <- mean(vect_beta_hml)
  hml <- mean(vect_hml)
  return(alphaJensen(vect_return, vect_risk_free_return, vect_beta, vect_market)-beta_smb*smb-beta_hml*hml)
}

alphaFF(data_renta$P1, data_renta$rf, data_beta$beta, data_renta$marche, data_betaSMB$P1, data_, data_betaHML$P1, data_)
alphaFF(data_renta$P10, data_renta$rf, data_beta$beta, data_renta$marche, data_betaSMB$P10, data_, data_betaHML$P10, data_)
alphaFF(data_renta$P10P1, data_renta$rf, data_beta$beta, data_renta$marche, data_betaSMB$P10P1, data_, data_betaHML$P10P1, data_)

t.test((data_renta$P1-data_renta$rf)-data_beta$P1*(data_renta$marche-data_renta$rf)-data_betaSMB$P1*data_betaSMB$SMB, mu=0)
t.test((data_renta$P10-data_renta$rf)-data_beta$P1*(data_renta$marche-data_renta$rf), mu=0)
t.test((data_renta$P10P1-data_renta$rf)-?*?-?*?-?*?, mu=0)


alphaCahart <- function(vect_return, vect_risk_free_return, vect_beta, vect_market, vect_beta_smb, vect_smb, vect_beta_hml, vect_hml, vect_beta_umd, vect_umd){
  beta_umd <- mean(vect_beta_umd)
  umd <- mean(vect_umd)
  return(alphaFF(vect_return, vect_risk_free_return, vect_beta, vect_market, vect_beta_smb, vect_smb, vect_beta_hml, vect_hml)-beta_umd*umd)
}


#alphaCahart(data_renta$P1, data_renta$Rf, ?, ?, ?, ?, ?, ?, ?)
#alphaCahart(data_renta$P10, data_renta$Rf, ?, ?, ?, ?, ?, ?, ?)
#alphaCahart(data_renta$P10-P1, data_renta$Rf, ?, ?, ?, ?, ?, ?, ?)
#t.test((data_renta$P1-data_renta$Rf)-?*?-?*?-?*?-?*?, mu=0)
#t.test((data_renta$P10-data_renta$Rf)-?*?-?*?-?*?-?*?, mu=0)
#t.test((data_renta$P10P1-data_renta$Rf)-?*?-?*?-?*?-?*?, mu=0)
