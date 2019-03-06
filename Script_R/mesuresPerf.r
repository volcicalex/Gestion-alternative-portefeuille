library(readxl)

data_renta <- data.frame(read_excel("../Excel/renta85-05.xlsx"))

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

alphaCahart <- function(vect_return, vect_risk_free_return, vect_beta, vect_market, vect_beta_smb, vect_smb, vect_beta_hml, vect_hml, vect_beta_umd, vect_umd){
  beta_umd <- mean(vect_beta_umd)
  umd <- mean(vect_umd)
  return(alphaFF(vect_return, vect_risk_free_return, vect_beta, vect_market, vect_beta_smb, vect_smb, vect_beta_hml, vect_hml)-beta_umd*umd)
}