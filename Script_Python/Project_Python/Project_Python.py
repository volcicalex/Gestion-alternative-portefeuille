# import modules
import pandas as pd
import numpy as np
from operator import mul
import operator

def mult(liste):
    r = 1
    for element in liste:
        r *= 1 + element
    return r

# Import the excel file and call it xls_file
xls_file = pd.ExcelFile("../../Excel/Selected_Actif_85-05.xlsx")

# Load the xls file's Sheet1 as a dataframe
df = xls_file.parse('Feuil1')

stock_numbers = df.stock_number.unique()[0:100]

def rentab(annee_periode, mois_periode, taille_periode):
	dico_rentab = dict()
	for k in stock_numbers:
		tab_k = df[(df["stock_number"] == k) & (df["year"] == annee_periode) & (df["month"] >= mois_periode) & (df["month"] < mois_periode + taille_periode)]
		renta = mult(tab_k["return_rf"] + tab_k["RiskFreeReturn"])
		dico_rentab[k] = renta - 1
	return dico_rentab

def composition_portefeuille(annee_periode, taille_periode):
	rentabilites = rentab(annee_periode - 1, 12 - taille_periode + 1, taille_periode)
	sorted_rentabilites = sorted(rentabilites.items(), key = operator.itemgetter(1))
	return [sorted_rentabilites[10*i:10*(i+1)] for i in range(10)]

def rentabilite_portefeuille(portefeuille):
	n = len(portefeuille)
	renta = 0
	for k in range(n):
		renta += portefeuille[k][1]
	return renta / n

P8 = composition_portefeuille(1995, 6)[7]
print(rentabilite_portefeuille(P8))
