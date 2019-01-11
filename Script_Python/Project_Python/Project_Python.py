# import modules
import pandas as pd
import numpy as np
from operator import mul
from xlwt import Workbook
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

premiere_annee = 1985
derniere_annee = 2004
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

def fill_renta_excel(rentaFrame, rentas_P):
    for indice_P in range(1,11):
        rentaFrame['P' + str(indice_P)] = rentas_P[indice_P - 1]

def write_renta(taille_periode):
    index = [str(mois) + "/" + str(annee) for annee in range(premiere_annee, derniere_annee+1) for mois in range(1,13)]
    colonne = ['mois', 'annee', 'P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7', 'P8', 'P9', 'P10', 'Marché', 'Rf']
    rentaFrame = pd.DataFrame(columns = colonne)
    rentaFrame['annee'] = [annee for annee in range(premiere_annee, derniere_annee+1) for mois in range(1,13)]
    rentaFrame['mois'] = [mois for annee in range(premiere_annee, derniere_annee+1) for mois in range(1,13)]
    rentas_P = [[0]*(12*(derniere_annee+1-premiere_annee)) for i in range(10)]
    market = [0 for i in range (12*(derniere_annee+1-premiere_annee))]
    rf = [0 for i in range (12*(derniere_annee+1-premiere_annee))]
    for annee in range(premiere_annee, derniere_annee + 1):
        listePortefeuille = composition_portefeuille(annee, taille_periode)
        for indice_P in range(len(listePortefeuille)):
            P = listePortefeuille[indice_P]
            for mois in range(1,13): 
                renta_k = 0
                for action in P:
                    tab_k = df[(df["stock_number"] == action[0]) & (df["year"] == annee) & (df["month"] == mois)]
                    renta_k += mult(tab_k["return_rf"] + tab_k["RiskFreeReturn"])
                market[(annee-premiere_annee)*12 + mois - 1] = mult(tab_k["Marketretrun"])
                rf[(annee-premiere_annee)*12 + mois - 1] = mult(tab_k["RiskFreeReturn"]) - 1
                rentas_P[indice_P][(annee-premiere_annee)*12 + mois - 1] = renta_k/10 - 1
    rentaFrame['Rf'] = rf                
    fill_renta_excel(rentaFrame, rentas_P)
    rentaFrame.to_excel('../../Excel/renta85-05.xlsx')

def dico_pf(annee_periode, duree_prec):
    actifs_pf = {}
    for index, pf in enumerate(composition_portefeuille(annee_periode, duree_prec)):
        for actif in pf:
            actifs_pf[str(actif[0])] = "P" + str(index+1) 
    return  actifs_pf


# Creation du fichier associant chaque actif a un portefeuille en tout temps
# duree_etude correspond a la duree de l'etude en nombre d'annees (nombre entier)
# duree_prec est le nombre de mois precedents a prendre en compte pour constituer le pf
def fichier_compo(duree_etude, duree_prec) :
    # création du fichier de composition de pf
    fich = Workbook()
    # création de la feuille 1
    feuil1 = fich.add_sheet('feuille 1')
    # ajout des en-têtes
    feuil1.write(0,0,'Year')
    feuil1.write(0,1,'Month')
    for k in range(2, len(stock_numbers)+2):
        feuil1.write(0,k,str(stock_numbers[k-2]))
    for i, annee in enumerate(df.year.unique()[0:duree_etude]):
        for mois in df.month.unique()[0:12]:
            #colonne 1 : annee
            feuil1.write(int(12*i+mois),0,annee)
            #colonne 2 : mois
            feuil1.write(int(12*i+mois),1,mois)
            if mois == 1 :
                data = dico_pf(annee, duree_prec)
            for k in range(2,len(stock_numbers)+2):
                feuil1.write(int(12*i+mois),k, data.get(str(stock_numbers[k-2])))
    # création matérielle du fichier résultant
    fich.save('../../Excel/composition.xls')

fichier_compo(20,6)

