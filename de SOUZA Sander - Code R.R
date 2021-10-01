#Début du script
t0 <- Sys.time()


#Bibliothèques
library(stringr)
library(openxlsx)
library(dplyr)
library(knitr)
library(rmarkdown)
library(markdown)
library(ggplot2)
library(lubridate)
library(tableone)
library(tinytex)
library(questionr)
library(esquisse)
library(plotly)

#Fonctions

prep <- function(cadena) #Gestion des accents avec tout en majuscule sans accent à la fin
{
  retour = stringr::str_squish(cadena)
  retour = str_replace(retour, pattern = "Ã©", replacement = "E")
  retour = str_replace(retour, pattern = "Ã©", replacement = "E")
  retour = str_replace(retour, pattern = "Ã¨", replacement = "E")
  retour = str_replace(retour, pattern = "Ãˆ", replacement = "E")
  
  retour = str_replace(retour, pattern = "Ã«", replacement = "E")
  retour = str_replace(retour, pattern = "Ã–", replacement = "O")
  retour = str_replace(retour, pattern = "ÃŒ", replacement = "U")
  
  retour = str_replace(retour, pattern = "Ã¯", replacement = "I")
  retour = str_replace(retour, pattern = "Ã§", replacement = "C")
  
  retour = str_replace(retour, pattern = "--", replacement = " ")
  retour = str_replace(retour, pattern = "-", replacement = " ")
  retour = str_replace(retour, pattern = "' ", replacement = " ")
  retour = str_replace(retour, pattern = "'", replacement = " ")
  retour = str_replace(retour, pattern = "/", replacement = " ")
  retour = toupper(retour)
  return(retour)
}

#Répertoire de travail sur mon disque dur
setwd("R")


#Données extraites de SYADEM et COLIBRI
etu = read.csv(file="ssu_patients.csv", header=FALSE)
colnames(etu) = c("id","prenom", "nom", "ddn", "genre", "creation_cve", "mail", "varicelle", "sero_varicelle", "creation_etu")
etu$prenom = prep(etu$prenom)
etu$nom = prep(etu$nom)
etu$varicelle[substr(etu$varicelle,1,5) == "NON R"] = "NR"

vaccins = read.csv(file="ssu_vaccinations.csv", header=FALSE)
colnames(vaccins) = c("id", "date", "nom", "date_saisie", "valid", "date_modif", "nom_ps", "type_ps", "valence")

colibri = read.csv(file="ssu_colibri.csv", header = FALSE)
colnames(colibri) = c("uid", "date", "type", "encours", "resultat_brut")



#Formalisation des données COLIBRI
colibri$type[substr(colibri$type,1,3) == "IDR"] = "IDR"
colibri$type[substr(colibri$type,1,3) == "Rub"] = "Rubeole"
colibri$type[colibri$type == "Anti-HBS"] = "Anti-HBs"
colibri = colibri[colibri$encours == "false",]
colibri = colibri[, c("uid", "date", "type", "resultat_brut")]

colibri$resultat = NA
colibri$resultat[colibri$type == "IDR"] = gsub("[a-zA-Z ]", "", colibri$resultat_brut[colibri$type == "IDR"])
colibri$resultat[colibri$type == "Anti-HBs"] = gsub("[a-zA-Z /]", "", colibri$resultat_brut[colibri$type == "Anti-HBs"])

for (b in c("Varicelle", "Ag HBs", "Anti-HBc", "Anti-VHA", "Rubeole", "Varicelle", "ADN VHB", "Rougeole"))
{
  colibri$resultat[colibri$type == b & colibri$resultat_brut == "true"] = 1
  colibri$resultat[colibri$type == b & colibri$resultat_brut == "false"] = 0
}

colibri$resultat = as.double(colibri$resultat)


exceptions = read.xlsx("exceptions.xlsx")
exceptions$prenom = prep(exceptions$prenom)
exceptions$nom = prep(exceptions$nom)

scola = read.xlsx("scola.xlsx")
scola$prenom = prep(scola$prenom)
scola$nom = prep(scola$nom)
scola$n_etu = as.character(scola$n_etu)

s2 = read.xlsx("s2.xlsx")
scola$s2 = scola$n_etu %in% s2$n_etu
s2$inscola = s2$n_etu %in% scola$n_etu[scola$s2==TRUE]
manquants = s2$n_etu[s2$inscola==FALSE]
manquants


scola$devenir="EN ATTENTE"
devenir = read.xlsx("devenir.xlsx")
for (s in 1:nrow(devenir))
{
  if(devenir$n_etu[s] %in% scola$n_etu)
  {
    scola[scola$n_etu == devenir$n_etu[s],]$devenir = devenir$devenir[s]
  }
}

scola$devenir[scola$devenir == "EN ATTENTE"] = "AJOURNE"


scola$acces="---"
acces = read.xlsx("acces.xlsx")
for (s in 1:nrow(acces))
{
  if(acces$n_etu[s] %in% scola$n_etu)
  {
    scola[scola$n_etu == acces$n_etu[s],]$acces = acces$acces[s]
  }
}


#Constantes
chaine_exceptions=c("dtp", "vhb", "idr", "ror", "coq", "men", "var", "bcg")
professions=c("doctor"="MEDECIN", "pharmacist"="PHARMACIEN", "nurse"="INFIRMIER-E","midwife"="SAGE-FEMME")
rentree = "2020-09-01"
liste_ssu = c("ISABELLE CARRIER", "CAROLINE COMBES", "SANDER DE SOUZA") #À compléter ici : les noms des professionnels du centre de santé qui valident les CVE tels qu'ils apparaissent sur MesVaccins.net



bdd = data.frame(uid=NA, lieu=NA, filiere=NA, s2=NA, devenir=NA, acces=NA, nom=NA,prenom=NA, n_etu=NA, annee_univ=NA, ddn=NA, mail=NA, creation_cve=NA, mois_cve=NA, creation_etu=NA, adn=NA, age2021 = NA, genre=NA, inj_tot=NA, inj_invalide=NA, inj_invalide_pcent=NA, dtp_nb=NA, dtp_date=NA, dtp_recent=NA, dtp_age=NA, dtp_verdict=NA, vhb_nb=NA, vhb_date=NA, vhb_recent=NA, hbs_resultat=NA, hbs_date=NA, hbc_resultat=NA, hbc_date=NA, vhb_verdict=NA, idr_mm=NA, idr_date=NA, idr_verdict=NA, ror_nb=NA, ror_date=NA, ror_recent=NA, ror_verdict=NA, coq_date=NA, coq_recent=NA, coq_age=NA, coq_verdict=NA, coq_simul=NA, men_nb=NA, men_date=NA, men_recent=NA, men_verdict=NA, var_atcd=NA, var_sero=NA, var_nb=NA, var_date=NA, var_recent=NA, var_verdict=NA, bcg_nb=NA, bcg_verdict=NA, covid_nb=NA, covid_verdict=NA, obl_verdict=NA, reco_verdict=NA, erreurs=NA, liste_exceptions=NA, nb_validateurs=NA, noms_validateurs=NA, validation_ssu = NA)

bdd_invalide = bdd

for (i in 1:nrow(etu))
{
  
  #UID de MesVaccins.net
  prov_uid = etu$id[i]
  
  #Genre : RAS
  prov_genre = etu$genre[i]
  
  #Nom et prénom
  prov_nom = etu$nom[i]
  prov_prenom = etu$prenom[i]
  
  prov_vecteur_dist = vector()
  
  
  for(h in 1:nrow(scola))
  {
    prov_vecteur_dist <- c(prov_vecteur_dist, adist(x=paste(prov_prenom,prov_nom), y=paste(scola$prenom[h],scola$nom[h]))[1,1])
    #Distance de Levenshtein
  }
  
  prov_corresp = length(prov_vecteur_dist[prov_vecteur_dist <= 1])
  
  
  if(prov_corresp == 1)
  {
    
    prov_indice_match = which(prov_vecteur_dist<=1)

    prov_lieu = scola$lieu[prov_indice_match]
    prov_filiere = scola$filiere[prov_indice_match]
    prov_s2 = scola$s2[prov_indice_match]
    prov_n_etu = scola$n_etu[prov_indice_match]
    prov_devenir=scola$devenir[prov_indice_match]
    prov_acces=scola$acces[prov_indice_match]
    prov_annee_univ = str_c("20",substr(scola$n_etu[prov_indice_match], 2,3))
  }  else  {
    if(prov_corresp == 0)
    {
      prov_lieu = "INCONNU"
      prov_filiere = "INCONNU"
      prov_n_etu = "INCONNU"
      prov_annee_univ = "2020"
      prov_devenir = "INCONNU"
      prov_s2 = "INCONNU"
    }
    if(prov_corresp > 1)
    {
      prov_lieu = "AMBIGUITE"
      prov_filiere = "AMBIGUITE"
      prov_n_etu = "AMBIGUITE"
      prov_annee_univ = "2020"
      prov_devenir = "AMBIGUITE"
      prov_s2 = "AMBIGUITE"
    }
  }
  
  
  #Gestion des exceptions
  if(prov_corresp == 1)
  {
    if(sum(str_count(string = exceptions$n_etu, pattern=as.character(prov_n_etu)) == 1))
    {
      prov_exceptionOUI = TRUE
      prov_indice_exception = which(exceptions$n_etu == prov_n_etu)
    }
    else
    {
      prov_exceptionOUI = FALSE
      prov_indice_exception = 0
    }
  }
  
  #Date de naissance et âge
  prov_ddn = etu$ddn[i]
  prov_adn = as.numeric(substr(prov_ddn,1,4))
  prov_age2021 = 2021-prov_adn
  
  prov_vaccins = vaccins[vaccins$id==etu$id[i],]
  prov_vaccins$date = as.Date(prov_vaccins$date)
  
  prov_colibri = colibri[colibri$uid==prov_uid,]
  prov_colibri$date = as.Date(prov_colibri$date)
  
  
  #Date de création du CVE et si l'étudiant l'a créé lui-même ou non
  prov_creation_cve = substr(etu$creation_cve[i],1,10)
  prov_mois_cve = ifelse(test=(prov_creation_cve!=""), yes = substr(etu$creation_cve[i],1,7), no = "N/A")
  prov_creation_etu = etu$creation_etu[i]
  
  #Adresse électronique
  prov_mail = etu$mail[i]
  
  #Nombre d'injections totales
  prov_inj_tot = nrow(prov_vaccins)
  prov_inj_invalide = nrow(prov_vaccins[prov_vaccins$valid == "not_validated",])
  prov_inj_invalide_pcent = round((prov_inj_invalide / prov_inj_tot)*100, digits = 1)
  
  for (v in 1:2)
  {
    #On ne prend en compte que les vaccins validés pour l'une des deux bases de données
    if(v == 2)
    {
      prov_vaccins = prov_vaccins[prov_vaccins$valid != "not_validated",]
    }
    
    #DTP
    prov_vaccins$ouiDTP = str_count(string = prov_vaccins$valence, pattern= regex("(d|D);T;.*IPV"))
    prov_dtp_nb = sum(prov_vaccins$ouiDTP)
    prov_dtp_date = as.Date(max(prov_vaccins$date[prov_vaccins$ouiDTP == 1]))
    prov_dtp_recent = ifelse(test=prov_dtp_date>=as.Date(str_c(prov_annee_univ, "-01-01")), yes="OUI", no="NON")
    prov_dtp_age = as.numeric(substr(max(prov_vaccins$date[prov_vaccins$ouiDTP == 1]), 1,4)) - prov_adn
    prov_dtp_verdict = ifelse(test = ((prov_dtp_nb >= 4)&((prov_age2021>=25 & prov_dtp_age >= 25)|(prov_age2021<25 & prov_dtp_age >= 11))), yes="AJ", no="NAJ")
    
    #VHB
    prov_vaccins$ouiVHB = str_count(string = prov_vaccins$valence, pattern="HepB")
    prov_vhb_nb = sum(prov_vaccins$ouiVHB)
    prov_vhb_date = as.Date(max(prov_vaccins$date[prov_vaccins$ouiVHB == 1]))
    prov_vhb_recent = ifelse(test=prov_vhb_date>=as.Date(str_c(prov_annee_univ, "-01-01")), yes="OUI", no="NON")
    
    prov_colibri$ouiHBS = str_count(string = prov_colibri$type, pattern="Anti-HBs")
    if(sum(prov_colibri$ouiHBS) >= 1)
    {
      prov_hbs_date = as.Date(max(prov_colibri$date[prov_colibri$ouiHBS == 1]))[1]
      prov_hbs_resultat = prov_colibri[prov_colibri$date==prov_hbs_date & prov_colibri$type=="Anti-HBs",]$resultat
    } else  {
      prov_hbs_resultat = NA
      prov_hbs_date = NA
    }
    
    
    prov_colibri$ouiHBC = str_count(string = prov_colibri$type, pattern="Anti-HBc")
    if(sum(prov_colibri$ouiHBC) >= 1)
    {
      prov_hbc_date = as.Date(max(prov_colibri$date[prov_colibri$ouiHBC == 1]))[1]
      prov_hbc_resultat = ifelse(prov_colibri[prov_colibri$date==prov_hbc_date & prov_colibri$type=="Anti-HBc",]$resultat, "POS", "NEG")
      
    } else  {
      prov_hbc_resultat = NA
      prov_hbc_date = NA
    }
    
    
    if(!is.na(prov_hbs_resultat))
    {
      
      if(!is.na(prov_hbc_resultat))
      {
        if(prov_hbs_resultat >= 10)
        {
          if(prov_vhb_nb >= 3)
          {
            prov_vhb_verdict = "ACCEPTABLE"
          }
          else {
            prov_vhb_verdict = "SCHEMA INCOMPLET"
          }
        } else {
          if(prov_vhb_nb < 6)
          {
            prov_vhb_verdict = "CONTINUER A VACCINER"
          }
          else {
            prov_vhb_verdict = "NON REPONDEUR"
          }
        }
        
      } else {
        prov_vhb_verdict = "ANTI-HBC MANQUANTS"
        prov_hbc_resultat = NA
        prov_hbc_date = NA
      }
      
      if(prov_hbs_resultat > 100)
      {
        prov_vhb_verdict = "SUFFISANT"
      }
    }
    
    
    else{
      prov_vhb_verdict = "ANTI-HBS MANQUANTS"
    }
    
    #IDR
    prov_colibri$ouiIDR = str_count(string = prov_colibri$type, pattern="IDR")
    
    if(sum(prov_colibri$ouiIDR) >= 1)
    {
      prov_idr_date = as.Date(max(prov_colibri$date[prov_colibri$ouiIDR == 1]))[1]
      prov_idr_annee = as.double(substr(prov_idr_date, 1,4))
      prov_idr_mm = prov_colibri[prov_colibri$date==prov_idr_date & prov_colibri$type=="IDR",]$resultat
      
      
      if(prov_idr_annee >= as.double(prov_annee_univ))
      {
        prov_idr_verdict = "FAIT"
      }
      else
      {
        prov_idr_verdict = "AVANT"
      }
      
    }
    else
    {
      prov_idr_date = NA
      prov_idr_mm = NA
      prov_idr_verdict = "NON FAIT"
    }
    
    
    #ROR
    prov_vaccins$ouiROR = str_count(string = prov_vaccins$valence, pattern="Roug;Or-L;Rub") + str_count(string = prov_vaccins$valence, pattern="RougVivant;Or-L;Rub") + str_count(string = prov_vaccins$valence, pattern="Rub;Or-L;Roug") + str_count(string = prov_vaccins$nom, pattern=regex("VACCIN ROR"))
    prov_ror_nb = sum(prov_vaccins$ouiROR)
    prov_ror_date = as.Date(max(prov_vaccins$date[prov_vaccins$ouiROR == 1]))
    prov_ror_recent = ifelse(test=prov_ror_date>=as.Date(str_c(prov_annee_univ, "-01-01")), yes="OUI", no="NON")
    prov_ror_verdict = ifelse(test = ((prov_adn >= 1980 & prov_ror_nb >= 2 ) |(prov_adn < 1980 & prov_ror_nb >= 1)), yes="AJ", no="NAJ")
    
    #Coqueluche
    prov_vaccins$ouiCOQ = str_count(string = prov_vaccins$valence, pattern= regex(".*(Ca|Ce|ca).*"))
    prov_coq_date = as.Date(max(prov_vaccins$date[prov_vaccins$ouiCOQ == 1]))
    prov_coq_recent = ifelse(test=prov_coq_date>=as.Date(str_c(prov_annee_univ, "-01-01")), yes="OUI", no="NON")
    prov_coq_age = as.numeric(substr(max(prov_vaccins$date[prov_vaccins$ouiCOQ == 1]), 1,4)) - prov_adn
    prov_coq_verdict = ifelse(test = ((prov_age2021>=25 & prov_coq_age >= 25)|(prov_age2021<25 & prov_coq_age >= 11)), yes="AJ", no="NAJ")
    
    if(is.na(prov_coq_verdict))
    {
      prov_coq_verdict = "NAJ"
    }
    
    if(!is.na(prov_dtp_age) & !is.na(prov_coq_age))
    {
      prov_coq_simul = ifelse(test=(prov_dtp_age == prov_coq_age), yes="OUI", no="NON")
    }
    else
    {
      prov_coq_simul = "NC"
    }
    
    #Méningocoque C
    prov_vaccins$ouiMEN = str_count(string = prov_vaccins$valence, pattern=regex("MCV-C|MPV-AC|MPV4"))
    prov_men_nb = sum(prov_vaccins$ouiMEN)
    prov_men_date = as.Date(max(prov_vaccins$date[prov_vaccins$ouiMEN == 1]))
    prov_men_recent = ifelse(test=prov_men_date>=as.Date(str_c(prov_annee_univ, "-01-01")), yes="OUI", no="NON")
    prov_men_verdict = ifelse(test=(prov_men_nb >= 1), yes="AJ", no=ifelse(test = (prov_age2021 <= 24), yes="NAJ", no="TROP TARD"))
    
    #Varicelle
    prov_var_atcd = etu$varicelle[i]
    prov_var_sero = ifelse(test=(length(colibri$resultat[colibri$uid==prov_uid & colibri$type=="Varicelle" & colibri$resultat==1]) >= 1) | (etu$sero_varicelle[etu$id==prov_uid] == "OUI"), yes="OUI", no="NON")
    prov_vaccins$ouiVAR = str_count(string = prov_vaccins$valence, pattern="Var")
    prov_var_nb = sum(prov_vaccins$ouiVAR)
    prov_var_date = as.Date(max(prov_vaccins$date[prov_vaccins$ouiVAR == 1]))
    prov_var_recent = ifelse(test=prov_var_date>=as.Date(str_c(prov_annee_univ, "-01-01")), yes="OUI", no="NON")
    prov_var_verdict = ifelse(test = (prov_var_atcd == "OUI" | (prov_var_sero == "OUI") | prov_var_nb >= 2), yes="AJ", no="NAJ")
    
    #BCG
    prov_vaccins$ouiBCG = str_count(string = prov_vaccins$valence, pattern=regex("BCG")) + str_count(string = prov_vaccins$nom, pattern=regex("VACCIN BCG"))
    prov_bcg_nb = sum(prov_vaccins$ouiBCG)
    prov_bcg_verdict = ifelse(test=(prov_bcg_nb >= 1), yes="FAIT", no="NON FAIT")
    
    #Covid19
    prov_vaccins$ouiCOVID = str_count(string = prov_vaccins$valence, pattern=regex(".*Covid.*"))
    prov_covid_nb = sum(prov_vaccins$ouiCOVID)
    prov_covid_verdict = ifelse(test=(prov_covid_nb >= 1), yes="AU MOINS UNE DOSE", no="NON FAIT")
    
    
    #Gestion des exceptions
    if(prov_exceptionOUI)
    {
      prov_liste_exceptions = "Exceptions : "
      for (k in 1:length(chaine_exceptions))
      {
        if(!is.na(exceptions[exceptions$n_etu==prov_n_etu,][1,chaine_exceptions[k]]))
        {
          assign(x = str_c("prov_", chaine_exceptions[k], "_verdict"), value=exceptions[exceptions$n_etu==prov_n_etu,][1,chaine_exceptions[k]])
          prov_liste_exceptions = str_c(prov_liste_exceptions, chaine_exceptions[k],"; ")
        }
      }
    }
    else
    {
      prov_liste_exceptions = "N/A"
    }
    
    prov_erreurs = ""
    
    
    
    ## Liste des messages d'erreur
    if(prov_dtp_verdict =="NAJ")
    {
      if(prov_dtp_nb < 4)
      {
        prov_erreurs = paste(prov_erreurs, "Nombre de DTP < 4;")
      }
      
      if(prov_dtp_nb & prov_age2021 < 25 & prov_dtp_age < 11)
      {
        prov_erreurs = paste(prov_erreurs, "Dernier DTP fait avant 11 ans;")
      }
      
      if(prov_dtp_nb & prov_age2021 >= 25 & prov_dtp_age < 25)
      {
        prov_erreurs = paste(prov_erreurs, "Dernier DTP fait avant 25 ans;")
      }
    }
    
    
    if(prov_vhb_verdict == "NAJ" & is.na(prov_hbs_resultat))
    {
      prov_erreurs = paste(prov_erreurs, "Pas d'anti-HBs trouvé;")
    }
    
    if(prov_vhb_verdict == "NAJ" & !is.na(prov_hbs_resultat) & prov_hbs_resultat < 10)
    {
      prov_erreurs = paste(prov_erreurs, "Anti-HBs inférieurs à 10;") 
    }
    
    if(prov_vhb_verdict == "NAJ" & !is.na(prov_hbs_resultat) & prov_hbs_resultat >= 10 & prov_hbs_resultat <= 100 & is.na(prov_hbc_resultat))
    {
      prov_erreurs = paste(prov_erreurs, "Anti-HBs entre 10 et 100 sans anti-HBc;") 
    }
    
    if(prov_idr_verdict == "NON FAIT")
    {
      prov_erreurs = paste(prov_erreurs, "IDR non faite;") 
    }
    
    if(prov_idr_verdict == "AVANT")
    {
      prov_erreurs = paste(prov_erreurs, "IDR faite avant l'entrée dans les études;") 
    }
    
    
    if(prov_coq_verdict =="NAJ")
    {
      if(is.na(prov_coq_age))
      {
        prov_erreurs = paste(prov_erreurs, "Aucune trace de coqueluche;")
      }
      
      if(!is.na(prov_coq_age) & prov_age2021 < 25 & prov_coq_age < 11)
      {
        prov_erreurs = paste(prov_erreurs, "Dernier coqueluche avant 11 ans;")
      }
      
      if(!is.na(prov_coq_age) & prov_age2021 >= 25 & prov_coq_age < 25)
      {
        prov_erreurs = paste(prov_erreurs, "Dernier coqueluche avant 25 ans;")
      }
    }
    
    if(prov_ror_verdict =="NAJ")
    {
      prov_erreurs = paste(prov_erreurs, "Nombre ROR < 2;")
    }    
    
    if(prov_men_verdict =="NAJ")
    {
      prov_erreurs = paste(prov_erreurs, "Méningocoque C non fait;")
    }  
    
    if(prov_var_verdict =="NAJ")
    {
      prov_erreurs = paste(prov_erreurs, "Pas d'immunisation (antécédent ou deux doses de vaccin) trouvée contre la varicelle;")
    }  
    
    
    ## Décision finale
    prov_obl_verdict = ifelse(test=((prov_dtp_verdict=="AJ")&(prov_vhb_verdict %in% c("SUFFISANT", "ACCEPTABLE"))&(prov_idr_verdict=="FAIT")), yes="AJ", no="NAJ")
    prov_reco_verdict = ifelse(test=((prov_obl_verdict=="AJ")&(prov_ror_verdict=="AJ")&(prov_coq_verdict=="AJ")&(prov_men_verdict!="NAJ")&(prov_var_verdict=="AJ")), yes="AJ", no="NAJ")
    
    
    
    ### Validateurs du CVE (professionnels de santé)
    
    if(length(prov_vaccins$nom_ps) > 0)
    {
      prov_noms_validateurs = unique(prov_vaccins$nom_ps)
      prov_types_validateurs = unique(prov_vaccins$type_ps)
      prov_display_validateurs = paste( str_c(prep(prov_noms_validateurs)," (",professions[prov_types_validateurs],")"), collapse = " ; ")
      prov_nb_validateurs = length(prov_noms_validateurs)
      prov_validation_ssu = ifelse(test=(sum(str_count(pattern=liste_ssu, string=prep(prov_display_validateurs))) >= 1), yes="OUI", no="NON")
    }
    else
    {
      prov_display_validateurs = "N/A"
      prov_nb_validateurs = 0
      prov_validation_ssu = "N/A"
    }
    
    
    nouvelles_donnees = c(prov_uid, prov_lieu, prov_filiere, prov_s2, prov_devenir, prov_acces, prov_nom, prov_prenom, prov_n_etu, prov_annee_univ, prov_ddn, prov_mail, prov_creation_cve, prov_mois_cve, prov_creation_etu, prov_adn, prov_age2021, prov_genre, prov_inj_tot, prov_inj_invalide, prov_inj_invalide_pcent, prov_dtp_nb, format(prov_dtp_date, format="%Y-%m-%d"), prov_dtp_recent, prov_dtp_age, prov_dtp_verdict, prov_vhb_nb, format(prov_vhb_date, format="%Y-%m-%d"), prov_vhb_recent, prov_hbs_resultat, format(prov_hbs_date, format="%Y-%m-%d"), prov_hbc_resultat, format(prov_hbc_date, format="%Y-%m-%d"), prov_vhb_verdict, prov_idr_mm, format(prov_idr_date, format="%Y-%m-%d"), prov_idr_verdict, prov_ror_nb, format(prov_ror_date, format="%Y-%m-%d"), prov_ror_recent, prov_ror_verdict, format(prov_coq_date, format="%Y-%m-%d"), prov_coq_recent, prov_coq_age, prov_coq_verdict, prov_coq_simul, prov_men_nb, format(prov_men_date, format="%Y-%m-%d"), prov_men_recent, prov_men_verdict, prov_var_atcd, prov_var_sero, prov_var_nb, format(prov_var_date, format="%Y-%m-%d"), prov_var_recent, prov_var_verdict, prov_bcg_nb, prov_bcg_verdict, prov_covid_nb, prov_covid_verdict, prov_obl_verdict, prov_reco_verdict, prov_erreurs, prov_liste_exceptions, prov_nb_validateurs, prov_display_validateurs, prov_validation_ssu)
    
    if (v == 2)
    {
      bdd[i,] = nouvelles_donnees
    }
    if (v == 1)
    {
      bdd_invalide[i,] = nouvelles_donnees
    }
    
  }
  
}

bdd$vhb_verdict[bdd$hbs_resultat > 100] = "SUFFISANT"
bdd_invalide$vhb_verdict[bdd$hbs_resultat > 100] = "SUFFISANT"

bdd$date_majorite = str_c(as.character(as.double(substr(bdd$ddn, 1,4))+18), substr(bdd$ddn, 5,10))
bdd$cve_majorite = ifelse(test=as.Date(bdd$date_majorite) <= as.Date(bdd$creation_cve), yes="OUI", no="NON")
bdd$cve_majorite[is.na(bdd$cve_majorite)] = ifelse(test=as.Date(bdd$date_majorite[is.na(bdd$cve_majorite)]) <= as.Date("2020-09-01"), yes="OUI", no="NON")

bdd_invalide$date_majorite = str_c(as.character(as.double(substr(bdd_invalide$ddn, 1,4))+18), substr(bdd_invalide$ddn, 5,10))
bdd_invalide$cve_majorite = ifelse(test=as.Date(bdd_invalide$date_majorite) <= as.Date(bdd_invalide$creation_cve), yes="OUI", no="NON")
bdd_invalide$cve_majorite[is.na(bdd_invalide$cve_majorite)] = ifelse(test=as.Date(bdd_invalide$date_majorite[is.na(bdd_invalide$cve_majorite)]) <= as.Date("2020-09-01"), yes="OUI", no="NON")

bdd$laureat = ifelse(test=(bdd$devenir != "AJOURNE"), yes="OUI", no="NON")
bdd_invalide$laureat = bdd$laureat


bdd$diag_cve = ""
bdd$inj_tot = as.double(bdd$inj_tot)
bdd$inj_invalide_pcent = as.double(bdd$inj_invalide_pcent)
bdd[bdd$inj_tot == 0,]$diag_cve = "CVE-VIDE"
bdd[bdd$inj_tot > 0 & bdd$inj_invalide_pcent == 100,]$diag_cve = "AUCUN-VALIDE"
bdd[bdd$inj_tot > 0 & bdd$inj_invalide_pcent < 100,]$diag_cve = "PART-VALIDE"
bdd[bdd$inj_tot > 0 & bdd$inj_invalide_pcent == 0,]$diag_cve = "TOTAL-VALIDE"

bdd_invalide$diag_cve=bdd$diag_cve

bdda = bdd[(bdd$filiere=="PASS"|bdd$filiere=="PACES")&bdd$diag_cve=="TOTAL-VALIDE"&bdd$cve_majorite=="OUI",]
bddb = bdd_invalide[(bdd_invalide$filiere=="PASS"|bdd_invalide$filiere=="PACES")&bdd_invalide$diag_cve!="CVE-VIDE"&bdd_invalide$cve_majorite=="OUI",]

t1 <- Sys.time()
tdiff = t1-t0
print (tdiff)

# Fin du script