######
#
#  Data Mining INSEE populations française
#  Création d'une data frame à partir de http://www.insee.fr/fr/themes/detail.asp?reg_id=99&ref_id=base-cc-serie-historique
# 
# Auteur : Politis Laurent
# Mail : politis.laurent@gmail.com  
# 
########


PATH <-"data/"

# Importer les données 
#____________________________________________________________________________________________

population_ville <- read.csv(paste(PATH,"population_ville.csv",sep=""), sep=";", stringsAsFactors=FALSE,row.names = "CODGEO" ) 

# sous forme de data frame 
#____________________________________________________________________________________________

class(population_ville)

# explorer les données sous R studio 
#____________________________________________________________________________________________
  View(population_ville)
 
# voir le début et la fin d'un fichier 
#____________________________________________________________________________________________

  head(population_ville)
  tail(population_ville)
   
#  la dimension de la data frame 
#___________________________________________________________________________________________________
  
  dim_pop = dim(population_ville) # la fonction dim renvoie le nombre de ligne puis le nombre de colonne 
  
# complete.cases() renvoie un vecteur de boolean TRUE si la ligne est compléte FALSE sinon 
#____________________________________________________________________________________________  
  
 vec_na = complete.cases(population_ville)
 
 # La somme d'un vecteur de Boolean considére TRUE comme 1 et FALSE 0
 #____________________________________________________________________________________________  
 
 sum_na = sum(vec_na) # compte le nbr de ligne o? il n'y a pas de donn?es manquantes
 
 # test pour savoir si il y a des données manquantes dans les donn?es
 #____________________________________________________________________________________________  
 
 dim_pop[1] == sum_na
    
# Importer les noms des colonnes  
#____________________________________________________________________________________________

  legende <- read.csv(paste(PATH,"legende.csv",sep=""), sep=";",stringsAsFactors = F)
  
  colnames(population_ville) = legende[colnames(population_ville) == legende$CODGEO,"Code.géographique"]
  
  colnames(population_ville)[3] = "Communes"
  
  
  
 
  