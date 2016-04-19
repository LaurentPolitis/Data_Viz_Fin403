######
#
#  Data Mining INSEE populations française
#  Création d'une data frame à partir de http://www.insee.fr/fr/themes/detail.asp?reg_id=99&ref_id=base-cc-serie-historique
# 
# Auteur : Politis Laurent
# Mail : politis.laurent@gmail.com  
# 
########

data_import <- function(PATH = "data/"){


# Importer les données 
#____________________________________________________________________________________________

population_ville <- read.csv(paste(PATH,"population_ville.csv",sep=""), sep=";", stringsAsFactors=FALSE,row.names = "CODGEO" ) 

# Une autre fonction pour enlever les accents 
#____________________________________________________________________________________________

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

# Importer les noms des colonnes  
#____________________________________________________________________________________________

  legende <- read.csv(paste(PATH,"legende.csv",sep=""), sep=";",stringsAsFactors = F)
  legende$Code.géographique <- Unaccent(legende$Code.géographique) # enlever les accents 
  
# Importer les codes des regions http://www.insee.fr/fr/methodes/nomenclatures/cog/telechargement/2015/txt/reg2015.txt 
#____________________________________________________________________________________________
  reg2015 <- read.delim(paste(PATH,"reg2015.txt",sep=""),row.names = "REGION",stringsAsFactors = F)
  
  
# enlever les codes des régions 
#____________________________________________________________________________________________  

  colnames(population_ville) = legende[colnames(population_ville) == legende$CODGEO,"Code.géographique"]
  
  colnames(population_ville)[3] = "Communes"
  colnames(population_ville)[4:10]=paste("Population",c(2012,2007,1999,1990,1982,1975,1968))
  population_ville$Region = reg2015[as.character(population_ville$Region),"NCC"]
  
  return(population_ville)
}
 
  