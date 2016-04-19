######
#
#  Data Visualisation @Nom_des_données@
#  @url_du_site_web_des_données@
# 
# Auteur : 
# Mail :  
# 
########

#____________________________________________________________________________________________
#
# Partie data mining (importer les données sous R et arranger les données)
#
#____________________________________________________________________________________________

PATH <- "C:/Users/Laurent/Documents/Data_Viz_Fin403/Data_Viz_Fin403/"  # le chemin du dossier de travail  à changer

setwd(PATH)                         # définir le dossier courant de travail "~" designe le dossier "Documents" de 
                                    # windows .. permet d'aller au dossier au-dessus 

population_ville <- read.csv(paste(PATH,"data/population_ville.csv",sep=""),sep=";", stringsAsFactors=FALSE,row.names = "CODGEO" ) # Importer les données

# Une fonction pour enlever les accents 

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

# Importer les noms des colonnes  
#____________________________________________________________________________________________

legende <- read.csv(paste(PATH,"data/legende.csv",sep=""), sep=";",stringsAsFactors = F)  # importer les définitions des colonnes et des légendes 
legende$Code.géographique <- Unaccent(legende$Code.géographique)                          # enlever les accents 

# Importer les codes des regions http://www.insee.fr/fr/methodes/nomenclatures/cog/telechargement/2015/txt/reg2015.txt 
#____________________________________________________________________________________________
reg2015 <- read.delim(paste(PATH,"data/reg2015.txt",sep=""),row.names = "REGION",stringsAsFactors = F)


# enlever les codes des régions 
#____________________________________________________________________________________________  

colnames(population_ville) = legende[colnames(population_ville) == legende$CODGEO,"Code.géographique"]
colnames(population_ville)[3] = "Communes"
colnames(population_ville)[4:10]=paste("Population",c(2012,2007,1999,1990,1982,1975,1968))
population_ville$Region = reg2015[as.character(population_ville$Region),"NCC"]


#____________________________________________________________________________________________
#
# Partie data visualisation (création des graphiques)
#
#____________________________________________________________________________________________


### Box plots ( boîtes à moustache )
#________________________________________________________________________________
#install.packages(ggplot2)
library(ggplot2) # 


###  echelles des ordonnees modifiees
#________________________________________________________________________________

ggplot(population_ville) + geom_boxplot(aes(population_ville$`Population 2012`,x="ville")) + scale_y_sqrt()

ggplot(population_ville) + geom_boxplot(aes(y=`Population 2012`,x="France")) + 
  scale_y_log10() + geom_point(data= population_ville[population_ville$Communes=="Angers",],aes(y = `Population 2012` ,x='France'),color="red",size=4)

### Sans GGplot
#___________________________________________________

boxplot(population_ville$`Population 2012`,log="y", ylim=c(100,100000000))


### Encore des boxplots par région 
#___________________________________________________
ggplot(population_ville) + geom_boxplot(aes(y=`Population 2012`,x=Region,group=Region)) +  scale_y_log10() + coord_flip()


population_ville$dom_tom = population_ville$Region %in% c("MARTINIQUE","GUYANE","GUADELOUPE","MARTINIQUE","LA REUNION")


p = ggplot(population_ville,aes(y=`Population 2012`,x=Region,group=Region)) +  scale_y_log10() + coord_flip()


### des box plots exotiques 
#___________________________________________________
p + geom_violin()

p + geom_jitter()

p + geom_jitter(alpha=0.2) + geom_boxplot(color="red")

### couleurs 
#___________________________________________________


p + geom_jitter(alpha=0.2) + geom_boxplot(aes(fill=dom_tom))

p + geom_jitter(alpha=0.2) + geom_boxplot(aes(fill=dom_tom))+ facet_grid(facets = ~dom_tom)

p  + geom_violin(aes(fill=dom_tom))+ facet_grid(facets = ~dom_tom) + geom_jitter(alpha=0.1)


######## Courbe de densité proba. ###########################
#_________________________________________________________________________________________________________________

ggplot(population_ville) + geom_density(aes(x = population_ville$`Population 2012`)) 
ggplot(population_ville) + geom_density(aes(x = `Population 2012`)) +scale_x_sqrt()
ggplot(population_ville) + geom_density(aes(x = `Population 2012`,fill="kernel density \n estimation"),alpha=0.4)  +
  scale_x_log10() +facet_wrap(facets = ~Region)
ggplot(population_ville) + geom_density(aes(x = `Population 2012`,fill="kernel density \n estimation"),alpha=0.4)  +
  scale_x_log10() +facet_wrap(facets = ~Region,nrow=2,ncol=13)


######## Histogramme ###############################
####################################################
#_________________________________________________________________________________________________________________

ggplot(population_ville,aes(x=`Population 2012`)) + geom_histogram(binwidth = 10000) + scale_y_log10()


ggplot(population_ville[population_ville$Region=="BRETAGNE",],aes(x=`Population 2012`,fill=Departement)) + geom_histogram(binwidth = 1000) +
  scale_y_sqrt()

ggplot(population_ville[population_ville$Region=="BRETAGNE",],aes(x=`Population 2012`,fill=Departement)) + geom_histogram(binwidth = 1000) +
  scale_y_sqrt()+facet_wrap(facets = ~Departement)



######## Time series ###########################
#_________________________________________________________________________________________________________________
library(reshape2)
tmp=population_ville[population_ville$Region=="PAYS DE LA LOIRE",c(3,4:10)]
tmp.m=melt(tmp)
tmp.m$variable=as.numeric(substr(x = tmp.m$variable,12,15))

ggplot(tmp.m) + geom_line(aes(x=as.Date(paste(tmp.m$variable,"-01-01",sep="")),y=value,group=Communes))
ggplot(tmp.m) + geom_line(aes(x=as.Date(paste(tmp.m$variable,"-01-01",sep="")),y=value,group=Communes)) +scale_y_sqrt()
ggplot(tmp.m) + geom_line(aes(x=as.Date(paste(tmp.m$variable,"-01-01",sep="")),y=value,group=Communes)) +scale_y_log10()

tmp.m$Angers=tmp.m$Communes=="Angers" #"#CFCDCD", "#FFFFFF"

tmp.m=tmp.m[tmp.m$value>10000,]

ggplot(tmp.m) + geom_line(aes(x=as.Date(paste(tmp.m$variable,"-01-01",sep="")),y=value,group=Communes,colour=Angers,size=Angers))+scale_y_sqrt()  +
  scale_color_manual(values=c("#5E5C57", "red"))+scale_size_manual(values=c(1, 3))+geom_text(data=tmp.m[tmp.m$variable==2012,],aes(x = as.Date(paste(2012,"-01-01",sep=""))
                                                                                                                                   ,y=(value),label=Communes)) +geom_point(aes(x=as.Date(paste(tmp.m$variable,"-01-01",sep="")),y=value))



#Scatter plot  et lm partie modélisation
#_________________________________________________________________________________________________________________

#install.packages("ggExtra")
#install.packages("gridExtra")

library("ggExtra")


ggplot(data = tmp.m[tmp.m$Communes=="Angers",], aes(x = variable,  y =value)) +
  geom_point() +geom_smooth()

gg  = ggplot(data = tmp.m[tmp.m$Communes=="Angers",], aes(x = variable,  y =value)) +
  geom_point() +geom_smooth(method = "lm")


ggExtra::ggMarginalGadget(gg) # addins pour améliorer le scatter plot 

# Vérifications lm()
#_________________________________________________________________________________________________________________

mod=lm(tmp.m[tmp.m$Communes=="Angers",],formula = value ~ variable)

tmp.m$TREND=sapply(X = tmp.m$Communes, FUN = function(x){lm(tmp.m[tmp.m$Communes==x,],formula = value ~ variable)$coefficients[2]>0})


gg1  = ggplot(data = tmp.m[tmp.m$Communes==unique(tmp.m$Communes),], aes(x =TREND)) + geom_bar()



#____________________________________________________________________________________________
#
# Partie  exporter les  graphiques  via la méthode graphique sous Rstudio ou à l'aide de fonction R 
#
#____________________________________________________________________________________________

ggThemeAssist:::ggThemeAssist() # surligner avec le curseur le graphique ggplot que vous avez créer 


 pdf("mygraph.pdf") #	pdf file
 png("mygraph.png") #	png file
 jpeg("mygraph.jpg") # 	jpeg file
 postscript("mygraph.ps") #	postscript file 



