######
#
#  Data Visualisation des donnÃ©es INSEE 
#  de http://www.insee.fr/fr/themes/detail.asp?reg_id=99&ref_id=base-cc-serie-historique
# 
# Auteur : Politis Laurent
# Mail : politis.laurent@gmail.com  
# 
########

setwd("~/data-visualisationFIN403/")

source("prog/data_mining_communes_fonction.R")


### Box plots ( boîtes à moustache )
#________________________________________________________________________________
#install.packages(ggplot2)
library(ggplot2) # 

population_ville=data_import()
ggplot(population_ville) + geom_boxplot(aes(x="France",y= `Population 2012`))

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
######################################################
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
######################################################
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

# Modélisation Lm() data Mtcars 
#_________________________________________________________________________________________________________________

?matcars 

#Scatter plot 
#_________________________________________________________________________________________________________________

install.packages("ggExtra")
install.packages("gridExtra")

library("ggExtra")

ggplot(data = mtcars, aes(x = hp, y = mpg)) +
  geom_point() +geom_smooth(method = "lm")

p=ggplot(data = mtcars, aes(x = hp, y = mpg)) +
  geom_point() +geom_smooth(method = "lm",formula =y~poly(x = x,2))

p1=ggMarginalGadget(plot = ggplot(data = mtcars, aes(x = hp, y = mpg)) +
                            geom_point() +geom_smooth(method = "lm",formula =y~poly(x = x,4))
)


ggplot(data = tmp.m[tmp.m$Communes=="Angers",], aes(x = variable,  y =value)) +
  geom_point() +geom_smooth()

ggplot(data = tmp.m[tmp.m$Communes=="Angers",], aes(x = variable,  y =value)) +
  geom_point() +geom_smooth(method = "lm")


# Vérifications lm()
#_________________________________________________________________________________________________________________


lm1= lm(mtcars,formula = hp ~ mpg)
lm2= lm(mtcars,formula = hp ~ poly(mpg,4))

summary(lm1)
summary(lm2)

plot(lm1)
plot(lm2)


lm3= lm(mtcars[!(rownames(mtcars)=="Maserati Bora"),],formula = hp ~ poly(mpg,4))
