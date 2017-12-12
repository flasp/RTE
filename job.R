setwd ("C:/Users/Fabien/Desktop/RTEST")

source('./fonctions.R')

rte_2012<-nettoyage("rte_2012.csv")
rte_2013<-nettoyage("rte_2013.csv")
rte_2014<-nettoyage("rte_2014.csv")
rte_2015<-nettoyage("rte_2015.csv")
rte_2016<-nettoyage("rte_2016.csv")

date<-'17/01/2016'

stat.day(date,rte_2016)

stat.year(rte_2012)



