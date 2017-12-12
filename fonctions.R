nettoyage<-function(cheminFichier)
{
  chemin<-getwd()
  print("Repertoire de travail :")
  print(chemin)
  
  #Lecture fichier
  data <- read.csv(cheminFichier, header=TRUE, sep=";")
  noms<-labels(data)
  noms[[2]][c(1,6,7,11,16,25,28,31,34,35,36)]<-c("Perimetre","PrevisionJmoins1","PrevisionJ","Nucleaire","Bioenergies","Fioul Cogen","Gaz Cogen","Hydraulique Fil de l\'eau Ecluse","Bioenergies Dechets","Bioenergies Biomasse","Bioenergies Biogaz")
  names(data)<-noms[[2]]
  data<-data[-35137,]
  
  #Separation donnees utiles
  vNA<-which(is.na(data$Consommation)==TRUE)
  data_util<-data[-vNA,c(3:36)]
  print(paste(cheminFichier," : Nettoyage termine"))
  data_util
}


# graph_day<-function(date,data)
# {
#   #Initialisation des dates, maxi, mini et des heures pour le plot
#   vdate<-which(data$Date==date)
#   maxi<-max(data$Consommation[vdate])+1000
#   mini<-min(data$Consommation[vdate])-1000
#   hours<-seq(from=0, to=23.5,by=0.5)
#   
#   #calcul prÃ©cision
#   precis_Jmoins1<-(data$PrevisionJmoins1 - data$Consommation)/data$Consommation*100
#   
#   #Plot conso
#   par(mar=c(5,4,4,4)+0.3)
#   plot(hours,data$Consommation[vdate], type="b",ylim=c(mini,maxi), col='blue',xlab="Heure",ylab="Consommation")
#   #Ajout Prevision
#   lines(hours, data$PrevisionJmoins1[vdate], type="b", col='red',xlab="",ylab="")
#   
#   #Ajout moyenne sur la journÃ©e
#   moyenne<-mean(data$Consommation[vdate])
#   lines(hours, rep(moyenne,48), col="black",xlab="",ylab="")
#   
#   #Ajout prÃ©cision
#   par(new = TRUE)
#   #plot(precis_Jmoins1[1:48],ylim=c(-10,10),type="h",axes=FALSE)
#   color<-rgb(0,0,1,alpha=0.2) #def couleur et transparence bar
#   barplot(precis_Jmoins1[vdate],ylim=c(-10,10),col=color, axes = FALSE, bty = "n", xlab = "", ylab = "")
#   axis(side=4, at = c(-10:10))
#   mtext("Ecart %", side=4, line=3) #titre axe droit
#   print("Graphe termine")
# }


stat.day<-function(date,data)
{
  library(ggplot2)
  library(reshape2)
  library(gtable)
  library(gridExtra)
  
  #extract des donnees pour la conso, les previsions et calcul des ecarts en %
  extract<-subset(data,Date==date,select=c('Heures','Consommation','PrevisionJmoins1','PrevisionJ'))
  ecart<-data.frame("Heures"=extract$Heures,"EcartJmoins"=(extract$PrevisionJmoins1-extract$Consommation)/extract$Consommation*100,"EcartJ"=(extract$PrevisionJ-extract$Consommation)/extract$Consommation*100)
  extract<-melt(extract,id.vars = "Heures")
  ecart<-melt(ecart,id.vars="Heures")
  
  #donnees sur les sources d energie pour la journee concernee
  sources<-subset(data,Date==date,select=c(Heures,Consommation,Fioul:Bioenergies,Ech..physiques))
  sources$Consommation<- -sources$Consommation
  temp<-subset(sources,select=Fioul:Bioenergies)
  sources<-melt(sources,id.vars ="Heures")
  
  #calcul des pourcentages du mix de production energetique
  moyenne<-data.frame(Noms=names(temp),Moyenne=round(colMeans(temp)/sum(colMeans(temp))*100,0))
  
  #on exclut les valeurs du pompage si elles sont negatives (==pas une production)
  if (moyenne[8,2]<0){
    moyenne<-moyenne[-8,]
  }
  
  #plot de la conso reelle vs previsions J-1 vs previsions J
  p1<-ggplot(extract,aes(x = Heures,y=value,group=variable,colour=variable)) +
    geom_point() +
    geom_line(aes(linetype=variable)) +
    ylab("Demande (MW)")+
    theme(legend.title = element_blank())+
    scale_x_discrete(breaks=c('00:00','03:00','06:00','09:00','12:00','15:00','18:00','21:00','23:00'),name="")
 
  #plot bar du mix energetique en fonction de l'heure
  p2<-ggplot(ecart,aes(x = Heures,y=value,fill=variable,group=variable)) +
    geom_bar(stat = 'identity',position='dodge') +
    theme(legend.title = element_blank())+
    ylab("Ecart Reel/Prevision") +
    scale_y_continuous(limits = c(-10,10)) +
    scale_x_discrete(breaks=c('00:00','03:00','06:00','09:00','12:00','15:00','18:00','21:00','23:00'))
  
  #plot bar de l'ecart reel vs J-1 et reel vs j en %
  p3<-ggplot(data=sources,aes(x=Heures,y=value,fill=variable)) +
    geom_bar(stat='identity',position='stack') +
    geom_segment(aes(x=0,y=0,xend=49,yend=0),color='white') +
    ylab("Production/Demande (MW)") +
    scale_fill_discrete(name="")+
    scale_x_discrete(breaks=c('00:00','03:00','06:00','09:00','12:00','15:00','18:00','21:00','23:00'))
  
  #plot pie chart du decoupage du mix sur la journee
  p4<-ggplot(moyenne,aes(x="",y=Moyenne,fill=Noms)) +
    geom_bar(stat='identity',position='stack') +
    coord_polar(theta="y") +
    geom_text(aes(label = Moyenne), position = position_stack(vjust = 0.5),colour='white') +
    scale_fill_discrete(name="")+
    theme(axis.text = element_blank(),panel.grid = element_blank())+
    ylab("Distribution du mix energetique (%)") +
    xlab("")
  
  #on utilise gridArrange pour disposer les 4 plots sur la meme page
  g1<-ggplotGrob(p1)
  g2<-ggplotGrob(p2)
  g3<-ggplotGrob(p3)
  g4<-ggplotGrob(p4)
  
  grid.arrange(g1,g2,g3,g4, nrow=2,ncol=2,top=paste("Statistiques pour le ",date))
}





is.bisextile<-function(year)
{
  #fonction de test de la bissextilite d une annee
  ((year%%4==0)&&(year%%100!=0))|(year%%400==0)
}





stat.year<-function(data)
{
  library(RColorBrewer)
  library(ggplot2)
  
  #User input pour le choix de la catégorie
  {
    categorie<-names(data)[3:length(names(data))]
    texte<-" Liste des choix possibles : \n"
    
    for (k in 1:length(categorie))
    {
      texte<-paste(texte,k,"--->",categorie[k],'   \n')
    }
    
    cat(texte)
    n<-readline(prompt = "Quel est votre choix ? : ")
    n<-as.integer(n)
    Type<-categorie[n]
  }
  
  extract<-subset(data,select=c('Date',Type))
  
  #Identification de l annee des donnees
  year<-as.numeric(format(as.Date(extract$Date[1],format='%d/%m/%Y'),'%Y'))
  origine<-paste(toString(year-1),'-12-31',sep='')
  
  jour<-NULL
  moyenne<-NULL

  if(is.bisextile(year)){
  #calcul des moyennes pour une annee bissextile
    for (k in 1:366)
    {
      v_jour<-subset(extract,as.Date(Date,format='%d/%m/%Y')==as.Date(k,origine),select=Type)
      jour<-c(jour,k)
      moyenne<-c(moyenne,round(mean(v_jour[[1]],0)))
    }
  
    moy.an<-data.frame(Jour=format(as.Date(1:366,origine),'%d'),Mois=format(as.Date(1:366,origine),'%m'),Moyenne=moyenne)

  }
  
  else {
  #calcul des moyennes pour une annee classique
    for (k in 1:365)
    {
      v_jour<-subset(extract,as.Date(Date,format='%d/%m/%Y')==as.Date(k,origine),select=Type)
      jour<-c(jour,k)
      moyenne<-c(moyenne,round(mean(v_jour[[1]],0)))
    }
    
    moy.an<-data.frame(Jour=format(as.Date(1:365,origine),'%d'),Mois=format(as.Date(1:365,origine),'%m'),Moyenne=moyenne)
    
  }    

  #definition du milieux pour l echelle gradient
  mid<-mean(moy.an$Moyenne) 
  
  #plot tiles
  ggplot(moy.an,aes(x=Jour,y=Mois)) +
  geom_tile(aes(fill=Moyenne)) +
  scale_fill_gradient2(midpoint = mid,low='blue',mid='white',high='red',name="Moyenne Jour\n (MW)") +
  ggtitle(paste('Intensité de l\'utilisation sur l\'année : ',Type))

}




corr.year<-function(data)
{
  
  #User input
    categorie<-names(data)[3:length(names(data))]
    texte<-" Liste des choix possibles : \n"
    
    for (k in 1:length(categorie))
    {
      texte<-paste(texte,k,"--->",categorie[k],'   \n')
    }
    
    cat(texte)
    n1<-readline(prompt = "Quel est votre choix en abscisse ? : ")
    n1<-as.integer(n1)
    
    n2<-readline(prompt = "Quel est votre choix en ordonnee ? : ")
    n2<-as.integer(n2)
    
    A<-categorie[n1]
    O<-categorie[n2]
    val<-data.frame(subset(data,select=A),subset(data,select=O))

   ggplot(val,aes(x=val[[1]],y=val[[2]])) +
     geom_point(color='gray') +
     geom_smooth(method='lm') +
     ylab(O) +
     xlab(A)
  
}



