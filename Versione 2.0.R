library(Rcpp)
library(mice)
library(tidyverse)
library(readxl)
library(segmented)
library(BlandAltmanLeh)
library(ggplot2)
library(nortest)

setwd("~/Università/4. Quarto anno/Primo semestre/Modelli statistici e processi stocastici/Progetto")

col_types = c("numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")
nomi=c("B","C","D","E","F","G","H","I","J","K","L")

#Lettura dati e prime analisi/sistemazioni ----------------------------------
Dati<-read_excel("Dati.xlsx",col_types = col_types)
#Cambio nome colonne
colnames(Dati) =c("ANNO",nomi) 

#Prime analisi:
#First plot
x11()
plot(Dati)
#Analisi NA
anyNA(Dati) #ci dice se ci sono dei NA
is.na(Dati) #da un vettore logico con le posizioni dei NA dove c'è TRUE
conta_na=sum(is.na(Dati)) #conto quanti NA ci sono
#Distribuzione NA
x11()
md.pattern(Dati, rotate.names = T)


#Estrazione singole colonne e creazione matrice
t<-Dati$ANNO
B<-Dati$B
C<-Dati$C
D<-Dati$D
E<-Dati$E
EFFE<-Dati$F
G<-Dati$G
H<-Dati$H
I<-Dati$I
J<-Dati$J
K<-Dati$K
L<-Dati$L


matrice=as.matrix(Dati)[,2:ncol(Dati)]

#Parametri statistici ---------------------------

medie=apply(matrice,2,mean,na.rm=T) #il 2 indica di applicare la funzione per colonne
varianze=apply(matrice,2,var,na.rm=T)

medie_1900_1945=apply(matrice[which(Dati$ANNO==1900):which(Dati$ANNO==1945),],2,mean,na.rm=T)

#Analisi di normalità ---------------------------------------------------------

#Vettori da usare nei grafici
tipografico=c("QQnorm","Istogramma","BoxPlot")
colori=c( "red" ,"#FF9900", "#FFE500", "#CCFF00" , "#00FF19", "cyan" ,"#00B3FF",
          "#0019FF", "#3300FF", "#8000FF", "#FF00E6")
colonne=ncol(matrice)
metodo=c("Stalattiti-Austria","Uva-Svizzera","Chironomidi1-Svizzera","Uva-Francia",
         "Dendrocronologia-E.Nord-Mann","Chironomidi2-Svizzera","Dendrocronologia-Svizzera",
         "Dendrocronologia-Slovacchia","Dendrocronologia-Alpi","Indici storici-Eu. cent.",
         "Dati documentari-Eu. cent")
#Analisi di normalità dei dati - metodi qualitativi

#QQnorm
x11()
par( mfrow = c( 3, 4 ) )

for (i in 1:colonne) {
  
  ndati=length(matrice[,i])-sum(is.na(matrice[,i]))
  qqnorm( matrice[,i], main = paste(tipografico[1], metodo[i]), col = colori[i]  ) 
  qqline( matrice[,i], lwd = 2, lty = 5  )  
  legend('topleft',legend=paste("N° dati: ",ndati),col='black')
}
#vanno lanciati in modo strano - Marti & Boga

#ISTOGRAMMA 
x11()
par( mfrow = c( 3, 4 ) )

for (i in 1:colonne) {
  
  ndati=length(matrice[,i])-sum(is.na(matrice[,i]))
  hist( matrice[,i] , prob = TRUE, breaks = 100, main = paste(tipografico[2], metodo[i]),border="dark grey", xlab="Dati [°C]")
  griglia = sort(matrice[,i] ) 
  lines( griglia, dnorm( griglia, mean = mean(matrice[,i],na.rm=T), sd = sd(matrice[,i],na.rm=T ) ), 
         col = colori[i], lwd = 2 )
  legend('topright',legend=paste("N° dati: ",ndati),col='black')
}

#Analisi di normalità dei dati - metodi quantitativi

#Lillie test
p_lilli<-rep(NA,(ncol(matrice)))

for (i in 1:ncol(matrice)){
  
  p_lilli[i]<-lillie.test(matrice[,i])$p.value
  
}
test=p_lilli>0.1
p_lilli_mat=matrix(c(p_lilli,test),nrow=ncol(matrice),ncol=2) #unire i pvalue con i valori logici.
colnames(p_lilli_mat) =c("p_value","test>0.1") 
rownames(p_lilli_mat)=c(nomi)
View(p_lilli_mat)
#Da matrice passano la normalità: D, I, K 

#Dimostrazione che normalizzando i dati originari passano comunque D, I e K:
p_lillinorm<-rep(NA,(ncol(matrice)))

for (i in 1:ncol(matrice)){
  
  p_lilli[i]<-lillie.test(matrice[,i]-medie_1900_1945[i])$p.value
  
}
p_lilli>0.1



#Altri grafici ---------------------------

#Boxplot
x11()
par( mfrow = c( 3, 4 ) )

for (i in 1:colonne) {
  
  boxplot(matrice[,i],main = paste(tipografico[3], metodo[i]),col=colori[i], ylab="quantili")
  legend('topright',legend=paste("N° dati: ",ndati),col='black')

}


#Sistemazione dataset ---------------------
#eliminazione B
matrice2=matrix(c(t,C,D,E,EFFE,G,H,I,J,K,L),ncol=11)

#Individuazione dell'indice in cui iniziano tutte le serie
#Fare attenzione a quanti NA ci sono dopo il primo numero trovato e se ha senso riempirli
#Es. se trova il primo valore al 1450 ma dopo ci sono 10 NA in una colonna cercare il prossimo
esci=0
indice=0

for (i in 1:length(t)) {
  
  if(is.na(sum(matrice2[i,]))==F  && esci!=1 ){
    
    indice=i
    esci=1
    i=length(t)
  }
}
indice

#Dataset ridotto
matrice3=matrice2[indice:length(t),]
colnames(matrice3) =c("ANNI","C","D","E","F","G","H","I","J","K","L") 

x11()
md.pattern(matrice3,rotate.names = F) #Vediamo quanti NA ci sono e come sono distribuiti 

#Rimozione outliers ------------------------------------------------------------
#Mettiamo i NA al posto degli outlier
indici_outliers=NA*matrice3

for(i in 2:ncol(matrice3)){
  
  quantile75=quantile(matrice3[,i],0.75, na.rm=TRUE)
  quantile25=quantile(matrice3[,i],0.25, na.rm=TRUE)
  
  valoutsup=quantile75+1.5*(quantile75-quantile25)
  valoutinf=quantile25-1.5*(quantile75-quantile25)
  
  for(j in 1:nrow(matrice3)){
    
    if(is.na(matrice3[j,i])==FALSE){
      
      if(matrice3[j,i]>valoutsup||matrice3[j,i]<valoutinf){
        
        indici_outliers[j,i]=1 #salviamo le posizioni in cui si è messo un NA
        matrice3[j,i]=NA;
      }
    }
  }
}

indici_outliers[,1]=t[indice:length(t)] #rimettiamo colonna anni per non perdere la corrispondenza
View(matrice3)
View(indici_outliers)
#La concentrazione di outliers è più alta negli ultimi anni, probabilmente a causa della presenza del cambiamento climatico
#Cosa fare?

#Sistemazione ufficiale NA -----------------

medie3=apply(matrice3[,2:ncol(matrice3)],2,mean,na.rm=T)
sd3=apply(matrice3[,2:ncol(matrice3)],2,sd,na.rm=T)

#Ciclo per fine serie se prima del valore ci sono meno di 5 NA
fine_serie=rep(NA,(ncol(matrice3)-1))
for (i in 2:ncol(matrice3)){
  
    for(j in 1:nrow(matrice3)){
    
      if(is.na(matrice3[(nrow(matrice3)-j+1),i])==F && is.na(fine_serie[i-1])==T)
        if(is.na(mean(matrice3[(nrow(matrice3)-j+1-5):(nrow(matrice3)-j-1),i],na.rm=T))==F)
          fine_serie[i-1]=matrice3[(nrow(matrice3)-j+1),1]
  }
} 

#Sostituzione NA
matrice4=matrice3;
flag=0

for(i in 2:ncol(matrice4)){
  
  for(j in 1:which(matrice4[,1]==fine_serie[i-1])){
    
    if(is.na(matrice4[j,i])==T){
      
      flag=j
      
      while(is.na(matrice4[flag,i])==T){
        
        flag=flag+1; 
     
      }
      matrice4[j:(flag-1),i]=(matrice4[j-1,i]+matrice4[flag,i])/2; #metto uguali alla media
      
      for (k in j:(flag-1)){
        
        matrice4[k,i]=matrice4[k,i]+rnorm(1,0,sd3[i-1]) #rumore bianco - Usiamo sd3 o la dev std dei residui o cosa?
      
      } 
    }
    
    flag=0
  }
}

View(matrice4)
View(matrice3==matrice4) #Verifica che non abbia cambiato nessun valore intorno ai NA

#Normalizzazione sul periodo 1900-1945 ---------------------------------------
matricenorm=NA*matrice4

for (i in 2:ncol(matrice4)){
  
  matricenorm[,i]=matrice4[,i]-mean(matrice3[which(matrice3[,1]==1900):which(matrice3[,1]==1945),i],na.rm=T) #qui è giusto matrice 3 così la media è calcolata sui valori base 

}

matricenorm[,1]=matrice4[,1] #Rimettiamo gli anni davanti
nominorm=c("C","D","E","EFFE","G","H","I","J","K","L")
colnames(matricenorm) = c("A",nominorm)

#Plot in grafici separati
x11()
par( mfrow = c( 3, 4 ) )

for (i in 2:ncol(matricenorm)){
  
  plot(matricenorm[,1],matricenorm[,i],type='l',col=colori[i])
  lines(matricenorm[,1],matricenorm[,5],type='l',lwd=1,col=colori[5])
  
}

#Plot tutte assieme 
x11()

plot(matricenorm[,1],matricenorm[,2],type='l',col=colori[2])
for (i in 3:ncol(matricenorm)){
  
  lines(matricenorm[,1],matricenorm[,i],type='l',col=colori[i])

}
lines(matricenorm[,1],matricenorm[,5],type='l',lwd=2,col=colori[5])

#Normalità del dataset sistemato.
#Domanda: Le colonne pulite passano il test di normalità?

#Analisi di normalità - metodi qualitativi

#QQnorm
x11()
par( mfrow = c( 3, 4 ) )

for (i in 2:colonne) {
  
  ndati=length(matricenorm[,i])-sum(is.na(matricenorm[,i]))
  qqnorm( matricenorm[,i], main = paste(tipografico[1], metodo[i]), col = colori[i]  ) 
  qqline( matricenorm[,i], lwd = 2, lty = 5  )  
  legend('topleft',legend=paste("N° dati: ",ndati),col='black')
}
#vanno lanciati in modo strano - Marti & Boga

#ISTOGRAMMA 
x11()
par( mfrow = c( 3, 4 ) )

for (i in 2:colonne) {
  
  ndati=length(matricenorm[,i])-sum(is.na(matricenorm[,i]))
  hist( matricenorm[,i] , prob = TRUE, breaks = 100, main = paste(tipografico[2], metodo[i]),border="dark grey", xlab="Dati [°C]")
  griglia = sort(matricenorm[,i] ) 
  lines( griglia, dnorm( griglia, mean = mean(matricenorm[,i],na.rm=T), sd = sd(matricenorm[,i],na.rm=T ) ), 
         col = colori[i], lwd = 2 )
  legend('topright',legend=paste("N° dati: ",ndati),col='black')
}

#Analisi di normalità dei dati - metodi quantitativi

#Lillie test
p_lilli_norm<-rep(NA,(ncol(matricenorm)-1))

for (i in 2:ncol(matricenorm)){
  
  p_lilli_norm[i-1]<-lillie.test(matricenorm[,i])$p.value
}

test_norm=p_lilli_norm>0.1
p_lilli_normm=matrix(c(p_lilli_norm,test_norm),nrow=(ncol(matricenorm)-1),ncol=2) #unire i pvalue con i valori logici.
colnames(p_lilli_normm) =c("p_value","test>0.1") 
rownames(p_lilli_normm)=c(nomi2)
View(p_lilli_normm)

#Da matricenorm passano la normalità: D, H, I, K, L 

#Analisi correlazione colonne ------------
matricenorm=NA*matrice3;

for (i in 2:ncol(matrice4))
{
  matricenorm[,i]=matrice4[,i]-mean(matrice4[440:500,i],na.rm = TRUE) 
}
matricenorm[,1]=matrice4[,1]

matrice5=matricenorm[1:485,2:ncol(matrice4)]
matrice7=0*matrice5
for (i in 1:ncol(matrice5)) {
  matrice7[,i]=sort(matrice5[,i])
}


matrice_log=log(matrice5)
corr=cor(matrice5)
View(corr)
covarianza=cov(matrice5)
View(covarianza)

#trasformare in T e F in base a se supera una soglia di correlazione o test. 
stima=lm(matrice5[,3]~matrice5[,1])
summary(stima)

plot(matrice5[,1],matrice5[,3],type='p',col='black')
lines(matrice5[,1],-0.02917+0.42694*matrice5[,1],type='l',col='red',lwd='2')

corr7=cor(matrice7)
View(corr7)

stima=lm(matrice7[,3]~matrice7[,1])
summary(stima)

plot(matrice7[,1],matrice7[,3],type='p',col='black')
lines(matrice7[,1],-0.02917+0.42694*matrice7[,1],type='l',col='red',lwd='2')

#uva1 e uva2 sono linearmente correlate (ma R^2 basso)
View(corr)

#grafico tra 8 e 1
stima=lm(matrice5[,8]~matrice5[,1])
summary(stima)

plot(matrice5[,1],matrice5[,8],type='p',col='black')
lines(matrice5[,1],-1.30123+0.39596*matrice5[,1],type='l',col='red',lwd='2')



#Correlazione e altro con na presenti-----------------------------------------

correlazione=cor(Dati, Dati, na.rm = TRUE)
##non da correlazione perché ci sono i NA



library(mice)

imputed_Data1 <- mice(Dati, m=2, maxit = 50, method = 'mean', seed = 500)
imputed_Data1$imp

prova1 <- complete(imputed_Data1)
summary(prova1)

uva1=prova1$C
uva2=prova1$E
dendro1=prova1$em_nrd
dendro2=prova1$H
dendro3=prova1$I
dendro4=prova1$J
indici=prova1$K
sil_e_chir=prova1$D
chiro2=prova1$G
stalattiti=prova1$B
bo=prova1$L
anno=prova1$ANNO

plot(anno,uva2,type='l',ylim=c(-3,17))
lines(anno,uva1, col='blue')

plot(anno,dendro3,type='l',xlim=c(750,2000))
lines(anno,dendro1, col='red')
lines(anno,dendro4,col='green')
lines(anno,dendro2,col='blue')

plot(anno,chiro2,type='l')
lines(anno,sil_e_chir, col='red')

cor(prova1,prova1)

dev.new()
plot(prova1)


#Suddivisione per tipologia-----------------------------------------------------
#Nuovi nomi
uva1=Dati$C
uva2=Dati$E
dendro1=Dati$EFFE
dendro2=Dati$H
dendro3=Dati$I
dendro4=Dati$J
indici=Dati$K
sil_e_chir=Dati$D
chiro2=Dati$G
stalattiti=Dati$B
bo=Dati$L

x11()
par( mfrow = c( 3, 4 ) )
for (i in 1:colonne) {
  plot(t,matrice[,i],typ="l")
  
}
plot(t,EFFE,typ="l")

#Plot per tipologia uva
plot(t,uva2,type='l',ylim=c(-3,17),xlim=c(1200,2000))
lines(t,uva1, col='blue')
plot(uva2,uva1,type='p')
lines(uva2,-15.53550+1.05505*uva2,col="red",type='l')

#Plot per tipologia dendro
plot(t,dendro3,type='l',xlim=c(1500,2000),ylim=c(-4,5))
lines(t,dendro1, col='red')
lines(t,dendro4,col='green')
lines(t,dendro2,col='blue')

#Plot per tipologia chiro
plot(t,chiro2,type='l',ylim=c(-3,3), xlim=c(1000,2000))
lines(t,sil_e_chir, col='red')






#Applicazione di changing point a matricenorm e identificazione del miglior modello comune-------
cpoints=rep(NA,ncol(matricenorm)-1)
R2_segmented=rep(NA,ncol(matricenorm)-1)

x11() 
par( mfrow = c( 3, 4) )

for (i in 2:ncol(matricenorm)){

  df=data.frame(
    x=matricenorm[1:which(matricenorm[,1]==fine_serie[i-1]),1],
    y=matricenorm[1:which(matricenorm[,1]==fine_serie[i-1]),i]
  )

  fit_lm=lm(y~1+x,data=df)
  fitseg=segmented(fit_lm, seg.Z=~x, npsi=1)
  
  plot(fitseg,type='l',ylim=c(-3,2)) 
  points(df,type='l') 
  lines.segmented(fitseg) 
  points.segmented(fitseg,type='l')
  
  cp=fitseg[["psi"]]
  cpoints[i-1]=round(cp[2])
  
  R2_segmented[i-1]=summary(fitseg)$r.squared
  
}

#Dummy lineare --------------------------------------------
R2_dummy=rep(NA,(ncol(matricenorm)-1))
dummy2=rep(0,length(matricenorm[,1]))

for (i in 1:nrow(matricenorm)){
  
  if(i>which(matricenorm[,1]==cpoints[4]))
    dummy2[i]=1
  
}

dummy3=matricenorm[,1]*dummy2

for (i in 2:ncol(matricenorm)){
  
  dframe=data.frame(
    x=matricenorm[,1],
    y=matricenorm[,i],
    x2=dummy2,
    x3=dummy3
  )
  
  fit_model=lm(y~x2+x3,data=dframe)
  
  R2_dummy[i-1]=summary(fit_model)$r.squared
  
}

#Plot degli R^2 dei due metodi
x11()
plot(R2_dummy,ylim=c(0,0.5))
lines(R2_segmented,type='p',col='red')

#Standardizzare i dati-------
#matrice5 è quella dove iniziano tutti allo stesso punto e finiscono tutti allo stesso anno

for (i in 1:ncol(matrstd)) {
  matrstd[,i]=scale(matrice5[,i],center=T,scale=T)
}

corrstd=cov(matrstd)
View(corrstd)

stima=lm(matrstd[,3]~matrstd[,1])
summary(stima)

plot(matrstd[,1],matrstd[,3],type='p',col='black')
lines(matrstd[,1],-1.30123+0.39596*matrstd[,1],type='l',col='red',lwd='2')

err3=matrix(NA,nrow(matrice3),ncol(matrice3)-1) #creo matrice residui
mat3_teo = matrix(NA,nrow(matrice3),ncol(matrice3)); #normale teorica
mat3_teo[,1]=matrice3[,1]

for (i in 2:ncol(matrice3))
{
  mat3_teo[,i] = dnorm(matrice3[,i],medie3[i-1],sd3[i-1]) #riempita con numeri estratti da una normale
  mat3_teo[,i] = qnorm(mat3_teo[,i],medie3[i-1],sd3[i-1]) #riempita con numeri estratti da una normale
  #altra idea fare dnorm di quella teorica con un vettore generico di x mentre dnorm di matrice 3 con matrice 3
  #così poi si fa la differenza tra le y
}
err3=matrice3-mat3_teo

#impostazione test BlandAltman------
#script 
#install.packages("MethComp", repos="http://R-Forge.R-project.org")
#

#mydata <- read.table("MethComp.csv", header=TRUE, sep=";")

#library(MethComp)
#newdata <- Meth(mydata) # crea un oggetto Meth per la libreria
#plot.Meth(newdata)

#mydata è una tabella lunga che contiene 4 colonne:
#metodo, item, repl(?), y
#i valori devono essere entro -2,2 per far si che i metodi possano essere 
#congruenti
#Il plot mostra sulle ordinate la differenza tra le due misure, sulle 
#ascisse la misura di riferimento, ottenuta come media aritmetica delle due misure
#i livelli di confidenza sono ottenuti come [media  di riferimento]+-1.96SD
#se la variazione rispetto alla media non è importante i due metodi possono
#essere interscambiabili
#associate al Bland altman test sono associate misure di autocorrelazione 

#BLAND-ALTMAN
nomiba=c("C","D","E","F","G","H","I","J",'K',"L")
matrice7<-matricenorm[,2:ncol(matricenorm)]

x11() 
par( mfrow = c( 4, 7 ) )

for (i in 1:((ncol(matrice7))/2-2)){
  
  for(j in (i+1):(ncol(matrice7))){
    
    bland.altman.plot(matrice7[,i], matrice7[,j], main=paste(nomiba[i],nomiba[j]), xlab="Means", ylab="Differences")
    
    pl <- bland.altman.plot(matrice7[,i], matrice7[,j], graph.sys = "ggplot2")
    
  }
}

x11() 
par( mfrow = c( 4, 7 ) )

for (i in ((ncol(matrice7))/2-1):(ncol(matrice7)-1)){
  
  for(j in (i+1):(ncol(matrice7))){
    
    bland.altman.plot(matrice7[,i], matrice7[,j], main=paste(nomiba[i],nomiba[j]), xlab="Means", ylab="Differences")
    
    pl <- bland.altman.plot(matrice7[,i], matrice7[,j], graph.sys = "ggplot2")
    
  }
}

#correlazione--------
corr<-cor(matrice7[1:446,])
View(corr)
#metodi confrontabili: 
#metodi confrontabili: 
#"C","D","E","EFFE","G","H","I","J","K","L"
# 1   2   3     4    5   6   7   8   9  
#I-J, H-J, H-I(solo per valori centrali), E-H(solo valori centrali)
#E-G, D-E (solo per valori centrali), C-J (solo valori centrali), C-I, C-G, C-E, C-K, K-E


#fare regressioni dei metodi confrontabili e accoppiarli al grafico blendaltman


#Piccola era glaciale e caldo medioevo --------
#Era glaciale: 1350-1850
#Caldo medioevo: 850-1350

nomi2<-c('C', 'D', 'E', 'F', 'G', 'H', 'I', 'J','K', 'L')

mean_gl_norm=apply(matrice2[which(matrice2[,1]==1350):which(matrice2[,1]==1850),2:ncol(matrice2)],2,mean,na.rm=T)
mean_medioevo_norm=apply(matrice2[which(matrice2[,1]==850):which(matrice2[,1]==1350),2:ncol(matrice2)],2,mean,na.rm=T)
mean_tutto= apply(matrice2[which(matrice2[,1]==-500):which(matrice2[,1]==1960),2:ncol(matrice2)],2,mean,na.rm=T)

#Ciclo per individuare dove inizia la serie 
inizio_serie=rep(NA,ncol(matrice2)-1)
for (i in 2:ncol(matrice2)) {
  
  for (j in 1:nrow(matrice2)) {
    
    if(is.na(matrice2[j,i])==F && is.na(inizio_serie[i-1]))
      inizio_serie[i-1]=matrice2[j,1]
  }
}


#Grafici
posizioni_legenda=c("topleft","bottomleft","topleft","bottomleft","bottomleft","topleft","topleft","bottomleft","topleft","topleft")
posizioni_legenda2=c("bottomleft","topleft","bottomleft","topleft","topleft","bottomleft","bottomleft","topleft","bottomleft","bottomleft")

x11()
par( mfrow = c( 3, 4 ) )

for (i in 2:ncol(matrice2)){
  
  plot(t,matrice2[,i],type='l', main=paste(nomi2[i-1]), xlim=c(800,2020))
  lines(t[max(which(matrice2[,1]==inizio_serie[i-1]),which(matrice2[,1]==850)):which(matrice2[,1]==1350)],
        mean(matrice2[which(matrice2[,1]==850):which(matrice2[,1]==1350),i], na.rm=T)+0*t[max(which(matrice2[,1]==inizio_serie[i-1]),
                                                                                              which(matrice2[,1]==850)):which(matrice2[,1]==1350)],
        type='l',col='yellow',lwd='3')
  lines(t[max(which(matrice2[,1]==inizio_serie[i-1]),which(matrice2[,1]==1350)):which(matrice2[,1]==1850)],
        mean(matrice2[which(matrice2[,1]==1350):which(matrice2[,1]==1850),i], na.rm=T)+0*t[max(which(matrice2[,1]==inizio_serie[i-1]),
                                                                                               which(matrice2[,1]==1350)):which(matrice2[,1]==1850)],
        type='l',col='red',lwd='3')
  abline(a=(mean(matrice2[-(which(matrice2[,1]==1960):which(matrice2[,1]==2020)),i],na.rm=T)),b=0,col='blue') #ci fermiamo al 1960 perche il cambiamento climatico influenza la media 
  legend(posizioni_legenda[i-1],legend=c("Era glaciale", "Caldo medioevo", "Tutto"),col=c("red","yellow","blue"),lwd=1,pch=1)
  legend(posizioni_legenda2[i-1],legend=c(paste("pvalue gl: ",round(valueg[i-1],digits=2)), paste("pvalue cmed: ",round(valuec[i-1],digits=2))))
  
}

#ESCLUDIAMO i metodi che danno il contrario di quello che dovrebbe essere
#medio evo:C, E, I, K
#era glaciale: E, I, K

#T-TEST per confronto medie
#Si confrontano le medie dei due periodi con quella campionaria totale meno gli ultimi anni dal 1960 in poi

#Era glaciale 
valueg<-rep(NA,ncol(matrice2)-1)

for(i in 2:ncol(matrice2)){
  
  test=t.test(matrice2[-(which(matrice2[,1]==1960):which(matrice2[,1]==2020)),i], alternative = "less", mu=mean(matrice2[which(matrice2[,1]==1350):which(matrice2[,1]==1850),i], na.rm=T))#less perchè è l'ipotesti alternativa e noi dobbiamo verificarne l'opposto
  valueg[i-1]=test$p.value

}
valueg
#La soglia è 1-alpha quindi sotto 0.95 vanno tolti 

#Caldo medioevo
matricecaldo=matrix(c(D,EFFE,G,H,I,J,L),ncol=7)#Si prendono solo le colonne con i dati del periodo
valuec<-rep(NA,ncol(matricecaldo))

for(i in 1:ncol(matricecaldo)){
  
  test=t.test(matricecaldo[-(which(matrice2[,1]==1960):which(matrice2[,1]==2020)),i], alternative = "greater", mu=mean(matricecaldo[which(matrice2[,1]==850):which(matrice2[,1]==1350),i], na.rm=T))
  valuec[i]=test$p.value

}
valuec

#Dal test si conferma i metodi che beccano e che no in base ai p-value. 
#Capire perché alcuni sono 1 e altri 0.9

#TEST STATISTICI ------
#vanno fatti test di uguaglianza di media e varianza su tutti i campioni, uno con l'altro, per verificarne
#l'omogeneità. Se si vuole usare un rumore bianco per togliere i Na bisogna studiare la normalità dei residui
#o almeno fare un test di media = zero. Inoltre bisogna fare un test per verificare che i dati sono omoschedastici

#grazie a questi test si potranno poi applicare i vari modelli e fare tante cose belle :)

#bisognerà poi capire dalla teoria cosa fare e dove farlo, ma queste sono ipotesi alla base di tutto, oltre ad essere
#una delle richieste

# INTERVALLO DI CONFIDENZA BILATERO PER LA MEDIA --> TEST PER LA MEDIA
# H0 (ipotesi nulla) : mu = mu0
# H1 (ipotesi alternativa) : mu != mu0
alpha = 0.05
wn=NA*matrice3[,2:ncol(matrice3)]
normalewn=rep(NA,ncol(wn))

for (i in 1:ncol(wn)) {
  wn[,i]=matrice3[,i+1]-mean(matrice3[,i+1],na.rm=T)
  normalewn[i]=shapiro.test(wn[,i])$p.value
  
}
normalewn

p_valuewn=rep(NA,ncol(wn))
mediewn=rep(NA,ncol(wn))
for (i in 1:ncol(wn)) {
  mytest = t.test(wn[,i], conf.level = 1 - alpha)
  p_valuewn[i]=mytest$p.value
  mediewn[i]=mytest$estimate
  
}
p_valuewn
mediewn

#OMOSCHEDASTICITA'
omo_test = matrix(NA,ncol(matricenorm),ncol(matricenorm))
for (i in 1:ncol(matricenorm))
{
  for(j in 1:ncol(matricenorm))
  {
    test = list(matricenorm[,i],matricenorm[,j])
    bt = bartlett.test(test)
    omo_test[i,j] = bt$p.value
  }
}
colnames(omo_test)<-c('tempo',nomiba)
rownames(omo_test)<-c('metodo',nomiba)
View(omo_test)
btest<- omo_test>0.05
View(btest)
#levene test
library(lawstat)
lv_test = matrix(NA,ncol(matricenorm),ncol(matricenorm))
for (i in 1:ncol(matricenorm))
{
  for(j in 1:ncol(matricenorm))
  {
    y=c(matricenorm[1:446,i],matricenorm[1:446,j])
    Group=as.factor(c(rep(1, length(matricenorm[1:446,i])), rep(2, length(matricenorm[1:446,j]))))
    lt<-levene.test(y,Group)
    lv_test[i,j] = lt$p.value
  }
}
colnames(lv_test)<-c('tempo',nomiba)
rownames(lv_test)<-c('metodo',nomiba)
View(lv_test)
ltest<- omo_test>0.05
View(ltest)

#TEST SUI RESIDUI----------

nomiba=c("C","D","E","F","G","H","I","J",'K',"L")


for (i in 1:(ncol(matrice7))){
  for(j in (i+1):(ncol(matrice7))){
    
    x11() 
    par( mfrow = c( 2, 2) )
    stima<-lm(matrice7[,i]~matrice7[,j])
    summary(stima)
    plot(matrice7[,j],matrice7[,i],type='p', main=paste(nomiba[i],nomiba[j]))
    lines(matrice7[,j],stima$coefficients[1]+stima$coefficients[2]*matrice7[,j],col="red",type='l')
    summary(stima)
    
    #test sui residui
    #gaussianità dei residui
    res = stima$residuals
    qqnorm( res, main = 'QQ-Norm', pch = 16)
    qqline( res, col = 'red', lwd = 2, lty = 2)
    
    hist( res, prob = TRUE, breaks = 10, col = "lightcoral", main = "Istogramma residui")
    griglia = sort( res ) # ordino i dati
    lines( griglia, dnorm( griglia, mean = mean(res), sd = sd( res) ),
           col = 'maroon', lwd = 2 )
    
    #omoschedasticità
    res.std = stima$residuals/summary( stima )$sigma
    plot( stima$fitted.values, res.std, pch = 16,
          xlab = 'y stimata', ylab = 'residui standardizzati', main = 'Residui standardizzati' , ylim=c(-3,3))
    abline( h = 2, col = "red", lwd = 2, lty = 2 )
    abline( h = 0, col = "grey", lwd = 2, lty = 2 )
    abline( h = -2, col = "red", lwd = 2, lty = 2 )
    
  }
}
colnames(corr)<-nomiba
rownames(corr)<-nomiba
View(corr)
#omoschedascità barlett test: E-G, I-J, K-C, 
#omoschedasticità levene test: E-G, I-J, K-L
#C-E, C-K, D-J, K-E, L-J metodi correlati da matrice di correlazione
#I-J, H-J, H-I, E-H, E-G, D-E, C-J, C-I, C-G, C-E, C-K, K-E da blend altman test
#medio evo: D, F, G, H, J, L
#era glaciale: C, D, F, G, H, J, L
#changing point: E, F, G, K, L
#F,G,L
x11()

#ANOVA---------
#abbiamo individuato il caldo medio evo e la piccola era glaciale e impotizzando il cambiamento 
#climatico dividiamo il dataset in 5 gruppi 
#utilizzo i vettori delle colonne complete prima di essere riempite

matriceanova<-matrice2norm[,2:ncol(matrice2norm)]
mediatot<-0
anova(Dati)

#Legenda matrici ---------------
#matrice ha tutte le colonne estratte dal dataset senza quella degli anni
#matrice2 è senza B, ma con anni
#matrice3 è quella in cui sono ridotte le righe partendono dalla prima in cui iniziano 
          #tutte le serie (anno 1500). Non c'è B
#matrice4 è quello con i NA riempiti
#matricenorm ha gli anni e i valori normalizzati sulla media 1900-1945
#Commenti-----------------
#Tutto salvato nel file di testo allegato



#CLUSTERIZZARE LE SERIE TEMPORALI. LA SERIE IN QUESTO LUOGO ASSOMIGLIA A QUELLE IN QUEST'ALTRO LUOGO
#