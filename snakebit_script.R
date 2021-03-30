require(dplyr)
require(binom)
require(questionr)
getwd() # repertoire courant
repertoire<-"C:/Users/HP/Documents/base_morsure"
setwd(repertoire)
readLines("Base.txt",n=5)
Morsure<-read.table("Base.txt",header=T,dec=",",sep="\t",quote="")
View(Morsure)
dim(Morsure)
dput(colnames(Morsure))
table(Morsure$Date_morsure)
table(Morsure$NF_serpent)
####################################################
### Recodage de la base ###
Morsure$Sexe_r<-factor(Morsure$Sexe,levels=c(1,2),labels=c("Homme","Femme"))

Morsure$Socio_prof_r<-factor(Morsure$Socio_prof,
                             levels=c(1,2,3,4,5,6,7),
                             labels=c("Enfant","Agriculteur","Eleveur","Commercant",
                                      "Fonctionnaire","Secteur_Prive","Menager"))

Morsure$etat_seprent_r<-factor(Morsure$etat_seprent,levels=c(1,2),labels=c("Tue","Conserve"))

Morsure$Saison_r<-factor(Morsure$Saison,levels=c(1,2),labels=c("Pluvieuse","Seche"))

Morsure$Champetre_r<-factor(Morsure$Champetre,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Rech_bois_r<-factor(Morsure$Rech_bois,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Deplacement_r<-factor(Morsure$Deplacement,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Paturage_r<-factor(Morsure$Paturage,levels=c(1,2),labels=c("Oui","Non"))

Morsure$domicile_r<-factor(Morsure$domicile,levels=c(1,2),labels=c("Oui","Non"))

Morsure$chasse_r<-factor(Morsure$chasse,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Peche_r<-factor(Morsure$Peche,levels=c(1,2),labels=c("Oui","Non"))

Morsure$autre_r<-factor(Morsure$autre,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Main_r<-factor(Morsure$Main,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Bras_r<-factor(Morsure$Bras,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Avant_bras_r<-factor(Morsure$Avant_bras,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Pied_r<-factor(Morsure$Pied,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Jambe_r<-factor(Morsure$Jambe,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Cuisse_r<-factor(Morsure$Cuisse,levels=c(1,2),labels=c("Oui","Non"))

Morsure$oedeme_r<-factor(Morsure$oedeme,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Saignement_r<-factor(Morsure$Saignement,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Vomissement_r<-factor(Morsure$Vomissement,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Perte_de_connaissance_r<-factor(Morsure$Perte_de_connaissance,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Cephales_r<-factor(Morsure$Cephales,levels=c(1,2),labels=c("Oui","Non"))

Morsure$local_prise_en_charge_r<-factor(Morsure$local_prise_en_charge,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Centre_sante_prise_r<-factor(Morsure$Centre_sante_prise,levels=c(1,2),labels=c("Oui","Non"))

Morsure$amoxi_r<-factor(Morsure$amoxi,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Ampi_r<-factor(Morsure$Ampi,levels=c(1,2),labels=c("Oui","Non"))

Morsure$hydrocor_r<-factor(Morsure$hydrocor,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Para_r<-factor(Morsure$Para,levels=c(1,2),labels=c("Oui","Non"))

Morsure$penicilline_r<-factor(Morsure$penicilline,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Ibuprof_r<-factor(Morsure$Ibuprof,levels=c(1,2),labels=c("Oui","Non"))

Morsure$cocktail_r<-factor(Morsure$cocktail,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Fer_r<-factor(Morsure$Fer,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Metro_r<-factor(Morsure$Metro,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Pierre_noir_r<-factor(Morsure$Pierre_noir,levels=c(1,2),labels=c("Oui","Non"))

Morsure$Pcharge_locale_r<-factor(Morsure$Pcharge_locale,levels=c(1,2,3),
                                 labels=c("Pas_efficace","efficace","Avec_Sequelle"))

Morsure$Pcharge_CSPS_r<-factor(Morsure$Pcharge_CSPS,levels=c(1,2,3),
                               labels=c("Pas_efficace","efficace","Avec_Sequelle"))

#############   Fin_recodage       ########
Base_Mors<-data.frame(Id=Morsure$identifi,CSPS=Morsure$centre_sante,Village=Morsure$village, Eth=Morsure$Ethenie, Type_CS=Morsure$Type_centre_snate,Date_mors=Morsure$Date_morsure, Date_Enq=Morsure$Date_enquete, Age=Morsure$Age,Sexe=Morsure$Sexe_r,S_Prof=Morsure$Socio_prof_r,Etat_sep=Morsure$etat_seprent_r, Saison=Morsure$Saison_r,Champtre=Morsure$Champetre_r, Rch_bois=Morsure$Rech_bois_r,Dplacmnt=Morsure$Deplacement_r,Paturage=Morsure$Paturage_r,domicile=Morsure$domicile_r,Chasse=Morsure$chasse_r,
                      
                      Peche=Morsure$Peche_r,autre=Morsure$autre_r,Main=Morsure$Main_r, Bras=Morsure$Bras_r,Av_Bras=Morsure$Avant_bras_r, Pieds=Morsure$Pied_r,Jambe=Morsure$Jambe_r, Cuisse=Morsure$Cuisse_r,oedeme=Morsure$oedeme_r,Saignement=Morsure$Saignement_r,Vomissement=Morsure$Vomissement_r, P_conssanc=Morsure$Perte_de_connaissance_r,Cephales=Morsure$Cephales_r, LP_Chrge=Morsure$local_prise_en_charge_r,CS_Chrge=Morsure$Centre_sante_prise_r, 
                      
                      amoxi=Morsure$amoxi_r,Ampi=Morsure$Ampi_r, Hydrocoti=Morsure$hydrocor_r,Para=Morsure$Para_r, Peni=Morsure$penicilline_r,Ibu=Morsure$Ibuprof_r, Cktail=Morsure$cocktail_r,Fer=Morsure$Fer_r, Metro=Morsure$Metro_r,Pierre_noir=Morsure$Pierre_noir_r, Pchrg_locale=Morsure$Pcharge_locale_r,Pchrg_CSPS=Morsure$Pcharge_CSPS_r, Etat_Patient=Morsure$Etat_patient)

write.table(Base_Mors,"Mors.txt",dec=",",sep="\t",row.names=F)
## lecture de la base reocder ##
require(dplyr)
require(binom)
require(questionr)
getwd() # repertoire courant
repertoire<-"C:/Users/hp/Documents/base"
setwd(repertoire)
Mors<-read.table("Mors.txt",header=T,dec=",",sep="\t")
View(Mors)
dim(Mors)
dput(colnames(Mors))
##################################################
"Id"
"CSPS"
"Village"
"Eth"
"Type_CS"
"Date_Enq"
"Age" 
"Sexe"
"S_Prof"
"Etat_sep"
"Saison"
"Champtre"
"Rch_bois" 
"Dplacmnt"
"Paturage"
"domicile"
"Chasse"
"Peche"
"autre" 
"Main"
"Bras"
"Av_Bras"
"Pieds"
"Jambe"
"Cuisse"
"LP_Chrge" 
"CS_Chrge"
"amoxi"
"Ampi"
"Hydrocoti"
"Para"
"Peni"
"Ibu" 
"Cktail"
"Fer"
"Metro"
"Pierre_noir"
"Pchrg_locale"
"Pchrg_CSPS" 
"Etat_Patient"
"Age_q"
################################################# Traitement ######
Sexe<-round(table(Mors$Sexe)/sum(table(Mors$Sexe))*100,1)
Sexe


##############################################

Amox<-round(table(Mors$amoxi)/sum(table(Mors$amoxi))*100,1)
Amox



Ampi<-round(table(Mors$Ampi)/sum(table(Mors$Ampi))*100,1)
Ampi

hydro<-round(table(Mors$Hydrocoti)/sum(table(Mors$Hydrocoti))*100,1)
hydro

Paracet<-round(table(Mors$Para)/sum(table(Mors$Para))*100,1)
Paracet

Penicili<-round(table(Mors$Peni)/sum(table(Mors$Peni))*100,1)
Penicili

Ibup<-round(table(Mors$Ibu)/sum(table(Mors$Ibu))*100,1)
Ibup

cocktail<-round(table(Mors$Cktail)/sum(table(Mors$Cktail))*100,1)
cocktail

Fer_A<-round(table(Mors$Fer)/sum(table(Mors$Fer))*100,1)
Fer_A

Pnoire<-round(table(Mors$Pierre_noir)/sum(table(Mors$Pierre_noir))*100,1)
Pnoire
#### Treatment ###
treat<-c(47.8,28.5,39.3,55.9,13.8,7.2,6.2,1.9,59.1,8.2)
names(treat)<-c("Amo","Amp","Hyd","Para",
                "Peni","Ibu","Cock","Iron",
                "Met","B.st")
Mors_treat <-barplot(treat,
                     cex.main=1.5,
                     col.main=2,
                     font.main=8,  
                     xlab= "administrated treatments",
                     ylab="Treatments frequencies",
                     las=1,
                     col=rainbow(10),
                     space=c(0,0.3),
                     ylim=c(0,100))

legend(x=5,
       y=95,
       legend=c("Amoxicilline (Amo)",
                "Ampicilline (Amp)",
                "Hydrocortisone(Hyd)",
                "Paracetamol(Para)",
                "Penicilline(Peni)",
                "Ibuprofene(Ibu)",
                "Cocktail(Cock)",
                "Iron(Iron)",
                "Metronidazole(Met)",
                "Black stone(B.St)"),
       fill=rainbow(10),
       box.lwd=2,
       text.font=9,
       y.intersp = 0.95)
for(i in 1:length(treat))text(Mors_treat[i],
                              treat[i]+2.30,
                              paste(labels=as.character(treat[i]),"%"),
                              col="black",cex=1,font=9)
box(lwd=2)              
############# Signes cliniques ######
signes<-c(83.3,51,3,3,25.1)
names(signes)<-c("oed","Ble",
                 "Vomi","L.con","Head")
Mors_Signes <-barplot(signes,
                      cex.main=1.5,
                      col.main=2,
                      font.main=8,  
                      xlab= "Clinical Signs",
                      ylab="Fréquencies of Signs",
                      las=1,
                      col=rainbow(5),
                      space=0.2, 
                      ylim=c(0,100))
legend(x=2.6,
       y=90,
       legend=c("Oedema(oed)",
                "Bleeding(Ble)",
                "Vomiting(Vomi)",
                "loss of conciousness(L.con)",
                "Headache(Head)"),
       fill=rainbow(5),
       box.lwd=2,
       text.font=9,
       pt.cex=1.5)
for(i in 1:length(signes))text(Mors_Signes[i],
                               signes[i]+3.00,
                               paste(labels=as.character(signes[i]),"%"),col="black",cex=1.5,font=9)
box(lwd=2)              
#### Serpent identifiers ###
Serp<-c(1.32,10.16,88.5)
names(Serp)<-c("Elapid","Grass-snake","Viper")
Mors_Serp<-barplot(Serp,
                   cex.main=1.5,
                   col.main=2,
                   font.main=8,  
                   xlab= "Snakes Species",
                   ylab="Snakesbites of frequencies",
                   las=1,
                   col=c("black","skyblue","yellowgreen"),
                   space=0.2, 
                   ylim=c(0,100))
legend(x=0.19,
       y=80,
       legend=c("Elapid","Grass-snake","Viperidae(Viper)"),
       fill=c("black","skyblue","yellowgreen"),
       box.lwd=2,
       text.font=9,
       pt.cex=1.5)
for(i in 1:length(Serp))text(Mors_Serp[i],
                             Serp[i]+2.50,paste(labels=as.character(Serp[i]),"%"),col="black",cex=1.5,font=9)
box(lwd=2) 





###### Signe clinique ###
oedeme<-round(table(Mors$oedeme)/sum(table(Mors$oedeme))*100,1)
oedeme

Saignement<-round(table(Mors$Saignement)/sum(table(Mors$Saignement))*100,1)
Saignement

Vomissement<-round(table(Mors$Vomissement)/sum(table(Mors$Vomissement))*100,1)
Vomissement

P_conssanc<-round(table(Mors$P_conssanc)/sum(table(Mors$P_conssanc))*100,1)
P_conssanc

Cephales<-round(table(Mors$Cephales)/sum(table(Mors$Cephales))*100,1)
Cephales


################ date de la morsure #######
Mors$Date_mors<-as.character(Mors$Date_mors)
Mors$Jour<-substring(Mors$Date_mors,1,2)
Mors$mois<-substring(Mors$Date_mors,4,5)
Mors$annee<-substring(Mors$Date_mors,7,10)
## reviser les type de données #####
table(Mors$mois)
table(Mors$Jour)
table(Mors$annee)
###############
Mors$mois<-factor(Mors$mois)
Mors$annee<-factor(Mors$annee)

Annee<-round(table(Mors$annee)/sum(table(Mors$annee))*100,1) 
Annee
Anne_Mors <-barplot(Annee,
                    cex.main=1.5,
                    col.main="blue",
                    font.main=9,  
                    xlab= "Years of bites",
                    cex.axis=1,
                    cex.names=1,
                    ylab="Frequencies of bites",
                    las=1,
                    col=c("yellow","green","magenta","red","turquoise"),
                    lwd=2,
                    space=0.2, 
                    ylim=c(0,100))
## Legnedre du graphique ###               
legend(x=0.25,
       y=95,
       legend= c("2014","2015","2016","2017","2018"),
       fill=c("yellow","green","magenta","red","turquoise"),
       box.lwd=2,
       text.font=9,
       pt.cex=1.5)
for(i in 1:length(Annee))text(Anne_Mors[i],
                              Annee[i]+2.65,paste(labels=as.character(Annee[i]),"%"),col="black",cex=1.5,font=9)
box(lwd=2)

##############################################################
#### repartition des morsure en de mois de l'annee 2017 #####
Mors2017<-subset(Mors,Mors$annee == "2017")
mois<-round(table(Mors$mois)/sum(table(Mors$mois))*100,1)
mois
table(mois)
names(mois)<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
mois2017
mois <-barplot(mois,
                    cex.lab = 1,
                    las=1,
                    font.lab = 7,
                    cex.axis =1,
                    cex.names = 1,
                    font.axis =7,  
                    ylab= "(%) Snake bite",
                    xlab="Months of bite",
                    col=rainbow(12),
                    space=c(0,0.3),
                    ylim=c(0,100))
legend(x=9,
       y=90,
       legend= c("January","February",
                 "March","April","May",
                 "June","July","August",
                 "September","October",
                 "November","December"), 
       fill=rainbow(12),
       box.lwd=1,
       text.font=7,
       y.intersp = 0.8)
############## season ########
Mors$Date_mors<-as.character(Mors$Date_mors)
Mors$Jour<-substring(Mors$Date_mors,1,2)
Mors$mois<-substring(Mors$Date_mors,4,5)
Mors$annee<-substring(Mors$Date_mors,7,10)
## reviser les type de données #####
table(Mors$mois)
Mors$mois <- factor(Mors$mois)
table(Mors$Jour)
table(Mors$annee)
Mors$season <- as.character(Mors$mois)
table(Mors$mois)
Mors$season[Mors$mois =="05"] <- "raining season"
Mors$season[Mors$mois =="06"] <- "raining season"
Mors$season[Mors$mois =="07"] <- "raining season"
Mors$season[Mors$mois =="08"] <- "raining season"
Mors$season[Mors$mois =="09"] <- "raining season"
Mors$season[Mors$mois =="10"] <- "raining season"

Mors$season[Mors$mois =="11"] <- "drying season"
Mors$season[Mors$mois =="12"] <- "drying season"
Mors$season[Mors$mois =="01"] <- "drying season"
Mors$season[Mors$mois =="02"] <- "drying season"
Mors$season[Mors$mois =="03"] <- "drying season"
Mors$season[Mors$mois =="04"] <- "drying season"










Mors$season[Mors$mois %in% c("05","06","07","08","09","10")] <- "raining season"
Mors$seasonc[Mors$mois %in% c("11","12","01","02","03","04")] <- "drying season"
table(Mors$season)
Mors$season <- factor(Mors$season)


####### Activite pendant laquelle morsure #####
Mors$Activ_Mors<- NA
Mors$Activ_Mors[Mors$Champtre == "Oui"]<- "Chmptre"
Mors$Activ_Mors[Mors$Rch_bois == "Oui"]<- "R.bois"
Mors$Activ_Mors[Mors$Dplacmnt == "Oui"]<- "Dplcmt"
Mors$Activ_Mors[Mors$Paturage == "Oui"]<- "Paturage"
Mors$Activ_Mors[Mors$Chasse == "Oui"]<- "Chasse"
Mors$Activ_Mors[Mors$Peche == "Oui"]<- "Peche"
Mors$Activ_Mors[Mors$autre == "Oui"]<- "autre"
Mors$Activ_Mors<-factor(Mors$Activ_Mors)
table(Mors$Activ_Mors)
###############################################
tab<-table(Mors$S_Prof,Mors$Activ_Mors, deparse.level=2)
lprop(tab)
chisq.test(tab)




###### Catetgorie socio prof ####
Mors$Cat_socio<- NA
Mors$Cat_socio[Mors$S_Prof == "Agriculteur"]<- "Farm"
Mors$Cat_socio[Mors$S_Prof == "Commercant"]<- "Trad"
Mors$Cat_socio[Mors$S_Prof == "Eleveur"]<- "Bree"
Mors$Cat_socio[Mors$S_Prof == "Enfant"]<- "Child"
Mors$Cat_socio[Mors$S_Prof == "Fonctionnaire"]<-"CS"
Mors$Cat_socio[Mors$S_Prof == "Menager"]<- "HW"
Mors$Cat_socio[Mors$S_Prof == "Secteur_Prive"]<- "PS"
Mors$Cat_socio<-factor(Mors$Cat_socio)
table(Mors$Cat_socio)
Socio<-round(table(Mors$Cat_socio)/sum(table(Mors$Cat_socio))*100,1)
Socio
Socio_Mors <-barplot(Socio,
                     cex.main=1.5,
                     col.main="blue",
                     font.main=8,  
                     xlab= "Socio-Professional Category",
                     ylab="Frequencies of bites",
                     las=1,
                     col=rainbow(7),
                     space=0.2, 
                     ylim=c(0,100))
## Legnedre du graphique ###               
legend(x=6,
       y=90,
       legend= c("Breeder(Bree)",
                 "Children(Child)",
                 "Civil servant(CS)",
                 "Farmer(Farm)",
                 "HouseWife(HW)",
                 "Private sector(PS)",
                 "Trader(Trad)"),
       fill=rainbow(7),
       box.lwd=2,
       text.font=9,
       y.intersp = 0.7)
#####################
for(i in 1:length(Socio))text(Socio_Mors[i],
                              Socio[i]+2.25,paste(labels=as.character(Socio[i]),"%"),col="black",cex=1.25,font=9)
box(lwd=2)   


#############
Mors$Siege_Mors<- NA
Mors$Siege_Mors[Mors$Main == "Oui"]<- "Hand"
Mors$Siege_Mors[Mors$Bras == "Oui"]<- "Arm"
Mors$Siege_Mors[Mors$Av_Bras == "Oui"]<- "Forearm"
Mors$Siege_Mors[Mors$Pieds == "Oui"]<- "Feet"
Mors$Siege_Mors[Mors$Jambe == "Oui"]<- "Leg"
Mors$Siege_Mors[Mors$Cuisse == "Oui"]<- "Thigh"
Mors$Siege_Mors<-factor(Mors$Siege_Mors)
table(Mors$Siege_Mors)
Sige<-round(table(Mors$Siege_Mors)/sum(table(Mors$Siege_Mors))*100,1)
Sige
par(col.axis="red",bg="lightblue")
par(bg="lightblue")

S_Mors <-barplot(Sige,
                 cex.main=1.5,
                 col.main=2,
                 font.main=8,  
                 xlab= "Seats of bites",
                 ylab="Frequencies of bites",
                 las=1,
                 col=rainbow(6),
                 space=0.2, 
                 ylim=c(0,100))
legend(x=5,
       y=70,
       legend=c("Arm",
                "Feet",
                "Forearm",
                "Hand",
                "Leg",
                "Thigh"),
       fill=rainbow(6),
       box.lwd=2,
       text.font=9,
       y.intersp = 0.8)
for(i in 1:length(Sige))text(S_Mors[i],
                             Sige[i]+2.5,
                             paste(labels=as.character(Sige[i]),"%"),col="black",cex=1.5,font=9)
box(lwd=2)                


################################3
Mors$Age_q<- NA
Mors$Age_q[Mors$Age>=0 & Mors$Age<=4]<-"00-04"
Mors$Age_q[Mors$Age>=5 & Mors$Age<=9]<-"05-09"
Mors$Age_q[Mors$Age>=10 & Mors$Age<=14]<-"10-14"
Mors$Age_q[Mors$Age>=15 & Mors$Age<=19]<-"15-19"
Mors$Age_q[Mors$Age>=20 & Mors$Age<=24]<-"20-24"
Mors$Age_q[Mors$Age>=25 & Mors$Age<=29]<-"25-29"
Mors$Age_q[Mors$Age>=30 & Mors$Age<=34]<-"30-34"
Mors$Age_q[Mors$Age>=35 & Mors$Age<=39]<-"35-39"
Mors$Age_q[Mors$Age>=40 & Mors$Age<=44]<-"40-44"
Mors$Age_q[Mors$Age>=45 & Mors$Age<=49]<-"45-49"
Mors$Age_q[Mors$Age>=50 & Mors$Age<=54]<-"50-54"
Mors$Age_q[Mors$Age>=55 & Mors$Age<=59]<-"55-59"
Mors$Age_q[Mors$Age>=60 & Mors$Age<=64]<-"60-64"
Mors$Age_q[Mors$Age>=65 & Mors$Age<=69]<-"65-69"
Mors$Age_q[Mors$Age>=70 & Mors$Age<=74]<-"70-74"
Mors$Age_q[Mors$Age>=75 & Mors$Age<=79]<-"75-79"
Mors$Age_q[Mors$Age>=80]<-"80 over"
## convertion en variable qualitative###
Mors$Age_q<-factor(Mors$Age_q)
table(Mors$Age_q)

###### Pyramide des âges ####
require(plotrix)

tab1<-table(Mors$Age_q,Mors$Sexe)
cprop(tab1,total=T,percent=T,digits=2)

femme <- c(2.10,11.54,9.79,10.84,
           10.49,11.89,16.78,5.24,
           5.94,5.94,4.55,1.75,1.75,
           0.70,0.35,0.35)

homme <- c(2.39,14.59,20.16,11.41,
           9.55,7.43,5.04,7.16,5.31,
           5.57,3.98,1.86,2.12,0.80,
           1.59,1.04)

agelab <- c("00-04","05-09","10-14",
            "15-19","20-24","25-29",
            "30-34","35-39","40-44",
            "45-49","50-54","55-59",
            "60-64","65-69","70-74",
            "75-79")

mcolor<-color.gradient(c(0,0,0.5,1),
                        c(0,0,0.5,1),
                        c(1,1,0.5,1),
                        16) 

fcolor<-color.gradient(c(1,1,0.5,1),
                       c(0.5,0.5,0.5,1),
                        c(0.5,0.5,0.5,1),
                        16)
par(mar=pyramid.plot(homme,femme,
                     labels=agelab,
                     lxcol = mcolor,
                     rxcol = fcolor,
                     gap=1.2,
                     show.values = TRUE,
                     unit ="%"))
par(mar=oldmar, bg="transparent")

##########################################################
##  calcul de l'incidence et son intervalle de confiance 
##
#########################################################

P2014 <- c(22025,4096,6649,
           7653,4852,
           9885,12914,56865,
           17332,24656,166927)

P2015 <- c(22752,4251,6868,
           7906,5013,
           10183,13363,60442,
           17751,25469,173998)

P2016 <- c(23504,4369,7093,
           8164,5177,
           10516,1377,64693,
           18332,26303,169528)

P2017 <- c(24258,4511,7323,
           8428,5344,
           10857,14223,66789,
           18926,27154,187813)


P2018 <- c(25036,4804,7558,
           8698,5516,
           11205,14680,68932,
           19533,28025,193987)

Pop<-data.frame(P2014,P2015,P2016,P2017,P2018)
Pop



ncs_2014 <- c(0,1,5,
              12,0,
              8,9,0,
              68,26,129)

ncs_2015 <- c(0,8,4,
              0,0,
              11,30,0,
              60,26,139)

ncs_2016 <- c(7,4,15,
              3,7,
              13,19,1,
              36,27,132)

ncs_2017 <- c(6,9,3,
              11,12,
              24,33,3,
              22,23,146)

ncs_2018 <- c(6,12,10,
              4,5,
              8,15,2,
              28,28,118)


Ncs<-data.frame(ncs_2014,ncs_2015,ncs_2016,ncs_2017,ncs_2018)
Ncs


Incidence<-data.frame(Incid_14 = round(Ncs$ncs_2014/Pop$P2014*1000,2),
                      Incid_15 = round(Ncs$ncs_2015/Pop$P2015*1000,2),
                      Incid_16 = round(Ncs$ncs_2016/Pop$P2016*1000,2),
                      Incid_17 = round(Ncs$ncs_2017/Pop$P2017*1000,2),
                      Incid_18 = round(Ncs$ncs_2018/Pop$P2018*1000,2))

#etiquette des linge#
row.names(Incidence) <- c("Bolomakote",
                          "Bouende",
                          "Kiri",
                          "Leguema",
                          "Logofourousso",
                          "Matourkou",
                          "Nasso",
                          "Sarfalao",
                          "Soumousso",
                          "Valle du Kou",
                          "overall")
Incidence

options(digits=2) # Confirm default digits.
options(scipen=999)# Suppress scientific notation.

# installation du package rcompanion

if(!require(rcompanion)){install.packages("rcompanion")}
require("rcompanion")
#######################################

Incidenc_2014 <- data.frame(P2014,ncs_2014)
 
row.names(Incidenc_2014) <-c("Bol",
                  "Bou",
                  "Kir",
                  "Leg",
                  "Log",
                  "Mat",
                  "Nas",
                  "Sar",
                  "Sou",
                  "Val",
                  "over")


te


chisq.test(Incidenc_2014)

te <- as.matrix(Incidenc_2014)
#####
pairwiseNominalIndependence(te,
                            fisher = FALSE,
                            gtest  = FALSE,
                            chisq  = TRUE,
                            method = "fdr")


####

FUN = function(i,j){    
        chisq.test(matrix(c(Incidenc_2014[i,1],
                            Incidenc_2014[i,2],
                            Incidenc_2014[j,1],
                            Incidenc_2014[j,2]),
                            nrow=2,
                            byrow=TRUE))$ p.value
}

comp <- pairwise.table(FUN,
               rownames(Incidenc_2014),
               p.adjust.method="none")
comp

symnum(comp)









require("survival")

pois.approx <- function(x, pt, conf.level = .95) {
        # x = Poisson count
        # pt = person time
        # do calculations
        Z <- qnorm(0.5*(1+conf.level))
        SE.R <- sqrt(x/pt^2)
        lower <- x/pt - Z*SE.R
        upper <- x/pt + Z*SE.R
        # collect results into one object
        cbind(NCS=x,
              Pop=pt,
              Incidence=round(x/pt*1000,2),
              conf.level=conf.level,
              lower.ci=round(lower*1000,2), 
              upper.ci=round(upper*1000,2))
}



Incidence_2014_CI<- pois.approx(Ncs$ncs_2014,Pop$P2014)
Incidence_2014_CI

Incidence_2015_CI<- pois.approx(Ncs$ncs_2015,Pop$P2015)
Incidence_2015_CI

Incidence_2016_CI<- pois.approx(Ncs$ncs_2016,Pop$P2016)
Incidence_2016_CI

Incidence_2017_CI<- pois.approx(Ncs$ncs_2017,Pop$P2017)
Incidence_2017_CI

Incidence_2018_CI<- pois.approx(Ncs$ncs_2018,Pop$P2018)
Incidence_2018_CI




te <- t(Incidence)
te



Incidence


plot.ts(te, 
        plot.type="single",
        xlab=" ",
        ylab="#/10,000 person-years", 
        xaxt="n", 
        col=rainbow(11),
        lwd=4,
        lty=c(1,1,1,1),
        las=1)
points(rep(1:10,5),
       Incidence,
       pch=22,
       cex=Incidence)



axis(side=1 ,at=1:5, labels =c("2014",
                                "2015",
                                "2016",
                                "2017",
                                "2018"))

legend(x=4,
       y=13,
       legend=c("Bol",
                "Bou",
                "Kir",
                "Leg",
                "Log",
                "Mat",
                "Nas",
                "Sar",
                "Sou",
                "Val",
                "overall"),
       fill=rainbow(11),
       box.lwd=2,
       text.font=9,
       pt.cex=1.5)


tu <- Ncs/Pop
tu





################################
tab<-table(Mors$Age_q,Mors$Sexe)
Homme <-tab[,2]
Femme <- -tab[,1]
grad.abs.Femme <-seq(80,0,by=-5)
grad.abs.Homme <-seq(0,80,by=5)
grad.ord <-rownames(tab)
par(mfrow=c(1,2),ps=12)
par(mar=c(4,2,3,0))
barplot(Femme, horiz=T, main="Women",cex.main=1.5,col.main="brown",font.main=9,space=0,col="blue",axes=F,xlim=c(min(-grad.abs.Femme),0),
        axisnames=F,cex.axis=0.7)

axis(1,at=-grad.abs.Femme,labels=grad.abs.Femme,cex.axis=0.7,las=3)


par(mar=c(4,2,3,2))
barplot(Homme, horiz=T, main="Men",cex.main=1.5,col.main="brown",font.main=9,space=0,col="red",axes=F,xlim=c(0,max(grad.abs.Homme)),
        axisnames=F,cex.axis=0.7)

axis(1,at=grad.abs.Homme,labels=grad.abs.Homme,
     cex.axis=0.7,las=3)

axis(2, at=c(1:length(grad.ord)),labels=grad.ord,
     las=1,pos=5,padj=1.5,tcl=0,lty=0, cex.axis=0.8)

tab1<-table(Mors$Age_q,Mors$Sexe)
cprop(tab1,total=T,percent=T,digits=1)
###### Fin Pyramide des ages ##########