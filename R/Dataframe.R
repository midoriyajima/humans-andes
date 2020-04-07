# Libraries
library(readxl)
library(tidyverse)
library(writexl)

#### adjust Datasheet--------------------------------------
Datasheet_original<-read_excel("data/Datasheet_original.xlsx")

#add Bin_num column
Bin_num<-sub(".*\\-","",Datasheet_original$Bin)
Bin_num<-sub("\\ BP.*","",Bin_num)
Datasheet<-Datasheet_original%>%mutate(Bin_num=Bin_num)
Datasheet$Bin_num<-as.numeric(Datasheet$Bin_num)

#save new version of Datasheet
write_xlsx(Datasheet,"Datasheet.xlsx")

#### add country, lat, long to Datasheet--------------------
#Df with metadata for all sites (even the ones that are not in Datasheet)
LAPD_Andes <- read_excel("data/LAPD_Andes_MY.xlsx")
LAPD_Andes$LAPD_ID<-as.numeric(LAPD_Andes$LAPD_ID)

#select sites from LAPD_andes_final that are in Datasheet 
DS<-Datasheet%>%filter(LAPD_ID!="NA")%>%select(LAPD_ID)%>%distinct(LAPD_ID) #excluding sites NA (sites further added to Datasheet which were not present in LAPD_Andes_final)
DS<-as.numeric(DS$LAPD_ID)
A=LAPD_Andes[LAPD_Andes$LAPD_ID %in% DS, ]

#add Country, Lat, Long to NA Sites (sites that were absent in LAPD_andes_final)
na_id=Datasheet%>%filter(LAPD_ID=="NA")
na_sites<-data.frame(c("Tres Lagunas","El Triunfo","Anteojos Valley","Huila\n","Agua Blanca PAB III", "Paramo de Laguna Verde I","Paramo de Laguna Verde II","Paramo de Pena Negra 1" ),
                     c("Ecuador","Colombia","Ecuador","Ecuador","Colombia","Colombia","Colombia","Colombia"),
                     c(-3.05145,4.58545,-0.57946,-00.25405,5.00000,5.15000,5.15000,5.05000),
                     c(-79.24822,-75.19558,-78.24397,-78.01075,-74.10000,-74.00000,-74.00000,-74.05000))
names(na_sites)<-c("SiteName", "Country","Latitude", "Longitude")

#Create a single df containing metadata to add to Datasheet
A=bind_rows(A,na_sites)

#(1)add columns with country, latitude and longitude to Datasheet, per each site
Datasheet$Country<-A$Country[match(Datasheet$`Site Name`,A$SiteName)]
Datasheet$Latitude<-A$Latitude[match(Datasheet$`Site Name`,A$SiteName)]
Datasheet$Longitude<-A$Longitude[match(Datasheet$`Site Name`,A$SiteName)]

#(2)some values in Country,Lat,Long columns of Datasheet result as NA-> probably Site Names between A and Datasheet are different
#to check wich Sites have different names
Sitename=distinct(Datasheet,`Site Name`)
Nomatch<-match(Sitename$`Site Name`,LAPD_Andes$SiteName)
which(is.na(Nomatch))
Sites_Nomatch<-Sitename[c(7,10,13,22,32,33,36),]
Sites_Nomatch$`Site Name`
LAPD_Andes$SiteName

#rename Datasheet Site Names according to the ones in LAPD_Andes
to_replace<-c("Cerro Toledo", "El Tiro\n" , "Laguna Natosas forest", "Pedras Blancas","Huila", "Paramo de Agua Blanca III", "Paramo de pena Negra I")
with_this<-c("Cerro Toledo CT", "El Tiro", "Lagunas Natosas Forest", "Piedras Blancas", "Huila\n", "Agua Blanca PAB III", "Paramo de Pena Negra 1")
Datasheet$`Site Name`<-Datasheet$`Site Name`%>%plyr::mapvalues(to_replace,with_this)

#Repeat (1)
#see if there something NA left, and which site it corresponds to
view(filter(Datasheet,is.na(Country)))

#### new shorter datasheet------------------------------------
#select record spanning last 12000 yr
Datasheet_shaved<-Datasheet%>%filter(Bin_num<12500)

#select only indicators actually found
DF.zeros<-(DF.indicators.SUM%>%filter(IN==0)) #"DF.indicators.SUM" from Human indicators.R
Todrop<-as.vector(DF.zeros$IN.name)
Datasheet_shaved<-Datasheet_shaved[,!names(Datasheet_shaved)%in% Todrop]

write_xlsx(Datasheet_shaved,"Datasheet_shaved.xlsx")

#add column with records lenght to Datasheet_shaved-------
length_record<-Datasheet_shaved%>%group_by(`Site Name`)%>%summarise(max(Bin_num))
length_record$Length<-length_record$`max(Bin_num)`
Datasheet_shaved$Length<-length_record$Length[match(Datasheet_shaved$`Site Name`,length_record$`Site Name`)]

#add metadata absent in LAPD_Andes
which(is.na(LAPD_Andes_MY$Altitude))
LAPD_Andes_MY[72,"Altitude"]=2608 #value found in the original paper

write_xlsx(LAPD_Andes_MY, "LAPD_Andes_MY.xlsx")