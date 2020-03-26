library(readxl)
library(tidyverse)
#### OVERVIEW TABLE ------------------------------------

## Step 1: add coordinates and countries to Dataframe

#Df with presence/absence of indicators per time bin per site
Datasheet <- read_excel("data/Datasheet.xlsx")
#Df with metadata for all sites (even the ones that are not in Datasheet)
LAPD_Andes <- read_excel("data/LAPD_Andes_MY.xlsx")

LAPD_Andes$LAPD_ID<-as.numeric(LAPD_Andes$LAPD_ID)

#change name to LAPD_ID column, easier to handle
names(Datasheet)[names(Datasheet)=="LAPD_ID\r\n"]<-"LAPD_ID"

#select sites from LAPD_andes_final that are in Datasheet 
DS<-Datasheet%>%filter(LAPD_ID!="NA")%>%select(LAPD_ID)%>%distinct(LAPD_ID) #excluding sites NA (sites further added to Datasheet which were not present in LAPD_Andes_final)
DS<-as.numeric(DS$LAPD_ID)
A=LAPD_Andes[LAPD_Andes$LAPD_ID %in% DS, ]

#add Country, Lat, Long to NA Sites (sites that were absent in LAPD_andes_final)
na_id=Datasheet%>%filter(LAPD_ID=="NA")
na_sites<-data.frame(c("Tres Lagunas","El Triunfo","Anteojos Valley","Huila","Paramo de Agua Blanca III", "Paramo de Laguna Verde I","Paramo de Laguna Verde II","Paramo de pena Negra I" ),
                     c("Ecuador","Colombia","Ecuador","Ecuador","Colombia","Colombia","Colombia","Colombia"),
                     c(-3.05145,4.58545,-0.57946,-00.25405,5.00000,5.15000,5.15000,5.05000),
                     c(-79.24822,-75.19558,-78.24397,-78.01075,-74.10000,-74.00000,-74.00000,-74.05000))
names(na_sites)<-c("SiteName", "Country","Latitude", "Longitude")

#Create a single df containing metadata to add to Datasheet
A=bind_rows(A,na_sites)

#(1)
#add columns with country, latitude and longitude to Datasheet, per each site
Datasheet$Country<-A$Country[match(Datasheet$`Site Name`,A$SiteName)]
Datasheet$Latitude<-A$Latitude[match(Datasheet$`Site Name`,A$SiteName)]
Datasheet$Longitude<-A$Longitude[match(Datasheet$`Site Name`,A$SiteName)]

#(2)
#some values in Country,Lat,Long columns of Datasheet result as NA-> probably Site Names between A and Datasheet are different
#to check wich Sites have different names
Sitename=distinct(Datasheet,`Site Name`)
Nomatch<-match(Sitename$`Site Name`,LAPD_Andes$SiteName)
which(is.na(Nomatch))
Sites_Nomatch<-Datasheet[c(3,7,10,13,22,32,33,36),]
Sites_Nomatch$`Site Name`
LAPD_Andes$SiteName
#rename Datasheet Site Names according to the ones in LAPD_Andes
Datasheet$`Site Name`[Datasheet$`Site Name` %in% c("La Laguna\r\n" , "Cerro Toledo","El Tiro\r\n","Laguna Natosas forest", "Pedras Blancas"  , "Anteojos Valley" , "Paramo de Laguna Verde II")]<-c("La Laguna","Cerro Toledo CT","El Tiro" ,"Lagunas Natosas Forest","Piedras Blancas","Anteojos Valley" ,"Paramo de Laguna Verde II")
#see if there something NA left, and which site it corresponds to
view(filter(Datasheet,is.na(Country)))
#Repeat (1)

##Step 2: tableinstall.packages("formattable")
install.packages("formattable")
library(formattable)
#subset df
ToTable<-Datasheet%>%
  distinct(`Site Name`,Country,Latitude,Longitude,`Reference (short)`)
#order df according to Country
ToTable<-ToTable[order(ToTable$Country),]  
#fancy Table (not necessary)
Table_01<-formattable(ToTable)
#export toTable in excel, new version of Datasheet
library(writexl)
write_xlsx(ToTable,"Table_01.xlsx")
write_xlsx(Datasheet,"Datasheet.xlsx")

####MAP---------------------------------------------
library(OpenStreetMap)
library(ggmap)

range(Sites$Longitude)
range(Sites$Latitude)
Sites<-Datasheet%>%distinct(`Site Name`,Latitude,Longitude)

Andes<-get_stamenmap(bbox = c(left=-80,
                              bottom=-5,
                              right=-69, 
                              top=10),
                     zoom=5,maptype = "terrain-background")
AndesMap<-ggmap(Andes)+
  geom_point(data=Sites, aes(x= Longitude, y=Latitude))

AndesMap+
  geom_text(data = Sites, aes(x = Longitude + .001, y = Latitude, label=`Site Name`), size=1.8, hjust=-0.1, vjust=0)
  
  
