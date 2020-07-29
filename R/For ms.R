#-------------------------------------------------
#
#           RESULTS ANALYSIS 
#       (from datasheet compilation)
#
#-----------------------------------------------

require(tidyverse)

#number of suitable records
lapd_andes_suitable<-LAPD_Andes%>%
  filter(!DONE %in% c("C14 higher than 2ka",
                      "Not Andes","Peru",
                      "No diagram in PDF",
                      "No diagram in PDF\r\n",
                      "No diagram in pdf",
                      "Diagram unreadable",
                      "Too short" ))

view(lapd_andes_suitable%>%filter(Country=="Colombia"))
view(lapd_andes_suitable%>%filter(Country=="Venezuela"))
view(lapd_andes_suitable%>%filter(Country=="Ecuador"))
view(lapd_andes_suitable%>%filter(DONE=="Check Hardcopy"))
     
#number of records in the dataset
lapd_andes_done<-lapd_andes_suitable%>%
  filter(DONE %in% c( "DONE", "DONE\r\n"))

view(lapd_andes_done%>%filter(Country=="Colombia"))
view(lapd_andes_done%>%filter(Country=="Venezuela"))
view(lapd_andes_done%>%filter(Country=="Ecuador"))

#number of records does not match with the one in Datasheet
sitesname2<-distinct(Datasheet_shaved, `Site Name`)
nomatch2<-match(lapd_andes_done$SiteName,sitesname2$`Site Name`)
which(is.na(nomatch2))
sites_nomatch2<-lapd_andes_done[c(29,33,36),]
sites_nomatch2$SiteName

##BOXPLOT
Datasheet_shaved%>%distinct(`Site Name`, Altitude)%>%count(Altitude<2000)
Datasheet_shaved%>%distinct(`Site Name`, Altitude)%>%count(Altitude %in% 2000:3000)
Datasheet_shaved%>%distinct(`Site Name`, Altitude)%>%count(Altitude %in% 3000:4215)

Span<-as.data.frame(table(Datasheet_shaved$Bin_num))

tot<-Datasheet_shaved %>%
  group_by(`Site Name`) %>%
  summarise(MIN = min(Bin_num),
            MAX = max(Bin_num))%>%
  count(MAX)

Span$tot<-tot$n[match( Span$Var1, tot$MAX)]

Span[17:24,]%>%summarise(perc=sum(tot, na.rm = T)/38)
Span[9:16,]%>%summarise(perc=sum(tot, na.rm = T)/38)
Span[1:8,]%>%summarise(perc=sum(tot, na.rm = T)/38)

# same for each cluster
Datasheet_shaved%>%
  filter(cluster.id_2==1)%>%
  distinct(`Site Name`, Altitude)

Datasheet_shaved%>%
  filter(cluster.id_2==2)%>%
  distinct(`Site Name`, Altitude)

Datasheet_shaved%>%
  filter(cluster.id_2==3)%>%
  distinct(`Site Name`, Altitude)

Datasheet_shaved%>%
  filter(cluster.id_2==4)%>%
  distinct(`Site Name`, Altitude)%>%
  count(Altitude %in% 2000:3000)

Datasheet_shaved%>%
  filter(cluster.id_2==4)%>%
  distinct(`Site Name`, Altitude)%>%
  count(Altitude %in% 3000:4215)

tot1<-Datasheet_shaved %>%
  filter(cluster.id_2==4)%>%
  group_by(`Site Name`) %>%
  summarise(MIN = min(Bin_num),
            MAX = max(Bin_num))%>%
  count(MAX)



##Indicators
View(Human_indicators_original[-c(2,8,13,14,22,68),])

Human_indicators_original[-c(2,8,13,14,22,68),] %>%
  filter(Indicator=="Direct")

Human_indicators_original[-c(2,8,13,14,22,68),] %>%
  filter(`Potential food source (no/low/high)`=="HIGH")

Human_indicators_original[-c(2,8,13,14,22,68),] %>%
  filter(`Potential food source (no/low/high)` %in% c("LOW", "Low"))

Human_indicators_original[-c(2,8,13,14,22,68),] %>%
  filter(`Potential food source (no/low/high)` %in% c("NO", "no"))

Human_indicators%>%
  filter(Indicator=="Direct")
 
Human_indicators%>%
  filter(PotentionalFoodSource=="high")

Human_indicators%>%
  filter(PotentionalFoodSource=="low")

Human_indicators%>%
  filter(PotentionalFoodSource=="no")

# Direct indicators
View(Datasheet_sh.full.long%>%
       filter(IND == "Direct" & SUM>0)%>%
       select(c("Manihot",
                "Pinus",
                "Eucalyptus",
                "Phaseolus",
                "Zea mays",
                "Site Name",
                "SUM",
                "Bin")))

Datasheet_sh.full.long%>%
  mutate(clust.id= Datasheet_shaved$cluster.id_2
              [match(`Site Name long`, Datasheet_shaved$`Site Name long`)])%>%
  filter(IND == "Direct" & SUM>0 & clust.id == 4)%>%
    distinct(`Site Name long`)

Datasheet_sh.full.long%>%
  mutate(clust.id= Datasheet_shaved$cluster.id_2
         [match(`Site Name long`, Datasheet_shaved$`Site Name long`)])%>% 
  filter(clust.id == 3)%>%distinct(`Site Name long`)

#high food pot
dat%>%
  mutate (Pot=Human_indicators$PotentionalFoodSource
          [match(variable,Human_indicators$Taxa)])%>%
  filter(Pot=="high")%>%
  mutate(Pres=ifelse(Count>0,1,0))%>%
  summarise(Count=sum(Pres))

dat%>%
  mutate (Pot=Human_indicators$PotentionalFoodSource
          [match(variable,Human_indicators$Taxa)])%>%
  filter(Pot=="high"& cluster.id==4)%>%
  mutate(Pres=ifelse(Count>0,1,0))%>%
  summarise(Count=sum(Pres))

View(Datasheet_sh.pot.full.long%>%
       filter(IND == "High" & SUM>0)%>%
  select(c( "Amaranthceae/ Chenopodiaceae (Ama/Cheno)",
            "Annonaceae",
            "Arecaceae",
            "Convolvulaceae" ,
            "Fabaceae/Leguminosa" ,
            "Lupinus"  ,
            "Manihot",
            "Oxalidaceae",
            "Phaseolus",
            "Sapotaceae",
            "Solanaceae",
            "Umbelliferae/Apiaceae",
            "Zea mays" ,
            "Site Name",
            "SUM",
            "Bin_num")))

View(Datasheet_sh.pot.full.long%>%
       filter(IND == "High" & SUM>0)%>%
       select(c("Annonaceae",
                 "Arecaceae",
                 "Manihot",
                 "Oxalidaceae",
                 "Phaseolus",
                 "Sapotaceae",
                 "Zea mays" ,
                 "Site Name",
                 "SUM",
                 "Bin_num")))

# low food
dat%>%
  mutate (Pot=Human_indicators$PotentionalFoodSource
          [match(variable,Human_indicators$Taxa)])%>%
  filter(Pot=="high")%>%
  mutate(Pres=ifelse(Count>0,1,0))%>%
  summarise(Count=sum(Pres))