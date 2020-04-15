#################################################
#                                               #
#                                               #  
#     HUMAN INDICATORS NORTHERN ANDES           #
#             MIDORI YAJIMA                     #
#                                               #
#             MARCH 2020                        #
#                                               #      
#-----------------------------------------------#

## CONTRIBUTORS
# Ondrej XX <email>
# Suzette Flantua <s.g.a.flantua@gmail.com>



# ---------------------------------------------

# Packages
library(readxl)
library(tidyverse)
library(writexl)
library(OpenStreetMap)
library(ggmap)

# Load data
Datasheet <- read_excel("data/Datasheet.xlsx")
Datasheet_shaved<-read_excel("data/Datasheet_shaved.xlsx")
Human_indicators <- read_excel("data/01_Human indicators_V1.xlsx")
view(Datasheet)

####TABLE 1------------------------------------

# subset df
ToTable<-Datasheet_shaved%>%
  distinct(`Site Name`,Country,Latitude,Longitude,`Reference (short)`)

# order df according to Country
ToTable<-ToTable[order(ToTable$Country),]

# export toTable in excel
write_xlsx(ToTable,"Table_01.xlsx")

####FIGURE 1---------------------------------------------
## map

#subset df
Sites<-Datasheet_shaved%>%distinct(`Site Name`,Latitude,Longitude, Country, Altitude)


# obtain map of the area
range(Sites$Longitude)
range(Sites$Latitude)
Andes<-get_stamenmap(bbox = c(left=-84,
                              bottom=-7,
                              right=-67, 
                              top=12),
                     zoom=5,maptype = "terrain-background")

#map
Amap<- ggmap(Andes)+
  geom_point(data=Sites,
             aes(x= Longitude, y=Latitude))+
  geom_text(data=Sites, 
            aes(x = Longitude + .001, y = Latitude, label=`Site Name`),
            size=1, hjust=-0.1, vjust=0)

# same as above, subsetting Sites
# By location (Colombia, Venezuela, Ecuador)
#Col
Col<-Sites%>%filter(Country=="Colombia")
range(Col$Latitude) 
range(Col$Longitude)

ColMap<-get_stamenmap(bbox = c(left=-80,
                               bottom=0,
                               right=-70, 
                               top=7),
                      zoom=5,maptype = "terrain-background")

Colombia<-ggmap(ColMap)+
  geom_point(data=Col,
             aes(x= Longitude, y=Latitude), size=0.5)+
  geom_text(data=Col, 
            aes(x = Longitude + .001, y = Latitude, label=`Site Name`),
            size=2, hjust=-0.1, vjust=0)

 #Ven
Ven<-Sites%>%filter(Country=="Venezuela")
range(Ven$Latitude) 
range(Ven$Longitude)

VenMap<-get_stamenmap(bbox = c(left=-72,
                                    bottom=7,
                                    right=-68, 
                                    top=10),
                           zoom=5,maptype = "terrain-background")

Venezuela<-ggmap(VenMap)+
  geom_point(data=Ven,
             aes(x= Longitude, y=Latitude))+
  geom_text(data=Ven, 
            aes(x = Longitude + .001, y = Latitude, label=`Site Name`),
            size=2, hjust=-0.1, vjust=0)

# Ec
Ec<-Sites%>%filter(Country=="Ecuador")
range(Ec$Latitude) 
range(Ec$Longitude)

EcMap<-get_stamenmap(bbox = c(left=-80,
                               bottom=-5,
                               right=-77, 
                               top=1),
                      zoom=5,maptype = "terrain-background")

Ecuador<-ggmap(EcMap)+
  geom_point(data=Ec,
             aes(x= Longitude, y=Latitude), size=0.5)+
  geom_text(data=Ec, 
            aes(x = Longitude + .001, y = Latitude, label=`Site Name`),
            size=1.8, hjust=-0.1, vjust=0)

# By altitude
# >2000
Tk<-Sites%>%filter(Altitude<2000)

Tkmap<- ggmap(Andes)+
  geom_point(data=Tk,
             aes(x= Longitude, y=Latitude),size=0.5)+
  geom_text(data=Tk, 
            aes(x = Longitude + .001, y = Latitude, label=`Site Name`),
            size=1.5, hjust=-0.1, vjust=0)

# 2000<Altitude< 3000
Trk<-Sites%>%filter(Altitude %in% 2000:3000)

Trkmap<- ggmap(Andes)+
  geom_point(data=Trk,
             aes(x= Longitude, y=Latitude),size=0.5)+
  geom_text(data=Trk, 
            aes(x = Longitude + .001, y = Latitude, label=`Site Name`),
            size=1.5, hjust=-0.1, vjust=0)

# 3000<Altitude<4215 (max value)
Fk<-Sites%>%filter(Altitude %in% 3000:4215)

Fkmap<- ggmap(Andes)+
  geom_point(data=Fk,
             aes(x= Longitude, y=Latitude),size=0.5)+
  geom_text(data=Fk, 
            aes(x = Longitude + .001, y = Latitude, label=`Site Name`),
            size=1.5, hjust=-0.1, vjust=0)


####FIGURE 2-------------------------------
##time span of each site

#with Datasheet_shaved
ggplot(Datasheet_shaved)+
  geom_bar( aes(x=reorder(`Site Name`,Latitude), fill=Country))+
  theme(axis.text.y  = element_text( hjust=1))+
  theme(axis.title.x = element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.title.y = element_blank())+
  geom_text(aes(x=`Site Name`,label=Length),stat='count', hjust=-0.1, size=3)+
  coord_flip()+
  ggtitle("Time span of the records")


#### FIGURE 3 ---------------------------------
## number of records per time bin 

#with Datasheet_shaved
ggplot(Datasheet_shaved)+
  geom_bar(aes(x= reorder(Datasheet_shaved$Bin,-Datasheet_shaved$Bin_num)))+
  theme_bw()+
  theme(axis.text.x  = element_text(angle = 70, hjust=1),
        axis.title.y = element_blank())+
  xlab("Time bins")+
  ggtitle("Number of pollen records per time bin")+
  geom_text(aes(x=Bin,label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)
  
#### FIGURE 4 ---------------------------------
## number of times human indicators are counted in total

 # select only indicators
 Datasheet_sh.indicators<-Datasheet_shaved %>%
   select(.,-c("LAPD_ID","Site Name","Reference (short)","Bin", "Bin_num",
               "Latitude","Longitude", "Country","Length" ))
 
 # Convert counts from character to numeric
 Datasheet_sh.indicators <- apply (Datasheet_sh.indicators,2,
                                   FUN= function(x) as.numeric(unlist(x)))
 
 # Make datasheet with site vs number of times an indicator is counted  
 Datasheet_sh.indicators <- as.data.frame(Datasheet_sh.indicators)
 
 Datasheet_sh.ind.full <- bind_cols(Datasheet_shaved %>%                
                                      select(.,c("Site Name")),
                                      Datasheet_sh.indicators)  
 
 # calculate number of times (i.e num of bins) taxa are found per site
 IndxSite<- reshape2::melt(Datasheet_sh.ind.full) %>%
   group_by(`Site Name`,variable) %>%
   summarise(Count = sum(value)) 
 
 # print 1 if taxa are found in a site (despite the number of bins where it is found) 
 IndxSite$Pres<-ifelse(IndxSite$Count==0,0,1)
 
 # number of times human indicators are counted in total
 IndxSite%>%
 ggplot(aes(x=variable,y=Pres))+
   geom_bar(stat = "identity")+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 90, hjust=1),
         axis.title.y = element_blank())+
   xlab("Indicators")+
   ggtitle("Number of sites where Indicators are found")
   
#### FIGURE 5 --------------------------------
 ## trend of total human indicators through time per site
 
 #sum of indicators per time bin for each site
 DF_sh.tot<-data.frame(Sum_total=apply(Datasheet_sh.indicators,1,
                                       FUN = function(x) sum(x,na.rm = T)))
 
 Datasheet_sh.full<- bind_cols(Datasheet_shaved,DF_sh.tot)
 
 # plot
 Datasheet_sh.full %>% 
   ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_total)) +  
   geom_bar(stat = "identity",width = 0.5) +
   facet_wrap(~`Site Name`) +
   theme_bw() +
   theme(axis.text.x = element_text(angle = 90, hjust=1)) +
   geom_text(aes(label=Sum_total), vjust=-0.3, size=3) +
   ylab("Absolute frequency")+
   xlab("Time bins")+
   ggtitle("Absolute frequency of indicators through time per site")
 
#### FIGURE 6 ---------------------------------
## number of times human indicators are counted in each site

 reshape2::melt(Datasheet_sh.ind.full) %>%
   group_by(`Site Name`,variable) %>%
   summarise(Count = sum(value, na.rm = T))%>%
   ggplot(aes(x=variable,y=Count))+
   geom_bar(stat = "identity")+
   facet_wrap(~`Site Name`)+
   theme(axis.text.x = element_text(angle = 90, hjust=1))+
   ggtitle("Number of times Indicators are found per Site")


#### FIGURE 7 ---------------------------------
## in which sites are which indicators found and how often? 

 reshape2::melt(Datasheet_sh.ind.full) %>%
   group_by(variable,`Site Name`) %>%
   summarise(Count = sum(value, na.rm = T)) %>%
   ggplot(aes(x=`Site Name`,y=Count))+
   geom_bar(stat = "identity")+
   facet_wrap(~variable)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 90, hjust=1))+
   ggtitle("Number of times Indicators are found per Site")

#### Analyses of direct/indirect indicators-------

#Import dataset
Human_indicators <- read_excel("data/01_Human indicators_V1.xlsx")
 
# Select only direct indicators
Direct <- Human_indicators %>% 
  filter(Indicator=="Direct") %>%
  select(`Group (Taxa)`)

#select direct indicators in dataframe
Direct <- as.vector(Direct$`Group (Taxa)`)
DF_sh.Direct<- Datasheet_sh.indicators[,names(Datasheet_sh.indicators) %in% Direct]

# sum direct indicators per time bin
DF_sh.Direct.Sum <- data.frame(SUM=apply(DF_sh.Direct,1,FUN = function(x) sum(x,na.rm = T)))

## same for indirect
Indirect<-Human_indicators %>% 
  filter(Indicator=="Indirect") %>% 
  select(`Group (Taxa)`)

Indirect<-as.vector(Indirect$`Group (Taxa)`)
DF_sh.Indirect<-Datasheet_sh.indicators[,names(Datasheet_sh.indicators) %in% Indirect]
DF_sh.Indirect.Sum<-data.frame(SUM=apply(DF_sh.Indirect,1,FUN = function(x) sum(x,na.rm = T)))

# dataframe with all metadata + sum of direct ind.+ sum of indirect ind.
Datasheet_sh.full<- bind_cols(Datasheet_shaved,DF_sh.Direct.Sum)
Datasheet_sh.full<-rename(Datasheet_sh.full,"Sum_direct"="SUM")
Datasheet_sh.full<- bind_cols(Datasheet_sh.full,DF_sh.Indirect.Sum)
Datasheet_sh.full<-rename(Datasheet_sh.full,"Sum_indirect"="SUM")


#### FIGURE 8-------------------------------------------
##Absolute frequency of direct indicators through time per site 

Datasheet_sh.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_direct)) +  
  geom_bar(stat = "identity",width = 0.5) +
  facet_wrap(~`Site Name`) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  geom_text(aes(label=Sum_direct), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of direct indicators through time per site")

#### FIGURE 9----------------------------------------
## Absolute frequency of indirect indicators through time per site

Datasheet_sh.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_indirect)) +  
  geom_bar(stat = "identity",width = 0.5) +
  facet_wrap(~`Site Name`) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  geom_text(aes(label=Sum_indirect), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of inddirect indicators through time per site")

#### FIGURE 10-------------------------------------
## to have a single chart for direct and indirect indicators
DF_sh.Direct.Sum<-DF_sh.Direct.Sum%>%
  mutate(IND=rep("Direct"))

DF_sh.Indirect.Sum <- DF_sh.Indirect.Sum %>% 
  mutate(IND=rep("Indirect"))

Datasheet_sh.full.long<-rbind(bind_cols(Datasheet_shaved,DF_sh.Direct.Sum),
                              bind_cols(Datasheet_shaved,DF_sh.Indirect.Sum))

#direct and indirect side by side
ggplot(data=Datasheet_sh.full.long, 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

####FIGURE 11-----------------------------------------------
#Colmbia ind, with map
pCol<-Datasheet_sh.full.long %>%
  filter(Country== "Colombia") %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")


gridExtra::grid.arrange(Colombia,pCol, ncol=2)

####FIGURE 12-----------------------------------------------
pVen<-Datasheet_sh.full.long %>%
  filter(Country== "Venezuela") %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Venezuela,pVen, ncol=2)

#### FIGURE 13-----------------------------------------------
peC<-Datasheet_sh.full.long %>%
  filter(Country== "Ecuador") %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Ecuador,peC, ncol=2)

#### FIGURE 14-----------------------------------------------
p2k<-Datasheet_sh.full.long %>%
  filter(Altitude < 2000) %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Tkmap,p2k, ncol=2)

#### FIGURE 15------------------------------------------
p3k<-Datasheet_sh.full.long %>%
  filter(Altitude %in% 2000:3000) %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Trkmap,p3k, ncol=2)

#### FIGURE 16-------------------------------------------
p4k<-Datasheet_sh.full.long %>%
  filter(Altitude %in% 3000:4215) %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Fkmap,p4k, ncol=2)

####FIGURE 16----------------------------------------------------
#variation in percent of direct/indirect
ggplot(data=Datasheet_sh.full.long,
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 0.5, position = "fill")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("%")+
  xlab("Time bins")+
  ggtitle("Proportion of indirect/direct indicators through time")

#### FIGURE 17-----------------------------------------------
## trend of each indicator through time
## number of records where a certain indicator is found in a certain time bin 

#new df
Datasheet_sh.ind.time <- bind_cols(Datasheet_shaved %>%                
                                     select(.,c("Bin_num")),
                                   Datasheet_sh.indicators)  

#plot
reshape2::melt(Datasheet_sh.ind.time, id= "Bin_num") %>%
  group_by(variable,Bin_num)%>%
  summarise(Count = sum(value, na.rm = T)) %>%
  ggplot(aes(x=reorder(Bin_num,-Bin_num) ,y=Count))+
  geom_bar(stat = "identity")+
  facet_wrap(~variable)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Number of indicators per time bin")

####FIGURE 18------------------------------------------
## relative frequency of indicators through time
## proportion of records where an indicator is found in a certain time bin

#df with count of Bins (number of record covering a certain Time bin)
BinsTot<-as.data.frame(table(Datasheet_sh.ind.time$Bin_num))

#plot
reshape2::melt(Datasheet_sh.ind.time, id= "Bin_num") %>%
  group_by(variable,Bin_num)%>%
  summarise(Count = sum(value, na.rm = T))%>%
  mutate(Freq=BinsTot$Freq[match(Bin_num,BinsTot$Var1)])%>%
  mutate(Rel=Count/Freq)%>%
  ggplot(aes(x=reorder(Bin_num,-Bin_num) ,y=Rel))+
  geom_bar(stat = "identity")+
  facet_wrap(~variable)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins")+
  ggtitle("Relative frequency of indicators per time bin")

