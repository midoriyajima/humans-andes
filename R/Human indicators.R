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
#TODO: charcoal out from indicators
# TODO: FIG 5, 6, 6B, 6C color boxes for Country, order by latitude, sign for hiatuses,
# display lenght of cores or relative frequency (also for fig 7)
# new version with numbers instead of indicators name, split per Country
# TODO: faceted map with less colored background (shades for mountains)
# faceted map showing only "red" points, side by side with big map with all sites
# TODO: set fig 3 as timeline (inverted axis, arrow)

# Packages
library(readxl)
library(tidyverse)
library(writexl)
library(OpenStreetMap)
library(ggmap)
library(plotly)
library (magick)

# Load data
Datasheet <- read_excel("data/Datasheet.xlsx")
Datasheet_shaved<-read_excel("data/Datasheet_shaved.xlsx")
Human_indicators <- read_excel("data/Human_indicators.xlsx")

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
               "Latitude","Longitude", "Country","Length","Altitude" ))
 
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
 
 ## state wich ind is direct/indirect
 IndxSite$Ind<-Human_indicators$Indicator[match(IndxSite$variable,
                                                Human_indicators$`Group (Taxa)`)]
 
 ##state wich ind has high, low, no food potential
 IndxSite$Pot<-Human_indicators$`Potential food source`[match(IndxSite$variable,
                                                Human_indicators$`Group (Taxa)`)]
 
 ## to have columns ordered by heght in graph
 # create col with sum presence of indicators (number of sites where indicators found)
 TotPres<-IndxSite%>%group_by(variable)%>%summarise(Pres=sum(Pres,na.rm = T))
 IndxSite$TotPres<-TotPres$Pres[match(IndxSite$variable,
                                      TotPres$variable)]
 
 
 # plot
 IndxSite%>%
 ggplot(aes(x=reorder(variable,TotPres),y=Pres,fill=Ind))+
   geom_bar(stat = "identity")+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 70, hjust=1),
         axis.title.y = element_blank())+
   xlab("Indicators")+
   labs(fill="Indicator")+
   ggtitle("Number of sites where Indicators are found")
 
####FIGURE 4b---------------------------------
 # as above, distinguishing for Potential source of food
 IndxSite%>%
   ggplot(aes(x=reorder(variable,TotPres),y=Pres,fill=Pot))+
   geom_bar(stat = "identity")+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 70, hjust=1),
         axis.title.y = element_blank())+
   xlab("Indicators")+
   labs(fill="Potential food source")+
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
 
#### FIGURE 6b--------------------------------------
 # distinguish between direct and indirect
 
 reshape2::melt(Datasheet_sh.ind.full) %>%
   group_by(`Site Name`,variable) %>%
   summarise(Count = sum(value, na.rm = T))%>%
   mutate(Ind=Human_indicators$Indicator
          [match(variable,Human_indicators$`Group (Taxa)`)])%>%
   ggplot(aes(x=variable,y=Count, fill=Ind))+
   geom_bar(stat = "identity")+
   facet_wrap(~`Site Name`)+
   theme(axis.text.x = element_text(angle = 90, hjust=1))+
   ggtitle("Number of times Indicators are found per Site") 
 
#### FIGURE 6c------------------------------------
# distinguish beteween different potential food source
 
#sort indicators in new df by food potential 

 IndxPot<-Human_indicators%>% 
   group_by(`Potential food source`)%>%
   do( data.frame(with(data=., .[order(`Group (Taxa)`),] )) )%>% # sort human indiactors by group 
   filter(North.Andean.fossil.records...yes.no.=="yes")%>% #select only indicators that are found in the other df
   pull(Group..Taxa.) #create vector indicators (now ordered)
 
Datasheet_sh.ind.full.sort<-Datasheet_sh.ind.full[,IndxPot]%>%
  mutate(`Site Name`=Datasheet_shaved$`Site Name`)

#plot
 reshape2::melt(Datasheet_sh.ind.full.sort) %>%
   group_by(`Site Name`,variable) %>%
   summarise(Count = sum(value, na.rm = T))%>%
   mutate(Pot=Human_indicators$`Potential food source`
          [match(variable,Human_indicators$`Group (Taxa)`)])%>%
   ggplot(aes(x=variable,y=Count, fill=Pot))+
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

#### analyses of potential food sources

High<-Human_indicators %>% 
  filter(`Potential food source`=="high") %>%
  select(`Group (Taxa)`)

High <- as.vector(High$`Group (Taxa)`)
DF_sh.High<- Datasheet_sh.indicators[,names(Datasheet_sh.indicators) %in% High]
DF_sh.High.Sum <- data.frame(Sum_high=apply(DF_sh.High,1,FUN = function(x) sum(x,na.rm = T)))

Low<-Human_indicators %>% 
  filter(`Potential food source`=="low") %>%
  pull(`Group (Taxa)`)

DF_sh.Low<- Datasheet_sh.indicators[,names(Datasheet_sh.indicators) %in% Low]
DF_sh.Low.Sum <- data.frame(Sum_low=apply(DF_sh.Low,1,FUN = function(x) sum(x,na.rm = T)))

No<-Human_indicators %>% 
  filter(`Potential food source`=="no") %>%
  pull(`Group (Taxa)`)

DF_sh.No<- Datasheet_sh.indicators[,names(Datasheet_sh.indicators) %in% No]
DF_sh.No.Sum <- data.frame(Sum_no=apply(DF_sh.No,1,FUN = function(x) sum(x,na.rm = T)))

Datasheet_sh.pot.full<- bind_cols(Datasheet_shaved,
                                  DF_sh.High.Sum,
                                  DF_sh.Low.Sum,
                                  DF_sh.No.Sum)

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

#### FIGURE 8b---------------------------------------------------------
##same, without numbers and with time span of the records
Datasheet_sh.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_direct)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=4),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
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
  ggtitle("Absolute frequency of indirect indicators through time per site")

#### FIGURE 9b---------------------------------------------------------
##same, without numbers and with time span of the records
Datasheet_sh.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_indirect)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indirect indicators through time per site")

#### FIGURE 9c----------------------------------
#Absolute frequency of high food pot indicators through time per site
Datasheet_sh.pot.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_high)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators with high food potential through time per site")

#### FIGURE 9d---------------------------------------
#Absolute frequency of low food pot indicators through time per site

Datasheet_sh.pot.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_low)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators with low food potential through time per site")

#### FIGURE 10-------------------------------------
## to have a single chart for direct and indirect indicators
DF_sh.Direct.Sum<-DF_sh.Direct.Sum%>%
  mutate(IND=rep("Direct"))

DF_sh.Indirect.Sum <- DF_sh.Indirect.Sum %>% 
  mutate(IND=rep("Indirect"))

Datasheet_sh.full.long<-rbind(bind_cols(Datasheet_shaved,DF_sh.Indirect.Sum),
                              bind_cols(Datasheet_shaved,DF_sh.Direct.Sum))

#order levels of IND to show Indirect ind on the top of direct (in the graph)
Datasheet_sh.full.long$IND<-factor(Datasheet_sh.full.long$IND, 
                                   levels = c("Indirect","Direct"))

#direct and indirect side by side
ggplot(data=Datasheet_sh.full.long, 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

# FIGURE 10b-------------------------------------------------
#stacked bar chart, filled bars (easier to read)
ggplot(data=Datasheet_sh.full.long, 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

### FIGURE 10ba-----------------------------------------
# even better, smooth graph

ggplot(Datasheet_sh.full.long, 
       aes(-Bin_num,SUM,fill=IND,alpha=0.5))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"),name="Indicators")+
  scale_alpha_continuous(guide= "none")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

# FIGURE X---------------------------------
#The Idea is to have a plot for each Country and combine them
#cowplot::plot_grid(plot1, plot2, labels = c("1", "2"), nrow = 2)
# but maybe possible also with grid Extra

# Three altitudinal zones
Datasheet_sh.full.long$Alt_zone <-ifelse(Datasheet_sh.full.long$Altitude < 2000,
                                         "1000-2000 m",
                                         ifelse(Datasheet_sh.full.long$Altitude %in% 2000:3000,
                                                "2000-3000 m", "3000-4215 m" ))


Col2k<-ggplot(Datasheet_sh.full.long%>%
                 filter(Country=="Colombia")%>%
                filter(Alt_zone== "1000-2000 m"), 
       aes(-Bin_num,SUM,fill=IND,alpha=0.5))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"),name="Indicators")+
  scale_alpha_continuous(guide= "none")+
  facet_grid(Alt_zone ~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

Col3k<-ggplot(Datasheet_sh.full.long%>%
                filter(Country=="Colombia")%>%
                filter(Alt_zone== "2000-3000 m"), 
              aes(-Bin_num,SUM,fill=IND,alpha=0.5))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"),name="Indicators")+
  scale_alpha_continuous(guide= "none")+
  facet_grid(Alt_zone ~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

Col4k<-ggplot(Datasheet_sh.full.long%>%
                filter(Country=="Colombia")%>%
                filter(Alt_zone== "3000-4215 m"), 
              aes(-Bin_num,SUM,fill=IND,alpha=0.5))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"),name="Indicators")+
  scale_alpha_continuous(guide= "none")+
  facet_grid(Alt_zone ~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

ColAlt<-gridExtra::grid.arrange(Col2k,Col3k,Col4k) # don't like the output


#### FIGURE 10c------------------------------------------
# comparison for food potential, stacked bars 
#(good to see total indicators but not proportions)

DF_sh.High.Sum<-DF_sh.High.Sum%>%
  mutate(IND=rep("High"))
colnames(DF_sh.High.Sum)[1]<-"SUM"

DF_sh.Low.Sum <- DF_sh.Low.Sum %>% 
  mutate(IND=rep("Low"))
colnames(DF_sh.Low.Sum)[1]<-"SUM"

DF_sh.No.Sum <- DF_sh.No.Sum %>% 
  mutate(IND=rep("No"))
colnames(DF_sh.No.Sum)[1]<-"SUM"


Datasheet_sh.pot.full.long<-rbind(bind_cols(Datasheet_shaved,DF_sh.High.Sum),
                              bind_cols(Datasheet_shaved,DF_sh.Low.Sum),
                              bind_cols(Datasheet_shaved,DF_sh.No.Sum))
#plot
graph1<-ggplot(data=Datasheet_sh.pot.full.long, 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

### FIGURE 10 ca--------------------------------------------
#smooth graph, good for comparison in proportions of ind

Datasheet_sh.pot.full.long$IND<-factor(Datasheet_sh.pot.full.long$IND,
                                       levels = c("No","Low","High"))

ggplot(Datasheet_sh.pot.full.long, 
       aes(-Bin_num,SUM,fill=IND, alpha=0.7))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c("#619CFF","#00BA38","#F8766D"))+
  scale_alpha_continuous(guide= "none")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

# FIGURE 10d-----------------------------------------------
# comparison between high and low food potential indicators, stacked bars

#to see colors of plot above
ggplot_build(graph1)$data

#plot
Datasheet_sh.pot.full.long%>%
  filter(IND==c("High", "Low"))%>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#F8766D", "#00BA38"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

### FIGURE 10da--------------------------------------
# same, smooth bars
Datasheet_sh.pot.full.long%>%
  filter(IND==c("High", "Low"))%>%
  ggplot(aes(-Bin_num,SUM,fill=IND, alpha=0.7))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c( "#00BA38","#F8766D"))+
  scale_alpha_continuous(guide= "none")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

####FIGURE 11-----------------------------------------------
#Colmbia ind, with map

pCol<-Datasheet_sh.full.long %>%
  filter(Country== "Colombia") %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")


gridExtra::grid.arrange(Colombia,pCol, ncol=2)

#### FIGURE 11b-------------------------------------------
#same as 11, with stacked bars
pColb<-Datasheet_sh.full.long %>%
  filter(Country== "Colombia") %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")


gridExtra::grid.arrange(Colombia,pColb, ncol=2)

#### FIGURE 11c-----------------------------------
# smooth graph with factes ordered by Site Latitude

# sites ordered according to lat, to sort facets in plot
Datasheet_sh.full.long$`Site Name`<-factor(Datasheet_sh.full.long$`Site Name`)
Datasheet_sh.full.long$`Site Name`<-fct_reorder(Datasheet_sh.full.long$`Site Name`,
                                                -Datasheet_sh.full.long$Latitude)

# plot
pColc<-Datasheet_sh.full.long%>% 
  filter(Country== "Colombia") %>%  
  ggplot(aes(-Bin_num,y=SUM,fill=IND))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  scale_alpha_continuous(guide= "none")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Colombia,pColc, ncol=2)

#### FIGURE 11d-----------------------------------
# same as above, with potential food indicators
# sites ordered according to lat, does not work in sorting facets in plot
Datasheet_sh.pot.full.long$`Site Name`<-factor(Datasheet_sh.pot.full.long$`Site Name`)
Datasheet_sh.pot.full.long$`Site Name`<-fct_reorder(Datasheet_sh.pot.full.long$`Site Name`,
                                                -Datasheet_sh.pot.full.long$Latitude)

pCold<-Datasheet_sh.pot.full.long%>% 
  filter(Country== "Colombia") %>%  
  filter(IND==c("High", "Low"))%>%
  ggplot(aes(-Bin_num,SUM,fill=IND, alpha=0.7))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c( "#00BA38","#F8766D"))+
  scale_alpha_continuous(guide= "none")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Colombia,pCold, ncol=2)

####FIGURE 12-----------------------------------------------
#for Venezuela
pVen<-Datasheet_sh.full.long %>%
  filter(Country== "Venezuela") %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Venezuela,pVen, ncol=2)

####FIGURE 12b--------------------------------------------
#same as above, with stacked bars
pVenb<-Datasheet_sh.full.long %>%
  filter(Country== "Venezuela") %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")


gridExtra::grid.arrange(Venezuela,pVenb, ncol=2)

### FIGURE 12c----------------------------------------------
# smooth graph
pVenc<-Datasheet_sh.full.long%>% 
  filter(Country== "Venezuela") %>%  
  ggplot(aes(-Bin_num,y=SUM,fill=IND))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  scale_alpha_continuous(guide= "none")+
  facet_wrap(~`Site Name`, ncol = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Venezuela,pVenc, ncol=2)

#### FIGURE 12d-----------------------------------
# same as above, with potential food indicators

pVend<-Datasheet_sh.pot.full.long%>% 
  filter(Country== "Venezuela") %>%  
  filter(IND==c("High", "Low"))%>%
  ggplot(aes(-Bin_num,SUM,fill=IND, alpha=0.7))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c( "#00BA38","#F8766D"))+
  scale_alpha_continuous(guide= "none")+
  facet_wrap(~`Site Name`, ncol=1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Venezuela,pVend, ncol=2)

#### FIGURE 13-----------------------------------------------
#for Ecuador
peC<-Datasheet_sh.full.long %>%
  filter(Country== "Ecuador") %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Ecuador,peC, ncol=2)

#### FIGURE 13b----------------------------------------------
#same as above, with stacked bars
pEcb<-Datasheet_sh.full.long %>%
  filter(Country== "Ecuador") %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")


gridExtra::grid.arrange(Ecuador,pEcb, ncol=2)

#### FIGURE 13c-------------------------------------------
#smooth graph
pEcc<-Datasheet_sh.full.long%>% 
  filter(Country== "Ecuador") %>%  
  ggplot(aes(-Bin_num,y=SUM,fill=IND))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  scale_alpha_continuous(guide= "none")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Ecuador,pEcc, ncol=2)

#### FIGURE 13d-----------------------------------
# same as above, with potential food indicators

pEcd<-Datasheet_sh.pot.full.long%>% 
  filter(Country== "Ecuador") %>%  
  filter(IND==c("High", "Low"))%>%
  ggplot(aes(-Bin_num,SUM,fill=IND, alpha=0.7))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c( "#00BA38","#F8766D"))+
  scale_alpha_continuous(guide= "none")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Ecuador,pEcd, ncol=2)

#### FIGURE 14-----------------------------------------------
#for sites less than 2k mt altitude
p2k<-Datasheet_sh.full.long %>%
  filter(Altitude < 2000) %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Tkmap,p2k, ncol=2)

#### FIGURE 14b----------------------------------------

p2kb<-Datasheet_sh.full.long %>%
  filter(Altitude < 2000) %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Tkmap,p2kb, ncol=2)


#### FIGURE 14c-----------------------------------------
p2kc<-Datasheet_sh.full.long %>%
  filter(Altitude < 2000) %>%
  ggplot(aes(-Bin_num,y=SUM,fill=IND))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  scale_alpha_continuous(guide= "none")+
  facet_wrap(~`Site Name`, ncol=2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Tkmap,p2kc, ncol=2)

### FIGURE 14d----------------------------------------------
p2kd<-Datasheet_sh.pot.full.long %>%
  filter(Altitude < 2000) %>%
  filter(IND==c("High", "Low"))%>%
  ggplot(aes(-Bin_num,y=SUM,fill=IND,alpha=0.7))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c( "#00BA38","#F8766D"))+
  scale_alpha_continuous(guide= "none")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Tkmap,p2kd, ncol=2)


#### FIGURE 15------------------------------------------
p3k<-Datasheet_sh.full.long %>%
  filter(Altitude %in% 2000:3000) %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Trkmap,p3k, ncol=2)

#### FIGURE 15b------------------------------------------
p3kb<-Datasheet_sh.full.long %>%
  filter(Altitude %in% 2000:3000) %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Trkmap,p3kb, ncol=2)

### FIGURE 15c------------------------------------
p3kc<-Datasheet_sh.full.long %>%
  filter(Altitude %in% 2000:3000) %>%
  ggplot(aes(-Bin_num,y=SUM,fill=IND))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  scale_alpha_continuous(guide= "none")+
  facet_wrap(~`Site Name`, ncol=2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Trkmap,p3kc, ncol=2)

### FIGURE 15d----------------------------------------------
p3kd<-Datasheet_sh.pot.full.long %>%
  filter(Altitude %in% 2000:3000) %>%
  filter(IND==c("High", "Low"))%>%
  ggplot(aes(-Bin_num,y=SUM,fill=IND,alpha=0.7))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c( "#00BA38","#F8766D"))+
  scale_alpha_continuous(guide= "none")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Trkmap,p3kd, ncol=2)

#### FIGURE 16-------------------------------------------
p4k<-Datasheet_sh.full.long %>%
  filter(Altitude %in% 3000:4215) %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Fkmap,p4k, ncol=2)

####FIGURE 16b----------------------------------------------
p4kb<-Datasheet_sh.full.long %>%
  filter(Altitude %in% 3000:4215) %>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Fkmap,p4kb, ncol=2)

### FIGURE 16c-----------------------------------------------
p4kc<-Datasheet_sh.full.long %>%
  filter(Altitude %in% 3000:4215) %>%
  ggplot(aes(-Bin_num,y=SUM,fill=IND))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  scale_alpha_continuous(guide= "none")+
  facet_wrap(~`Site Name`, ncol=2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Fkmap,p4kc, ncol=2)

### FIGURE 16d----------------------------------------------
p4kd<-Datasheet_sh.pot.full.long %>%
  filter(Altitude %in% 3000:4215) %>%
  filter(IND==c("High", "Low"))%>%
  ggplot(aes(-Bin_num,y=SUM,fill=IND,alpha=0.7))+
  geom_density(stat="identity")+
  scale_fill_manual(values = c( "#00BA38","#F8766D"))+
  scale_alpha_continuous(guide= "none")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time")

gridExtra::grid.arrange(Fkmap,p4kd, ncol=2)

####FIGURE 17----------------------------------------------------
#variation in percent of direct/indirect
ggplot(data=Datasheet_sh.full.long,
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 0.5, position = "fill")+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("%")+
  xlab("Time bins")+
  ggtitle("Proportion of indirect/direct indicators through time")

####FIGURE 17b----------------------------------------------------
#variation in percent of ind with food potential
ggplot(data=Datasheet_sh.pot.full.long,
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 0.5, position = "fill")+
  scale_fill_manual(values = c("#619CFF","#00BA38","#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("%")+
  xlab("Time bins")+
  ggtitle("Proportion of indicators with no/low/high food potential through time")

#### FIGURE 18-----------------------------------------------
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

####FIGURE 19------------------------------------------
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

#### FIGURE 20----------------------------------------
## map of direct indicators 
Amap+
  geom_point(data=Datasheet_sh.full.long%>%
               filter(IND=="Direct")%>%
               filter(SUM!=0),
             aes(x= Longitude, y=Latitude),color="red")+
  ggtitle("Sites with direct indicators")

#### FIGURE 21--------------------------------------
## map of sites with direct indicators through time

#new df with column for Colors
DFcol<-Datasheet_sh.full.long%>%
  filter(IND =="Direct")%>%
  mutate(Color = ifelse(SUM > 0, "red", "black"))

Amap+
  geom_point(data= DFcol,
             aes(x= Longitude, y=Latitude, color= DFcol$Color),
             alpha=0.3)+
  scale_color_identity()+
  facet_wrap(~Bin_num)+
  labs(color="Indicator")+
  ggtitle("Sites with direct indicators through time")


#### FIGURE 22-------------------------------------
## map of sites with direct indicators and their abundances
##and how changed through time
Amap+
  geom_point(data= DFcol,
             aes(x= Longitude, y=Latitude, color= DFcol$Color, size=SUM),
             alpha=0.3)+
  scale_color_identity()+
  facet_wrap(~Bin_num)+
  labs(size="Number of indicators found")+
  ggtitle("Map of direct indicators through time")

# FIGURE 23----------------------------------
# map for potential food indicators
Amap+
  geom_point(data=Datasheet_sh.pot.full.long%>%
               filter(IND=="High")%>%
               filter(SUM!=0),
             aes(x= Longitude, y=Latitude),color="red")+
  ggtitle("Sites with indicators with high food potential")

# FIGURE 24-----------------------------------------------
## map for potential food indicators through time
Amap+
  geom_point(data=Datasheet_sh.pot.full.long%>%
               filter(IND=="High")%>%
               filter(SUM!=0),
             aes(x= Longitude, y=Latitude),color="red")+
  facet_wrap(~Bin_num)+
  labs(color="Indicator")+
  ggtitle("Indicators with high food potential through time")



