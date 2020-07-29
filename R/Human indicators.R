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

#TODO: rerun map,maps faceted, chart with direct ind per sequence, with food pot per sequence, time span of records, 
#     number of direct and indirect across time, number of indirect per sequence, number of food pot across time,
#     number of bins per alt belt with new coord for Huila
# -----------------------------------------------

# Packages
library(readxl)
library(tidyverse)
library(writexl)
library(OpenStreetMap)
library(ggmap)
library(plotly)
library (magick)
library(gtable)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(cowplot)
library(plotrix)
library(NbClust)
library(scales)
library(ggpubr)
library(vegan)

#------------------------------------------------------------------------

#### Setting up dataframes (do only once)-------------------------------------------------------

# read spreadsheet
Datasheet_original <- read_excel("data/Datasheet_original.xlsx")


# add Bin_num column
Bin_num <- sub(".*\\-","",Datasheet_original$Bin)
Bin_num <- sub("\\ BP.*","",Bin_num)
Datasheet <- Datasheet_original %>%
  mutate(Bin_num = Bin_num)
Datasheet$Bin_num <- as.numeric(Datasheet$Bin_num)


# add country, lat, long to Datasheet
# Df with metadata for all sites (even the ones that are not in Datasheet)
LAPD_Andes <- read_excel("data/LAPD_Andes_MY.xlsx")
LAPD_Andes$LAPD_ID <- as.numeric(LAPD_Andes$LAPD_ID)

# add missing metadata to LAPD_Andes
# altitude
which(is.na(LAPD_Andes$Altitude))
LAPD_Andes[72,"Altitude"]=2608 # value found in the original paper

# latitude
view(LAPD_Andes%>%filter(is.na(Latitude)))
LAPD_Andes[70,"Latitude"] = 4.58545
LAPD_Andes[70,"Longitude"] = -75.19558
LAPD_Andes[71,"Latitude"] = -0.57946
LAPD_Andes[71,"Longitude"] = -78.24397
LAPD_Andes[72,"Latitude"] = -00.25405 
LAPD_Andes[72,"Longitude"] = -78.01075

#to check which Sites have different names
Sitename = distinct(Datasheet,`Site Name`)
Nomatch <- match(Sitename$`Site Name`,LAPD_Andes$SiteName)
which(is.na(Nomatch))
Sites_Nomatch <- Sitename[c(8,11, 14, 23, 34, 35, 38),]
Sites_Nomatch$`Site Name`
LAPD_Andes$SiteName

# rename Datasheet Site Names according to the ones in LAPD_Andes
to_replace<-c("Cerro Toledo",  "El Tiro\n"  , "Laguna Natosas forest", "Pedras Blancas", "Paramo de Agua Blanca III", "Paramo de pena Negra I")
with_this<-c("Cerro Toledo CT", "El Tiro", "Lagunas Natosas Forest", "Piedras Blancas", "Agua Blanca PAB III", "Paramo de Pena Negra 1")
Datasheet$`Site Name`<-Datasheet$`Site Name` %>%
  plyr::mapvalues(to_replace,with_this)

# insert correct lat, long for Pantano de Pecho  # IS THIS WRONG IN LAPD_ANDES?
LAPD_Andes[47,"Latitude"]= -0.20
LAPD_Andes[47,"Longitude"]= -78.37

# add columns with country, latitude and longitude to Datasheet, per each site
Datasheet$Country <- LAPD_Andes$Country[match(Datasheet$`Site Name`,LAPD_Andes$SiteName)]
Datasheet$Latitude <- LAPD_Andes$Latitude[match(Datasheet$`Site Name`,LAPD_Andes$SiteName)]
Datasheet$Longitude <- LAPD_Andes$Longitude[match(Datasheet$`Site Name`,LAPD_Andes$SiteName)]

# see if there something NA left, and which site it corresponds to
view(filter(Datasheet,is.na(Country)))

# save
write_xlsx(LAPD_Andes,"data/LAPD_Andes_MY.xlsx")


#-----------------------------------------------------------------------------


## SELECTION OF SEQUENCES OF INTEREST

#### new shorter datasheet
# select record spanning last 12000 yr
Datasheet_shaved <- Datasheet %>%
  filter(Bin_num < 12500)

Datasheet_sh_charcoal <- Datasheet %>%
  filter(Bin_num < 12500)
# select only indicators actually found
# Step 1: Filter indicators from dataset
Datasheet.indicators <- Datasheet %>%
   select(.,-c("LAPD_ID\n","Site Name",
               "Reference (short)","Bin", "Bin_num", "Latitude","Longitude", "Country" ))

# Step 2: Convert counts from character to numeric
Datasheet.indicators <- apply (Datasheet.indicators,2,
                               FUN= function(x) as.numeric(unlist(x)))

# Step 4: Make datasheet with site vs number of times an indicator is counted  
Datasheet.indicators <- as.data.frame(Datasheet.indicators)

# calculate the sum per human indicator
DF.indicators.SUM <-data.frame(IN=apply(Datasheet.indicators, 2, 
                                        FUN = function(x) sum(x,na.rm = T)))
DF.indicators.SUM$IN.name <- row.names(DF.indicators.SUM) %>% as.factor() 

# datasheet with only indicators actually found
DF.zeros<-(DF.indicators.SUM%>%filter(IN==0)) 
Todrop<-as.vector(DF.zeros$IN.name)
Datasheet_shaved<-Datasheet_shaved[,!names(Datasheet_shaved)%in% Todrop]

Datasheet_sh_charcoal<-Datasheet_sh_charcoal[,!names(Datasheet_sh_charcoal)%in% Todrop]
# remove charcoal (has to be analyzed separately)
Datasheet_shaved$Charcoal<- NULL

# add columns to Datasheet_shaved
# records length
length_record <- Datasheet_shaved %>% 
  group_by(`Site Name`) %>%
  summarise(max(Bin_num))

length_record$Length<-length_record$`max(Bin_num)`

Datasheet_shaved$Length<-length_record$Length[match(Datasheet_shaved$`Site Name`,
                                                    length_record$`Site Name`)]

# Altitude of sites
Datasheet_shaved$Altitude <- LAPD_Andes$Altitude[match(Datasheet_shaved$`Site Name`,
                                                     LAPD_Andes$SiteName)]


# correct other typos (found later in "Human indicators.R)
Datasheet[558,3]<-"Giraldo-Giraldo et al, 2018"   
Datasheet_shaved[426,3]<-"Giraldo-Giraldo et al, 2018"


write_xlsx(Datasheet_shaved,"Datasheet_shaved.xlsx")
write_xlsx(Datasheet,"Datasheet.xlsx")


# Import and adjust dataframe with right indicators
Human_indicators_original <- read_excel("data/02_Human indicators_V2.xlsx")

Human_indicators<-Human_indicators_original[-c(2,8,13,14,22,25,68),] %>%
   select(c("Group (Taxa)",
            "Family",
            "Indicator",
            "Potential food source (no/low/high)",
            "Disturbance", # Catalina will double check this
            "North Andean fossil records? [yes/no]" ))

Human_indicators[47,"Indicator"]<-"Indirect"  

colnames(Human_indicators)[4]<-"Potential food source" 

to_replaceI<-c("HIGH","LOW","Low","NO") 
with_thisI<-c("high", "low","low", "no") 
Human_indicators$`Potential food source`<- Human_indicators$`Potential food source` %>%
   plyr::mapvalues(to_replaceI,with_thisI)

write_xlsx(Human_indicators,"data/Human_indicators.xlsx")

# human indicators with charcoal
Human_indicators2<-Human_indicators_original[-c(2,8,13,14,22,68),] %>%
  select(c("Group (Taxa)",
           "Family",
           "Indicator",
           "Potential food source (no/low/high)",
           "Disturbance", # Catalina will double check this
           "North Andean fossil records? [yes/no]" ))

Human_indicators2[48,"Indicator"]<-"Indirect"  

colnames(Human_indicators2)[4]<-"Potential food source" 

to_replaceI<-c("HIGH","LOW","Low","NO") 
with_thisI<-c("high", "low","low", "no") 
Human_indicators2$`Potential food source`<- Human_indicators2$`Potential food source` %>%
  plyr::mapvalues(to_replaceI,with_thisI)

# Change Ind names in Datasheet shaved (match names in Human_indicators)
Col.to <- pull(Human_indicators%>% 
                filter(`North Andean fossil records? [yes/no]`=="yes")%>%
                select(`Group (Taxa)`))

Col.from <- names(Datasheet_shaved)[5:47]

#sort Col.to in alphabetical number (to match column order in Datasheet_shaved)
Col.to <- sort(Col.to)

Datasheet_shaved<-Datasheet_shaved %>% rename_at(vars(Col.from), ~Col.to)

#change Colname
colnames(Datasheet_shaved)[2]<-"Site Name long"
colnames(Datasheet_shaved)[3]<-"Site Name"

write_xlsx(Datasheet_shaved,"data/Datasheet_shaved.xlsx")


## this is not working here, but also no-where else used
# df with first appearance of direct ind
#FirstInd <- Datasheet_sh.full %>% 
#  filter(Sum_direct>0)

#FirstInd <- FirstInd[with(FirstInd, order(-Bin_num)), ]

#FirstInd <- FirstInd[match(unique(FirstInd$`Site Name`), FirstInd$`Site Name`),]





### --------------------------------------------------------
#                                                         
#             Load adjusted dataframes
# 
### --------------------------------------------------------


Datasheet <- read_excel("data/Datasheet.xlsx")
Datasheet_shaved<-read_excel("data/Datasheet_shaved.xlsx")
Human_indicators <- read_excel("data/Human_indicators.xlsx")

Sites<-Datasheet_shaved %>% 
  distinct(`Site Name`,Latitude,Longitude, Country, Altitude)


view(Datasheet)

### --------------------------------------------------------
#                                                         
#                      TABLE 1                         
#                                                         
### --------------------------------------------------------

# subset df
ToTable<-Datasheet_shaved%>%
  distinct(`Site Name`,Country,Latitude,Longitude,`Reference (short)`)

# order df according to Country
ToTable<-ToTable[order(-ToTable$Latitude),]

#adjust content
ToTable[25,5]<-"Frederik et al, 2018"

grid.newpage()
grid.table(ToTable)

# export toTable in excel
write_xlsx(ToTable,"Table_01.xlsx")


### --------------------------------------------------------
#                                                         
#         SITE CLUSTERING BASED ON THE GEOGRAPHY          
#                                                         
### ---------------------------------------------------------

# set the best number of clusters based on the location and elevation (max 10) 
clusters <- Sites %>%
  mutate(long.s = scale(Longitude),
         lat.s = scale(Latitude),
         elev.s = scale(Altitude)) %>%
  dplyr::select(long.s, lat.s, elev.s) %>%
  NbClust(., min.nc = 3, max.nc = 10, method = "kmeans")

# add the cluster definition to the Sites DF
Sites$cluster.id <- as.factor(clusters$Best.partition)

#export site
write_xlsx(Sites,"Sites.xlsx")

#load modified sites file
Sites<-read_xlsx("data/Sites.xlsx")

colnames(Sites)[8]<-"cluster.id_2"
Sites$cluster.id_2<- as.factor(Sites$cluster.id_2)

# create colors pallete for each of the cluster
names(Color_legend_cluster) <- levels(Sites$cluster.id_2)
Color_legend_cluster <- brewer.pal(n = levels(Sites$cluster.id_2) %>% length(), name = 'Set2') 

# add the color to the Site tibble
Sites <- Sites %>%
  left_join(.,data.frame(cluster.id_2 = names(Color_legend_cluster), cluster.color_2 = Color_legend_cluster), by="cluster.id_2")

#export site
write_xlsx(Sites,"data/Sites_2.xlsx")

# add the color to the Datasheet_shaved tibble
Datasheet_shaved <- Datasheet_shaved %>% 
  left_join(.,Sites %>% 
              dplyr::select(`Site Name`,cluster.id_2,cluster.color_2), by= "Site Name")




### --------------------------------------------------------
#                                                         
#                       FIGURES          
#                                                         
### ---------------------------------------------------------


#### FIGURE 1---------------------------------------------

##add altitudinal "class"
Sites$Alt_class<-ifelse(Sites$Altitude < 2000, "1000-2000 masl", 
                        ifelse(Sites$Altitude %in% 2000:3000, "2000-3000 masl", "3000-4215 masl"))


# version 2
s <- "element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative%7Celement:geometry%7Cvisibility:off&style=feature:administrative.country%7Celement:geometry.stroke%7Ccolor:0x000000%7Cvisibility:on&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Cvisibility:off&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road%7Celement:labels.icon%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit%7Cvisibility:off&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&size=480x360"

Andes2 <- get_googlemap(center = c(-76, 2.6), zoom=5, scale = 1, style = s)


Amap2<-ggmap(Andes2)+
  geom_point(data=Sites, aes(x= Longitude, 
                             y=Latitude, 
                             color=cluster.id_2, 
                             size=Alt_class))+
  geom_point(data=Sites, aes(x= Longitude, 
                             y=Latitude),
             color="black", size=1)+
  scale_color_manual(values= Color_legend_cluster)+
  guides(color=F)+
  labs(y="Latitude",
       x="Longitude",
       size="Elevation")

#version 3
bbox = c(left=-84,
         bottom=-7,
         right=-67, 
         top=12)

Andes3<-get_map(location = bbox, source = "google", maptype = "terrain")
plot(Andes3)

#version 4
Andes4<-getMap(resolution = "coarse")
plot(Andes4, xlim=c(-84,-67), ylim=c(12,-7))

#version 5
study_area<-map_data("world")

#### FIGURE 2-------------------------------
## time span of each site

# with Datasheet_shaved
p02<- Datasheet_shaved %>%
  group_by(`Site Name`) %>%
  summarise(MIN = min(Bin_num),
            MAX = max(Bin_num)) %>%
  left_join(.,Sites %>% dplyr::select(`Site Name`,  Altitude, Latitude, cluster.id_2), by="Site Name") %>%
  ggplot() +
  geom_hline(yintercept = seq(from=0, to=12e3, by=500), color="gray80")+
  geom_bar(aes(x=reorder(`Site Name`,Latitude),y= MAX, fill=cluster.id_2), colour="gray30", stat = "identity")+
  scale_fill_manual(values = Color_legend_cluster)+
  theme_classic()+
  theme(axis.text.y  = element_text( hjust=1))+
  theme(axis.title.x = element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.title.y = element_blank())+
  geom_text(aes(x=`Site Name`,y=MAX, label=MAX), stat='identity', hjust=-0.1, size=3, color="gray30")+
  coord_flip(ylim = c(0,13e3))+
  labs(fill = "Cluster ID")
  
p02

cluster_ID <- get_legend(p02)

p02 <- p02 + guides(fill = F)

p02.fin <- ggarrange(
  p02, cluster_ID,
  nrow = 1, widths = c(12,1)
)

p02.fin

ggsave("figures/NEW/02.pdf",p02.fin,
       units = "cm", width = 25, height = 20, dpi = 600)




#### FIGURE 3 ---------------------------------
## number of records per time bin 

# with Datasheet_shaved
(p03<- ggplot(Datasheet_shaved%>%
                mutate(Bin_2=sub("\\ BP.*","",Bin)))+
  geom_bar(aes(x= reorder(Bin,-Bin_num)), color="gray30", fill="gray80")+
  theme_classic()+
  theme(axis.text.x  = element_text(angle = 70, hjust=1),
        axis.title.y = element_blank())+
  xlab("Time bins (yr BP)"))


ggsave("figures/NEW/03.pdf",p03,
       units = "cm", width = 25, height = 20, dpi = 600)




#### FIGURE 3b ---------------------------------
## number of times human indicators are counted in total

# add number of sites to Human_indicators tibble
Human_indicators <- Datasheet_shaved %>%
  select(.,-c(names(Datasheet_shaved)[c(1:2,4:5,49:56)])) %>%
  pivot_longer(names(.)[-1]) %>%
  group_by(`Site Name`,name) %>%
  summarise(COUNT = sum(as.double(value), na.rm = T),
            PRES = ifelse(COUNT==0,0,1)) %>%
  ungroup() %>%
  group_by(name) %>%
  summarise(N.SITES = sum(PRES)) %>%
  left_join(., Human_indicators, by=c("name"="Group (Taxa)")) %>%
  rename("Taxa" = name,
         "PotentionalFoodSource" = `Potential food source`,
         "fossilRecords" = `North Andean fossil records? [yes/no]`)

# plot
(p03b<- Human_indicators %>%
  ggplot(aes(x=reorder(Taxa,N.SITES),y=N.SITES,fill=Indicator)) +
  geom_bar(stat = "identity", color="gray30") +
  scale_fill_manual(values = my_colors_ind)+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, hjust=1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  xlab("Indicators") +
  labs(fill="Indicator"))

ggsave("figures/NEW/03b.pdf",p03b,
       units = "cm", width = 25, height = 20, dpi = 600)

#### FIGURE 3c---------------------------------
# as above, distinguishing for Potential source of food

p03c<- Human_indicators%>%
  ggplot(aes(x=reorder(Taxa,N.SITES),y=N.SITES,fill=PotentionalFoodSource))+
  geom_bar(stat = "identity", color="gray30")+
  scale_fill_manual(values=my_colors_food)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 70, hjust=1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  xlab("Indicators")+
  labs(fill="Food potential")

p03c

ggsave("figures/NEW/03c.pdf",p03c,
       units = "cm", width = 25, height = 20, dpi = 600)


#### FIGURE 3d---------------------------------
# ORDINATION

# select only indicator data and turn ten into numbers
# and replace all missing values with zeros ( compositional data cannot have NA)
cca.data <-  Datasheet_shaved %>% 
  dplyr::select(-names(Datasheet_shaved)[c(1:5,49:56)]) %>%
  apply(., 2, FUN= function(x){
    x[x=="NA"] <-0;
    y<- as.numeric(x);
    return(y)
  })  

#add row names
row.names(cca.data) <- Datasheet_shaved$`Site Name`

# select enviromrntal values (time)
cca.env <- Datasheet_shaved %>%
  select(c("Bin_num","Site Name"))

# subset the data to exclude rows and cols with SUM = 0
cca.env <- cca.env[rowSums(cca.data, na.rm = T) != 0, ]
cca.data <- cca.data[rowSums(cca.data, na.rm = T) != 0, colSums(cca.data, na.rm = T) != 0]

# check if everything is OK
all(rowSums(cca.data)>0)
all(colSums(cca.data)>0)

# the data is in binary so lineral prediction is better. But just for sure
# DCA on data
decorana(cca.data)
# first axis length is under 2.5 -> linear predictor -> RDA

rda.1 <- rda(cca.data ~ cca.env$Bin_num + Condition(cca.env$`Site Name`), scale =F) 

# summary
smry <- summary(rda.1)

df2  <- data.frame(smry$species[,1:3]) %>% # loadings for PC1 and PC2
  rownames_to_column() %>%
  rename(IND = rowname) %>%
  left_join(.,Human_indicators %>%
              rename(IND = Taxa), by= "IND") %>%
  as_tibble()

x_lim = c(-0.3,0.2)
y_lim =c(-1,0.25)

axis_one <- "RDA1"
axis_two <- "PC1"

my_colors_ind<-c("#DC143C","#FFFFFF")
my_colors_food<-c("#053061", "#2166AC", "#A9A9A9")

#short name for some of the indicators
df2[4,1]<-"Ama/Cheno"
df2[20,1]<-"Fab/Leg"
df2[22,1]<-"Gramin/Poa"
df2[41,1]<-"Umbre/Api"
df2[28,1]<-"Myr/Morel"

rda.plot.ind.base <- ggplot(df2, aes(x=0, xend=get(axis_one), y=0, yend=get(axis_two)))+
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  coord_fixed(xlim= x_lim, ylim = y_lim)+ 
  theme_classic()

rda.plot.ind.01 <- rda.plot.ind.base+
  geom_segment(arrow=arrow(length=unit(0.01,"npc")), aes(color=Indicator)) +
  geom_text(data=df2,check_overlap = F,
            aes(x=get(axis_one),y=get(axis_two),
                label=IND,color=Indicator,
                hjust=0.5*(1-sign(get(axis_one))),
                vjust=0.5*(1-sign(get(axis_two)))), size=4)+
  scale_color_manual(values=c("#DC143C","#"))+
  labs(x="axis one",y="axis two")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

rda.plot.ind.01

rda.plot.ind.01.legend <- get_legend(rda.plot.ind.01)

rda.plot.ind.03 <- rda.plot.ind.base+
  geom_segment(arrow=arrow(length=unit(0.01,"npc")), aes(color=PotentionalFoodSource)) +
  geom_text(data=df2,check_overlap = F,
            aes(x=get(axis_one),y=get(axis_two),label=IND,color=PotentionalFoodSource,
                hjust=0.5*(1-sign(get(axis_one))),vjust=0.5*(1-sign(get(axis_two)))), size=4)+
    scale_color_manual(values= my_colors_food)+
  labs(x="axis one",y="axis two", color= "Food potential")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

rda.plot.ind.03.legend <- get_legend(rda.plot.ind.03)

rda.plot.ind.sum <- ggarrange(
  rda.plot.ind.01+guides(color=F),
  rda.plot.ind.03+guides(color=F),
  nrow=1, align = "h", labels = c("a)","b)")
)

rda.plot.ind.legend <- ggarrange(
  rda.plot.ind.01.legend,
  rda.plot.ind.03.legend,
  nrow = 1
)


rda.plot.ind.sum <- annotate_figure(rda.plot.ind.sum,left = "second axis",bottom="first axis")

rda.plot.ind.legend <- ggarrange(
  rda.plot.ind.01.legend,
  rda.plot.ind.03.legend,
  nrow = 1
)


p03.d <- ggarrange(rda.plot.ind.sum,
                   rda.plot.ind.legend,
                   ncol = 1, heights = c(10,1))

p03.d

ggsave("figures/To use/26_ordination.pdf",p03.d,
       units = "cm", width = 25, height = 20, dpi = 600)

# A = colored by direct/indirect
# B = colored by the Family
# C = colored by Potentional food source 





   
#### FIGURE 4 --------------------------------
 ## trend of total human indicators through time per site
 
 #sum of indicators per time bin for each site
 DF_sh.tot<-data.frame(Sum_total=apply(Datasheet_sh.indicators,1,
                                       FUN = function(x) sum(x,na.rm = T)))
 
Datasheet_sh.full<- bind_cols(Datasheet_shaved,DF_sh.tot)
 
# new column for labels
Datasheet_sh.full$ForLabs<-Datasheet_sh.full$Sum_total
# change 0 in H (except for two in Rio Timbio, real absences) 
Datasheet_sh.full$ForLabs[Datasheet_sh.full$ForLabs==0]<-"H"
Datasheet_sh.full[123:124,"ForLabs"]<-"0"

#add altitude to site names
Datasheet_sh.full$`Site Name`<-paste(Datasheet_sh.full$`Site Name`,"(",Datasheet_sh.full$Altitude, "masl",")")

# sites ordered according to lat, to sort facets in plot
Datasheet_sh.full$`Site Name`<-factor(Datasheet_sh.full$`Site Name`)
Datasheet_sh.full$`Site Name`<-fct_reorder(Datasheet_sh.full$`Site Name`,
                                                -Datasheet_sh.full$Latitude)

# plot
   p <- ggplot(data= Datasheet_sh.full, 
               aes(x=reorder(Bin_num,-Bin_num), y=Sum_total)) +  
     geom_bar(stat = "identity",width = 0.5) +
     geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
                col=ifelse(Datasheet_sh.full$ForLabs=="H","orange","black"), pch=25 ,
                bg=ifelse(Datasheet_sh.full$ForLabs=="H","orange","black"), cex=0.7)+
     facet_wrap(~`Site Name`) +
     theme_bw() +
     theme(axis.text.x = element_text(angle = 50, hjust=1)) +
     ylab("Absolute frequency")+
     xlab("Time bin")+
     ggtitle("Total number of indicators found per time bin per site")
   
# same with lines and points   
 ggplot(data= Datasheet_sh.full, 
          aes(x=-Bin_num, y=Sum_total)) +  
     geom_point()+
   geom_line()+
     facet_wrap(~`Site Name`) +
     theme_bw() +
     theme(axis.text.x = element_text(angle = 50, hjust=1)) +
     ylab("Absolute frequency")+
     xlab("Time bin")+
     ggtitle("Total number of indicators found per time bin per site")+
   geom_point(aes(x=-Bin_num,y=25),
              col=ifelse(Datasheet_sh.full$ForLabs=="H","orange","black"), pch=25 ,
              bg=ifelse(Datasheet_sh.full$ForLabs=="H","orange","black"), cex=0.7)

   
   
# to color facet according to Country   
g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)

strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                            x = grobs_df$gPath_full)]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                             x = grobs_df$gPath_full)]

fills <-  c(rep("#636363",17), # vector of colors to fill rectangles
            rep("#BDBDBD",2),
            rep("#636363",5),
            rep( "#BDBDBD",7),
            rep( "#F0F0F0",2),
            rep( "#BDBDBD",5)) 

txt_colors <- c(rep("white",17), # vector of colors for text
         rep("black",2),
         rep("white",5),
         rep( "black",7),
         rep( "black",2),
         rep( "black",5))

for (i in 1:length(strip_bg_gpath)){ # Edit the grobs
  g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
  g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
} 

#add legend from fig 8b
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegC <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)


upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)







#### FIGURE 4 --------------------------------
## trend of total human indicators through time per site

#sum of indicators per time bin for each site

(p04 <-Datasheet_shaved %>%
  select(.,-c(names(Datasheet_shaved)[c(1:2,4:5,50:56)])) %>%
  pivot_longer(names(.)[-c(1,45)]) %>%
  group_by(`Site Name`,Bin_num) %>%
  summarise(COUNT = sum(as.double(value), na.rm = T)) %>%
  mutate(Hiatus = ifelse(COUNT==0,TRUE,FALSE)) %>%
  left_join(Sites, by="Site Name") %>% 
  ggplot(aes(x=as.double(Bin_num), y=COUNT)) +
  geom_line(color="gray30")+
  geom_point(data = . %>% filter(Hiatus == F), shape= 15, color="gray30", size=1)+
  geom_point(data = . %>% filter(Hiatus == T), shape= 0, color="orange", size= 1)+
  facet_wrap(~reorder(`Site Name`,-Latitude )) +
  scale_x_continuous(trans = "reverse", breaks = seq(to=12e3, from=0, by=2e3))+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.4)) +
  ylab("Absolute frequency")+
  xlab("Time (yr BC)")+
  ggtitle("Total number of indicators found per time bin per site"))

 

# to color facet according to Cluster  
p04.e <- ggplot_gtable(ggplot_build(p04))
stripr <- which(grepl('strip-t', p04.e$layout$name))

for (i in stripr) {
  if (class(p04.e$grobs[[i]]) != "zeroGrob"){
    j <- which(grepl('rect', p04.e$grobs[[i]]$grobs[[1]]$childrenOrder))
    k <- which(grepl('title', p04.e$grobs[[i]]$grobs[[1]]$childrenOrder))
    
    p04.e$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- Sites %>% 
      filter(`Site Name` == p04.e$grobs[[i]]$grobs[[1]]$children[[k]]$children[[1]]$label) %>%
      dplyr::select(cluster.color) %>%
      pluck(1) %>%
      as.character()
  }
}

plot(p04.e)

p04.e.fin <-ggarrange(p04.e, cluster_ID, nrow = 1, widths = c(10,1))

p04.e.fin

ggsave("figures/NEW/04.pdf",p04.e.fin,
       units = "cm", width = 25, height = 20, dpi = 600)





#### FIGURE 5 (discarded, hard to read) ---------------------------------
## number of times human indicators are counted in each site
   

 p<-reshape2::melt(Datasheet_sh.ind.full%>%
                      select(-c("Latitude"))) %>%
   group_by(`Site Name`,variable) %>%
   summarise(Count = sum(value, na.rm = T))%>%
   ggplot(aes(x=variable,y=Count))+
   geom_bar(stat = "identity")+
   facet_wrap(~`Site Name`)+
   scale_x_discrete(label=function(x) abbreviate(x, 2))+
   theme_bw() +
   theme(axis.text.x = element_text(angle = 50, hjust=1))+
   labs(x= "Indicator")+
   ggtitle("In how many time bins each indicator was found, per site")
   
 g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
 grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
 grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
 grobs_df$gPath_full <- gsub(pattern = "layout::", 
                             replacement = "", 
                             x = grobs_df$gPath_full, 
                             fixed = TRUE)
 
 strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                             x = grobs_df$gPath_full)]
 strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                              x = grobs_df$gPath_full)]
 
 fills <-  c(rep("#636363",17), # vector of colors to fill rectangles
             rep("#BDBDBD",2),
             rep("#636363",5),
             rep( "#BDBDBD",7),
             rep( "#F0F0F0",2),
             rep( "#BDBDBD",5)) 
 
 txt_colors <- c(rep("white",17), # vector of colors for text
                 rep("black",2),
                 rep("white",5),
                 rep( "black",7),
                 rep( "black",2),
                 rep( "black",5))
 
 for (i in 1:length(strip_bg_gpath)){
   g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
   g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
 } # Edit the grobs

 
 # add table and legend extracted from FIG.2
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
 vpleg <- viewport(width = 0.22, height =1, x = 0.87, y = 0.5)
 vplegC <- viewport(width = 0.15, height = 0., x = 0.8, y = 0.9)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 upViewport(0)
 pushViewport(vplegC)
 grid.draw(legend)
 
 
 
 #### FIGURE 5b( discarded, hard to read)--------------------------------------
 # distinguish between direct and indirect
 
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                       select(-c("Latitude"))) %>%
    group_by(`Site Name`,variable) %>%
    summarise(Count = sum(value, na.rm = T))%>%
    mutate(Ind=Human_indicators$Indicator
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
    ggplot(aes(x=variable,y=Count, fill=Ind))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
    scale_fill_discrete(guide=FALSE)+
    scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x= "Indicator",
         fill="Indicator")+
    ggtitle("Number of times Indicators are found per Site",
            subtitle = "In how many time bins each indicator was found, per site") 
 
 g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
 
 # extract legend from plot
 legend_di<-get_legend(g_1) 
 # add scale_fill_dscete to original plot p
 
 
 grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
 grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
 grobs_df$gPath_full <- gsub(pattern = "layout::", 
                             replacement = "", 
                             x = grobs_df$gPath_full, 
                             fixed = TRUE)
 
 strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                             x = grobs_df$gPath_full)]
 strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                              x = grobs_df$gPath_full)]
 
 fills <-  c(rep("#636363",17), # vector of colors to fill rectangles
             rep("#BDBDBD",2),
             rep("#636363",5),
             rep( "#BDBDBD",7),
             rep( "#F0F0F0",2),
             rep( "#BDBDBD",5)) 
 
 txt_colors <- c(rep("white",17), # vector of colors for text
                 rep("black",2),
                 rep("white",5),
                 rep( "black",7),
                 rep( "black",2),
                 rep( "black",5))
 
 for (i in 1:length(strip_bg_gpath)){
    g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
    g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
 } # Edit the grobs
 
 # add table(from fig 6), legend extracted from FIG.2, legend extracted from this plot
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
 vpleg <- viewport(width = 0.22, height = 1, x = 0.87, y = 0.5)
 vplegC <- viewport(width = 0.1, height = 0.2, x = 0.8, y = 0.9)
 vplegI<- viewport(width = 0.1, height = 0.2, x = 0.9, y = 0.9)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 
 upViewport(0)
 pushViewport(vplegC)
 grid.draw(legend)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 #### FIGURE 5b_Colombia----------------------------------------
 # order sites by lat   
 Datasheet_sh.ind.full$Latitude<-Datasheet_sh.full$Latitude  
 Datasheet_sh.ind.full$`Site Name`<-factor(Datasheet_sh.ind.full$`Site Name`)
 Datasheet_sh.ind.full$`Site Name`<-fct_reorder(Datasheet_sh.ind.full$`Site Name`,
                                                -Datasheet_sh.ind.full$Latitude)
 
 # create table as legend for the used abbreviations
 ToAbbr<-reshape2::melt(Datasheet_sh.ind.full%>%select(-c("Latitude")))%>%
   distinct(variable)%>%
   pull()
 ForLegend<-as.data.frame(abbreviate(ToAbbr,minlength = 3,method = "both.sides", named = T))
 ForLegend<-rownames_to_column(ForLegend, "Indicator")
 colnames(ForLegend)[2]<-"Abbreviation"
 #ForLegend[4,1]<-"Amaranthceae/ Chenopodiaceae"
 ForLegend[c(4,28),2]<-c("A/C", "Myr")
 
 my_table <- tableGrob( ForLegend ,theme = ttheme_default (base_size = 7, padding = unit(c(4,2),"mm")),
                        rows=NULL)
 
 my_table$widths <- unit(rep(1/ncol(my_table), ncol(my_table)), "npc")
 
 ## plot
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                       mutate(Country=Datasheet_sh.full$Country)%>%
                       filter(Country== "Colombia")%>%
                       select(-c("Latitude"))) %>%
    group_by(`Site Name`,variable) %>%
    summarise(Count = sum(value, na.rm = T))%>%
    mutate(Ind=Human_indicators$Indicator
           [match(variable,Human_indicators$`Group (Taxa)`)],
           Short=ForLegend$Abbreviation
           [match(variable,ForLegend$Indicator)])%>%
    ggplot(aes(x=Short,y=Count, fill=Ind))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`,ncol = 2)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
    scale_fill_discrete(guide=FALSE)+
   #guides(fill = guide_legend(nrow = 1))+
    labs(x= "Indicator",
         fill="Indicator")+
    ggtitle("Number of bins indicators are found per Site, Colombia") 
 
 g_1 <- grid.force(ggplotGrob(p))
 
 # extract legend from plot
 legend_di<-get_legend(g_1)
 
 # add scale_fill_dscete to original plot p and re run
 
 # add table(from fig 6) and legend extracted from FIG.2, legend from plot 6b
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .55)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.55)
 vplegI<- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 #### FIGURE 5b_Ecuador----------------------------------------
 ## split plot per countries
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                       mutate(Country=Datasheet_sh.full$Country)%>%
                       filter(Country== "Ecuador")%>%
                       select(-c("Latitude"))) %>%
    group_by(`Site Name`,variable) %>%
    summarise(Count = sum(value, na.rm = T))%>%
    mutate(Ind=Human_indicators$Indicator
           [match(variable,Human_indicators$`Group (Taxa)`)],
           Short=ForLegend$Abbreviation
           [match(variable,ForLegend$Indicator)])%>%
   ggplot(aes(x=Short,y=Count, fill=Ind))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`,ncol = 2)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
    scale_fill_discrete(guide=FALSE)+
    labs(x="Indicator")+
    ggtitle("Number of bins Indicators found per Site, Ecuador")
 
 g_1 <- grid.force(ggplotGrob(p))

 
 # add table(from fig 6) and legend extracted from FIG.2, legend from plot 6b
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .55)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.55)
 vplegI<- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 #### FIGURE 5b_Venezuela----------------------------------------
 ## split plot per countries
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                       mutate(Country=Datasheet_sh.full$Country)%>%
                       filter(Country== "Venezuela")%>%
                       select(-c("Latitude"))) %>%
    group_by(`Site Name`,variable) %>%
    summarise(Count = sum(value, na.rm = T))%>%
    mutate(Ind=Human_indicators$Indicator
           [match(variable,Human_indicators$`Group (Taxa)`)],
           Short=ForLegend$Abbreviation
           [match(variable,ForLegend$Indicator)])%>%
   ggplot(aes(x=Short,y=Count, fill=Ind))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
    scale_fill_discrete (guide=FALSE)+
    scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x="Indicator",
         fill="Indicator")+
    ggtitle("Number of bins Indicators found per Site, Venezuela")
 
 g_1 <- grid.force(ggplotGrob(p))
 
 
 # add table(from fig 6) and legend extracted from FIG.2, legend from plot 6b
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .55)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.55)
 vplegI<- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 # FIGURE 5b_under 2k mt-------------------------------------------------
 
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                       mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                       filter(Altitude < 2000)%>%
                       select(-c("Latitude", "Altitude"))) %>%
    group_by(`Site Name`,variable) %>%
    summarise(Count = sum(value, na.rm = T))%>%
    mutate(Ind=Human_indicators$Indicator
           [match(variable,Human_indicators$`Group (Taxa)`)],
           Short=ForLegend$Abbreviation
           [match(variable,ForLegend$Indicator)])%>%
   ggplot(aes(x=Short,y=Count, fill=Ind))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
    scale_fill_discrete(guide=FALSE)+
    labs(x="Indicator",
         fill="Indicator")+
    ggtitle("Number of bins Indicators found per Site, between 1k and 2k masl")
 
 g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
 
 # extract legend from plot
 legend_di<-get_legend(g_1)
 
 # add scale_fill_dscete to original plot p and re run
 
 grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
 grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
 grobs_df$gPath_full <- gsub(pattern = "layout::", 
                             replacement = "", 
                             x = grobs_df$gPath_full, 
                             fixed = TRUE)
 
 strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                             x = grobs_df$gPath_full)]
 strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                              x = grobs_df$gPath_full)]
 
 fills <-  c( rep("#BDBDBD",5)) # vector of colors to fill rectangles
 
 txt_colors <- c(rep("black",5)) # vector of colors for text
 
 for (i in 1:length(strip_bg_gpath)){
    g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
    g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
 } # Edit the grobs
 
 # add table(from fig 6), legend extracted from FIG.2, legend extracted from this plot
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .6)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
 vplegC <- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)
 vplegI<- viewport(width = 0.1, height = 0.1, x = 0.5, y = 0.1)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 upViewport(0)
 pushViewport(vplegC)
 grid.draw(legend)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 # FIGURE 5b_between 2 and 3k mt-------------------------------------------------
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                       mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                       filter(Altitude %in% 2000:3000)%>%
                       select(-c("Latitude", "Altitude"))) %>%
    group_by(`Site Name`,variable) %>%
    summarise(Count = sum(value, na.rm = T))%>%
   mutate(Ind=Human_indicators$Indicator
          [match(variable,Human_indicators$`Group (Taxa)`)],
          Short=ForLegend$Abbreviation
          [match(variable,ForLegend$Indicator)])%>%
   ggplot(aes(x=Short,y=Count, fill=Ind))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
    scale_fill_discrete(guide=FALSE)+
    labs(x="Indicator")+  
   ggtitle("Number of bins Indicators found per Site, between 2k and 3k masl")
 
 
 g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
 grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
 grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
 grobs_df$gPath_full <- gsub(pattern = "layout::", 
                             replacement = "", 
                             x = grobs_df$gPath_full, 
                             fixed = TRUE)
 
 strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                             x = grobs_df$gPath_full)]
 strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                              x = grobs_df$gPath_full)]
 
 fills <-  c(rep ("#636363",6) ,rep("#BDBDBD",2),rep ("#636363",1)) # vector of colors to fill rectangles
 
 txt_colors <- c(rep("white",6),rep("black",2), rep("white",1)) # vector of colors for text
 
 for (i in 1:length(strip_bg_gpath)){
    g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
    g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
 } # Edit the grobs
 
 
 # add table(from fig 6), legend extracted from FIG.2, legend extracted from 05b_2k
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .6)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
 vplegC <- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)
 vplegI<- viewport(width = 0.1, height = 0.1, x = 0.5, y = 0.1)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 upViewport(0)
 pushViewport(vplegC)
 grid.draw(legend)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 # FIGURE 5b_between 3 and 4k mt-------------------------------------------------
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                       mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                       filter(Altitude %in% 3000:4215)%>%
                       select(-c("Latitude", "Altitude"))) %>%
    group_by(`Site Name`,variable) %>%
    summarise(Count = sum(value, na.rm = T))%>%
   mutate(Ind=Human_indicators$Indicator
          [match(variable,Human_indicators$`Group (Taxa)`)],
          Short=ForLegend$Abbreviation
          [match(variable,ForLegend$Indicator)])%>%
   ggplot(aes(x=Short,y=Count, fill=Ind))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
    scale_fill_discrete(guide=FALSE)+
    labs(x="Indicator")+
   ggtitle("Number of bins Indicators found per Site, between 3k and 4215 masl")
 
 
 
 g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
 grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
 grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
 grobs_df$gPath_full <- gsub(pattern = "layout::", 
                             replacement = "", 
                             x = grobs_df$gPath_full, 
                             fixed = TRUE)
 
 strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                             x = grobs_df$gPath_full)]
 strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                              x = grobs_df$gPath_full)]
 
 fills <-  c(rep ("#636363",14) ,rep("#BDBDBD",4),
             rep ("#636363",1),rep( "#F0F0F0",2),
             rep ("#BDBDBD",3)) # vector of colors to fill rectangles
 
 txt_colors <- c(rep("white",14),
                 rep("black",4), 
                 rep("white",1), rep("black",5)) # vector of colors for text
 
 for (i in 1:length(strip_bg_gpath)){
    g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
    g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
 } # Edit the grobs
 
 # add table(from fig 6), legend extracted from FIG.2, legend extracted from fig 05b_2k
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .6)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
 vplegC <- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)
 vplegI<- viewport(width = 0.1, height = 0.1, x = 0.5, y = 0.1)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 
 upViewport(0)
 pushViewport(vplegC)
 grid.draw(legend)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
#### FIGURE 6 (discarded, hard to read)-----------------------------
 
 p<-reshape2::melt(Datasheet_sh.ind.full%>%
                     select(-c("Latitude"))) %>%
   group_by(`Site Name`,variable) %>%
   summarise(Count = sum(value, na.rm = T))%>%
   mutate(TotBins=TotBins$Freq
          [match(`Site Name`,TotBins$Var1)])%>%
   mutate(Freq=Count/TotBins)%>%
   ggplot(aes(x=variable,y=Freq))+
   geom_bar(stat = "identity")+
   facet_wrap(~`Site Name`)+
   scale_x_discrete(label=function(x) abbreviate(x, 2))+
   theme_bw() +
   theme(axis.text.x = element_text(angle = 50, hjust=1))+
   labs(x= "Indicator")+
   ggtitle("Relative frequency of indicators",
           subtitle = "Proportion of number of bins indicators found/ number of bins covered in a record" )
 
 g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
 grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
 grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
 grobs_df$gPath_full <- gsub(pattern = "layout::", 
                             replacement = "", 
                             x = grobs_df$gPath_full, 
                             fixed = TRUE)
 
 strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                             x = grobs_df$gPath_full)]
 strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                              x = grobs_df$gPath_full)]
 
 fills <-  c(rep("#636363",17), # vector of colors to fill rectangles
             rep("#BDBDBD",2),
             rep("#636363",5),
             rep( "#BDBDBD",7),
             rep( "#F0F0F0",2),
             rep( "#BDBDBD",5)) 
 
 txt_colors <- c(rep("white",17), # vector of colors for text
                 rep("black",2),
                 rep("white",5),
                 rep( "black",7),
                 rep( "black",2),
                 rep( "black",5))
 
 for (i in 1:length(strip_bg_gpath)){
   g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
   g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
 } # Edit the grobs
 

 # add table(from fig 6) and legend extracted from FIG.2
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
 vpleg <- viewport(width = 0.22, height = 1, x = 0.87, y = 0.5)
 vplegC <- viewport(width = 0.2, height = 0.2, x = 0.8, y = 0.9)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 
 upViewport(0)
 pushViewport(vplegC)
 grid.draw(legend)
 
 #### FIGURE 6b (discarded, hard to read)--------------------------------------
 # relative freq., distinguish between direct and indirect
 
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                      select(-c("Latitude"))) %>%
   group_by(`Site Name`,variable) %>%
   summarise(Count = sum(value, na.rm = T))%>%
   mutate(TotBins=TotBins$Freq
          [match(`Site Name`,TotBins$Var1)])%>%
   mutate(Freq=Count/TotBins)%>%
   mutate(Ind=Human_indicators$Indicator
          [match(variable,Human_indicators$`Group (Taxa)`)])%>%
   ggplot(aes(x=variable,y=Freq, fill=Ind))+
   geom_bar(stat = "identity")+
   facet_wrap(~`Site Name`)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   scale_fill_discrete(guide=FALSE)+
   scale_x_discrete(label=function(x) abbreviate(x, 2))+
   labs(x="Indicator")+
   ggtitle("Relative frequency of indicators",
           subtitle = "Proportion of number of bins indicators found/ number of bins covered in a record" )
 
 g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
 grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
 grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
 grobs_df$gPath_full <- gsub(pattern = "layout::", 
                             replacement = "", 
                             x = grobs_df$gPath_full, 
                             fixed = TRUE)
 
 strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                             x = grobs_df$gPath_full)]
 strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                              x = grobs_df$gPath_full)]
 
 fills <-  c(rep("#636363",17), # vector of colors to fill rectangles
             rep("#BDBDBD",2),
             rep("#636363",5),
             rep( "#BDBDBD",7),
             rep( "#F0F0F0",2),
             rep( "#BDBDBD",5)) 
 
 txt_colors <- c(rep("white",17), # vector of colors for text
                 rep("black",2),
                 rep("white",5),
                 rep( "black",7),
                 rep( "black",2),
                 rep( "black",5))
 
 for (i in 1:length(strip_bg_gpath)){
   g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
   g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
 } # Edit the grobs
 

 # add table(from fig 6) and legend extracted from FIG.2
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
 vpleg <- viewport(width = 0.22, height = 1, x = 0.87, y = 0.5)
 vplegC <- viewport(width = 0.1, height = 0.2, x = 0.8, y = 0.9)
 vplegI<- viewport(width = 0.1, height = 0.2, x = 0.9, y = 0.9)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 
 upViewport(0)
 pushViewport(vplegC)
 grid.draw(legend)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 #### FIGURE 6b_Colombia----------------------------------------
 ## same with relative frequencies of indicators (n bins ind found/n bins covered from that site)
 #df with number of bins for each site (excluding bins where hiatuses)
 TotBins=as.data.frame (table(Datasheet_sh.full%>%
                                filter(ForLabs!="H")%>%
                                select(`Site Name`)))
 
 
 ## split plot per countries
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                      mutate(Country=Datasheet_sh.full$Country)%>%
                      filter(Country== "Colombia")%>%
                      select(-c("Latitude"))) %>%
   group_by(`Site Name`,variable) %>%
   summarise(Count = sum(value, na.rm = T))%>%
   mutate(TotBins=TotBins$Freq
          [match(`Site Name`,TotBins$Var1)],
          Freq=Count/TotBins,
          Ind=Human_indicators$Indicator
          [match(variable,Human_indicators$`Group (Taxa)`)],
          Short=ForLegend$Abbreviation
          [match(variable,ForLegend$Indicator)])%>%
   ggplot(aes(x=Short,y=Freq, fill=Ind))+
   geom_bar(stat = "identity")+
   facet_wrap(~`Site Name`,ncol = 2)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   scale_fill_discrete(guide=FALSE)+
    labs(x="Indicator",
         y="Relative frequency")+
   ggtitle("Proportion of number of bins indicators found/ number of bins covered , per site (Colombia)") 
 
 g_1 <- grid.force(ggplotGrob(p))
 
 # add table(from fig 6) and legend extracted from FIG.2, legend from fig 05b_Col
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .55)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.55)
 vplegI<- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 #### FIGURE 6b_Ecuador----------------------------------------
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                      mutate(Country=Datasheet_sh.full$Country)%>%
                      filter(Country== "Ecuador")%>%
                      select(-c("Latitude"))) %>%
   group_by(`Site Name`,variable) %>%
   summarise(Count = sum(value, na.rm = T))%>%
   mutate(TotBins=TotBins$Freq
          [match(`Site Name`,TotBins$Var1)],
          Freq=Count/TotBins,
          Ind=Human_indicators$Indicator
          [match(variable,Human_indicators$`Group (Taxa)`)],
          Short=ForLegend$Abbreviation
          [match(variable,ForLegend$Indicator)])%>%
   ggplot(aes(x=Short,y=Freq, fill=Ind))+
   geom_bar(stat = "identity")+
   facet_wrap(~`Site Name`,ncol = 2)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   scale_fill_discrete(guide=FALSE)+
    labs(x="Indicator",
         y="Relative frequency")+
   ggtitle("Proportion of number of bins indicators found/ number of bins covered , per site (Ecuador)")
 
 g_1 <- grid.force(ggplotGrob(p))
 
 # add table(from fig 6) and legend extracted from FIG.2, legend from plot 5b_Col
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .55)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.55)
 vplegI<- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 #### FIGURE 6b_Venezuela----------------------------------------
 ## split plot per countries
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                      mutate(Country=Datasheet_sh.full$Country)%>%
                      filter(Country== "Venezuela")%>%
                      select(-c("Latitude"))) %>%
   group_by(`Site Name`,variable) %>%
   summarise(Count = sum(value, na.rm = T))%>%
   mutate(TotBins=TotBins$Freq
          [match(`Site Name`,TotBins$Var1)],
          Freq=Count/TotBins,
          Ind=Human_indicators$Indicator
          [match(variable,Human_indicators$`Group (Taxa)`)],
          Short=ForLegend$Abbreviation
          [match(variable,ForLegend$Indicator)])%>%
   ggplot(aes(x=Short,y=Freq, fill=Ind))+
   geom_bar(stat = "identity")+
   facet_wrap(~`Site Name`)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   scale_fill_discrete(guide=FALSE)+
    labs(x="Indicator",
         y="Relative frequency",
         fill="Indicator")+
   ggtitle("Proportion of number of bins indicators found/ number of bins covered , per site (Venezuela)") 
 
 g_1 <- grid.force(ggplotGrob(p))
 
 # add table(from fig 6) and legend extracted from FIG.2, legend from plot 5b_Col
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .55)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.55)
 vplegI<- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 
 # FIGURE 6b_under 2k mt-------------------------------------------------
 
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                      mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                      filter(Altitude < 2000)%>%
                      select(-c("Latitude", "Altitude"))) %>%
   group_by(`Site Name`,variable) %>%
   summarise(Count = sum(value, na.rm = T))%>%
   mutate(TotBins=TotBins$Freq
          [match(`Site Name`,TotBins$Var1)],
          Freq=Count/TotBins,
          Ind=Human_indicators$Indicator
          [match(variable,Human_indicators$`Group (Taxa)`)],
          Short=ForLegend$Abbreviation
          [match(variable,ForLegend$Indicator)])%>%
   ggplot(aes(x=Short,y=Freq, fill=Ind))+
   geom_bar(stat = "identity")+
   facet_wrap(~`Site Name`)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   scale_fill_discrete(guide=FALSE)+
    labs(x="Indicator",
         y="Relative frequency",
         fill="Indicator")+ 
   ggtitle( "Proportion of number of bins indicators found/ number of bins covered, per site (between 1k and 2k masl)")
 
 g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
 
 grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
 grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
 grobs_df$gPath_full <- gsub(pattern = "layout::", 
                             replacement = "", 
                             x = grobs_df$gPath_full, 
                             fixed = TRUE)
 
 strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                             x = grobs_df$gPath_full)]
 strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                              x = grobs_df$gPath_full)]
 
 fills <-  c( rep("#BDBDBD",5)) # vector of colors to fill rectangles
 
 txt_colors <- c(rep("black",5)) # vector of colors for text
 
 for (i in 1:length(strip_bg_gpath)){
   g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
   g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
 } # Edit the grobs

 
 # add table(from fig 6), legend extracted from FIG.2, legend fron fig 05b_2k
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .6)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
 vplegC <- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)
 vplegI<- viewport(width = 0.1, height = 0.1, x = 0.5, y = 0.1)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 upViewport(0)
 pushViewport(vplegC)
 grid.draw(legend)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 # FIGURE 6b_between 2 and 3k mt-------------------------------------------------
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                      mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                      filter(Altitude %in% 2000:3000)%>%
                      select(-c("Latitude", "Altitude"))) %>%
   group_by(`Site Name`,variable) %>%
   summarise(Count = sum(value, na.rm = T))%>%
   mutate(TotBins=TotBins$Freq
          [match(`Site Name`,TotBins$Var1)],
          Freq=Count/TotBins,
          Ind=Human_indicators$Indicator
          [match(variable,Human_indicators$`Group (Taxa)`)],
          Short=ForLegend$Abbreviation
          [match(variable,ForLegend$Indicator)])%>%
   ggplot(aes(x=Short,y=Freq, fill=Ind))+
   geom_bar(stat = "identity")+
   facet_wrap(~`Site Name`)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   scale_fill_discrete(guide=FALSE)+
   labs(x="Indicator",
        y="Relative frequency",
        fill="Indicator")+ 
   ggtitle( "Proportion of number of bins indicators found/ number of bins covered, per site (between 2k and 3k masl)")
 
 
 g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
 grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
 grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
 grobs_df$gPath_full <- gsub(pattern = "layout::", 
                             replacement = "", 
                             x = grobs_df$gPath_full, 
                             fixed = TRUE)
 
 strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                             x = grobs_df$gPath_full)]
 strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                              x = grobs_df$gPath_full)]
 
 fills <-  c(rep ("#636363",6) ,rep("#BDBDBD",2),rep ("#636363",1)) # vector of colors to fill rectangles
 
 txt_colors <- c(rep("white",6),rep("black",2), rep("white",1)) # vector of colors for text
 
 for (i in 1:length(strip_bg_gpath)){
   g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
   g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
 } # Edit the grobs
 
 # add table(from fig 6), legend extracted from FIG.2, legend extracted from 05b_2k
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .6)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
 vplegC <- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)
 vplegI<- viewport(width = 0.1, height = 0.1, x = 0.5, y = 0.1)
 
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 upViewport(0)
 pushViewport(vplegC)
 grid.draw(legend)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 # FIGURE 6b_between 3 and 4k mt-------------------------------------------------
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                      mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                      filter(Altitude %in% 3000:4215)%>%
                      select(-c("Latitude", "Altitude"))) %>%
   group_by(`Site Name`,variable) %>%
   summarise(Count = sum(value, na.rm = T))%>%
   mutate(TotBins=TotBins$Freq
          [match(`Site Name`,TotBins$Var1)],
          Freq=Count/TotBins,
          Ind=Human_indicators$Indicator
          [match(variable,Human_indicators$`Group (Taxa)`)],
          Short=ForLegend$Abbreviation
          [match(variable,ForLegend$Indicator)])%>%
   ggplot(aes(x=Short,y=Freq, fill=Ind))+
   geom_bar(stat = "identity")+
   facet_wrap(~`Site Name`)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   scale_fill_discrete(guide=FALSE)+
   labs(x="Indicator",
        y="Relative frequency",
        fill="Indicator")+ 
   ggtitle( "Proportion of number of bins indicators found/ number of bins covered, per site (between 3k and 4215 masl)")
 
 
 g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
 grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
 grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
 grobs_df$gPath_full <- gsub(pattern = "layout::", 
                             replacement = "", 
                             x = grobs_df$gPath_full, 
                             fixed = TRUE)
 
 strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                             x = grobs_df$gPath_full)]
 strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                              x = grobs_df$gPath_full)]
 
 fills <-  c(rep ("#636363",14) ,rep("#BDBDBD",4),
             rep ("#636363",1),rep( "#F0F0F0",2),
             rep ("#BDBDBD",3)) # vector of colors to fill rectangles
 
 txt_colors <- c(rep("white",14),
                 rep("black",4), 
                 rep("white",1), rep("black",5)) # vector of colors for text
 
 for (i in 1:length(strip_bg_gpath)){
   g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
   g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
 } # Edit the grobs
 
 # add table(from fig 6), legend extracted from FIG.2, legend extracted from this plot
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .6)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
 vplegC <- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)
 vplegI<- viewport(width = 0.1, height = 0.1, x = 0.5, y = 0.1)
 
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 
 upViewport(0)
 pushViewport(vplegC)
 grid.draw(legend)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 

#### FIGURE 7 (discarded, hard to read) ---------------------------------
## in which sites are which indicators found and how often? 

dat<-reshape2::melt(Datasheet_sh.ind.full%>%
                 select(-c("Latitude"))) %>%
  group_by(variable,`Site Name`) %>%
  summarise(Count = sum(value, na.rm = T)) %>%
  mutate (Country=Datasheet_sh.full$Country
          [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
          Latitude=Datasheet_sh.full$Latitude
          [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
        `Site Num` = rep(1:38,1))
 
   dat$`Site Num`= factor(dat$`Site Num`)
   dat$`Site Num`=fct_reorder(dat$`Site Num`,
                -dat$Latitude)
 
 
p<- dat%>%
 ggplot(aes(x=`Site Num`, y=Count, fill= Country))+
  scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
  geom_bar(stat = "identity",colour="black")+
  facet_wrap(~variable)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  labs(x="Sites")+
  ggtitle("Number of times Indicators are found per Site")

g_1 <- grid.force(ggplotGrob(p)) 
 
# create table as legend for the used abbreviations
ForLegend2<-ToTable%>%select("Site Name")%>% rownames_to_column()
colnames(ForLegend2)[1]<-"Abbreviation"

my_table2 <- tableGrob( ForLegend2 ,theme = ttheme_default (base_size = 7, padding = unit(c(4,2),"mm")),
                       rows=NULL)

my_table2$widths <- unit(rep(1/ncol(my_table2), ncol(my_table2)), "npc")



#do the same as fig 6, with plot and table from fig 7 and legend from fig 2
grid.newpage()
vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
vpleg <- viewport(width = 0.22, height = 1, x = 0.87, y = 0.5)
vplegC <- viewport(width = 0.2, height = 0.2, x = 0.8, y = 0.95)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vpleg)
grid.draw(my_table2)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

#### FIGURE 7 _Direct---------------------------------
## in which sites are Direct indicators found and how often? 
Datasheet_sh.full$Cluster.id_2<- Datasheet_shaved$cluster.id_2
Datasheet_sh.full$Cluster.col_2<- Datasheet_shaved$cluster.color_2

dat<-reshape2::melt(Datasheet_sh.ind.full%>%
                      select(-c("Latitude"))) %>%
  group_by(variable,`Site Name`) %>%
  summarise(Count = sum(value, na.rm = T)) %>%
  mutate (Country=Datasheet_sh.full$Country
          [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
          Latitude=Datasheet_sh.full$Latitude
          [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
          cluster.id=Datasheet_sh.full$Cluster.id_2
          [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
          cluster.col=Datasheet_sh.full$Cluster.col_2
          [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
          `Site Num` = rep(1:38,1))

dat$`Site Num`= factor(dat$`Site Num`)
dat$`Site Num`=fct_reorder(dat$`Site Num`,
                           -dat$Latitude)


p<-dat %>%
   mutate (Ind=Human_indicators$Indicator
                  [match(variable,Human_indicators$Taxa)])%>%
   filter(Ind=="Direct" & variable != "Zea mays") %>%
   ggplot(aes(x=reorder(`Site Num`,desc(`Site Num`)), y=Count, fill= cluster.id))+
   scale_fill_manual(values = Color_legend_cluster, guide=F)+
   geom_bar(stat = "identity",colour="black")+
  coord_flip()+
   facet_wrap(~variable, nrow = 1)+
   theme_classic()+
   theme(axis.text.x = element_text( hjust=1))+
  labs(x="Record",
       y="Number of time bins")

ggsave("figures/To use/07_Direct(1).pdf",p,
       units = "cm", width = 25, height = 20, dpi = 600)

p<-dat %>%
  mutate (Ind=Human_indicators$Indicator
          [match(variable,Human_indicators$Taxa)])%>%
  filter(variable == "Zea mays") %>%
  ggplot(aes(x=reorder(`Site Num`,desc(`Site Num`)), y=Count, fill= cluster.id))+
  scale_fill_manual( values = Color_legend_cluster, guide=F)+
  geom_bar(stat = "identity",colour="black")+
  coord_flip()+
  facet_wrap(~variable, nrow = 1)+
  theme_classic()+
  theme(axis.text.x = element_text( hjust=1))+
  labs(x="Record",
       y="Number of time bins")

ggsave("figures/To use/07_Direct(2).pdf",p,
       units = "cm", width = 25, height = 20, dpi = 600)

# create table as legend for the used abbreviations
# subset df
ForLegend2<-Datasheet_shaved%>%
  distinct(`Site Name`,Altitude,Latitude)

# order df according to Country
ForLegend2<-ForLegend2[order(-ForLegend2$Latitude),]

ForLegend2<-ForLegend2%>%select(c("Site Name", "Altitude"))%>% rownames_to_column()
colnames(ForLegend2)[1]<-"Abbreviation"
colnames(ForLegend2)[3]<-"masl"

my_table2 <- tableGrob( ForLegend2 ,theme = ttheme_default (base_size = 7, padding = unit(c(4,2),"mm")),
                        rows=NULL)

my_table2$widths <- unit(rep(1/ncol(my_table2), ncol(my_table2)), "npc")


#do the same as fig 6, with plot and table from fig 7 and legend from fig 2
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .6)
vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegC <- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vpleg)
grid.draw(my_table2)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

#### FIGURE 7 _Indirect---------------------------------
## in which sites are Direct indicators found and how often? 

p<-dat %>%
  mutate (Ind=Human_indicators$Indicator
          [match(variable,Human_indicators$Taxa)])%>%
   filter(Ind=="Indirect") %>%
   ggplot(aes(x=`Site Num`, y=Count, fill= cluster.id))+
   scale_fill_manual(values = Color_legend_cluster, guide=F)+
   geom_bar(stat = "identity",color="gray30")+
   facet_wrap(~variable)+
  coord_flip()+
   theme_classic()+
  labs(x="Record",
       y="Number of time bins")+
   theme(axis.text.x = element_text(hjust=1))

ggsave("figures/To use/07_Indirect.pdf",p,
       units = "cm", width = 25, height = 20, dpi = 600)



#do the same as fig 6, with plot and table from fig 7 and legend from fig 2
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .6)
vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegC <- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vpleg)
grid.draw(my_table2)


upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

#### FIGURE 7 _High/ Low---------------------------------
## in which sites are indicators with food potential found and how often? 

p<- dat%>%
  mutate (Pot=Human_indicators$PotentionalFoodSource
          [match(variable,Human_indicators$Taxa)],
          Ind=Human_indicators$Indicator
          [match(variable,Human_indicators$Taxa)],
          var_di= ifelse(Ind=="Direct", paste(variable, "*"), paste(variable)),
          Ind_ord=ifelse(Pot=="high",1,2))%>%
   filter(Pot %in% c("high", "low")) %>%
   ggplot(aes(x=reorder(`Site Num`,desc(`Site Num`)), y=Count, fill= cluster.id))+
   scale_fill_manual(values = Color_legend_cluster, guide=F)+
   geom_bar(stat = "identity",colour="black")+
  coord_flip()+
   facet_wrap(~reorder(var_di,Ind_ord), nrow =2)+
   theme_classic()+
   theme(axis.text.x = element_text(hjust=1))+
  labs(x="Record",
       y="Number of time bins")

g_1 <- grid.force(ggplotGrob(p)) 


grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)


strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                            x = grobs_df$gPath_full)]

strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                             x = grobs_df$gPath_full)]



fills <-  c(rep("#053061",3),
            rep("#2166AC",7),
            rep("#053061",10)) # vector of colors to fill rectangles
            

txt_colors <- c(rep("white",20))


for (i in 1:length(strip_bg_gpath)){
   g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
   g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
} # Edit the grobs


#do the same as fig 6, with plot and table from fig 7 and legend for countries from fig 2, legend for indicators from fig 13b_Col
grid.newpage()

grid.draw(g_1)

vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .6)
vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegC <- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)
vplegI<- viewport(width = 0.1, height = 0.1, x = 0.5, y = 0.1)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_hl)



#### FIGURE 7 _No---------------------------------
## in which sites are  indicators with no food pot found and how often? 

p<-dat%>%
  mutate (Pot=Human_indicators$PotentionalFoodSource
          [match(variable,Human_indicators$Taxa)])%>%
   filter(Pot=="no") %>%
   ggplot(aes(x=`Site Num`, y=Count, fill= Country))+
   scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
   geom_bar(stat = "identity",colour="black")+
   facet_wrap(~variable)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
  labs(x="Site")+
   ggtitle("Number of bins indicators with no food potential are found, per site")

g_1 <- grid.force(ggplotGrob(p)) 


#do the same as fig 6, with plot and table from fig 7 and legend for countries from fig 2
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .6)
vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegC <- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)


upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vpleg)
grid.draw(my_table2)


upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

#### FIGURE 7_disturbance---------------------------------------
Datasheet_sh.ind.full2<-Datasheet_sh.ind.full%>% 
  mutate(Charcoal=Datasheet_sh_charcoal$Charcoal)

Datasheet_sh.ind.full2$Charcoal<-as.numeric(Datasheet_sh.ind.full2$Charcoal)

dat2<-reshape2::melt(Datasheet_sh.ind.full2 %>%
  select(-c("Latitude"))) %>%
  group_by(variable,`Site Name`) %>%
  summarise(Count = sum(value, na.rm = T)) %>%
  mutate (Country=Datasheet_sh.full$Country
          [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
          Latitude=Datasheet_sh.full$Latitude
          [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
          cluster.id=Datasheet_sh.full$Cluster.id_2
          [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
          cluster.col=Datasheet_sh.full$Cluster.col_2
          [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
          `Site Num` = rep(1:38,1))

dat2$`Site Num`= factor(dat2$`Site Num`)
dat2$`Site Num`=fct_reorder(dat2$`Site Num`,
                           -dat2$Latitude)

dat2 %>%
  mutate (Ind=Human_indicators2$Disturbance
          [match(variable,Human_indicators2$`Group (Taxa)`)])%>%
  filter(Ind=="Yes") %>%
  ggplot(aes(x=reorder(`Site Num`,desc(`Site Num`)), y=Count, fill= cluster.id))+
  scale_fill_manual(values = Color_legend_cluster, guide=F)+
  geom_bar(stat = "identity",colour="black")+
  coord_flip()+
  facet_wrap(~variable)+
  theme_classic()+
  theme(axis.text.x = element_text( hjust=1))+
  labs(x="Record",
       y="Number of time bins")

#### FIGURE 7b (discarded, hard to read)------------------------------------------
## relative freq
p<-dat%>%
  mutate(TotBins=TotBins$Freq
         [match(`Site Name`,TotBins$Var1)],
         Freq=Count/TotBins)%>%
  ggplot(aes(x=`Site Num`, y=Freq, fill= Country))+
 scale_x_discrete(label=function(x) abbreviate(x, 2))+
  scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
  geom_bar(stat = "identity",colour="black")+
  facet_wrap(~variable)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ggtitle("Relative frequency of indicators",
          subtitle = "For each site, percentage of bins where each indicator is found")

g_1 <- grid.force(ggplotGrob(p)) 



# do the same as fig 6, with plot and table from fig 7 and legend from fig 2
grid.newpage()
vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
vpleg <- viewport(width = 0.22, height = 1, x = 0.87, y = 0.5)
vplegC <- viewport(width = 0.2, height = 0.2, x = 0.8, y = 0.95)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vpleg)
grid.draw(my_table2)


upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

#### FIGURE 7b_Direct------------------------------------------
## relative freq
p<-dat%>%
  mutate(TotBins=TotBins$Freq
         [match(`Site Name`,TotBins$Var1)],
         Freq=Count/TotBins,
         Ind=Human_indicators$Indicator
         [match(variable,Human_indicators$`Group (Taxa)`)])%>%
   filter(Ind=="Direct") %>%
  ggplot(aes(x=`Site Num`, y=Freq, fill= Country))+
   scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
   geom_bar(stat = "identity",colour="black")+
   facet_wrap(~variable)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
  labs(x="Site",
       y="Relative frequency")+
   ggtitle("For each site, percentage of bins where  direct indicators are found")

g_1 <- grid.force(ggplotGrob(p)) 



# do the same as fig 6, with plot and table from fig 7 and legend from fig 2
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .6)
vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegC <- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vpleg)
grid.draw(my_table2)


upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

#### FIGURE 7b_Indirect------------------------------------------
## relative freq
p<-dat%>%
  mutate(TotBins=TotBins$Freq
         [match(`Site Name`,TotBins$Var1)],
         Freq=Count/TotBins,
         Ind=Human_indicators$Indicator
         [match(variable,Human_indicators$`Group (Taxa)`)])%>%
   filter(Ind=="Indirect") %>%
   ggplot(aes(x=`Site Num`, y=Freq, fill= Country))+
   scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
   geom_bar(stat = "identity",colour="black")+
   facet_wrap(~variable)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
  labs(x="Site",
       y="Relative frequency")+
   ggtitle("For each site, percentage of bins where indirect indicators are found")

g_1 <- grid.force(ggplotGrob(p)) 



# do the same as fig 6, with plot and table from fig 7 and legend from fig 2
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .6)
vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegC <- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vpleg)
grid.draw(my_table2)


upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

#### FIGURE 7b_H/L------------------------------------------
## relative freq
p<-dat%>%
  mutate(TotBins=TotBins$Freq
         [match(`Site Name`,TotBins$Var1)],
         Freq=Count/TotBins,
         Ind=Human_indicators$Indicator
         [match(variable,Human_indicators$`Group (Taxa)`)],
          Pot=Human_indicators$`Potential food source`
          [match(variable,Human_indicators$`Group (Taxa)`)])%>%
   filter(Pot %in% c("high", "low")) %>%
   ggplot(aes(x=`Site Num`, y=Freq, fill= Country))+
   scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
   geom_bar(stat = "identity",colour="black")+
   facet_wrap(~variable, ncol=2)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
  labs(x="Site",
       y="Relative frequency")+
   ggtitle("For each site, percentage of bins where indicators with food potential are found")

g_1 <- grid.force(ggplotGrob(p)) 

grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)

strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                            x = grobs_df$gPath_full)]


fills <-  c(rep("#F8766D",5), # vector of colors to fill rectangles
            rep("#2166AC",1),
            rep("#F8766D",2),
            rep( "#2166AC",1),
            rep( "#F8766D",2),
            rep( "#2166AC",2),
            rep("#F8766D",1),
            rep("#2166AC",2),
            rep("#F8766D",3),
            "#2166AC") 


for (i in 1:length(strip_bg_gpath)){
   g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
} # Edit the grobs

plot(g_1)


#### FIGURE 7b_no------------------------------------------
## relative freq
p<-dat%>%
  mutate(TotBins=TotBins$Freq
         [match(`Site Name`,TotBins$Var1)],
         Freq=Count/TotBins,
         Ind=Human_indicators$Indicator
         [match(variable,Human_indicators$`Group (Taxa)`)],
         Pot=Human_indicators$`Potential food source`
         [match(variable,Human_indicators$`Group (Taxa)`)])%>%
  filter(Pot=="no") %>%
   ggplot(aes(x=`Site Num`, y=Freq, fill= Country))+
   scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
   geom_bar(stat = "identity",colour="black")+
   facet_wrap(~variable)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
  labs(x="Site",
       y="Relative frequency")+
   ggtitle("For each site, percentage of bins where indicators with no food potential are found")

g_1 <- grid.force(ggplotGrob(p)) 



# do the same as fig 6, with plot and table from fig 7 and legend from fig 2
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .6)
vpleg <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegC <- viewport(width = 0.1, height = 0.1, x = 0.4, y = 0.1)


upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vpleg)
grid.draw(my_table2)


upViewport(0)
pushViewport(vplegC)
grid.draw(legend)


#### Analysis of direct/indirect indicators-------
 
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
Datasheet_sh.full<- bind_cols(Datasheet_sh.full,DF_sh.Direct.Sum)
Datasheet_sh.full<-rename(Datasheet_sh.full,"Sum_direct"="SUM")
Datasheet_sh.full<- bind_cols(Datasheet_sh.full,DF_sh.Indirect.Sum)
Datasheet_sh.full<-rename(Datasheet_sh.full,"Sum_indirect"="SUM")

### same for disturbance indicators------------------------------------
Disturbance<-Human_indicators2 %>% 
  filter(Disturbance=="Yes") %>% 
  select(`Group (Taxa)`)

Disturbance<-as.vector(Disturbance$`Group (Taxa)`)

Datasheet_sh.indicators2<-Datasheet_sh.indicators%>%
  mutate(Charcoal=Datasheet_sh_charcoal$Charcoal)

DF_sh.Disturbance<-Datasheet_sh.indicators2[,names(Datasheet_sh.indicators2) %in% Disturbance]

DF_sh.Disturbance$Charcoal<-as.numeric(DF_sh.Disturbance$Charcoal)

DF_sh.Disturbance.Sum<-data.frame(SUM=apply(DF_sh.Disturbance,1,FUN = function(x) sum(x,na.rm = T)))

Datasheet_sh.dist.full<- bind_cols(Datasheet_shaved,
                                  DF_sh.High.Sum,
                                  DF_sh.Low.Sum,
                                  DF_sh.No.Sum,
                                  Datasheet_sh.full%>%
                                    select(ForLabs))
#add altitude to site name
Datasheet_sh.pot.full$`Site Name`<-paste(Datasheet_sh.pot.full$`Site Name`,
                                         "(",
                                         Datasheet_sh.pot.full$Altitude,
                                         "masl",
                                         ")")

# order sites by lat
Datasheet_sh.pot.full$`Site Name`<-factor(Datasheet_sh.pot.full$`Site Name`)
Datasheet_sh.pot.full$`Site Name`<-fct_reorder(Datasheet_sh.pot.full$`Site Name`,
                                               -Datasheet_sh.full$Latitude)  
#### analyses of potential food sources---------------

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
                                  DF_sh.No.Sum,
                                  Datasheet_sh.full%>%
                                    select(ForLabs))
#add altitude to site name
Datasheet_sh.pot.full$`Site Name`<-paste(Datasheet_sh.pot.full$`Site Name`,
                                         "(",
                                         Datasheet_sh.pot.full$Altitude,
                                         "masl",
                                         ")")

# order sites by lat
Datasheet_sh.pot.full$`Site Name`<-factor(Datasheet_sh.pot.full$`Site Name`)
Datasheet_sh.pot.full$`Site Name`<-fct_reorder(Datasheet_sh.pot.full$`Site Name`,
                                           -Datasheet_sh.full$Latitude)    



#### FIGURE 8b---------------------------------------------------------
##same, without numbers and with time span of the records
p<-Datasheet_sh.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_direct)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=4),
             col=ifelse(Datasheet_sh.full$ForLabs=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.full$ForLabs=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of direct indicators through time per site")

g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)

strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                            x = grobs_df$gPath_full)]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                             x = grobs_df$gPath_full)]

fills <-  c(rep("#636363",17), # vector of colors to fill rectangles
            rep("#BDBDBD",2),
            rep("#636363",5),
            rep( "#BDBDBD",7),
            rep( "#F0F0F0",2),
            rep( "#BDBDBD",5)) 

txt_colors <- c(rep("white",17), # vector of colors for text
                rep("black",2),
                rep("white",5),
                rep( "black",7),
                rep( "black",2),
                rep( "black",5))

for (i in 1:length(strip_bg_gpath)){
  g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
  g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
} # Edit the grobs

#create plot to extract legend
df.random<-data.frame(Lenght=c("Hiatus","Record"), 
                      x=c(1,2),
                      y=2)
random<-ggplot(df.random)+
  geom_point(aes(x=x,y=y,col=Lenght), pch=25,cex=0.7 )+
  scale_color_manual(values = c("orange", "black"))
                     
# extract legend from plot
legend_l<-get_legend(random)

# add legend(extracted from FIG.2 and from previous plot)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegC <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)


upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

#### FIGURE 9b---------------------------------------------------------
##same, without numbers and with time span of the records
p<-Datasheet_sh.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_indirect)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
             col=ifelse(Datasheet_sh.full$ForLabs=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.full$ForLabs=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indirect indicators through time per site")

g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)

strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                            x = grobs_df$gPath_full)]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                             x = grobs_df$gPath_full)]

fills <-  c(rep("#636363",17), # vector of colors to fill rectangles
            rep("#BDBDBD",2),
            rep("#636363",5),
            rep( "#BDBDBD",7),
            rep( "#F0F0F0",2),
            rep( "#BDBDBD",5)) 

txt_colors <- c(rep("white",17), # vector of colors for text
                rep("black",2),
                rep("white",5),
                rep( "black",7),
                rep( "black",2),
                rep( "black",5))

for (i in 1:length(strip_bg_gpath)){
  g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
  g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
} # Edit the grobs

# add legend(extracted from FIG.2 and 8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegC <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)


upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)


#### FIGURE 10 ----------------------------------
#Absolute frequency of high food pot indicators through time per site
p<-Datasheet_sh.pot.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_high)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
             col=ifelse(Datasheet_sh.full$ForLabs=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.full$ForLabs=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators with high food potential through time per site")

g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)

strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                            x = grobs_df$gPath_full)]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                             x = grobs_df$gPath_full)]

fills <-  c(rep("#636363",17), # vector of colors to fill rectangles
            rep("#BDBDBD",2),
            rep("#636363",5),
            rep( "#BDBDBD",7),
            rep( "#F0F0F0",2),
            rep( "#BDBDBD",5)) 

txt_colors <- c(rep("white",17), # vector of colors for text
                rep("black",2),
                rep("white",5),
                rep( "black",7),
                rep( "black",2),
                rep( "black",5))

for (i in 1:length(strip_bg_gpath)){
  g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
  g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
} # Edit the grobs

# add legend(extracted from FIG.2 and 8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegC <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

#### FIGURE 11---------------------------------------
#Absolute frequency of low food pot indicators through time per site

p<-Datasheet_sh.pot.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_low)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
             col=ifelse(Datasheet_sh.full$ForLabs=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.full$ForLabs=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators with low food potential through time per site")

g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)

strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                            x = grobs_df$gPath_full)]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                             x = grobs_df$gPath_full)]

fills <-  c(rep("#636363",17), # vector of colors to fill rectangles
            rep("#BDBDBD",2),
            rep("#636363",5),
            rep( "#BDBDBD",7),
            rep( "#F0F0F0",2),
            rep( "#BDBDBD",5)) 

txt_colors <- c(rep("white",17), # vector of colors for text
                rep("black",2),
                rep("white",5),
                rep( "black",7),
                rep( "black",2),
                rep( "black",5))

for (i in 1:length(strip_bg_gpath)){
  g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
  g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
} # Edit the grobs

# add legend(extracted from FIG.2 and  8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegC <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

# FIGURE 13-------------------------------------------------
# new df
DF_sh.Direct.Sum1<-DF_sh.Direct.Sum%>%
  mutate(IND=rep("Direct"))

DF_sh.Indirect.Sum1 <- DF_sh.Indirect.Sum %>% 
  mutate(IND=rep("Indirect"))

Datasheet_sh.full.long<-rbind(bind_cols(Datasheet_shaved,
                                        DF_sh.Indirect.Sum1,
                                        Datasheet_sh.full%>% select(ForLabs)),
                              bind_cols(Datasheet_shaved,
                                        DF_sh.Direct.Sum1,
                                        Datasheet_sh.full%>% select(ForLabs)))

#add altitude to site name
Datasheet_sh.full.long$`Site Name`<-paste(Datasheet_sh.full.long$`Site Name`,
                                         "(",
                                         Datasheet_sh.full.long$Altitude,
                                         "masl",
                                         ")")

# sites ordered according to lat, to sort facets in plot
Datasheet_sh.full.long$`Site Name`<-factor(Datasheet_sh.full.long$`Site Name`)
Datasheet_sh.full.long$`Site Name`<-fct_reorder(Datasheet_sh.full.long$`Site Name`,
                                                -Datasheet_sh.full.long$Latitude)

#order levels of IND to show Indirect ind on the top of direct (in the graph)
Datasheet_sh.full.long$IND<-factor(Datasheet_sh.full.long$IND, 
                                   levels = c("Indirect","Direct"))


#stacked bar chart, filled bars (easier to read)
my_col_2<-c("#FFFFFF","#DC143C")

p<-ggplot(data=Datasheet_sh.full.long, 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1, color="gray30")+
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
             col=ifelse(Datasheet_sh.full.long$ForLabs=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.full.long$ForLabs=="H","orange","black"), cex=0.7)+
  scale_fill_manual(values = my_col_2,guide=F)+
  facet_wrap(~`Site Name`)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")

# don't have enough RAM for this

#p <- ggplot_gtable(ggplot_build(p))
#stripr <- which(grepl('strip-t', p$layout$name))

#for (i in stripr) {
 # if (class(p$grobs[[i]]) != "zeroGrob"){
  #  j <- which(grepl('rect', p$grobs[[i]]$grobs[[1]]$childrenOrder))
   # k <- which(grepl('title', p$grobs[[i]]$grobs[[1]]$childrenOrder))
    
    #p$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- Sites %>% 
     # filter(`Site Name` == p$grobs[[i]]$grobs[[1]]$children[[k]]$children[[1]]$label) %>%
      #dplyr::select(cluster.color_2) %>%
      #pluck(1) %>%
      #as.character()
  #}
#}

#plot(p)

#ggsave("figures/To use/13.pdf",p,
      # units = "cm", width = 25, height = 20, dpi = 600)

g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)

strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                            x = grobs_df$gPath_full)]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                             x = grobs_df$gPath_full)]

fills <-  c(rep("#E78AC3",17), # vector of colors to fill rectangles
            rep("#8DA0CB",1),
            rep("#E78AC3",6),
            rep("#FC8D62",3),
            rep( "#8DA0CB",4),
            rep( "#66C2A5",2),
            rep("#FC8D62",5)) 


for (i in 1:length(strip_bg_gpath)){
  g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
} # Edit the grobs

# add legend(extracted from FIG.2 5b_Col and  8b)
grid.newpage()
plot_grid(g_1)


vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegC <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.4)
vplegI<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_di)

# with disturbance indicators
Datasheet_sh.full.long2<-Datasheet_sh.full.long2%>% 
  mutate(ForLabs=Datasheet_sh.full$ForLabs)
Datasheet_sh.full.long2$`Site Name`<-fct_reorder(Datasheet_sh.full.long2$`Site Name`,
                                                 -Datasheet_sh.full.long2$Latitude)

p<-ggplot(data=Datasheet_sh.full.long2,
          aes(x = reorder(Bin_num,-Bin_num),y=SUM))+
  geom_bar(stat = "identity",width = 1, color="gray30")+
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
             col=ifelse(data$ForLabs=="H","orange","black"), pch=25 ,
             bg=ifelse(data$ForLabs=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")

p <- ggplot_gtable(ggplot_build(p))
stripr <- which(grepl('strip-t', p$layout$name))

for (i in stripr) {
 if (class(p$grobs[[i]]) != "zeroGrob"){
  j <- which(grepl('rect', p$grobs[[i]]$grobs[[1]]$childrenOrder))
 k <- which(grepl('title', p$grobs[[i]]$grobs[[1]]$childrenOrder))

p$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- Sites %>% 
 filter(`Site Name` == p$grobs[[i]]$grobs[[1]]$children[[k]]$children[[1]]$label) %>%
dplyr::select(cluster.color_2) %>%
pluck(1) %>%
as.character()
}
}

plot(p)

#ggsave("figures/To use/13.pdf",p,
# units = "cm", width = 25, height = 20, dpi = 600)

#### FIGURE 13_Col-----------------------------------------

p<-ggplot(data=Datasheet_sh.full.long%>%
         filter(Country=="Colombia"), 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), guide=F)+
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
             col=ifelse(Datasheet_sh.full.long%>%
                          filter(Country=="Colombia")%>%
                          select(ForLabs)=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.full.long%>%
                         filter(Country=="Colombia")%>%
                         select(ForLabs)=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Cumulated number of direct and indirect indicators per bin, per site (Colombia)")

g_1 <- grid.force(ggplotGrob(p))

# add legend(extracted from FIG.2 and  8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegI <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_di)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

#### FIGURE 13_Ven ------------------------------------------

p<-ggplot(data=Datasheet_sh.full.long%>%
         filter(Country=="Venezuela"), 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), guide=F)+
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
             col=ifelse(Datasheet_sh.full.long%>%
                          filter(Country=="Venezuela")%>%
                          select(ForLabs)=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.full.long%>%
                         filter(Country=="Venezuela")%>%
                         select(ForLabs)=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Cumulated number of direct and indirect indicators per bin, per site (Venezuela)")

g_1 <- grid.force(ggplotGrob(p))

# add legend(extracted from FIG.2 and  8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegI <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_di)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

#### FIGURE 13_Ec ------------------------------------------

p<-ggplot(data=Datasheet_sh.full.long%>%
         filter(Country=="Ecuador"), 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), guide=F)+
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
             col=ifelse(Datasheet_sh.full.long%>%
                          filter(Country=="Ecuador")%>%
                          select(ForLabs)=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.full.long%>%
                         filter(Country=="Ecuador")%>%
                         select(ForLabs)=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Cumulated number of direct and indirect indicators per bin, per site (Ecuador)")

g_1 <- grid.force(ggplotGrob(p))

# add legend(extracted from FIG.2 and  8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegI <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_di)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

# FIGURE 13_under 2k mt-------------------------------------------------

p<-ggplot(data=Datasheet_sh.full.long%>%
            filter(Altitude < 2000), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), guide=F)+
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
             col=ifelse(Datasheet_sh.full.long%>%
                          filter(Altitude < 2000)%>%
                          select(ForLabs)=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.full.long%>%
                         filter(Altitude < 2000)%>%
                         select(ForLabs)=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Cumulated number of direct and indirect indicators per bin, per site (between 1000 and 2000 m of altitude)")

g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)

strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                            x = grobs_df$gPath_full)]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                             x = grobs_df$gPath_full)]

fills <-  c( rep("#BDBDBD",5)) # vector of colors to fill rectangles

txt_colors <- c(rep("black",5)) # vector of colors for text

for (i in 1:length(strip_bg_gpath)){
  g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
  g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
} # Edit the grobs


# add legend(extracted from FIG.2 5b_Col and  8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegC <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.4)
vplegI<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_di)

# FIGURE 13_between 2 and 3k mt-------------------------------------------------

p<-ggplot(data=Datasheet_sh.full.long%>%
            filter(Altitude %in% 2000:3000), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), guide=F)+
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
             col=ifelse(Datasheet_sh.full.long%>%
                          filter(Altitude %in% 2000:3000)%>%
                          select(ForLabs)=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.full.long%>%
                         filter(Altitude %in% 2000:3000)%>%
                         select(ForLabs)=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Cumulated number of direct and indirect indicators per bin, per site (between 2000 and 3000 m of altitude)")

g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)

strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                            x = grobs_df$gPath_full)]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                             x = grobs_df$gPath_full)]

fills <-  c(rep ("#636363",6) ,rep("#BDBDBD",2),rep ("#636363",1)) # vector of colors to fill rectangles

txt_colors <- c(rep("white",6),rep("black",2), rep("white",1)) # vector of colors for text

for (i in 1:length(strip_bg_gpath)){
  g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
  g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
} # Edit the grobs

# add legend(extracted from FIG.2 5b_Col and  8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegC <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.4)
vplegI<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_di)

# FIGURE 13_between 3 and 4k mt-------------------------------------------------

p<-ggplot(data=Datasheet_sh.full.long%>%
            filter(Altitude %in% 3000:4215), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), guide=F)+
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
             col=ifelse(Datasheet_sh.full.long%>%
                          filter(Altitude %in% 3000:4215)%>%
                          select(ForLabs)=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.full.long%>%
                         filter(Altitude %in% 3000:4215)%>%
                         select(ForLabs)=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Cumulated number of direct and indirect indicators per bin, per site (between 3000 and 4215 m of altitude)")


g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)

strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                            x = grobs_df$gPath_full)]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                             x = grobs_df$gPath_full)]

fills <-  c(rep ("#636363",14) ,rep("#BDBDBD",4),
            rep ("#636363",1),rep( "#F0F0F0",2),
            rep ("#BDBDBD",3)) # vector of colors to fill rectangles

txt_colors <- c(rep("white",14),
                rep("black",4), 
                rep("white",1), rep("black",5)) # vector of colors for text

for (i in 1:length(strip_bg_gpath)){
  g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
  g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
} # Edit the grobs

# add legend(extracted from FIG.2 5b_Col and  8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegC <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.4)
vplegI<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_di)

#### FIGURE 13b------------------------------------------
# comparison for food potential, stacked bars 

DF_sh.High.Sum1<-DF_sh.High.Sum%>%
  mutate(IND=rep("High"))
colnames(DF_sh.High.Sum1)[1]<-"SUM"

DF_sh.Low.Sum1 <- DF_sh.Low.Sum %>% 
  mutate(IND=rep("Low"))
colnames(DF_sh.Low.Sum1)[1]<-"SUM"

DF_sh.No.Sum1<- DF_sh.No.Sum %>% 
  mutate(IND=rep("No"))
colnames(DF_sh.No.Sum1)[1]<-"SUM"


Datasheet_sh.pot.full.long<-rbind(bind_cols(Datasheet_shaved,
                                            DF_sh.High.Sum1,
                                            Datasheet_sh.full%>% select(ForLabs)),
                              bind_cols(Datasheet_shaved,
                                        DF_sh.Low.Sum1,
                                        Datasheet_sh.full%>% select(ForLabs)),
                              bind_cols(Datasheet_shaved,
                                        DF_sh.No.Sum1,
                                        Datasheet_sh.full%>% select(ForLabs)))

# sites ordered according to lat, does not work in sorting facets in plot
Datasheet_sh.pot.full.long$`Site Name`<-factor(Datasheet_sh.pot.full.long$`Site Name`)
Datasheet_sh.pot.full.long$`Site Name`<-fct_reorder(Datasheet_sh.pot.full.long$`Site Name`,
                                                    -Datasheet_sh.pot.full.long$Latitude)

#plot
p<-ggplot(data=Datasheet_sh.pot.full.long, 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
             col=ifelse(Datasheet_sh.pot.full.long$ForLabs=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.pot.full.long$ForLabs=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  #scale_fill_discrete(breaks = c("High","Low"))+    #to produce legend for fig 7_H/L
  scale_fill_manual(values=my_colors_food,guide=F)+
  labs(fill="Indicator")+
  ggtitle("Cumulated number of different food potential indicators per bin")

p <- ggplot_gtable(ggplot_build(p))
stripr <- which(grepl('strip-t', p$layout$name))

for (i in stripr) {
  if (class(p$grobs[[i]]) != "zeroGrob"){
    j <- which(grepl('rect', p$grobs[[i]]$grobs[[1]]$childrenOrder))
    k <- which(grepl('title', p$grobs[[i]]$grobs[[1]]$childrenOrder))
    
    p$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- Sites %>% 
      filter(`Site Name` == p$grobs[[i]]$grobs[[1]]$children[[k]]$children[[1]]$label) %>%
      dplyr::select(cluster.color_2) %>%
      pluck(1) %>%
      as.character()
  }
}

plot(p)


g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title

# extract legend from plot
legend_hln<-get_legend(g_1)

#extract legend(use for fig 7b_H/L)
#legend_hl<-get_legend(g_1)

# add scale_fill_dscete( scale= F) to original plot p

grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)

strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                            x = grobs_df$gPath_full)]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                             x = grobs_df$gPath_full)]

fills <-  c(rep("#636363",17), # vector of colors to fill rectangles
            rep("#BDBDBD",2),
            rep("#636363",5),
            rep( "#BDBDBD",7),
            rep( "#F0F0F0",2),
            rep( "#BDBDBD",5)) 

txt_colors <- c(rep("white",17), # vector of colors for text
                rep("black",2),
                rep("white",5),
                rep( "black",7),
                rep( "black",2),
                rep( "black",5))

for (i in 1:length(strip_bg_gpath)){
  g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
  g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
} # Edit the grobs

# add legend(extracted from FIG.2, prev plot and  8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegC <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.4)
vplegI<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_hln)

### FIGURE 13b_Col---------------------------------

p<-ggplot(data=Datasheet_sh.pot.full.long%>%
         filter(Country=="Colombia"), 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_discrete(guide=F)+
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
             col=ifelse(Datasheet_sh.pot.full.long%>%
                          filter(Country=="Colombia")%>%
                          select(ForLabs)=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.pot.full.long%>%
                         filter(Country=="Colombia")%>%
                         select(ForLabs)=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Cumulated number different food potential indicators per bin, per site (Colombia)")

g_1 <- grid.force(ggplotGrob(p))

# add legend(extracted from FIG.2, prev plot and  8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.4)
vplegI<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_hln)

### FIGURE 13b_Ven---------------------------------

p<-ggplot(data=Datasheet_sh.pot.full.long%>%
            filter(Country=="Venezuela"), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_discrete(guide=F)+
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
             col=ifelse(Datasheet_sh.pot.full.long%>%
                          filter(Country=="Venezuela")%>%
                          select(ForLabs)=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.pot.full.long%>%
                         filter(Country=="Venezuela")%>%
                         select(ForLabs)=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Cumulated number of different food potential indicators per bin, per site (Venezuela)")

g_1 <- grid.force(ggplotGrob(p))

# add legend(extracted from FIG.2, prev plot and  8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.4)
vplegI<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_hln)


### FIGURE 13b_Ec---------------------------------

p<-ggplot(data=Datasheet_sh.pot.full.long%>%
            filter(Country=="Ecuador"), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_discrete(guide=F)+
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
             col=ifelse(Datasheet_sh.pot.full.long%>%
                          filter(Country=="Ecuador")%>%
                          select(ForLabs)=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.pot.full.long%>%
                         filter(Country=="Ecuador")%>%
                         select(ForLabs)=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Cumulated number different food potential indicators per bin, per site (Ecuador)")

g_1 <- grid.force(ggplotGrob(p))

# add legend(extracted from FIG.2, prev plot and  8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.4)
vplegI<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_hln)

# FIGURE 13b_under 2k mt-------------------------------------------------

p<-ggplot(data=Datasheet_sh.pot.full.long%>%
            filter(Altitude < 2000), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_discrete(guide=F)+
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
             col=ifelse(Datasheet_sh.pot.full.long%>%
                          filter(Altitude < 2000)%>%
                          select(ForLabs)=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.pot.full.long%>%
                         filter(Altitude < 2000)%>%
                         select(ForLabs)=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Cumulated number of direct and indirect indicators per bin, per site (between 1000 and 2000 m of altitude)")

g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)

strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                            x = grobs_df$gPath_full)]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                             x = grobs_df$gPath_full)]

fills <-  c( rep("#BDBDBD",5)) # vector of colors to fill rectangles

txt_colors <- c(rep("black",5)) # vector of colors for text

for (i in 1:length(strip_bg_gpath)){
  g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
  g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
} # Edit the grobs


# add legend(extracted from FIG.2 5b_Col and  8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegC <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.4)
vplegI<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_hln)

# FIGURE 13b_between 2 and 3k mt-------------------------------------------------

p<-ggplot(data=Datasheet_sh.pot.full.long%>%
            filter(Altitude %in% 2000:3000), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_discrete(guide=F)+
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
             col=ifelse(Datasheet_sh.pot.full.long%>%
                          filter(Altitude %in% 2000:3000)%>%
                          select(ForLabs)=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.pot.full.long%>%
                         filter(Altitude %in% 2000:3000)%>%
                         select(ForLabs)=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Cumulated number of direct and indirect indicators per bin, per site (between 2000 and 3000 m of altitude)")

g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)

strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                            x = grobs_df$gPath_full)]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                             x = grobs_df$gPath_full)]

fills <-  c(rep ("#636363",6) ,rep("#BDBDBD",2),rep ("#636363",1)) # vector of colors to fill rectangles

txt_colors <- c(rep("white",6),rep("black",2), rep("white",1)) # vector of colors for text

for (i in 1:length(strip_bg_gpath)){
  g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
  g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
} # Edit the grobs


# add legend(extracted from FIG.2 5b_Col and  8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegC <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.4)
vplegI<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_hln)

# FIGURE 13b_between 3 and 4k mt-------------------------------------------------

p<-ggplot(data=Datasheet_sh.pot.full.long%>%
            filter(Altitude %in% 3000:4215), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_discrete(guide=F)+
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=25),
             col=ifelse(Datasheet_sh.pot.full.long%>%
                          filter(Altitude %in% 3000:4215)%>%
                          select(ForLabs)=="H","orange","black"), pch=25 ,
             bg=ifelse(Datasheet_sh.pot.full.long%>%
                         filter(Altitude %in% 3000:4215)%>%
                         select(ForLabs)=="H","orange","black"), cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Cumulated number of direct and indirect indicators per bin, per site (between 3000 and 4215 m of altitude)")

g_1 <- grid.force(ggplotGrob(p))   #df to see the right path of text title
grobs_df <- do.call(cbind.data.frame, grid.ls(g_1, print = FALSE)) 
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::") # Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)

strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", # Get the gPaths of the strip background grob
                                            x = grobs_df$gPath_full)]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", # Get the gPaths of the strip titles
                                             x = grobs_df$gPath_full)]

fills <-  c(rep ("#636363",14) ,rep("#BDBDBD",4),
            rep ("#636363",1),rep( "#F0F0F0",2),
            rep ("#BDBDBD",3)) # vector of colors to fill rectangles

txt_colors <- c(rep("white",14),
                rep("black",4), 
                rep("white",1), rep("black",5)) # vector of colors for text

for (i in 1:length(strip_bg_gpath)){
  g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
  g_1 <- editGrob(grob = g_1, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors[i]))
} # Edit the grobs

# add legend(extracted from FIG.2 5b_Col and  8b)
grid.newpage()
vp1 <- viewport(width = 0.75, height = 0.8, x = 0.4, y = .5)
vplegC <- viewport(width = 0.25, height = 1, x = 0.9, y = 0.6)
vplegL<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.4)
vplegI<- viewport(width = 0.25, height = 1, x = 0.9, y = 0.5)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegL)
grid.draw(legend_l)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_hln)

#### FIGURE 15-----------------------------------------------
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
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Trend in time for number of records where indicators found")

####FIGURE 16------------------------------------------
## relative frequency of indicators through time
## proportion of records where an indicator is found in a certain time bin

#df with count of Bins (number of record covering a certain Time bin)
BinsTot<-as.data.frame(table(Datasheet_sh.ind.time%>%
                               mutate(ForLabs=Datasheet_sh.full$ForLabs)%>%
                               filter(ForLabs!="H")%>%
                               select(Bin_num)))

BinsTot1<-as.data.frame(table(Datasheet_sh.ind.time%>%
                               mutate(ForLabs=Datasheet_sh.full$ForLabs,
                                      clust.id= Datasheet_shaved$cluster.id_2)%>%
                               filter(ForLabs!="H"& clust.id==1)%>%
                               select(Bin_num)))

BinsTot2<-as.data.frame(table(Datasheet_sh.ind.time%>%
                                mutate(ForLabs=Datasheet_sh.full$ForLabs,
                                       clust.id= Datasheet_shaved$cluster.id_2)%>%
                                filter(ForLabs!="H"& clust.id==2)%>%
                                select(Bin_num)))

BinsTot3<-as.data.frame(table(Datasheet_sh.ind.time%>%
                                mutate(ForLabs=Datasheet_sh.full$ForLabs,
                                       clust.id= Datasheet_shaved$cluster.id_2)%>%
                                filter(ForLabs!="H"& clust.id==3)%>%
                                select(Bin_num)))

BinsTot4<-as.data.frame(table(Datasheet_sh.ind.time%>%
                                mutate(ForLabs=Datasheet_sh.full$ForLabs,
                                       clust.id= Datasheet_shaved$cluster.id_2)%>%
                                filter(ForLabs!="H"& clust.id==4)%>%
                                select(Bin_num)))
#plot
reshape2::melt(Datasheet_sh.ind.time, id= "Bin_num") %>%
  group_by(variable,Bin_num)%>%
  summarise(Count = sum(value, na.rm = T))%>%
  mutate(Freq=BinsTot$Freq[match(Bin_num,BinsTot$Var1)])%>%
  mutate(Rel=Count/Freq)%>%
  ggplot(aes(x=reorder(Bin_num,-Bin_num) ,y=Rel))+
  geom_bar(stat = "identity")+
  facet_wrap(~variable)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins (yr BP)")

#### FIGURE 17----------------------------------------
## map of direct indicators 

Amap+
  geom_point(data=Datasheet_sh.full.long%>%
               filter(IND=="Direct")%>%
               filter(SUM!=0),
             aes(x= Longitude, y=Latitude),color="red")+
  ggtitle("Sites with direct indicators")

#### FIGURE 18--------------------------------------
## map of sites with direct indicators through time
#v_1
Amap1<-ggmap (Andes)+
  geom_point(data= Datasheet_sh.full.long%>%
                filter(IND=="Direct"),
             aes(x= Longitude, y=Latitude, alpha = SUM == 0), 
             color= "red")+
   scale_alpha_manual(values = c(0.3,0)) +
   guides(alpha = FALSE)+
  facet_wrap(~Bin_num)+
   theme_bw()

gridExtra::grid.arrange(Amap,Amap1, ncol=2, 
                        top = textGrob(" Sites with direct indicators through time (compared with location of all sites) ",gp=gpar(fontsize=15)))



#### FIGURE 19-------------------------------------
## map of sites with direct indicators and their abundances
##and how changed through time
Amap2<-ggmap (Andes2)+
   geom_point(data= Datasheet_sh.full.long%>%
                 filter(IND=="Direct"),
             aes(x= Longitude, y=Latitude, size=SUM, alpha = SUM == 0),
             color= "red")+
   scale_alpha_manual(values = c(0.3,0)) +
   scale_size_continuous(breaks=c(1,2,3))+
   guides(alpha = FALSE)+
  labs(size= "Number of indicators")+
   facet_wrap (~Bin_num)+
   theme_bw()

plot(Amap2)   

# FIGURE 20----------------------------------
# map for  potential food indicators
Amap+
  geom_point (data=Datasheet_sh.pot.full.long%>%
               filter(IND=="High" & SUM!=0),
             aes(x= Longitude, y=Latitude), color="#F8766D")+
   ggtitle("Sites with indicators with high food potential (compared with location of all sites)")
   

# FIGURE 21-----------------------------------------------
## map for high potential food indicators through time
Amap3<-ggmap (Andes)+
   geom_point (data=Datasheet_sh.pot.full.long%>%
                  filter(IND=="High"),
               aes(x= Longitude, y=Latitude, alpha = SUM == 0),
               color="#F8766D")+
   scale_fill_manual(values = c("#F8766D", "#00BA38"))+
   scale_alpha_manual(values = c(0.3,0))+
   guides(alpha = FALSE)+
   facet_wrap (~Bin_num)+
   theme_bw()

gridExtra::grid.arrange(Amap,Amap3, ncol=2, 
                        top = textGrob(" Sites with indicators with high food potential through time (compared with location of all sites)",
                                       gp=gpar(fontsize=15)))

# FIGURE 22-----------------------------------------------
## map for high potential food indicators through time and abundances
Amap4<-ggmap (Andes2)+
   geom_point (data=Datasheet_sh.pot.full.long%>%
                  filter(IND=="High"),
               aes(x= Longitude, y=Latitude, size= SUM, alpha = SUM == 0),
               color="#053061")+
   scale_alpha_manual(values = c(0.3,0))+
  scale_size_continuous(range = c(1,7),breaks = c(seq(from=1, to=7, by =1)))+
  # scale_size_continuous(breaks=c(1,2,3))+
   guides(alpha = FALSE)+
  labs(size= "Number of indicators")+
   facet_wrap (~Bin_num)+
   theme_bw()

### FIGURE 22_disturbance-----------------------------

#new df
Datasheet_sh.full.long2<-rbind(bind_cols(Datasheet_shaved,
                                        DF_sh.Disturbance.Sum))
                                       
ggmap (Andes2)+
  geom_point (data=Datasheet_sh.full.long,
              aes(x= Longitude, y=Latitude, size= SUM, alpha = SUM == 0))+
  scale_alpha_manual(values = c(0.3,0))+
  scale_size_continuous(range = c(1,7),breaks = c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))+
  # scale_size_continuous(breaks=c(1,2,3))+
  guides(alpha = FALSE)+
  labs(size= "Number of indicators")+
  facet_wrap (~Bin_num)+
  theme_bw()                             

## FIGURE 23--------------------------------------------
Bin=Datasheet_sh.full.long%>%
  distinct(Bin)
 Bin= sub("\\ BP.*","",Bin$Bin)

Datasheet_sh.full.long%>%
  filter(IND=="Direct")%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot$Freq[match(Bin_num,BinsTot$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins (yr BP)")

## FIGURE 23B------------------------------------------
a<-Datasheet_sh.full.long%>%
  mutate(clust.id= Datasheet_shaved$cluster.id_2
         [match(`Site Name long`, Datasheet_shaved$`Site Name long`)])%>%
  filter(IND=="Direct" & clust.id==2)%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot2$Freq[match(Bin_num,BinsTot2$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins (yr BP)")

b<-Datasheet_sh.full.long%>%
  mutate(clust.id= Datasheet_shaved$cluster.id_2
         [match(`Site Name long`, Datasheet_shaved$`Site Name long`)])%>%
  filter(IND=="Direct" & clust.id==3)%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot3$Freq[match(Bin_num,BinsTot3$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins (yr BP)")

c<-Datasheet_sh.full.long%>%
  mutate(clust.id= Datasheet_shaved$cluster.id_2
         [match(`Site Name long`, Datasheet_shaved$`Site Name long`)])%>%
  filter(IND=="Direct" & clust.id==4)%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot4$Freq[match(Bin_num,BinsTot4$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins (yr BP)")

ggarrange(
  a+guides(color=F),
  b+guides(color=F),
  c+guides(color=F),
  ncol = 1, align = "v", labels = c("Nothern Colombian Andes",
                                    "Colombian inter-Andean dry valleys",
                                    "Ecuadorean Andes")
)

## FIGURE 24--------------------------------------------

Datasheet_sh.pot.full.long%>%
  filter(IND=="High")%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot$Freq[match(Bin_num,BinsTot$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins (yr BP)")

## FUGURE 24B-----------------------------------------

a<-Datasheet_sh.pot.full.long%>%
  mutate(clust.id= Datasheet_shaved$cluster.id_2
         [match(`Site Name long`, Datasheet_shaved$`Site Name long`)])%>%
  filter(IND=="High" & clust.id==1)%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot1$Freq[match(Bin_num,BinsTot1$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins (yr BP)")

b<-Datasheet_sh.pot.full.long%>%
  mutate(clust.id= Datasheet_shaved$cluster.id_2
         [match(`Site Name long`, Datasheet_shaved$`Site Name long`)])%>%
  filter(IND=="High" & clust.id==2)%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot2$Freq[match(Bin_num,BinsTot2$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins (yr BP)")

c<-Datasheet_sh.pot.full.long%>%
  mutate(clust.id= Datasheet_shaved$cluster.id_2
         [match(`Site Name long`, Datasheet_shaved$`Site Name long`)])%>%
  filter(IND=="High" & clust.id==3)%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot3$Freq[match(Bin_num,BinsTot3$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins (yr BP)")

d<-Datasheet_sh.pot.full.long%>%
  mutate(clust.id= Datasheet_shaved$cluster.id_2
         [match(`Site Name long`, Datasheet_shaved$`Site Name long`)])%>%
  filter(IND=="High" & clust.id==4)%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot4$Freq[match(Bin_num,BinsTot4$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins (yr BP)")

ggarrange(
  a+guides(color=F),
  b+guides(color=F),
  c+guides(color=F),
  d+guides(color=F),
  ncol = 1, align = "v", labels = c("Southern Venezuelan Andes",
                                    "Nothern Colombian Andes",
                                    "Colombian inter-Andean dry valleys",
                                    "Ecuadorean Andes")
)

#### FIGURE 25---------------------------------------------------
Datasheet_sh.full.long2%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot$Freq[match(Bin_num,BinsTot$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins (yr BP)")

### FIGURE 25---------------------------------------------------
a<-Datasheet_sh.full.long2%>%
  mutate(clust.id= Datasheet_shaved$cluster.id_2
         [match(`Site Name long`, Datasheet_shaved$`Site Name long`)])%>%
  filter(clust.id==1)%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot1$Freq[match(Bin_num,BinsTot1$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins (yr BP)")

b<-Datasheet_sh.full.long2%>%
  mutate(clust.id= Datasheet_shaved$cluster.id_2
         [match(`Site Name long`, Datasheet_shaved$`Site Name long`)])%>%
  filter(clust.id==2)%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot2$Freq[match(Bin_num,BinsTot2$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins (yr BP)")

c<-Datasheet_sh.full.long2%>%
  mutate(clust.id= Datasheet_shaved$cluster.id_2
         [match(`Site Name long`, Datasheet_shaved$`Site Name long`)])%>%
  filter(clust.id==3)%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot3$Freq[match(Bin_num,BinsTot3$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins (yr BP)")

d<-Datasheet_sh.pot.full.long%>%
  mutate(clust.id= Datasheet_shaved$cluster.id_2
         [match(`Site Name long`, Datasheet_shaved$`Site Name long`)])%>%
  filter(clust.id==4)%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot4$Freq[match(Bin_num,BinsTot4$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins (yr BP)")

ggarrange(
  a+guides(color=F),
  b+guides(color=F),
  c+guides(color=F),
  d+guides(color=F),
  ncol = 1, align = "v", labels = c("Southern Venezuelan Andes",
                                    "Nothern Colombian Andes",
                                    "Colombian inter-Andean dry valleys",
                                    "Ecuadorean Andes")
)
