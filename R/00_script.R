#################################################
#                                               #
#                                               #  
#     HUMAN INDICATORS NORTHERN ANDES           #
#             MIDORI YAJIMA                     #
#                                               #
#               MAY 2020                        #
#                                               #      
#-----------------------------------------------#

## CONTRIBUTORS
# Ondrej Mottl <ondrej.mottl@gmail.com>
# Suzette Flantua <s.g.a.flantua@gmail.com>

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

### --------------------------------------------------------
#                                                         
#               DATA MANAGEMENT (do only once)              
#                                                         
### --------------------------------------------------------

# adjust Datasheet
Datasheet_original<-read_excel("data/Datasheet_original.xlsx")


#add Bin_num column
Bin_num<-sub(".*\\-","",Datasheet_original$Bin)
Bin_num<-sub("\\ BP.*","",Bin_num)
Datasheet<-Datasheet_original%>%mutate(Bin_num=Bin_num)
Datasheet$Bin_num<-as.numeric(Datasheet$Bin_num)


# add country, lat, long to Datasheet
#Df with metadata for all sites (even the ones that are not in Datasheet)
LAPD_Andes <- read_excel("data/LAPD_Andes_MY.xlsx")
LAPD_Andes$LAPD_ID<-as.numeric(LAPD_Andes$LAPD_ID)

#add missing metadata to LAPD_Andes
#altitude
which(is.na(LAPD_Andes$Altitude))
LAPD_Andes[72,"Altitude"]=2608 #value found in the original paper
#latitude
view(LAPD_Andes%>%filter(is.na(Latitude)))
LAPD_Andes[70,"Latitude"]=4.58545
LAPD_Andes[70,"Longitude"]=-75.19558
LAPD_Andes[71,"Latitude"]=-0.57946
LAPD_Andes[71,"Longitude"]=-78.24397
LAPD_Andes[72,"Latitude"]=--00.25405
LAPD_Andes[72,"Longitude"]=-78.01075

#to check wich Sites have different names
Sitename=distinct(Datasheet,`Site Name`)
Nomatch<-match(Sitename$`Site Name`,LAPD_Andes$SiteName)
which(is.na(Nomatch))
Sites_Nomatch<-Sitename[c(8,11, 14, 23, 34, 35, 38),]
Sites_Nomatch$`Site Name`
LAPD_Andes$SiteName

#rename Datasheet Site Names according to the ones in LAPD_Andes
to_replace<-c("Cerro Toledo",  "El Tiro\n"  , "Laguna Natosas forest", "Pedras Blancas", "Paramo de Agua Blanca III", "Paramo de pena Negra I")
with_this<-c("Cerro Toledo CT", "El Tiro", "Lagunas Natosas Forest", "Piedras Blancas", "Agua Blanca PAB III", "Paramo de Pena Negra 1")
Datasheet$`Site Name`<-Datasheet$`Site Name`%>%plyr::mapvalues(to_replace,with_this)

#insert correct lat, long for Pantano de Pecho
LAPD_Andes[47,"Latitude"]= -0.20
LAPD_Andes[47,"Longitude"]= -78.37

#add columns with country, latitude and longitude to Datasheet, per each site
Datasheet$Country<-LAPD_Andes$Country[match(Datasheet$`Site Name`,LAPD_Andes$SiteName)]
Datasheet$Latitude<-LAPD_Andes$Latitude[match(Datasheet$`Site Name`,LAPD_Andes$SiteName)]
Datasheet$Longitude<-LAPD_Andes$Longitude[match(Datasheet$`Site Name`,LAPD_Andes$SiteName)]

#see if there something NA left, and which site it corresponds to
view(filter(Datasheet,is.na(Country)))

#save
write_xlsx(LAPD_Andes,"LAPD_Andes_MY.xlsx")

#### new shorter datasheet
#select record spanning last 12000 yr
Datasheet_shaved<-Datasheet%>%filter(Bin_num<12500)

#select only indicators actually found
#Step 1: Filter indicators from dataset
Datasheet.indicators <- Datasheet %>%
  select(.,-c("LAPD_ID\n","Site Name",
              "Reference (short)","Bin", "Bin_num", "Latitude","Longitude", "Country" ))

# Step 2: Convert counts from character to numeric
Datasheet.indicators <- apply (Datasheet.indicators,2,
                               FUN= function(x) as.numeric(unlist(x)))

# Step 4: Make datasheet with site vs number of times an indicator is counted  
Datasheet.indicators <- as.data.frame(Datasheet.indicators)

# calculate  the sum per human indicator
DF.indicators.SUM <-data.frame(IN=apply(Datasheet.indicators, 2, 
                                        FUN = function(x) sum(x,na.rm = T)))
DF.indicators.SUM$IN.name <- row.names(DF.indicators.SUM) %>% as.factor() 

#datasheet with only indicators actually found
DF.zeros<-(DF.indicators.SUM%>%filter(IN==0)) 
Todrop<-as.vector(DF.zeros$IN.name)
Datasheet_shaved<-Datasheet_shaved[,!names(Datasheet_shaved)%in% Todrop]

#remove charcoal (has to be analyzed separatedly)
Datasheet_shaved$Charcoal<- NULL


#add columns to Datasheet_shaved
#records lenght
length_record<-Datasheet_shaved%>%group_by(`Site Name`)%>%summarise(max(Bin_num))
length_record$Length<-length_record$`max(Bin_num)`
Datasheet_shaved$Length<-length_record$Length[match(Datasheet_shaved$`Site Name`,
                                                    length_record$`Site Name`)]

# Altitude of sites
Datasheet_shaved$Altitude<-LAPD_Andes$Altitude[match(Datasheet_shaved$`Site Name`,
                                                     LAPD_Andes$SiteName)]
# correct other typos (found later in "Human indicators.R)
Datasheet[558,3]<-"Giraldo-Giraldo et al, 2018"
Datasheet_shaved[426,3]<-"Giraldo-Giraldo et al, 2018"

write_xlsx(Datasheet_shaved,"Datasheet_shaved.xlsx")
write_xlsx(Datasheet,"Datasheet.xlsx")

# Import and adjust dataframe with right indicators
Human_indicators_original <- read_excel("data/02_Human indicators_V2.xlsx")

Human_indicators<-Human_indicators_original[-c(2,8,13,14,22,25,68),]%>%
  select(c("Group (Taxa)",
           "Family",
           "Indicator",
           "Potential food source (no/low/high)",
           "North Andean fossil records? [yes/no]" ))

Human_indicators[47,"Indicator"]<-"Indirect"

colnames(Human_indicators)[4]<-"Potential food source"

to_replaceI<-c("HIGH","LOW","Low","NO")
with_thisI<-c("high", "low","low", "no")
Human_indicators$`Potential food source`<-Human_indicators$`Potential food source`%>%
  plyr::mapvalues(to_replaceI,with_thisI)

write_xlsx(Human_indicators,"Human_indicators.xlsx")

# Change Ind names in Datasheet shaved (match names in Human_indicators)
Col.to<-pull(Human_indicators%>% 
               filter(`North Andean fossil records? [yes/no]`=="yes")%>%
               select(`Group (Taxa)`))

Col.from<-names(Datasheet_shaved)[5:47]

#sort Col.to in alphabetical number (to match column order in Datasheet_shaved)
Col.to<-sort(Col.to)

Datasheet_shaved<-Datasheet_shaved %>% rename_at(vars(Col.from), ~Col.to)

#change Colname
colnames(Datasheet_shaved)[2]<-"Site Name long"
colnames(Datasheet_shaved)[3]<-"Site Name"

write_xlsx(Datasheet_shaved,"Datasheet_shaved.xlsx")

# df with first appearance of direct ind
FirstInd<-Datasheet_sh.full%>%filter(Sum_direct>0)
FirstInd<-FirstInd[with(FirstInd, order(-Bin_num)), ]
FirstInd<-FirstInd[match(unique(FirstInd$`Site Name`), FirstInd$`Site Name`),]




### --------------------------------------------------------
#                                                         
#             Load dataframes adjusted
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

# create colors pallete for each of the cluster
Color_legend_cluster <- brewer.pal(n = levels(Sites$cluster.id) %>% length(), name = 'Set2')
names(Color_legend_cluster) <- levels(Sites$cluster.id)

# add the color to the Site tibble
Sites <- Sites %>%
  left_join(.,data.frame(cluster.id = names(Color_legend_cluster), cluster.color = Color_legend_cluster), by="cluster.id")

# add the color to the Datasheet_shaved tibble
Datasheet_shaved <- Datasheet_shaved %>% 
  left_join(.,Sites %>% dplyr::select(`Site Name`,cluster.id,cluster.color), by= "Site Name")

### --------------------------------------------------------
#                                                         
#                       FIGURES          
#                                                         
### ---------------------------------------------------------


####FIGURE 1---------------------------------------------
## map, version 1

# obtain map of the area
range(Sites$Longitude)
range(Sites$Latitude)

Amap<-ggmap(get_stamenmap(bbox = c(left=-84,
                             bottom=-7,
                             right=-67, 
                             top=12),
                    zoom=5,maptype = "terrain-background"))+
  geom_point(data=Sites, aes(x= Longitude, 
                             y=Latitude, 
                             color=cluster.id, 
                             size=Altitude))+
  geom_point(data=Sites, aes(x= Longitude, 
                             y=Latitude),
             color="black", size=1)+
  scale_color_manual(values= Color_legend_cluster)+
  scale_size_continuous(range = c(2,6),breaks = c(seq(from=1e3, to=5e3, by =500)))+
  guides(color=F)+
  labs(y="Latitude",
       x="Longitude")

Amap

ggsave("figures/NEW/01_v1.pdf",Amap,
       units = "cm", width = 20, height = 30, dpi = 600)


# version 2
s <- "element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative%7Celement:geometry%7Cvisibility:off&style=feature:administrative.country%7Celement:geometry.stroke%7Ccolor:0x000000%7Cvisibility:on&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Cvisibility:off&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road%7Celement:labels.icon%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit%7Cvisibility:off&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&size=480x360"

Andes2 <- get_googlemap(center = c(-76, 2.6), zoom=5, scale = 1, style = s)


ggmap(Andes2)+
  geom_point(data=Sites,
             aes(x= Longitude, y=Latitude))+
  geom_text(data=Sites, 
            aes(x = Longitude + .001, y = Latitude, label=`Site Name`),
            size=1, hjust=-0.1, vjust=0)


####FIGURE 2-------------------------------
##time span of each site

#with Datasheet_shaved
p02<- Datasheet_shaved %>%
  group_by(`Site Name`) %>%
  summarise(MIN = min(Bin_num),
            MAX = max(Bin_num)) %>%
  left_join(.,Sites %>% dplyr::select(`Site Name`,  Altitude, cluster.id), by="Site Name") %>%
  ggplot() +
  geom_hline(yintercept = seq(from=0, to=12e3, by=500), color="gray80")+
  geom_bar(aes(x=reorder(`Site Name`,-Altitude),y= MAX, fill=cluster.id), colour="gray30", stat = "identity")+
  scale_fill_manual(values = Color_legend_cluster)+
  theme_classic()+
  theme(axis.text.y  = element_text( hjust=1))+
  theme(axis.title.x = element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.title.y = element_blank())+
  geom_text(aes(x=`Site Name`,y=MAX, label=MAX), stat='identity', hjust=-0.1, size=3, color="gray30")+
  coord_flip(ylim = c(0,13e3))+
  labs(fill = "Cluster ID")+
  ggtitle("Time span of the records")

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
p03<- ggplot(Datasheet_shaved)+
  geom_hline(yintercept = seq(from=0, to=40, by=5), color="gray80")+
  geom_bar(aes(x= reorder(Bin,-Bin_num)), color="gray30", fill="gray80")+
  theme_classic()+
  theme(axis.text.x  = element_text(angle = 70, hjust=1),
        axis.title.y = element_blank())+
  xlab("Time bins")+
  ggtitle("Number of pollen records per time bin")

p03

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
p03b<- Human_indicators%>%
  ggplot(aes(x=reorder(Taxa,N.SITES),y=N.SITES,fill=Indicator))+
  geom_hline(yintercept = seq(from=0,to=40,by=10), color="gray80")+
  geom_bar(stat = "identity", color="gray30")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 70, hjust=1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  xlab("Indicators")+
  labs(fill="Indicator")+
  ggtitle("Number of pollen records where Indicators are found")

p03b

ggsave("figures/NEW/03b.pdf",p03b,
       units = "cm", width = 25, height = 20, dpi = 600)

####FIGURE 3c---------------------------------
# as above, distinguishing for Potential source of food

p03c<- Human_indicators%>%
  ggplot(aes(x=reorder(Taxa,N.SITES),y=N.SITES,fill=PotentionalFoodSource))+
  geom_hline(yintercept = seq(from=0,to=40,by=10), color="gray80")+
  geom_bar(stat = "identity", color="gray30")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 70, hjust=1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  xlab("Indicators")+
  labs(fill="Potentional food source")+
  ggtitle("Number of pollen records where Indicators are found")

p03c

ggsave("figures/NEW/03c.pdf",p03c,
       units = "cm", width = 25, height = 20, dpi = 600)


####FIGURE 3d---------------------------------
# ORDINATION

# selecte only indicator data and tunr ten into numbers
# and replace all missing values with zeros ( composional data cannot have NA)
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

# subset the data to exlude rows and cols with SUM = 0
cca.env <- cca.env[rowSums(cca.data, na.rm = T) != 0, ]
cca.data <- cca.data[rowSums(cca.data, na.rm = T) != 0, colSums(cca.data, na.rm = T) != 0]

# check if everything is OK
all(rowSums(cca.data)>0)
all(colSums(cca.data)>0)

# the data is in binary so lineral prediction is better. But just for sure
# DCA on data
decorana(cca.data)
# first axis lengt is under 2.5 -> linear predictor -> RDA

rda.1 <- rda(cca.data ~ cca.env$Bin_num + Condition(cca.env$`Site Name`), scale =T) 

# summary
smry <- summary(rda.1)

df2  <- data.frame(smry$species[,1:3]) %>% # loadings for PC1 and PC2
  rownames_to_column() %>%
  rename(IND = rowname) %>%
  left_join(.,Human_indicators %>%
              rename(IND = Taxa), by= "IND") %>%
  as_tibble()

x_lim = c(-0.7,0.5)
y_lim =c(-1.6,0.5)

axis_one <- "RDA1"
axis_two <- "PC1"

rda.plot.ind.base <- ggplot(df2, aes(x=0, xend=get(axis_one), y=0, yend=get(axis_two)))+
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  coord_fixed(xlim= x_lim, ylim = y_lim)+ 
  theme_classic()

rda.plot.ind.01 <- rda.plot.ind.base+
  geom_segment(arrow=arrow(length=unit(0.01,"npc")), aes(color=Indicator)) +
  geom_text(data=df2,check_overlap = T,
            aes(x=get(axis_one),y=get(axis_two),label=IND,color=Indicator,
                hjust=0.5*(1-sign(get(axis_one))),vjust=0.5*(1-sign(get(axis_two)))), size=4)+
  labs(x="axis one",y="axis two")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

rda.plot.ind.01.legend <- get_legend(rda.plot.ind.01)

rda.plot.ind.02 <- rda.plot.ind.base+
  geom_segment(arrow=arrow(length=unit(0.01,"npc")), aes(color=Family)) +
  geom_text(data=df2,check_overlap = T,
            aes(x=get(axis_one),y=get(axis_two),label=IND,color=Family,
                hjust=0.5*(1-sign(get(axis_one))),vjust=0.5*(1-sign(get(axis_two)))), size=4)+
  labs(x="axis one",y="axis two")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

rda.plot.ind.02.legend <- get_legend(rda.plot.ind.02)

rda.plot.ind.03 <- rda.plot.ind.base+
  geom_segment(arrow=arrow(length=unit(0.01,"npc")), aes(color=PotentionalFoodSource)) +
  geom_text(data=df2,check_overlap = T,
            aes(x=get(axis_one),y=get(axis_two),label=IND,color=PotentionalFoodSource,
                hjust=0.5*(1-sign(get(axis_one))),vjust=0.5*(1-sign(get(axis_two)))), size=4)+
  labs(x="axis one",y="axis two")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

rda.plot.ind.03.legend <- get_legend(rda.plot.ind.03)

rda.plot.ind.sum <- ggarrange(
  rda.plot.ind.01+guides(color=F),
  rda.plot.ind.02+guides(color=F),
  rda.plot.ind.03+guides(color=F),
  nrow=1, align = "h", labels = c("A","B","C")
)


rda.plot.ind.sum <- annotate_figure(rda.plot.ind.sum,left = "second axis",bottom="first axis")

rda.plot.ind.legend <- ggarrange(
  rda.plot.ind.01.legend,
  rda.plot.ind.03.legend,
  nrow = 1
)


p03.d <- ggarrange(rda.plot.ind.sum,
          rda.plot.ind.legend,
          rda.plot.ind.02.legend,
          ncol = 1, heights = c(15,1,5))

p03.d

ggsave("figures/NEW/03d.pdf",p03.d,
       units = "cm", width = 25, height = 20, dpi = 600)

# A = colored by direct/indirect
# B = colored by the Family
# C = colored by Potentional food source 


#### FIGURE 4 --------------------------------
## trend of total human indicators through time per site

#sum of indicators per time bin for each site

p04 <-Datasheet_shaved %>%
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
  facet_wrap(~`Site Name`) +
  scale_x_continuous(trans = "reverse")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  ylab("Absolute frequency")+
  xlab("Time (yr BC)")+
  ggtitle("Total number of indicators found per time bin per site")

p04  

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


