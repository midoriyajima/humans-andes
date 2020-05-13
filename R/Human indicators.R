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
# TODO MAPS:zoom map, change background (no writes), connect site names on ppt to make them readable 
#        (operational map), OR with numbers and circles
# TODO DATASET: change Huila/n
# TODO FIG 2: sign when direct ind appear
# TODO FIG 3b: modify title, count number of sites (not bins)
# TODO FIG 5 on: add altitude
# TODO ALL FIGURES: short version of site names, self explanatory titles, add altitude to table 
#                  when present and to titles when no table, check if everything is readable, check y axes
# TODO FIG 5 to 6c: only direct ind, how to make them understandable, less figures 
# TODO: climate chart

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

#### Setting up dataframes (do only once)-------------------------------------------------------

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

write_xlsx(Datasheet_shaved,"Datasheet_shaved.xlsx")


#------------------------------------------------------------------------

# Load dataframes adjusted
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
grid.newpage()
grid.table(ToTable)

# export toTable in excel
write_xlsx(ToTable,"Table_01.xlsx")

####FIGURE 1---------------------------------------------
## map, version 1

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


# version 2
median(Sites$Longitude)
median(Sites$Latitude)

Andes2 <- get_googlemap(center = c(-77.85538, 0.427025),  maptype = "satellite", zoom = 5) 

ggmap(Andes2)+
   geom_point(data=Sites,
              aes(x= Longitude, y=Latitude))+
   geom_text(data=Sites, 
             aes(x = Longitude + .001, y = Latitude, label=`Site Name`),
             size=1, hjust=-0.1, vjust=0)

# version 2, b/n
Andes4 <- get_googlemap(center = c(-77.85538, 0.427025),  maptype = "satellite", zoom = 5, color='bw') 

Amap4<-ggmap(Andes4)+
   geom_point(data=Sites,
              aes(x= Longitude, y=Latitude))+
   geom_text(data=Sites, 
             aes(x = Longitude + .001, y = Latitude, label=`Site Name`),
             size=1, hjust=-0.1, vjust=0)

# version 3
Andes2b <- get_googlemap(center = c(-77.85538, 0.427025),  maptype = "terrain", zoom = 5) 

ggmap(Andes2b)+
   geom_point(data=Sites,
              aes(x= Longitude, y=Latitude))+
   geom_text(data=Sites, 
             aes(x = Longitude + .001, y = Latitude, label=`Site Name`),
             size=1, hjust=-0.1, vjust=0)


# version 3, b/n
box<-make_bbox(lat=Sites$Latitude, lon=Sites$Longitude)

Andes3 <-get_map(location = box, 
                 source = "google", zoom = 5,color='bw')

Amap3<-ggmap(Andes3)+
   geom_point(data=Sites,
              aes(x= Longitude, y=Latitude))+
   geom_text(data=Sites, 
             aes(x = Longitude + .001, y = Latitude, label=`Site Name`),
             size=1, hjust=-0.1, vjust=0)


# same as above, subsetting Sites (not used for the moment)----------------------------
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

### subsetting By altitude
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

#see which colours are colorblind friendly
display.brewer.all(colorblindFriendly = TRUE)
display.brewer.pal(n = 3, name = 'Greys')
brewer.pal(n = 3, name = 'Greys')

#with Datasheet_shaved
plot<-ggplot(Datasheet_shaved)+
  geom_bar( aes(x=reorder(`Site Name`,Latitude), fill=Country),colour="black")+
  scale_fill_manual(values=c("#BDBDBD","#636363","#F0F0F0"))+
  theme_bw()+
  theme(axis.text.y  = element_text( hjust=1))+
  theme(axis.title.x = element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.title.y = element_blank())+
  geom_text(aes(x=`Site Name`,label=Length),stat='count', hjust=-0.1, size=3)+
  coord_flip()+
  ggtitle("Time span of the records")

# extract legend from here, to use later in other plots

legend<-get_legend(plot)

#### FIGURE 3 ---------------------------------
## number of records per time bin 

# with Datasheet_shaved
ggplot(Datasheet_shaved)+
  geom_bar(aes(x= reorder(Datasheet_shaved$Bin,-Datasheet_shaved$Bin_num)))+
  theme_bw()+
  theme(axis.text.x  = element_text(angle = 70, hjust=1),
        axis.title.y = element_blank())+
  xlab("Time bins")+
  ggtitle("Number of pollen records per time bin")+
  geom_text(aes(x=Bin,label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)
  
#### FIGURE 3b ---------------------------------
## number of times human indicators are counted in total

 # select only indicators
 Datasheet_sh.indicators<-Datasheet_shaved %>%
   select(.,-c("LAPD_ID\n" ,"Site Name","Reference (short)","Bin", "Bin_num",
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
   summarise(Count = sum(value, na.rm = T)) 
 
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
 ggplot(aes(x=reorder(variable,TotPres),y=TotPres,fill=Ind))+
   geom_bar(stat = "identity")+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 70, hjust=1),
         axis.title.y = element_blank())+
   xlab("Indicators")+
   labs(fill="Indicator")+
   ggtitle("Number of sites where Indicators are found")
 
####FIGURE 3c---------------------------------
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


# sites ordered according to lat, to sort facets in plot
Datasheet_sh.full$`Site Name`<-factor(Datasheet_sh.full$`Site Name`)
Datasheet_sh.full$`Site Name`<-fct_reorder(Datasheet_sh.full$`Site Name`,
                                                -Datasheet_sh.full$Latitude)


   p <- ggplot(data= Datasheet_sh.full, 
               aes(x=reorder(Bin_num,-Bin_num), y=Sum_total)) +  
     geom_bar(stat = "identity",width = 0.5) +
     facet_wrap(~`Site Name`) +
     theme_bw() +
     theme(axis.text.x = element_text(angle = 50, hjust=1)) +
     geom_text(aes(label=ForLabs), vjust=-0.3, size=3) +
     ylab("Absolute frequency")+
     xlab("Time bins")+
     ggtitle("Absolute frequency of indicators through time per site")
   
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

# add legend(extracted from FIG.2)
plot_grid( g_1, legend, ncol = 2, rel_widths  = c(1, .2))

#### FIGURE 5 ---------------------------------
## number of times human indicators are counted in each site
# order sites by lat   
Datasheet_sh.ind.full$Latitude<-Datasheet_sh.full$Latitude  
Datasheet_sh.ind.full$`Site Name`<-factor(Datasheet_sh.ind.full$`Site Name`)
Datasheet_sh.ind.full$`Site Name`<-fct_reorder(Datasheet_sh.ind.full$`Site Name`,
                                              -Datasheet_sh.ind.full$Latitude)
   

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
   ggtitle("Number of times Indicators are found per Site",
           subtitle = "In how many time bins each indicator was found, per site")
   
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
 
 # create table as legend for the used abbreviations
 ToAbbr<-reshape2::melt(Datasheet_sh.ind.full%>%select(-c("Latitude")))%>%
   distinct(variable)%>%
   pull()
 ForLegend<-as.data.frame(abbreviate(ToAbbr,2, named = T))
 ForLegend<-rownames_to_column(ForLegend, "Indicator")
 colnames(ForLegend)[2]<-"Abbreviation"
 ForLegend[4,1]<-"Amaranthceae/ Chenopodiaceae"
 
 my_table <- tableGrob( ForLegend ,theme = ttheme_default (base_size = 7, padding = unit(c(4,2),"mm")),
                        rows=NULL)
 
 my_table$widths <- unit(rep(1/ncol(my_table), ncol(my_table)), "npc")
 
 # add table and legend extracted from FIG.2
 
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
 
 #### FIGURE 5b--------------------------------------
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
 ## split plot per countries
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                       mutate(Country=Datasheet_sh.full$Country)%>%
                       filter(Country== "Colombia")%>%
                       select(-c("Latitude"))) %>%
    group_by(`Site Name`,variable) %>%
    summarise(Count = sum(value, na.rm = T))%>%
    mutate(Ind=Human_indicators$Indicator
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
    ggplot(aes(x=variable,y=Count, fill=Ind))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`,ncol = 2)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
    scale_fill_discrete(guide=FALSE)+
    scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x= "Indicator")+
    ggtitle("Number of times Indicators are found per Site, Colombia",
            subtitle = "In how many time bins each indicator was found, per site") 
 
 g_1 <- grid.force(ggplotGrob(p))
 
 # extract legend from plot
 legend_di<-get_legend(g_1)
 
 # add scale_fill_dscete to original plot p
 
 # add table(from fig 6) and legend extracted from FIG.2, legend from plot 6b
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.87, y = 0.5)
 vplegI<-  viewport(width = 0.1, height = 0.2, x = 0.8, y = 0.9)
 
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
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
    ggplot(aes(x=variable,y=Count, fill=Ind))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`,ncol = 2)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
    scale_fill_discrete(guide=FALSE)+
    scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x="Indicator")+
    ggtitle("Number of times Indicators are found per Site, Ecuador",
            subtitle = "In how many time bins each indicator was found, per site") 
 
 g_1 <- grid.force(ggplotGrob(p))
 
 # extract legend from plot
 legend_di<-get_legend(g_1)
 
 # add scale_fill_dscete to original plot p
 
 # add table(from fig 6) and legend extracted from FIG.2, legend from plot 6b
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.87, y = 0.5)
 vplegI<-  viewport(width = 0.1, height = 0.2, x = 0.8, y = 0.9)
 
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
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
    ggplot(aes(x=variable,y=Count, fill=Ind))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
   # guides(fill = guide_legend(nrow = 1))+
    scale_fill_discrete (guide=FALSE)+
    scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x="Indicator",
         fill="Indicator")+
    ggtitle("Number of times Indicators are found per Site, Venezuela",
            subtitle = "In how many time bins each indicator was found, per site") 
 
 g_1 <- grid.force(ggplotGrob(p))
 
 # extract legend from plot
 legend_di<-get_legend(g_1)
 
 # add scale_fill_dscete to original plot p
 
 # add table(from fig 6) and legend extracted from FIG.2, legend from plot 6b
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = .5, x = 0.35, y = .6)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.87, y = 0.55)
 vplegI<- viewport(width = 0.1, height = 0.2, x = 0.3, y = 0.24)
 
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
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
    ggplot(aes(x=variable,y=Count, fill=Ind))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
    scale_fill_discrete(guide=FALSE)+
    scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x="Indicator",
         fill="Indicator")+
    ggtitle("Number of times Indicators are found per Site, between 1k and 2k m Altitude",
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
 
 fills <-  c( rep("#BDBDBD",5)) # vector of colors to fill rectangles
 
 txt_colors <- c(rep("black",5)) # vector of colors for text
 
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
 
 
 
 
 # FIGURE 5b_between 2 and 3k mt-------------------------------------------------
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                       mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                       filter(Altitude %in% 2000:3000)%>%
                       select(-c("Latitude", "Altitude"))) %>%
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
    labs(x="Indicator")+  
    ggtitle("Number of times Indicators are found per Site, between 2000 and 3000 m of altitude",
            subtitle = "In how many time bins each indicator was found, per site") 
 
 
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
 
 # FIGURE 5b_between 3 and 4k mt-------------------------------------------------
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                       mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                       filter(Altitude %in% 3000:4215)%>%
                       select(-c("Latitude", "Altitude"))) %>%
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
    labs(x="Indicator")+
    ggtitle("Number of times Indicators are found per Site, between 3000 m and max altitude where sites found",
            subtitle = "In how many time bins each indicator was found, per site") 
 
 
 
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
 
 #### FIGURE 5c------------------------------------
 # distinguish beteween different potential food source
 
 #sort indicators in new df by food potential 
 
 IndxPot<-Human_indicators%>% 
    group_by(`Potential food source`)%>%
    do( data.frame(with(data=., .[order(`Group (Taxa)`),] )) )%>% # sort human indiactors by group 
    filter(North.Andean.fossil.records...yes.no.=="yes")%>% #select only indicators that are found in the other df
    pull(Group..Taxa.) #create vector indicators (now ordered)
 
 Datasheet_sh.ind.full.sort<-Datasheet_sh.ind.full[,IndxPot]%>%
    mutate(`Site Name`=Datasheet_shaved$`Site Name`)%>%
    mutate(Latitude=Datasheet_sh.full$Latitude ) 
 
 Datasheet_sh.ind.full.sort$`Site Name`<-factor(Datasheet_sh.ind.full.sort$`Site Name`)
 Datasheet_sh.ind.full.sort$`Site Name`<-fct_reorder(Datasheet_sh.ind.full.sort$`Site Name`,
                                                     -Datasheet_sh.ind.full$Latitude)
 #plot
 p<- reshape2::melt(Datasheet_sh.ind.full.sort%>%
                       select(-c("Latitude"))) %>%
    group_by(`Site Name`,variable) %>%
    summarise(Count = sum(value, na.rm = T))%>%
    mutate(Pot=Human_indicators$`Potential food source`
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
    ggplot(aes(x=variable,y=Count, fill=Pot))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 40, hjust=1))+
    scale_fill_discrete(guide=FALSE)+
    scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x="Indicator")+
         #fill= "Potential food")+
    ggtitle("Number of times Indicators are found per Site",
            subtitle = "In how many time bins each indicator was found, per site")  
 
 g_1 <- grid.force(ggplotGrob(p))   
 
 
 # extract legend from plot
 legend_di<-get_legend(g_1)
 
 # add scale_fill_dscete= "none" to original plot p
 
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
 
 #### FIGURE 5c_Colombia----------------------------------------
 ## split plot per countries
 p<- reshape2::melt(Datasheet_sh.ind.full.sort%>%
                       mutate(Country=Datasheet_sh.full$Country)%>%
                       filter(Country== "Colombia")%>%
                       select(-c("Latitude"))) %>%
    group_by(`Site Name`,variable) %>%
    summarise(Count = sum(value, na.rm = T))%>%
    mutate(Pot=Human_indicators$`Potential food source`
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
    ggplot(aes(x=variable,y=Count, fill=Pot))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`,ncol = 2)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
    scale_fill_discrete(guide=FALSE)+
    scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x="Indicator")+
    ggtitle("Number of times Indicators are found per Site, Colombia",
            subtitle = "In how many time bins each indicator was found, per site")
 
 g_1 <- grid.force(ggplotGrob(p))
 
 # add table(from fig 6) and legend extracted from FIG.2, legend from plot 6b
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.87, y = 0.5)
 vplegI<- viewport(width = 0.1, height = 0.2, x = 0.8, y = 0.9)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)

 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 #### FIGURE 5c_Ecuador----------------------------------------
 ## split plot per countries
 p<- reshape2::melt(Datasheet_sh.ind.full.sort%>%
                       mutate(Country=Datasheet_sh.full$Country)%>%
                       filter(Country== "Ecuador")%>%
                       select(-c("Latitude"))) %>%
    group_by(`Site Name`,variable) %>%
    summarise(Count = sum(value, na.rm = T))%>%
    mutate(Pot=Human_indicators$`Potential food source`
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
    ggplot(aes(x=variable,y=Count, fill=Pot))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`,ncol = 2)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
    scale_fill_discrete(guide=FALSE)+
    scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x="Indicator")+
    ggtitle("Number of times Indicators are found per Site, Ecuador",
            subtitle = "In how many time bins each indicator was found, per site")
 
 
 g_1 <- grid.force(ggplotGrob(p))
 
 
 # add table(from fig 6) and legend extracted from FIG.2, legend from plot 6b
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.87, y = 0.5)
 vplegI<- viewport(width = 0.1, height = 0.2, x = 0.84, y = 0.9)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 #### FIGURE 5c_Venezuela----------------------------------------
 ## split plot per countries
 p<- reshape2::melt(Datasheet_sh.ind.full.sort%>%
                       mutate(Country=Datasheet_sh.full$Country)%>%
                       filter(Country== "Venezuela")%>%
                       select(-c("Latitude"))) %>%
    group_by(`Site Name`,variable) %>%
    summarise(Count = sum(value, na.rm = T))%>%
    mutate(Pot=Human_indicators$`Potential food source`
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
    ggplot(aes(x=variable,y=Count, fill=Pot))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`,ncol = 2)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1),
          legend.direction = "horizontal",
          legend.box = "horizontal")+
    #guides(fill = guide_legend(nrow = 1))+
    scale_fill_discrete(guide=FALSE)+
    scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x="Indicator",
         fill="Potential food")+
    ggtitle("Number of times Indicators are found per Site, Venezuela",
            subtitle = "In how many time bins each indicator was found, per site")
 
 g_1 <- grid.force(ggplotGrob(p))
 
 # extract legend from plot, different from the one from fig 6c_Col, horizontal
 legend_di<-get_legend(g_1)
 
 # add scale_fill_dscete to original plot p
 
 # add table(from fig 6) and legend extracted from FIG.2, legend from plot 6b
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = .5, x = 0.35, y = .6)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.87, y = 0.6)
 vplegI<- viewport(width = 0.1, height = 0.2, x = 0.4, y = 0.25)
 
 upViewport(0)
 pushViewport(vp1)
 grid.draw(g_1)
 
 upViewport(0)
 pushViewport(vpleg)
 grid.draw(my_table)
 
 upViewport(0)
 pushViewport(vplegI)
 grid.draw(legend_di)
 
 # FIGURE 5c_under 2k mt-------------------------------------------------
 
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                       mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                       filter(Altitude < 2000)%>%
                       select(-c("Latitude", "Altitude"))) %>%
    group_by(`Site Name`,variable) %>%
    summarise(Count = sum(value, na.rm = T))%>%
    mutate(Pot=Human_indicators$`Potential food source`
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
    ggplot(aes(x=variable,y=Count, fill=Pot))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
    scale_fill_discrete(guide=FALSE)+
    scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x="Indicator",
         fill="Potential food")+
    ggtitle("Number of times Indicators are found per Site, between 1k and 2k m Altitude",
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
 
 fills <-  c( rep("#BDBDBD",5)) # vector of colors to fill rectangles
 
 txt_colors <- c(rep("black",5)) # vector of colors for text
 
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
 
 
 
 
 # FIGURE 5c_between 2 and 3k mt-------------------------------------------------
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                       mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                       filter(Altitude %in% 2000:3000)%>%
                       select(-c("Latitude", "Altitude"))) %>%
    group_by(`Site Name`,variable) %>%
    summarise(Count = sum(value, na.rm = T))%>%
    mutate(Pot=Human_indicators$`Potential food source`
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
    ggplot(aes(x=variable,y=Count, fill=Pot))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
    scale_fill_discrete(guide=FALSE)+
    scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x="Indicator")+
    ggtitle("Number of times Indicators are found per Site, between 2000 and 3000 m of altitude",
            subtitle = "In how many time bins each indicator was found, per site")  
 
 
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
 
 # FIGURE 5c_between 3 and 4k mt-------------------------------------------------
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                       mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                       filter(Altitude %in% 3000:4215)%>%
                       select(-c("Latitude", "Altitude"))) %>%
    group_by(`Site Name`,variable) %>%
    summarise(Count = sum(value, na.rm = T))%>%
    mutate(Pot=Human_indicators$`Potential food source`
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
    ggplot(aes(x=variable,y=Count, fill=Pot))+
    geom_bar(stat = "identity")+
    facet_wrap(~`Site Name`)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust=1))+
    scale_fill_discrete(guide=FALSE)+
    scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x="Indicator")+
    ggtitle("Number of times Indicators are found per Site, between 3000 m and max altitude where sites found",
            subtitle = "In how many time bins each indicator was found, per site")  
 
 
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
 
 
#### FIGURE 6-----------------------------
 ## same with relative frequencies of indicators (n bins ind found/n bins covered from that site)
 #df with number of bins for each site (excluding bins where hiatuses)
 TotBins=as.data.frame (table(Datasheet_sh.full%>%
                               filter(ForLabs!="H")%>%
                               select(`Site Name`)))
 
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
 
 #### FIGURE 6b--------------------------------------
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
 ## split plot per countries
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                      mutate(Country=Datasheet_sh.full$Country)%>%
                      filter(Country== "Colombia")%>%
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
   facet_wrap(~`Site Name`,ncol = 2)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   scale_fill_discrete(guide=FALSE)+
   scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x="Indicator")+
   ggtitle("Relative frequency of indicators, Colombia",
           subtitle = "Proportion of number of bins indicators found/ number of bins covered in a record" ) 
 
 g_1 <- grid.force(ggplotGrob(p))
 
 # extract legend from plot
 legend_di<-get_legend(g_1)
 
 # add scale_fill_dscete to original plot p
 
 # add table(from fig 6) and legend extracted from FIG.2, legend from plot 6b
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.87, y = 0.5)
 vplegI<- viewport(width = 0.1, height = 0.2, x = 0.8, y = 0.9)
 
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
 ## split plot per countries
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                      mutate(Country=Datasheet_sh.full$Country)%>%
                      filter(Country== "Ecuador")%>%
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
   facet_wrap(~`Site Name`,ncol = 2)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   scale_fill_discrete(guide=FALSE)+
   scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x="Indicator")+
   ggtitle("Relative frequency of indicators, Ecuador",
           subtitle = "Proportion of number of bins indicators found/ number of bins covered in a record" )  
 
 g_1 <- grid.force(ggplotGrob(p))
 
 # add table(from fig 6) and legend extracted from FIG.2, legend from plot 6b
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.87, y = 0.5)
 vplegI<- viewport(width = 0.1, height = 0.2, x = 0.8, y = 0.9)
 
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
          [match(`Site Name`,TotBins$Var1)])%>%
   mutate(Freq=Count/TotBins)%>%
   mutate(Ind=Human_indicators$Indicator
          [match(variable,Human_indicators$`Group (Taxa)`)])%>%
   ggplot(aes(x=variable,y=Freq, fill=Ind))+
   geom_bar(stat = "identity")+
   facet_wrap(~`Site Name`)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   #guides(fill = guide_legend(nrow = 1))+
   scale_fill_discrete(guide=FALSE)+
   scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x="Indicator",
         fill="Indicator")+
   ggtitle("Relative frequency of indicators, Venezuela",
           subtitle = "Proportion of number of bins indicators found/ number of bins covered in a record" )  
 
 g_1 <- grid.force(ggplotGrob(p))
 
 # extract legend from plot
 legend_di<-get_legend(g_1)
 
 # add scale_fill_dscete to original plot p
 
 # add table(from fig 6) and legend extracted from FIG.2, legend from plot 6b
 
 grid.newpage()
 vp1 <- viewport(width = 0.75, height = .5, x = 0.35, y = .6)
 vpleg <- viewport(width = 0.25, height = 1, x = 0.87, y = 0.55)
 vplegI<- viewport(width = 0.1, height = 0.2, x = 0.4, y = 0.25)
 
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
    labs(x="Indicator",
         fill="Indicator")+ 
   ggtitle("Relative frequency of indicators, between 1k and 2k m Altitude",
           subtitle = "Proportion of number of bins indicators found/ number of bins covered in a record" ) 
 
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
 
 fills <-  c( rep("#BDBDBD",5)) # vector of colors to fill rectangles
 
 txt_colors <- c(rep("black",5)) # vector of colors for text
 
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
 
 # FIGURE 6b_between 2 and 3k mt-------------------------------------------------
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                      mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                      filter(Altitude %in% 2000:3000)%>%
                      select(-c("Latitude", "Altitude"))) %>%
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
   ggtitle("Relative frequency of indicators, between 2000 and 3000 m of altitude",
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
 
 fills <-  c(rep ("#636363",6) ,rep("#BDBDBD",2),rep ("#636363",1)) # vector of colors to fill rectangles
 
 txt_colors <- c(rep("white",6),rep("black",2), rep("white",1)) # vector of colors for text
 
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
 
 # FIGURE 6b_between 3 and 4k mt-------------------------------------------------
 p<- reshape2::melt(Datasheet_sh.ind.full%>%
                      mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                      filter(Altitude %in% 3000:4215)%>%
                      select(-c("Latitude", "Altitude"))) %>%
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
   ggtitle("Relative frequency of indicators, between 3000 m and max altitude where sites found",
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
 
 

#### FIGURE 6c----------------------------------
# relative freq

p<- reshape2::melt(Datasheet_sh.ind.full.sort%>%
                     select(-c("Latitude"))) %>%
  group_by(`Site Name`,variable) %>%
  summarise(Count = sum(value, na.rm = T))%>%
  mutate(TotBins=TotBins$Freq
         [match(`Site Name`,TotBins$Var1)])%>%
  mutate(Freq=Count/TotBins)%>%
  mutate(Pot=Human_indicators$`Potential food source`
         [match(variable,Human_indicators$`Group (Taxa)`)])%>%
  ggplot(aes(x=variable,y=Freq, fill=Pot))+
  geom_bar(stat = "identity")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  scale_fill_discrete(guide= F)+
  scale_x_discrete(label=function(x) abbreviate(x, 2))+
    labs(x="Indicator",
         fill="Potential food")+
  ggtitle("Relative frequency of indicators",
          subtitle = "Proportion of number of bins indicators found/ number of bins covered in a record" )

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

# add table(from fig 6) and legend extracted from FIG.2

grid.newpage()
vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
vpleg <- viewport(width = 0.22, height = 1, x = 0.87, y = 0.5)
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

#### FIGURE 6c_Colombia----------------------------------------
## split plot per countries
p<- reshape2::melt(Datasheet_sh.ind.full.sort%>%
                     mutate(Country=Datasheet_sh.full$Country)%>%
                     filter(Country== "Colombia")%>%
                     select(-c("Latitude"))) %>%
  group_by(`Site Name`,variable) %>%
  summarise(Count = sum(value, na.rm = T))%>%
  mutate(TotBins=TotBins$Freq
         [match(`Site Name`,TotBins$Var1)])%>%
  mutate(Freq=Count/TotBins)%>%
  mutate(Ind=Human_indicators$Indicator
         [match(variable,Human_indicators$`Group (Taxa)`)])%>%
  mutate(Pot=Human_indicators$`Potential food source`
         [match(variable,Human_indicators$`Group (Taxa)`)])%>%
  ggplot(aes(x=variable,y=Freq, fill=Pot))+
  geom_bar(stat = "identity")+
  facet_wrap(~`Site Name`,ncol = 2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  scale_fill_discrete(guide=FALSE)+
  scale_x_discrete(label=function(x) abbreviate(x, 2))+
   labs(x="Indicator")+
  ggtitle("Relative frequency of indicators, Colombia",
          subtitle = "Proportion of number of bins indicators found/ number of bins covered in a record" ) 

g_1 <- grid.force(ggplotGrob(p))

# extract legend from plot
legend_di<-get_legend(g_1)

# add scale_fill_dscete to original plot p

# add table(from fig 6) and legend extracted from FIG.2, legend from plot 6b

grid.newpage()
vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
vpleg <- viewport(width = 0.25, height = 1, x = 0.87, y = 0.5)
vplegI<- viewport(width = 0.1, height = 0.2, x = 0.84, y = 0.9)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vpleg)
grid.draw(my_table)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_di)

#### FIGURE 6c_Ecuador----------------------------------------
## split plot per countries
p<- reshape2::melt(Datasheet_sh.ind.full.sort%>%
                     mutate(Country=Datasheet_sh.full$Country)%>%
                     filter(Country== "Ecuador")%>%
                     select(-c("Latitude"))) %>%
  group_by(`Site Name`,variable) %>%
  summarise(Count = sum(value, na.rm = T))%>%
  mutate(TotBins=TotBins$Freq
         [match(`Site Name`,TotBins$Var1)])%>%
  mutate(Freq=Count/TotBins)%>%
  mutate(Ind=Human_indicators$Indicator
         [match(variable,Human_indicators$`Group (Taxa)`)])%>%
  mutate(Pot=Human_indicators$`Potential food source`
         [match(variable,Human_indicators$`Group (Taxa)`)])%>%
  ggplot(aes(x=variable,y=Freq, fill=Pot))+
  geom_bar(stat = "identity")+
  facet_wrap(~`Site Name`,ncol = 2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  scale_fill_discrete(guide=FALSE)+
  scale_x_discrete(label=function(x) abbreviate(x, 2))+
   labs(x="Indicator")+
  ggtitle("Relative frequency of indicators,, Ecuador",
          subtitle = "Proportion of number of bins indicators found/ number of bins covered in a record" )  

g_1 <- grid.force(ggplotGrob(p))


# add scale_fill_dscete to original plot p

# add table(from fig 6) and legend extracted from FIG.2, legend from plot 6b

grid.newpage()
vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
vpleg <- viewport(width = 0.25, height = 1, x = 0.87, y = 0.5)
vplegI<- viewport(width = 0.1, height = 0.2, x = 0.84, y = 0.9)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vpleg)
grid.draw(my_table)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_di)

#### FIGURE 6c_Venezuela----------------------------------------
## split plot per countries
p<- reshape2::melt(Datasheet_sh.ind.full.sort%>%
                     mutate(Country=Datasheet_sh.full$Country)%>%
                     filter(Country== "Venezuela")%>%
                     select(-c("Latitude"))) %>%
  group_by(`Site Name`,variable) %>%
  summarise(Count = sum(value, na.rm = T))%>%
  mutate(TotBins=TotBins$Freq
         [match(`Site Name`,TotBins$Var1)])%>%
  mutate(Freq=Count/TotBins)%>%
  mutate(Ind=Human_indicators$Indicator
         [match(variable,Human_indicators$`Group (Taxa)`)])%>%
  mutate(Pot=Human_indicators$`Potential food source`
         [match(variable,Human_indicators$`Group (Taxa)`)])%>%
  ggplot(aes(x=variable,y=Freq, fill=Pot))+
  geom_bar(stat = "identity")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  scale_fill_discrete(guide=FALSE)+
  scale_x_discrete(label=function(x) abbreviate(x, 2))+
 # guides(fill = guide_legend(nrow = 1))+
   labs(x="Indicator",
        fill="Potential food")+
  ggtitle("Relative frequency of indicators,, Venezuela",
          subtitle = "Proportion of number of bins indicators found/ number of bins covered in a record" )  

g_1 <- grid.force(ggplotGrob(p))

# extract legend from plot
legend_di<-get_legend(g_1)
# add scale_fill_dscete to original plot p

# add table(from fig 6) and legend extracted from FIG.2, legend from plot 6b

grid.newpage()
vp1 <- viewport(width = 0.75, height = .5, x = 0.35, y = .6)
vpleg <- viewport(width = 0.25, height = 1, x = 0.87, y = 0.6)
vplegI<- viewport(width = 0.1, height = 0.2, x = 0.4, y = 0.25)

upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vpleg)
grid.draw(my_table)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_di)

# FIGURE 6c_under 2k mt-------------------------------------------------

p<- reshape2::melt(Datasheet_sh.ind.full%>%
                     mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                     filter(Altitude < 2000)%>%
                     select(-c("Latitude", "Altitude"))) %>%
  group_by(`Site Name`,variable) %>%
  summarise(Count = sum(value, na.rm = T))%>%
  mutate(TotBins=TotBins$Freq
         [match(`Site Name`,TotBins$Var1)])%>%
  mutate(Freq=Count/TotBins)%>%
  mutate(Pot=Human_indicators$`Potential food source`
         [match(variable,Human_indicators$`Group (Taxa)`)])%>%
  ggplot(aes(x=variable,y=Freq, fill=Pot))+
  geom_bar(stat = "identity")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  scale_fill_discrete(guide=FALSE)+
  scale_x_discrete(label=function(x) abbreviate(x, 2))+
  labs(x="Indicator",
       fill="Potential food")+
  ggtitle("Relative frequency of indicators, between 1k and 2k m Altitude",
          subtitle = "Proportion of number of bins indicators found/ number of bins covered in a record" )

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

fills <-  c( rep("#BDBDBD",5)) # vector of colors to fill rectangles

txt_colors <- c(rep("black",5)) # vector of colors for text

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

# FIGURE 6c_between 2 and 3k mt-------------------------------------------------
p<- reshape2::melt(Datasheet_sh.ind.full%>%
                     mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                     filter(Altitude %in% 2000:3000)%>%
                     select(-c("Latitude", "Altitude"))) %>%
  group_by(`Site Name`,variable) %>%
  summarise(Count = sum(value, na.rm = T))%>%
  mutate(TotBins=TotBins$Freq
         [match(`Site Name`,TotBins$Var1)])%>%
  mutate(Freq=Count/TotBins)%>%
  mutate(Pot=Human_indicators$`Potential food source`
         [match(variable,Human_indicators$`Group (Taxa)`)])%>%
  ggplot(aes(x=variable,y=Freq, fill=Pot))+
  geom_bar(stat = "identity")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  scale_fill_discrete(guide=FALSE)+
  scale_x_discrete(label=function(x) abbreviate(x, 2))+
  labs(x="Indicator",
       fill="Potential food")+
  ggtitle("Relative frequency of indicators, between 2000 and 3000 m of altitude",
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

fills <-  c(rep ("#636363",6) ,rep("#BDBDBD",2),rep ("#636363",1)) # vector of colors to fill rectangles

txt_colors <- c(rep("white",6),rep("black",2), rep("white",1)) # vector of colors for text

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

# FIGURE 6c_between 3 and 4k mt-------------------------------------------------
p<- reshape2::melt(Datasheet_sh.ind.full%>%
                     mutate(Altitude=Datasheet_sh.full$Altitude)%>%
                     filter(Altitude %in% 3000:4215)%>%
                     select(-c("Latitude", "Altitude"))) %>%
  group_by(`Site Name`,variable) %>%
  summarise(Count = sum(value, na.rm = T))%>%
  mutate(TotBins=TotBins$Freq
         [match(`Site Name`,TotBins$Var1)])%>%
  mutate(Freq=Count/TotBins)%>%
  mutate(Pot=Human_indicators$`Potential food source`
         [match(variable,Human_indicators$`Group (Taxa)`)])%>%
  ggplot(aes(x=variable,y=Freq, fill=Pot))+
  geom_bar(stat = "identity")+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  scale_fill_discrete(guide=FALSE)+
  scale_x_discrete(label=function(x) abbreviate(x, 2))+
  labs(x="Indicator")+
  ggtitle("Relative frequency of indicators, between 3000 m and max altitude where sites found",
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



#### FIGURE 7 ---------------------------------
## in which sites are which indicators found and how often? 

p<-reshape2::melt(Datasheet_sh.ind.full%>%
                 select(-c("Latitude"))) %>%
  group_by(variable,`Site Name`) %>%
  summarise(Count = sum(value, na.rm = T)) %>%
  mutate (Country=Datasheet_sh.full$Country
          [match(`Site Name`, Datasheet_sh.full$`Site Name`)])%>%
  ggplot(aes(x=`Site Name`, y=Count, fill= Country))+
  scale_x_discrete(label=function(x) abbreviate(x, 2))+
  scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
  geom_bar(stat = "identity",colour="black")+
  facet_wrap(~variable)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  labs(x="Sites")+
  ggtitle("Number of times Indicators are found per Site")

g_1 <- grid.force(ggplotGrob(p)) 
 
# create table as legend for the used abbreviations
ToAbbr2<-Datasheet_sh.ind.full%>%distinct(`Site Name`)%>%pull()
ForLegend2<-as.data.frame(abbreviate(ToAbbr2,2, named = T))
ForLegend2<-rownames_to_column(ForLegend2, "Site Name")
colnames(ForLegend2)[2]<-"Abbreviation"


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

p<-reshape2::melt(Datasheet_sh.ind.full%>%
                  select(-c("Latitude"))) %>%
   group_by(variable,`Site Name`) %>%
   summarise(Count = sum(value, na.rm = T)) %>%
   mutate (Country=Datasheet_sh.full$Country
           [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
           Ind=Human_indicators$Indicator
                  [match(variable,Human_indicators$`Group (Taxa)`)])%>%
   filter(Ind=="Direct") %>%
   ggplot(aes(x=`Site Name`, y=Count, fill= Country))+
   scale_x_discrete(label=function(x) abbreviate(x, 2))+
   scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
   geom_bar(stat = "identity",colour="black")+
   facet_wrap(~variable)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   ggtitle("Number of times direct indicators are found per Site")

g_1 <- grid.force(ggplotGrob(p)) 


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

#### FIGURE 7 _Indirect---------------------------------
## in which sites are Direct indicators found and how often? 

p<-reshape2::melt(Datasheet_sh.ind.full%>%
                     select(-c("Latitude"))) %>%
   group_by(variable,`Site Name`) %>%
   summarise(Count = sum(value, na.rm = T)) %>%
   mutate (Country=Datasheet_sh.full$Country
           [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
           Ind=Human_indicators$Indicator
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
   filter(Ind=="Indirect") %>%
   ggplot(aes(x=`Site Name`, y=Count, fill= Country))+
   scale_x_discrete(label=function(x) abbreviate(x, 2))+
   scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
   geom_bar(stat = "identity",colour="black")+
   facet_wrap(~variable)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   ggtitle("Number of times indirect indicators are found per Site")

g_1 <- grid.force(ggplotGrob(p)) 


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

#### FIGURE 7 _High/ Low---------------------------------
## in which sites are indicators with food potential found and how often? 

p<-reshape2::melt(Datasheet_sh.ind.full%>%
                     select(-c("Latitude"))) %>%
   group_by(variable,`Site Name`) %>%
   summarise(Count = sum(value, na.rm = T)) %>%
   mutate (Country=Datasheet_sh.full$Country
           [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
           Pot=Human_indicators$`Potential food source`
                  [match(variable,Human_indicators$`Group (Taxa)`)])%>%
   filter(Pot %in% c("high", "low")) %>%
   ggplot(aes(x=`Site Name`, y=Count, fill= Country))+
   scale_x_discrete(label=function(x) abbreviate(x, 2))+
   scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
   geom_bar(stat = "identity",colour="black")+
   facet_wrap(~variable, ncol = 2)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   ggtitle("Number of times indicators with food potential are found per Site")

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
            rep("#00BA38",1),
            rep("#F8766D",2),
            rep( "#00BA38",1),
            rep( "#F8766D",2),
            rep( "#00BA38",2),
            rep("#F8766D",1),
            rep("#00BA38",2),
            rep("#F8766D",3),
            "#00BA38") 


for (i in 1:length(strip_bg_gpath)){
   g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
} # Edit the grobs


#do the same as fig 6, with plot and table from fig 7 and legend for countries from fig 2, legend for indicators from fig 13c
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
grid.draw(my_table2)


upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_di)



#### FIGURE 7 _No---------------------------------
## in which sites are  indicators with no food pot found and how often? 

p<-reshape2::melt(Datasheet_sh.ind.full%>%
                     select(-c("Latitude"))) %>%
   group_by(variable,`Site Name`) %>%
   summarise(Count = sum(value, na.rm = T)) %>%
   mutate (Country=Datasheet_sh.full$Country
           [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
           Pot=Human_indicators$`Potential food source`
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
   filter(Pot=="no") %>%
   ggplot(aes(x=`Site Name`, y=Count, fill= Country))+
   scale_x_discrete(label=function(x) abbreviate(x, 2))+
   scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
   geom_bar(stat = "identity",colour="black")+
   facet_wrap(~variable)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   ggtitle("Number of times indicators with no food potential are found per Site")

g_1 <- grid.force(ggplotGrob(p)) 


#do the same as fig 6, with plot and table from fig 7 and legend for countries from fig 2
grid.newpage()
vp1 <- viewport(width = 0.75, height = 1, x = 0.35, y = .5)
vpleg <- viewport(width = 0.22, height = 1, x = 0.87, y = 0.5)
vplegC <- viewport(width = 0.1, height = 0.2, x = 0.8, y = 0.9)


upViewport(0)
pushViewport(vp1)
grid.draw(g_1)

upViewport(0)
pushViewport(vpleg)
grid.draw(my_table2)


upViewport(0)
pushViewport(vplegC)
grid.draw(legend)



#### FIGURE 7b------------------------------------------
## relative freq
p<-reshape2::melt(Datasheet_sh.ind.full%>%
                 select(-c("Latitude"))) %>%
  group_by(variable,`Site Name`) %>%
  summarise(Count = sum(value, na.rm = T)) %>%
  mutate(TotBins=TotBins$Freq
         [match(`Site Name`,TotBins$Var1)])%>%
  mutate(Freq=Count/TotBins)%>%
  mutate (Country=Datasheet_sh.full$Country
          [match(`Site Name`, Datasheet_sh.full$`Site Name`)])%>%
  ggplot(aes(x=`Site Name`, y=Freq, fill= Country))+
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
p<-reshape2::melt(Datasheet_sh.ind.full%>%
                     select(-c("Latitude"))) %>%
   group_by(variable,`Site Name`) %>%
   summarise(Count = sum(value, na.rm = T)) %>%
   mutate(TotBins=TotBins$Freq
          [match(`Site Name`,TotBins$Var1)])%>%
   mutate(Freq=Count/TotBins)%>%
   mutate (Country=Datasheet_sh.full$Country
           [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
           Ind=Human_indicators$Indicator
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
   filter(Ind=="Direct") %>%
   ggplot(aes(x=`Site Name`, y=Freq, fill= Country))+
   scale_x_discrete(label=function(x) abbreviate(x, 2))+
   scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
   geom_bar(stat = "identity",colour="black")+
   facet_wrap(~variable)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   ggtitle("Relative frequency of direct indicators",
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

#### FIGURE 7b_Indirect------------------------------------------
## relative freq
p<-reshape2::melt(Datasheet_sh.ind.full%>%
                     select(-c("Latitude"))) %>%
   group_by(variable,`Site Name`) %>%
   summarise(Count = sum(value, na.rm = T)) %>%
   mutate(TotBins=TotBins$Freq
          [match(`Site Name`,TotBins$Var1)])%>%
   mutate(Freq=Count/TotBins)%>%
   mutate (Country=Datasheet_sh.full$Country
           [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
           Ind=Human_indicators$Indicator
           [match(variable,Human_indicators$`Group (Taxa)`)])%>%
   filter(Ind=="Indirect") %>%
   ggplot(aes(x=`Site Name`, y=Freq, fill= Country))+
   scale_x_discrete(label=function(x) abbreviate(x, 2))+
   scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
   geom_bar(stat = "identity",colour="black")+
   facet_wrap(~variable)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   ggtitle("Relative frequency of indirect indicators",
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

#### FIGURE 7b_H/L------------------------------------------
## relative freq
p<-reshape2::melt(Datasheet_sh.ind.full%>%
                     select(-c("Latitude"))) %>%
   group_by(variable,`Site Name`) %>%
   summarise(Count = sum(value, na.rm = T)) %>%
   mutate(TotBins=TotBins$Freq
          [match(`Site Name`,TotBins$Var1)],
          Freq=Count/TotBins,
          Country=Datasheet_sh.full$Country
                                      [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
          Pot=Human_indicators$`Potential food source`
                                      [match(variable,Human_indicators$`Group (Taxa)`)])%>%
   filter(Pot %in% c("high", "low")) %>%
   ggplot(aes(x=`Site Name`, y=Freq, fill= Country))+
   scale_x_discrete(label=function(x) abbreviate(x, 2))+
   scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
   geom_bar(stat = "identity",colour="black")+
   facet_wrap(~variable, ncol=2)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   ggtitle("Relative frequency of indicators with food potential",
           subtitle = "For each site, percentage of bins where each indicator is found")

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
            rep("#00BA38",1),
            rep("#F8766D",2),
            rep( "#00BA38",1),
            rep( "#F8766D",2),
            rep( "#00BA38",2),
            rep("#F8766D",1),
            rep("#00BA38",2),
            rep("#F8766D",3),
            "#00BA38") 


for (i in 1:length(strip_bg_gpath)){
   g_1 <- editGrob(grob = g_1, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
} # Edit the grobs



#do the same as fig 6, with plot and table from fig 7 and legend for countries from fig 2, legend for indicators from fig 10d
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
grid.draw(my_table2)


upViewport(0)
pushViewport(vplegC)
grid.draw(legend)

upViewport(0)
pushViewport(vplegI)
grid.draw(legend_di)

#### FIGURE 7b_no------------------------------------------
## relative freq
p<-reshape2::melt(Datasheet_sh.ind.full%>%
                     select(-c("Latitude"))) %>%
   group_by(variable,`Site Name`) %>%
   summarise(Count = sum(value, na.rm = T)) %>%
   mutate(TotBins=TotBins$Freq
          [match(`Site Name`,TotBins$Var1)],
          Freq=Count/TotBins,
          Country=Datasheet_sh.full$Country
                   [match(`Site Name`, Datasheet_sh.full$`Site Name`)],
          Pot=Human_indicators$`Potential food source`
                   [match(variable,Human_indicators$`Group (Taxa)`)])%>%
              filter(Pot=="no") %>%
   ggplot(aes(x=`Site Name`, y=Freq, fill= Country))+
   scale_x_discrete(label=function(x) abbreviate(x, 2))+
   scale_fill_manual(values = c("#BDBDBD","#636363","#F0F0F0"), guide=F)+
   geom_bar(stat = "identity",colour="black")+
   facet_wrap(~variable)+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 60, hjust=1))+
   ggtitle("Relative frequency of indicators with no food potential",
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

# order sites by lat
Datasheet_sh.pot.full$`Site Name`<-factor(Datasheet_sh.pot.full$`Site Name`)
Datasheet_sh.pot.full$`Site Name`<-fct_reorder(Datasheet_sh.pot.full$`Site Name`,
                                           -Datasheet_sh.full$Latitude)                        

#### FIGURE 8-------------------------------------------
##Absolute frequency of direct indicators through time per site 

p<-Datasheet_sh.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_direct)) +  
  geom_bar(stat = "identity",width = 0.5) +
  facet_wrap(~`Site Name`) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  geom_text(aes(label= ifelse(ForLabs=="H","H",Sum_direct)), vjust=-0.3, size=3) +
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


# add legend(extracted from FIG.2)
plot_grid( g_1, legend, ncol = 2, rel_widths  = c(1, .2))


#### FIGURE 8b---------------------------------------------------------
##same, without numbers and with time span of the records
p<-Datasheet_sh.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_direct)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=4),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
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


# add legend(extracted from FIG.2)
plot_grid( g_1, legend, ncol = 2, rel_widths  = c(1, .2))


#### FIGURE 8b_Col (do not do)------------------------------------
Datasheet_sh.full %>%
  filter(Country=="Colombia")%>%
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_direct)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=4),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of direct indicators through time per site")

#### FIGURE 8b_Ven (do not do)------------------------------------
Datasheet_sh.full %>%
  filter(Country=="Venezuela")%>%
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_direct)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=4),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of direct indicators through time per site")

#### FIGURE 8b_Ec (do not do)------------------------------------
Datasheet_sh.full %>%
  filter(Country=="Ecuador")%>%
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_direct)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=4),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of direct indicators through time per site")
#### FIGURE 9----------------------------------------
## Absolute frequency of indirect indicators through time per site

p<-Datasheet_sh.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_indirect)) +  
  geom_bar(stat = "identity",width = 0.5) +
  facet_wrap(~`Site Name`) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  geom_text(aes(label= ifelse(ForLabs=="H","H",Sum_indirect)), vjust=-0.3, size=3) +
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

# add legend(extracted from FIG.2)
plot_grid( g_1, legend, ncol = 2, rel_widths  = c(1, .2))

#### FIGURE 9b---------------------------------------------------------
##same, without numbers and with time span of the records
p<-Datasheet_sh.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_indirect)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
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

# add legend(extracted from FIG.2)
plot_grid( g_1, legend, ncol = 2, rel_widths  = c(1, .2))


#### FIGURE 9b_Col (do not do)----------------------------
Datasheet_sh.full %>% 
  filter(Country=="Colombia")%>%
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_indirect)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indirect indicators through time per site, Colombia")
  
  #### FIGURE 9b_Ven (do not do)----------------------------
  Datasheet_sh.full %>% 
    filter(Country=="Venezuela")%>%
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_indirect)) +  
    geom_bar(stat = "identity",width = 0.7) +
    geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
               col="orange", pch=25 ,bg="orange", cex=0.7)+
    facet_wrap(~`Site Name`)+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust=1)) +
    geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
    ylab("Absolute frequency")+
    xlab("Time bins")+
    ggtitle("Absolute frequency of indirect indicators through time per site, Venezuela")

  #### FIGURE 9b_Ec(do not do)----------------------------
  Datasheet_sh.full %>% 
    filter(Country=="Ecuador")%>%
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_indirect)) +  
    geom_bar(stat = "identity",width = 0.7) +
    geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
               col="orange", pch=25 ,bg="orange", cex=0.7)+
    facet_wrap(~`Site Name`)+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust=1)) +
    geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
    ylab("Absolute frequency")+
    xlab("Time bins")+
    ggtitle("Absolute frequency of indirect indicators through time per site, Ecuador")
  

#### FIGURE 10 ----------------------------------
#Absolute frequency of high food pot indicators through time per site
p<-Datasheet_sh.pot.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_high)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
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

# add legend(extracted from FIG.2)
plot_grid( g_1, legend, ncol = 2, rel_widths  = c(1, .2))

####FIGURE 10_Col(do not do)------------------------------

Datasheet_sh.pot.full %>% 
  filter(Country=="Colombia")%>%
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_high)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators with high food potential through time per site, Colombia")

####FIGURE 10_Ven (do not do)------------------------------

Datasheet_sh.pot.full %>% 
  filter(Country=="Venezuela")%>%
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_high)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators with high food potential through time per site, Venezuela")

####FIGURE 10_Ec (do not do)------------------------------

Datasheet_sh.pot.full %>% 
  filter(Country=="Ecuador")%>%
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_high)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators with high food potential through time per site, Ecuador")

#### FIGURE 11---------------------------------------
#Absolute frequency of low food pot indicators through time per site

p<-Datasheet_sh.pot.full %>% 
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_low)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
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

# add legend(extracted from FIG.2)
plot_grid( g_1, legend, ncol = 2, rel_widths  = c(1, .2))

#### FIGURE 11_Col (do not do)---------------------------------

Datasheet_sh.pot.full %>% 
  filter(Country=="Colombia")%>%
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_low)) +  
  geom_bar(stat = "identity",width = 0.7) +
  geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
             col="orange", pch=25 ,bg="orange", cex=0.7)+
  facet_wrap(~`Site Name`)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators with low food potential through time per site, Colombia")

  #### FIGURE 11_vEN (do not do)---------------------------------
  
  Datasheet_sh.pot.full %>% 
    filter(Country=="Venezuela")%>%
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_low)) +  
    geom_bar(stat = "identity",width = 0.7) +
    geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
               col="orange", pch=25 ,bg="orange", cex=0.7)+
    facet_wrap(~`Site Name`)+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust=1)) +
    geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
    ylab("Absolute frequency")+
    xlab("Time bins")+
    ggtitle("Absolute frequency of indicators with low food potential through time per site, Venezuela")  

#### FIGURE 11_Ec (do not do)---------------------------------
  
  Datasheet_sh.pot.full %>% 
    filter(Country=="Ecuador")%>%
  ggplot(aes(x=reorder(Bin_num,-Bin_num), y=Sum_low)) +  
    geom_bar(stat = "identity",width = 0.7) +
    geom_point(aes(x=reorder(Bin_num,-Bin_num),y=22),
               col="orange", pch=25 ,bg="orange", cex=0.7)+
    facet_wrap(~`Site Name`)+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust=1)) +
    geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
    ylab("Absolute frequency")+
    xlab("Time bins")+
    ggtitle("Absolute frequency of indicators with low food potential through time per site, Ecuador")
  
  #### FIGURE 12-------------------------------------
## to have a single chart for direct and indirect indicators
DF_sh.Direct.Sum<-DF_sh.Direct.Sum%>%
  mutate(IND=rep("Direct"))

DF_sh.Indirect.Sum <- DF_sh.Indirect.Sum %>% 
  mutate(IND=rep("Indirect"))

Datasheet_sh.full.long<-rbind(bind_cols(Datasheet_shaved,
                                        DF_sh.Indirect.Sum,
                                        Datasheet_sh.full%>% select(ForLabs)),
                              bind_cols(Datasheet_shaved,
                                        DF_sh.Direct.Sum,
                                        Datasheet_sh.full%>% select(ForLabs)))

# sites ordered according to lat, to sort facets in plot
Datasheet_sh.full.long$`Site Name`<-factor(Datasheet_sh.full.long$`Site Name`)
Datasheet_sh.full.long$`Site Name`<-fct_reorder(Datasheet_sh.full.long$`Site Name`,
                                                -Datasheet_sh.full.long$Latitude)

#order levels of IND to show Indirect ind on the top of direct (in the graph)
Datasheet_sh.full.long$IND<-factor(Datasheet_sh.full.long$IND, 
                                   levels = c("Indirect","Direct"))


#direct and indirect side by side (confusing, do not use)
ggplot(data=Datasheet_sh.full.long, 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Absolute frequency of indicators through time")

# FIGURE 13-------------------------------------------------
#stacked bar chart, filled bars (easier to read)
p<-ggplot(data=Datasheet_sh.full.long, 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"),guide=F)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Absolute frequency of indicators through time",
          subtitle = "Number of direct/indirect indicators")

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

# extract legend from plot
legend_di<-get_legend(g_1)

# add scale_fill_dscete( scale= F) to original plot p

# add legend extracted from FIG.2, legend from this plot 
plot_grid( g_1, plot_grid(legend_di,legend, ncol=1, nrow = 5, align='v'),
           ncol = 2, rel_widths  = c(1, .2))

#### FIGURE 13_Col-----------------------------------------

ggplot(data=Datasheet_sh.full.long%>%
         filter(Country=="Colombia"), 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Absolute frequency of indicators through time, Colombia",
          subtitle = "Number of direct/indirect indicators")


#### FIGURE 13_Ven ------------------------------------------

ggplot(data=Datasheet_sh.full.long%>%
         filter(Country=="Venezuela"), 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Absolute frequency of indicators through time, Venezuela",
          subtitle = "Number of direct/indirect indicators")

#### FIGURE 13_Ec ------------------------------------------

ggplot(data=Datasheet_sh.full.long%>%
         filter(Country=="Ecuador"), 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Absolute frequency of indicators through time, Ecuador",
          subtitle = "Number of direct/indirect indicators")

# FIGURE 13_under 2k mt-------------------------------------------------

p<-ggplot(data=Datasheet_sh.full.long%>%
            filter(Altitude < 2000), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), guide=F)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time, between 1000 and 2000 m of altitude",
          subtitle = "Number of direct/indirect indicators")

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


# add legend extracted from FIG.2, legend from fig 10b 
plot_grid( g_1, plot_grid(legend_di,legend, ncol=1, nrow = 5, align='v'),
           ncol = 2, rel_widths  = c(1, .2))

# FIGURE 13_between 2 and 3k mt-------------------------------------------------

p<-ggplot(data=Datasheet_sh.full.long%>%
            filter(Altitude %in% 2000:3000), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), guide=F)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Absolute frequency of indicators through time, between 2000 and 3000 m of altitude",
          subtitle = "Number of direct/indirect indicators")

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

# add legend extracted from FIG.2, legend from fig 10b 
plot_grid( g_1, plot_grid(legend_di,legend, ncol=1, nrow = 5, align='v'),
           ncol = 2, rel_widths  = c(1, .2))

# FIGURE 13_between 3 and 4k mt-------------------------------------------------

p<-ggplot(data=Datasheet_sh.full.long%>%
            filter(Altitude %in% 3000:4215), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"), guide=F)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Absolute frequency of indicators through time, between 3000 m and max altitude where sites found",
          subtitle = "Number of direct/indirect indicators")

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

# add legend extracted from FIG.2, legend from fig 10b 
plot_grid( g_1, plot_grid(legend_di,legend, ncol=1, nrow = 5, align='v'),
           ncol = 2, rel_widths  = c(1, .2))


#### FIGURE 13b------------------------------------------
# comparison for food potential, stacked bars 

DF_sh.High.Sum<-DF_sh.High.Sum%>%
  mutate(IND=rep("High"))
colnames(DF_sh.High.Sum)[1]<-"SUM"

DF_sh.Low.Sum <- DF_sh.Low.Sum %>% 
  mutate(IND=rep("Low"))
colnames(DF_sh.Low.Sum)[1]<-"SUM"

DF_sh.No.Sum <- DF_sh.No.Sum %>% 
  mutate(IND=rep("No"))
colnames(DF_sh.No.Sum)[1]<-"SUM"


Datasheet_sh.pot.full.long<-rbind(bind_cols(Datasheet_shaved,
                                            DF_sh.High.Sum,
                                            Datasheet_sh.full%>% select(ForLabs)),
                              bind_cols(Datasheet_shaved,
                                        DF_sh.Low.Sum,
                                        Datasheet_sh.full%>% select(ForLabs)),
                              bind_cols(Datasheet_shaved,
                                        DF_sh.No.Sum,
                                        Datasheet_sh.full%>% select(ForLabs)))

# sites ordered according to lat, does not work in sorting facets in plot
Datasheet_sh.pot.full.long$`Site Name`<-factor(Datasheet_sh.pot.full.long$`Site Name`)
Datasheet_sh.pot.full.long$`Site Name`<-fct_reorder(Datasheet_sh.pot.full.long$`Site Name`,
                                                    -Datasheet_sh.pot.full.long$Latitude)

#plot
p<-ggplot(data=Datasheet_sh.pot.full.long, 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  scale_fill_discrete(guide=F)+
  labs(fill="Indicator")+
  ggtitle("Absolute frequency of indicators through time",
          subtitle = "Number of indicators with high/low/no food potential")

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


# extract legend from plot
legend_di<-get_legend(g_1)

# add scale_fill_dscete( scale= F) to original plot p

# add legend extracted from FIG.2, legend from this plot 
plot_grid( g_1, plot_grid(legend_di,legend, ncol=1, nrow = 5, align='v'),
           ncol = 2, rel_widths  = c(1, .2))

### FIGURE 13b_Col---------------------------------

ggplot(data=Datasheet_sh.pot.full.long%>%
         filter(Country=="Colombia"), 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Absolute frequency of indicators through time, Colombia",
          subtitle = "Number of indicators with high/low/no food potential")

### FIGURE 13b_Ven---------------------------------

ggplot(data=Datasheet_sh.pot.full.long%>%
         filter(Country=="Venezuela"), 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Absolute frequency of indicators through time, Venezuela",
          subtitle = "Number of indicators with high/low/no food potential")

### FIGURE 13b_Ec---------------------------------

ggplot(data=Datasheet_sh.pot.full.long%>%
         filter(Country=="Ecuador"), 
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Absolute frequency of indicators through time, Ecuador",
          subtitle = "Number of indicators with high/low/no food potential")

# FIGURE 13b_under 2k mt-------------------------------------------------

p<-ggplot(data=Datasheet_sh.pot.full.long%>%
            filter(Altitude < 2000), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  scale_fill_discrete(guide=F)+
  ggtitle("Absolute frequency of indicators through time, between 1000 and 2000 m of altitude",
          subtitle = "Number of indicators with high/low/no food potential")

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


# add legend extracted from FIG.2, legend from fig 10c
plot_grid( g_1, plot_grid(legend_di,legend, ncol=1, nrow = 5, align='v'),
           ncol = 2, rel_widths  = c(1, .2))

# FIGURE 13b_between 2 and 3k mt-------------------------------------------------

p<-ggplot(data=Datasheet_sh.pot.full.long%>%
            filter(Altitude %in% 2000:3000), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  scale_fill_discrete(guide=F)+
  ggtitle("Absolute frequency of indicators through time, between 2000 and 3000 m of altitude",
          subtitle = "Number of direct/indirect indicators")

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


# add legend extracted from FIG.2, legend from fig 10c
plot_grid( g_1, plot_grid(legend_di,legend, ncol=1, nrow = 5, align='v'),
           ncol = 2, rel_widths  = c(1, .2))

# FIGURE 13b_between 3 and 4k mt-------------------------------------------------

p<-ggplot(data=Datasheet_sh.pot.full.long%>%
            filter(Altitude %in% 3000:4215), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  scale_fill_discrete(guide=F)+
  ggtitle("Absolute frequency of indicators through time, between 3000 m and max altitude where sites found",
          subtitle = "Number of direct/indirect indicators")

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

# add legend extracted from FIG.2, legend from fig 10c
plot_grid( g_1, plot_grid(legend_di,legend, ncol=1, nrow = 5, align='v'),
           ncol = 2, rel_widths  = c(1, .2))

# FIGURE 13c (do not do)-----------------------------------------------
# comparison between high and low food potential indicators, stacked bars

#to see colors of plot above
ggplot_build(p)$data

#plot
p<-Datasheet_sh.pot.full.long%>%
  filter(IND %in% c("High", "Low"))%>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#F8766D", "#00BA38"))+#, guide=F)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  labs(fill="Potential food")+
  ggtitle("Absolute frequency of indicators through time",
          subtitle="Number of indicators with high/ low food potential")

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


grid.newpage(); grid.draw(g_1) # Draw the edited plot

# extract legend from plot
legend_di<-get_legend(g_1)

# add scale_fill_dscete( scale= F) to original plot p

# add legend extracted from FIG.2, legend from this plot 
plot_grid( g_1, plot_grid(legend_di,legend, ncol=1, nrow = 5, align='v'),
           ncol = 2, rel_widths  = c(1, .2))

#### FIGURE 13c_Col (do not do)-----------------------------------

Datasheet_sh.pot.full.long%>%
  filter(IND %in% c("High", "Low") & Country=="Colombia")%>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#F8766D", "#00BA38"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time, Colombia",
          subtitle="Number of indicators with high/ low food potential")

#### FIGURE 13c_Ven (do not do)-----------------------------------

Datasheet_sh.pot.full.long%>%
  filter(IND %in% c("High", "Low") & Country=="Venezuela")%>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#F8766D", "#00BA38"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time, Venezuela",
          subtitle="Number of indicators with high/ low food potential")

#### FIGURE 13c_Ec (do not do)-----------------------------------

Datasheet_sh.pot.full.long%>%
  filter(IND %in% c("High", "Low") & Country=="Ecuador")%>%
  ggplot(aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#F8766D", "#00BA38"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time, Ecuador",
          subtitle="Number of indicators with high/ low food potential")

# FIGURE 13c_under 2k mt (do not do)-------------------------------------------------

p<-ggplot(data=Datasheet_sh.pot.full.long%>%
            filter(Altitude < 2000 & IND %in% c("High", "Low")), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#F8766D", "#00BA38"), guide=F)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time, between 1000 and 2000 m of altitude",
          subtitle = "Number of indicators with high/low/no food potential")

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


grid.newpage(); grid.draw(g_1) # Draw the edited plot

# add legend extracted from FIG.2, legend from plot 10d
plot_grid( g_1, plot_grid(legend_di,legend, ncol=1, nrow = 5, align='v'),
           ncol = 2, rel_widths  = c(1, .2))

# FIGURE 13c_between 2 and 3k mt (do not do)-------------------------------------------------

p<-ggplot(data=Datasheet_sh.pot.full.long%>%
            filter(Altitude %in% 2000:3000 & IND %in% c("High", "Low")), 
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#F8766D", "#00BA38"), guide=F)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time, between 2000 and 3000 m of altitude",
          subtitle = "Number of direct/indirect indicators")

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


grid.newpage(); grid.draw(g_1) # Draw the edited plot

# add legend extracted from FIG.2, legend from fig 10d 
plot_grid( g_1, plot_grid(legend_di,legend, ncol=1, nrow = 5, align='v'),
           ncol = 2, rel_widths  = c(1, .2))

# FIGURE 13c_between 3 and 4k mt (do not do)-------------------------------------------------

p<-ggplot(data=Datasheet_sh.pot.full.long%>%
            filter(Altitude %in% 3000:4215 & IND %in% c("High", "Low")),
          aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 1)+
  scale_fill_manual(values = c("#F8766D", "#00BA38"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label= ifelse(ForLabs=="H","H","")), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Absolute frequency of indicators through time, between 3000 m and max altitude where sites found",
          subtitle = "Number of direct/indirect indicators")

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


grid.newpage(); grid.draw(g_1) # Draw the edited plot


# add legend extracted from FIG.2, legend from fig 10d
plot_grid( g_1, plot_grid(legend_di,legend, ncol=1, nrow = 5, align='v'),
           ncol = 2, rel_widths  = c(1, .2))



####FIGURE 14 (do not do)----------------------------------------------------
#variation in percent of direct/indirect
ggplot(data=Datasheet_sh.full.long,
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",position = "fill",width = 0.5)+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("%")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Proportion of indirect/direct indicators through time")


####FIGURE 14b (do not do)----------------------------------------------------
#variation in percent of ind with food potential
ggplot(data=Datasheet_sh.pot.full.long,
       aes(x = reorder(Bin_num,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 0.5, position = "fill")+
  scale_fill_manual(values = c("#F8766D","#00BA38","#619CFF"))+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("%")+
  xlab("Time bins")+
  labs(fill="Indicator")+
  ggtitle("Proportion of indicators with no/low/high food potential through time")

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
  ggtitle("Number of indicators per time bin")

####FIGURE 16------------------------------------------
## relative frequency of indicators through time
## proportion of records where an indicator is found in a certain time bin

#df with count of Bins (number of record covering a certain Time bin)
BinsTot<-as.data.frame(table(Datasheet_sh.ind.time%>%
                               mutate(ForLabs=Datasheet_sh.full$ForLabs)%>%
                               filter(ForLabs!="H")%>%
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
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  ylab("Relative frequency")+
  xlab("Time bins")+
  ggtitle("Relative frequency of indicators per time bin")

#### FIGURE 17----------------------------------------
## map of direct indicators 
# v_1
Amap3+
  geom_point(data=Datasheet_sh.full.long%>%
               filter(IND=="Direct")%>%
               filter(SUM!=0),
             aes(x= Longitude, y=Latitude),color="red")+
  ggtitle("Sites with direct indicators")

#v_2
Amap4+
   geom_point(data=Datasheet_sh.full.long%>%
                 filter(IND=="Direct")%>%
                 filter(SUM!=0),
              aes(x= Longitude, y=Latitude),color="red")+
   ggtitle("Sites with direct indicators")


#### FIGURE 18--------------------------------------
## map of sites with direct indicators through time
#v_1
Amap3f<-ggmap (Andes3)+
  geom_point(data= Datasheet_sh.full.long%>%
                filter(IND=="Direct"),
             aes(x= Longitude, y=Latitude, alpha = SUM == 0), 
             color= "red")+
   scale_alpha_manual(values = c(0.3,0)) +
   guides(alpha = FALSE)+
  facet_wrap(~Bin_num)+
   theme_bw()

gridExtra::grid.arrange(Amap3,Amap3f, ncol=2, 
                        top = textGrob(" Sites with direct indicators through time ",gp=gpar(fontsize=15)))

#v2
Amap4f<-ggmap (Andes4)+
   geom_point(data= Datasheet_sh.full.long%>%
                 filter(IND=="Direct"),
              aes(x= Longitude, y=Latitude, alpha = SUM == 0), 
              color= "red")+
   scale_alpha_manual(values = c(0.3,0)) +
   guides(alpha = FALSE)+
   facet_wrap(~Bin_num)+
   theme_bw()
   

gridExtra::grid.arrange(Amap4,Amap4f, ncol=2,
                        top = textGrob(" Sites with direct indicators through time ",gp=gpar(fontsize=15)))

#### FIGURE 19-------------------------------------
## map of sites with direct indicators and their abundances
##and how changed through time
Amap3fs<-ggmap (Andes3)+
   geom_point(data= Datasheet_sh.full.long%>%
                 filter(IND=="Direct"),
             aes(x= Longitude, y=Latitude, size=SUM, alpha = SUM == 0),
             color= "red")+
   scale_alpha_manual(values = c(0.3,0)) +
   scale_size_continuous(breaks=c(1,2,3))+
   guides(alpha = FALSE)+
   facet_wrap (~Bin_num)+
   theme_bw()

gridExtra::grid.arrange(Amap3,Amap3fs, ncol=2, 
                        top = textGrob(" Sites with direct indicators through time ",gp=gpar(fontsize=15)))             
# FIGURE 20----------------------------------
# map for  potential food indicators
ggmap (Andes3)+
  geom_point (data=Datasheet_sh.pot.full.long%>%
               filter(IND=="High" & SUM!=0),
             aes(x= Longitude, y=Latitude), color="#F8766D")+
   ggtitle("Sites with indicators with high food potential")
   

# FIGURE 21-----------------------------------------------
## map for high potential food indicators through time
Amap3_pf<-ggmap (Andes3)+
   geom_point (data=Datasheet_sh.pot.full.long%>%
                  filter(IND=="High"),
               aes(x= Longitude, y=Latitude, alpha = SUM == 0),
               color="#F8766D")+
   scale_fill_manual(values = c("#F8766D", "#00BA38"))+
   scale_alpha_manual(values = c(0.3,0))+
   guides(alpha = FALSE)+
   facet_wrap (~Bin_num)+
   theme_bw()

gridExtra::grid.arrange(Amap3,Amap3_pf, ncol=2, 
                        top = textGrob(" Sites with indicators with high food potential through time ",
                                       gp=gpar(fontsize=15)))

# FIGURE 22-----------------------------------------------
## map for high potential food indicators through time and abundances
Amap3_pfs<-ggmap (Andes3)+
   geom_point (data=Datasheet_sh.pot.full.long%>%
                  filter(IND=="High"),
               aes(x= Longitude, y=Latitude, size= SUM, alpha = SUM == 0),
               color="#F8766D")+
   scale_fill_manual(values = c("#F8766D", "#00BA38"))+
   scale_alpha_manual(values = c(0.3,0))+
   scale_size_continuous(breaks=c(1,2,3))+
   guides(alpha = FALSE)+
   facet_wrap (~Bin_num)+
   theme_bw()

gridExtra::grid.arrange(Amap3,Amap3_pfs, ncol=2, 
                        top = textGrob(" Sites with indicators with high food potential through time ",
                                       gp=gpar(fontsize=15)))

## FIGURE 23--------------------------------------------
Bin=Datasheet_sh.full.long%>%distinct(Bin)

Datasheet_sh.full.long%>%
  filter(IND=="Direct")%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot$Freq[match(Bin_num,BinsTot$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin$Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= round(Rel, digits = 2)), vjust=-0.3, size=3) +
  ylab("Relative frequency")+
  xlab("Time bins")+
  ggtitle("Proportion of number of sites with direct indicators per time bin",
          subtitle = "Number of sites with direct indicators in a time bin/ Number of sites covering that time bin")

## FIGURE 24--------------------------------------------

Datasheet_sh.pot.full.long%>%
  filter(IND=="High")%>%
  mutate(Pres=ifelse(SUM>0,1,0))%>%
  group_by(Bin_num)%>%
  summarise(Count=sum(Pres))%>%
  mutate(Freq=BinsTot$Freq[match(Bin_num,BinsTot$Var1)])%>%
  mutate(Rel=Count/Freq,
         Bin=Bin$Bin)%>%
  ggplot(aes(y=Rel, x=reorder(Bin,-Bin_num)))+
  geom_bar(stat = "identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  geom_text(aes(label= round(Rel, digits = 2)), vjust=-0.3, size=3) +
  ylab("Relative frequency")+
  xlab("Time bins")+
  ggtitle("Proportion of number of sites with indicators with high food potential per time bin",
          subtitle = "Number of sites with indicators with high food potential in a time bin/ Number of sites covering that time bin")
