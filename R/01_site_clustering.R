# --------------------------------------------------------------- 
#               SITE CLUSTERING BASED ON THE GEOGRAPHY
# ---------------------------------------------------------------
library(tidyverse)
library(readxl)
#install.packages("NbClust")
library(NbClust)
library(RColorBrewer)
library(ggmap)

Datasheet_shaved <- read_excel("data/Datasheet_shaved.xlsx")
Sites<-Datasheet_shaved %>% 
  distinct(`Site Name`,Latitude,Longitude, Country, Altitude)


# set the best number of clusters based on the location and elevation (max 10) 
clusters <- Sites %>%
  mutate(long.s = scale(Longitude),
         lat.s = scale(Latitude),
         elev.s = scale(Altitude)) %>%
  dplyr::select(long.s, lat.s, elev.s) %>%
  NbClust(., min.nc = 3, max.nc = 10, method = "kmeans")

# add the cluster definition to the Sites DF
Sites$cluster <- as.factor(clusters$Best.partition)

# create colors pallete for each of the cluster
Color.legen <- brewer.pal(n = levels(Sites$cluster) %>% length(), name = 'Set2')
names(Color.legen) <- levels(Sites$cluster)

# check the distribution of suggested separation
ggmap(get_stamenmap(bbox = c(left=-84,
                             bottom=-7,
                             right=-67, 
                             top=12),
                    zoom=5,maptype = "terrain-background"))+
  geom_point(data=Sites, aes(x= Longitude, 
                             y=Latitude, 
                             color=cluster, 
                             size=Altitude))+
  scale_color_manual(values= Color.legen)+
  guides(color=F)+
  labs(y="Latitude",
       x="Longitude")

# add the color to the Site tibble
Sites <- Sites %>%
  left_join(.,data.frame(cluster = names(Color.legen), cluster.color = Color.legen), by="cluster")


# !!! I recomemend using this site clasification in all figures. The color palete Color.legen can be used in all charts
