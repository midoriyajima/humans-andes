# --------------------------------------------------------------- 
#               SITE CLUSTERING BASED ON THE GEOGRAPHY
# ---------------------------------------------------------------
library(tidyverse)
#install.packages("NbClust")
library(NbClust)

Datasheet_shaved <- read_excel("data/Datasheet_shaved.xlsx")
Sites<-Datasheet_shaved %>% 
  distinct(`Site Name`,Latitude,Longitude, Country, Altitude)


# set the best number of clusters based on the location and elevation (max 10) 
clusters <- Sites %>%
  mutate(Altitude.log = log(Altitude)) %>%
  dplyr::select(Latitude,Longitude,Altitude.log) %>%
  NbClust(., min.nc = 3, max.nc = 10, method = "complete")

# ***** Conclusion *****                            
#* According to the majority rule, the best number of clusters is  4 

# add the cluster definition to the Sites DF
Sites$cluster <- as.factor(clusters$Best.partition)

Color.legen <- brewer.pal(n = 4, name = 'Set2')
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
  labs(y="Latitude",
       x="Longitude")

# add the color to the Site tibble
Sites <- Sites %>%
  left_join(.,data.frame(cluster = names(Color.legen), cluster.color = Color.legen), by="cluster")


# !!! I recomemend using this site clasification in all figures. The color palete Color.legen can be used in all charts
