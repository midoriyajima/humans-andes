# --------------------------------------------------------------- 
#
#                             SETUP
#
# ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(vegan)

#install.packages("ggcorrplot")
library(ggcorrplot)

# Load data
Datasheet <- read_excel("data/Datasheet2.xlsx")

# selecte only indicator data and tunr ten into numbers
# and replace all missing values with zeros ( composional data cannot have NA)
cca.data <-  Datasheet %>% 
  select(-c("LAPD_ID","Site Name","Bin","Reference (short)","Bin number","Bin_num","Country","Latitude","Longitude")) %>%
  apply(., 2, FUN= function(x){
    x[x=="NA"] <-0;
    y<- as.numeric(x);
    return(y)
  })  

#add row names
row.names(cca.data) <- Datasheet$`Site Name`

# select enviromrntal values (time)
cca.env <- Datasheet %>%
  select(c("Bin_num","Site Name"))


# subset the data to exlude rows and cols with SUM = 0
cca.env <- cca.env[rowSums(cca.data, na.rm = T) != 0, ]
cca.data <- cca.data[rowSums(cca.data, na.rm = T) != 0, colSums(cca.data, na.rm = T) != 0]

# check if everything is OK
all(rowSums(cca.data)>0)
all(colSums(cca.data)>0)


# --------------------------------------------------------------- 
#
#             REDUCING THE AMOUNT OF INDICATORS
#
# ---------------------------------------------------------------


# --------------------------------------------------------------- 
# correlation beween all indicators without acknowlaging the time
# ---------------------------------------------------------------
ggcorrplot(cor(cca.data)) 

# select treshold of correlation betwen indicators 
treshold <- 0.75 # correlation of 0.75 is standard,

# check what indicators are corelated higher than the treshold
cor(cca.data) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_longer(cols = -c(rowname)) %>%
  mutate(SAME = rowname == name) %>%
  filter(.,SAME != TRUE & abs(value) > treshold) %>%
  select(-c(SAME))

# Eucalyptus and Pinus! Therefore on of those can be deleted
# can play with the treshold (.06 - 0.8) is all reasonable

# --------------------------------------------------------------- 
#                 correlation using ordination
# ---------------------------------------------------------------

# the data is in binary so lineral prediction is better. But just for sure
# DCA on data
decorana(cca.data)
# first axis lengt is under 2.5 -> linear predictor


rda.1 <- rda(cca.data ~ cca.env$Bin_num + Condition(cca.env$`Site Name`), scale = F)
plot(rda.1)

# summary
smry <- summary(rda.1)

# ---------------------
# ----- APROACH 1 -----
# ---------------------

# what is the correlation between individual indicators based on their ordination scores?

# plot correlation
smry$species %>%
  t() %>%
  cor() %>%
  ggcorrplot()


# treshold <- 0.75 
# create a dataframe with correlation abowe selected treshold ordered by the correlation
cor.ser.by.ord <- smry$species %>%
  t() %>%
  cor() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_longer(cols = -c(rowname)) %>%
  mutate(SAME = rowname == name) %>%
  filter(.,SAME != TRUE & abs(value) > treshold) %>%
  select(-c(SAME)) %>%
  rename(ONE = rowname, TWO = name) %>%
  mutate (COMB = apply(., 1 , FUN = function(x){
    y<- sort((c(as.character(x[1]), as.character(x[2]))))
    z <- paste(y[1],"-",y[2])
    return(z)
  })) %>%
  distinct(COMB,value, .keep_all= T) %>%
  arrange(desc(abs(value)) )

cor.ser.by.ord

# see the dataframe
View(cor.ser.by.ord) # Quite a lot!

# -> Now go trought the list and namualy delete on of the higly correlated indicators


# --------------------------------------------------------------- 
#
#         REDUCING THE AMOUNT OF Sites (clustering)
#
# ---------------------------------------------------------------

# manually instal the package MVPART
install.packages("R/mvpart_1.6-2.tar.gz", type="source")

# OR 
install.packages("devtools")
library(devtools)
devtools::install_github("cran/mvpart")

library(mvpart)

site.list <- as.factor(cca.env$`Site Name`) %>%
  levels()

# custom function made by Vivian
renumgr <- function(clusgr){
  aa<-1
  renum <- rep(1, length(clusgr))
  for(i in 2:length(clusgr)){
    if(clusgr[i]!=clusgr[i-1]) aa <- aa+1
    renum[i]<-aa 
  }
  return(renum)	
}



# multivariate regrresion trees to for each site to individual BINS into ZONES based on the indicators

# prelocate space for the result
cca.env$ZONE <- NA

for(i in 1:length(site.list)) # for each site
{
  # save the focus site
  selected.site <- site.list[i]
  
  # subset the data
  data.temp <- cca.data[cca.env$`Site Name` ==selected.site,]
  data.temp.env <- cca.env[cca.env$`Site Name` ==selected.site,]
  
  # calculate only if the the number of number of BINS > 3
  if (nrow(data.temp.env) > 3)
  {
    # calculate the MRT
    mvpart.1 <- mvpart(data.temp ~  data.temp.env$Bin_num, 
                       xv = "1se", plot.add = FALSE, data = as.data.frame(data.temp), xval= 100,xvmult=100 )
    # save the result 
    cca.env$ZONE[cca.env$`Site Name` ==selected.site] <- renumgr(mvpart.1$where)  
  }
   
}

cca.env %>%
  ggplot(aes(x=Bin_num,y=ZONE))+
  geom_line(aes(group=`Site Name`))+
  geom_smooth(method = "loess", se=F, color="red")+
  theme_classic()+
  scale_x_continuous(trans = "reverse")+
  coord_flip()+
  xlab("Age")+ylab("Indicator zone")
  

#install.packages("FD")
library(FD)

temmp <- cca.env %>%
  pivot_wider(id_cols = `Site Name`, 
              names_from = Bin_num, 
              values_from = ZONE, 
              values_fn = list(ZONE = mean)) 

gow.dis<- temmp %>%
  select(-c(`Site Name`)) %>%
  as.data.frame() %>%
  gowdis(x=.) %>%
  as.matrix() %>%
  as.data.frame()

names(gow.dis) <- temmp$`Site Name`
row.names(gow.dis) <- temmp$`Site Name`


# Similarity of sites based on the ZONING of bins
ggcorrplot(gow.dis)


# --------------------------------------------------------------- 
#
#                   PLOT ORDINAtION (WIP)
#
# ---------------------------------------------------------------


df1  <- data.frame(smry$sites[,1:2]) %>% # PC1 and PC2
  mutate(plot = row.names(cca.data)) 

df2  <- data.frame(smry$species[,1:2]) %>% # loadings for PC1 and PC2
  rownames_to_column()


rda.plot <- ggplot(df1, aes(x=RDA1, y=PC1)) + 
  #geom_text(aes(label=rownames(df1)),size=4) +
  geom_line(aes(group=plot))+
  geom_point(aes(color=cca.env$Bin_num), size=3)+
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  coord_fixed()+
  scale_color_gradient(low="gray",high = "black") + 
  theme_classic()

rda.plot.ind <- ggplot(df2, aes(x=0, xend=RDA1, y=0, yend=PC1))+
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  coord_fixed()+
  theme_classic()+
  geom_segment(color="red", arrow=arrow(length=unit(0.01,"npc"))) +
  geom_text(data=df2, 
            aes(x=RDA1,y=PC1,label=rowname,
                hjust=0.5*(1-sign(RDA1)),vjust=0.5*(1-sign(PC1))), 
            color="red", size=4)
rda.plot.ind

rda.biplot <- rda.plot +
  geom_segment(data=df2, aes(x=0, xend=RDA1, y=0, yend=PC1), 
               color="red", arrow=arrow(length=unit(0.01,"npc"))) +
  geom_text(data=df2, 
            aes(x=RDA1,y=PC1,label=rowname,
                hjust=0.5*(1-sign(RDA1)),vjust=0.5*(1-sign(PC1))), 
            color="red", size=4)
rda.biplot

# --------------------------------------------------------------- 
#
#               SITE CLUSTERING BASED ON THE GEO
#
# ---------------------------------------------------------------

install.packages("factoextra")
install.packages("NbClust")
library(factoextra)
library(NbClust)




# set the best number of clusters based on the geo (max 5) [we should include also the elevation!!!]
cluster.res <- fviz_nbclust(Sites[,c(2:3)], kmeans, nstart = 25,  method = "gap_stat", nboot = 500, k.max = 5)
cluster.res  # the maximum is the best (5)

clusters <- kmeans(Sites[,c(2,3)], 5)
Sites$cluster <- as.factor(clusters$cluster)


ggmap(Andes)+
  geom_point(data=Sites, aes(x= Longitude, y=Latitude, color=cluster))
