# --------------------------------------------------------------- 
#
#                             ZONING
#
# ---------------------------------------------------------------

# manually instal the package MVPART
# install.packages("R/mvpart_1.6-2.tar.gz", type="source")

# OR 
# install.packages("devtools")
# library(devtools)
# devtools::install_github("cran/mvpart")

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
    zones <- renumgr(mvpart.1$where) %>% as.factor()
    
    # replace zones by binary 
    l.l <- levels(zones) %>% length()
    print(paste("site number",i,"number of zones =",l.l))
    
    if((l.l %% 2) == 0) {
      levels(zones) <-rep(c(0,1),l.l/2)
    } else {
      new.z <- rep(c(0,1),(l.l/2)+1)
      new.z<- new.z[1:(length(new.z)-1)]
      levels(zones) <- new.z
    }
    
    # save 
    cca.env$ZONE[cca.env$`Site Name` ==selected.site] <- zones
  }
  
}

# filter out the sites that are withour results (too short)
cca.env <- cca.env %>%
  filter(is.na(ZONE) == F)

# change to binary
cca.env$ZONE <- replace(cca.env$ZONE, cca.env$ZONE==1,0)
cca.env$ZONE <- replace(cca.env$ZONE, cca.env$ZONE==2,1)
cca.env$ZONE <- as.factor(cca.env$ZONE)

# add the Site info
Sites_2<-read_excel("data/Sites_2.xlsx")

cca.env <- cca.env %>%
  left_join(Sites_2, by= "Site Name")

cca.env<- cca.env %>% 
  mutate(siteNameLong= Datasheet_shaved$`Site Name long`
         [match(`Site Name`, Datasheet_shaved$`Site Name`)],
         site_num= dat$`Site Num`
         [match(siteNameLong, dat$SiteNameLong)])

cca.env$site_num<-paste(cca.env$site_num,
                                       "(",
                                       cca.env$Altitude,
                                       "masl",
                                       ")")

cca.env<- cca.env %>%
  group_by(`Site Name`) %>%
  mutate(ORDER = mean(as.double(ZONE)*mean(Bin_num), na.rm = T))

cca.env$`Site Name` <- as.factor(cca.env$`Site Name`)
cca.env$`Site Name`<- reorder(cca.env$`Site Name` , cca.env$Latitude)

cca.env$site_num <- as.factor(cca.env$site_num)
cca.env$site_num<- reorder(cca.env$site_num , cca.env$Latitude)


# --------------------------------------------------------------- 
#                         FINAL PLOT
# ---------------------------------------------------------------

# showing changes in the "zones" which are based on the presence of indicatzors
# the colors are base on the clustering

p.cca<-cca.env %>%
  ggplot(aes(x=-Bin_num,y= site_num))+
  geom_line(color="gray80")+
  geom_point(aes(shape= ZONE),color=cca.env$cluster.color_2, size=6)+
  scale_shape_manual(values = c(1,15))+
  theme_classic()+
  labs(x="Age(cal yr BP)",
       y="Site")+ 
  theme(legend.position = "none")

ggsave("figures/To use/28.pdf",p.cca,
       units = "cm", width = 25, height = 20, dpi = 600)

