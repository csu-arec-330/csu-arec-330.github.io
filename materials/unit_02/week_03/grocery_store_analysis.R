#This script preps the population and income data for the grocery store analysis

library(pacman)
p_load(tidyverse,conflicted,readxl,tigris,sf,broom,factoextra,GGally)

conflict_prefer("filter", "dplyr")

#Load population data 
pop_raw <- read_excel("https://csu-arec-330.github.io/materials/unit_02/inputs/hauer_county_totpop_SSPs.xlsx") 

#process population data (source: https://sedac.ciesin.columbia.edu/data/set/popdynamics-us-county-level-pop-projections-sex-race-age-ssp-2020-2100/data-download)
pop <- pop_raw %>%
  select(geoid,ssp22020,ssp22050) %>% #keep only the middle of the road projection
  filter(str_sub(geoid,1,2)=="04") %>%
  arrange(geoid)


#Load and process income data
inc_raw <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/inc.csv") %>%
  rename(geoid=FIPS)

#Read grocery store data
gs_raw <- read_csv("https://csu-arec-330.github.io/materials/unit_02/inputs/arizona_grocery_foot_traffic.csv")

#need county polygons
az_co <- tigris::counties(state = "AZ",cb=T) %>%
  janitor::clean_names() %>%
  mutate(aland=aland/2.59e+6) %>%
  st_transform(4326)

#convert grocery store points to spatial points object and intersect/join with county polygons
gs_geo <- st_as_sf(gs_raw,coords=c("longitude","latitude"),crs=4326) %>%
  select(placekey) %>%
  st_join(select(az_co,geoid))

#make nonspatial object and join with other datasets
analysis_ds <- gs_geo %>%
  st_set_geometry(NULL) %>%
  group_by(geoid) %>%
  summarize(stores=n()) %>%
  ungroup() %>%
  inner_join(az_co %>% st_set_geometry(NULL) %>% select(geoid,aland,name) ,by = "geoid") %>% #converting sq meters to sq miles
  mutate(gs_mi = stores/aland) %>%
  inner_join(pop,by = "geoid") %>%
  inner_join(inc_raw,by = "geoid")

#####################################
#plot correlations
ggpairs(analysis_ds,columns = c(2,3,5,7,8))

###################################
#Clustering analysis
analysis_scaled <- analysis_ds %>%
  select(stores,aland,ssp22020,unemployment,median_hh_inc) %>%
  scale()

#Find optimal number of clusters (its 3)
fviz_nbclust(analysis_scaled,FUNcluster = kmeans, method = "silhouette") + theme_bw()

# perform k-means clustering with k=3
set.seed(20) # for reproducibility
kmeans_fit <- kmeans(analysis_scaled, 
                     centers=3,  #the number of clusters
                     nstart = 25) #the number of random starts

# add cluster labels to dataset and join to location info
data_clustered <- analysis_ds %>% 
  mutate(cluster = kmeans_fit$cluster) 

#function for plotting
plot_dat <- function(xvar,yvar,xname,yname){
  ggplot(data_clustered,aes(x={{xvar}},y={{yvar}},color=factor(cluster))) + 
    geom_point() + 
    scale_color_discrete(name="Cluster") +
    labs(x=xname,y=yname) +
    theme_bw(base_size = 15)
  ggsave(str_c(xname," x ",yname,".png"),width = 6,height = 4,units = "in")
}

plot_dat(stores,ssp22020,"Store Number","Pop 2020")
plot_dat(stores,aland,"Store Number","Land Area (sq mi)")
plot_dat(stores,unemployment,"Store Number","Unemployment (percent)")

###################################
#Regression
m1 <- lm(stores ~ I(ssp22020/1000) + I(median_hh_inc/1000) + I(aland/1000) + unemployment,
         data = analysis_ds)


summary(m1)

tidy(m1) %>% knitr::kable(format = "simple",digits = 3)


#first analysis: plot relationship between population and stores
analysis_ds %>%
  ggplot(aes(x=ssp22020,y=stores)) +
  geom_smooth(method = "lm",se=F) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x="Population (log scale)",y="Stores (log scale)") +
  theme_bw(base_size = 15)
  
ggsave("reg_line.png",height = 5,width = 6,units = "in")

#Project using new population
new_data <- analysis_ds %>%
  rename(old_pop=ssp22020) %>%
  rename(ssp22020=ssp22050) #replacing the 2020 variable name used in the regression

#creating new dataset and calculating difference between new estimate and current number of stores
gs_new <- augment(m1,newdata = new_data) %>%
  mutate(add_stores = .fitted - stores,
         flag=add_stores>0)

#Set up data for change plot
gs_new_2020 <- select(gs_new,geoid,stores,pop=old_pop,flag,name) %>% mutate(year="2020")
gs_new_2050 <- select(gs_new,geoid,stores=.fitted,pop=ssp22020,flag,name) %>% mutate(year="2050")
gs_all <- bind_rows(gs_new_2020,gs_new_2050)

#change dumbell plot
ggplot() +
  geom_segment(data = gs_new_2020,
               aes(x=stores,y=reorder(name,stores),xend=gs_new_2050$stores,yend=gs_new_2050$name,color = flag),
               size = 2.5, #Note that I sized the segment to fit the points
               alpha = .5,
               show.legend = F) +
  geom_point(data=gs_all,aes(x=stores,y=name,shape=year),size = 2, show.legend = T) +
  xlim(0,220) +
  labs(x="Stores",y="County") +
  theme_bw(base_size = 15)

ggsave("dumbell.png",height = 5,width = 6,units = "in")
