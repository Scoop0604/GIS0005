library(spData)
library(tidyverse)
library(geosphere)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(here)
library(tmaptools)
library(dplyr)
library(spgwr)
library(spatstat)
library(rgeos)
library(GISTools)
library(geojson)
library(maptools)
library(raster)
library(stringr)
library(fpc)
library(dbscan)
library(dbscan)
library(ggplot2)
library(spdep)
library(caret)
library(corrr)
library(RSQLite)
library(stringr)
library(Matrix)




#Read the csv file and process the variable
csv1 <- read_csv("C:/Users/DeLL/Desktop/listings.csv") 
csv2 <- csv1[,c(1,23,29,33,49,50,53,54,55,56,61,65,66,67,83,87)]
csv2$host_is_superhost <- as.numeric(csv2$host_is_superhost)
csv2 <- na.omit(csv2)
csv3 <- filter(csv2,host_is_superhost == 1)


#Change the price pattern and remove the zero value
csv3$price<- gsub("[\\$, ]", "",  csv3$price)
csv3$price<- gsub("[,]", "",  csv3$price)
csv3$price <- as.numeric(csv3$price)

csv4 <- filter(csv3,room_type == "Entire home/apt")
csv5 <- filter(csv3,room_type == "Private room")
csv5 <-filter(csv5,price>0)
csv5 <-filter(csv5,price<1000)
write.csv(csv4,"C:/Users/Dell/Desktop/dentire.csv",row.names = FALSE)
write.csv(csv5,"C:/Users/Dell/Desktop/dprivate.csv",row.names = FALSE)

#Calculate the distance to the city center
Buckingham_Palace = c(0.1419, 51.5014)
for (i in 1:5199)
{
  others=c(csv4$longitude[i], csv4$latitude[i])
  xy = rbind(Buckingham_Palace=Buckingham_Palace, others=others)
  a=distm(xy)
  a=data.frame(a)
  csv4$distance[i]=a$X2[1]
}


Buckingham_Palace = c(0.1419, 51.5014)
for (i in 1:4809)
{
  others=c(csv5$longitude[i], csv5$latitude[i])
  xy = rbind(Buckingham_Palace=Buckingham_Palace, others=others)
  a=distm(xy)
  a=data.frame(a)
  csv5$distance[i]=a$X2[1]
}

#Describe data
summary(csv4)
summary(csv5)

#Read the boundary
LondonWardsMerged <- st_read(here::here("C:/Users/DeLL/Desktop", 
                                        "statistical-gis-boundaries-london", 
                                        "ESRI",
                                        "London_Ward_CityMerged.shp"))%>%
  st_transform(.,27700)

qtm(LondonWardsMerged)

Airbnb_entire<- st_as_sf(csv4, 
                         coords = c("longitude","latitude"), 
                         crs = 4326)%>%
  st_transform(., 27700)
qtm(Airbnb_entire)

Airbnb_private<- st_as_sf(csv5, 
                          coords = c("longitude","latitude"), 
                          crs = 4326)%>%
  st_transform(., 27700)
qtm(Airbnb_private)

#Price distribution     

figure1_en <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(Airbnb_entire)+
  tm_dots( col = "price",
           palette = "YlOrRd",
           breaks = c(0,50,100,200,300,500,1000,5000),
           size = 0.1,
           popup.vars = TRUE)+
  tm_layout(title = "Price spatial distribution of Entire home/apt", title.position = c("left","top"),title.size=3)
figure1_en
tmap_save(figure1_en, 'C:/Users/DeLL/Desktop/Price_entire_distribution.png',dpi = 300)


figure1_pr <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(Airbnb_private)+
  tm_dots( col = "price",
           palette = "YlOrRd",
           breaks = c(0,50,100,200,300,500,1000,5000),
           size = 0.1,
           popup.vars = TRUE)+
  tm_layout(title = "Price spatial distribution of Private room", title.position = c("left","top"),title.size=3)
figure1_pr
tmap_save(figure1_pr, 'C:/Users/DeLL/Desktop/Price_private_distribution.png',dpi = 300)

#histogram
figure2_en_1 <- ggplot(Airbnb_entire, aes(x=price)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 30)
figure2_en_1

figure2_en_2 <- ggplot(Airbnb_entire, aes(x=log(price))) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.15)+
  labs(title="Entire home/apt Price ",
       x="The natural logarithm  of price",
       y="Frequency")
figure2_en_2

ggsave(figure2_en_2, file = "C:/Users/DeLL/Desktop/Histogram of price_en.png",dpi=500)

figure2_pr_1 <- ggplot(Airbnb_private, aes(x=price)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 20)
figure2_pr_1

figure2_pr_2 <- ggplot(Airbnb_private, aes(x=log(price))) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.15)+
  labs(title="Private room Price ",
       x="The natural logarithm  of price",
       y="Frequency")
figure2_pr_2

ggsave(figure2_pr_2, file = "C:/Users/DeLL/Desktop/Histogram of price_pr.png",dpi=500)

#Spatial autocorrelation
coordsW <- Airbnb_entire%>%
  st_centroid()%>%
  st_geometry()
plot(coordsW)

coordsWP <- Airbnb_private%>%
  st_centroid()%>%
  st_geometry()
plot(coordsWP)

#entire,k=4
knn_wards_entire_1 <-coordsW %>%
  knearneigh(., k=4)

LWard_knn_entire_1 <- knn_wards_entire_1 %>%
  knn2nb()

plot(LWard_knn_entire_1, st_geometry(coordsW), col="blue")

Lward.knn_4_weight <- LWard_knn_entire_1 %>%
  nb2listw(., style="C")

Knn_LWard_Global_entire_1 <- Airbnb_entire %>%
  pull(price) %>%
  as.vector()%>%
  moran.test(., Lward.knn_4_weight)
Knn_LWard_Global_entire_1

I_LWard_Local_entire <- Airbnb_entire %>%
  pull(price) %>%
  as.vector()%>%
  localmoran(., Lward.knn_4_weight)%>%
  as_tibble()
slice_head(I_LWard_Local_entire, n=5)
Airbnb_entire <- Airbnb_entire %>%
  mutate(entire_Iz = as.numeric(I_LWard_Local_entire$Ii))


Figure3_en <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(Airbnb_entire)+
  tm_dots( col = "entire_Iz",
           breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4,5,10,20,50,100),
           palette="RdYlBu",
           size = 0.1,
           style="fixed",
           midpoint = 0)+
  tm_layout(title = "Local Moran's I of Entire home/apt", title.position = c("left","top"),title.size=3)
Figure3_en
tmap_save(Figure3_en, 'C:/Users/DeLL/Desktop/Price_entire_local.png',dpi = 300)



#private, k=4       
knn_wards_private_1 <-coordsWP %>%
  knearneigh(., k=4)

LWard_knn_private_1 <- knn_wards_private_1 %>%
  knn2nb()

Lward.knn_4_weight_private <- LWard_knn_private_1 %>%
  nb2listw(., style="C")

Knn_LWard_Global_private_1 <- Airbnb_private %>%
  pull(price) %>%
  as.vector()%>%
  moran.test(., Lward.knn_4_weight_private)
Knn_LWard_Global_private_1

I_LWard_Local_private <- Airbnb_private %>%
  pull(price) %>%
  as.vector()%>%
  localmoran(., Lward.knn_4_weight_private)%>%
  as_tibble()
slice_head(I_LWard_Local_private, n=5)
Airbnb_private <- Airbnb_private %>%
  mutate(private_Iz = as.numeric(I_LWard_Local_private$Ii))


Figure3_pr <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(Airbnb_private)+
  tm_dots( col = "private_Iz",
           breaks=c(-4,-3,-2,-1,0,1,2,3,4,5,10,20,50,100),
           palette="RdYlBu",
           size = 0.1,
           style="fixed",
           midpoint = 0)+
  tm_layout(title = "Local Moran's I of Private room", title.position = c("left","top"),title.size=3)
Figure3_en
tmap_save(Figure3_pr, 'C:/Users/DeLL/Desktop/Price_private_local.png',dpi = 300)

#OLS

Airbnb_entire$distance <- (Airbnb_entire$distance)/1000
OLS_model_entire <- lm(log(price) ~
                         bathrooms+
                         accommodates+
                         bedrooms+
                         number_of_reviews+
                         review_scores_rating+
                         distance,
                       data = Airbnb_entire)
tidy(OLS_model_entire)
glance(OLS_model_entire)
vif(OLS_model_entire)

OLS_model_private<- lm(log(price)~
                         bathrooms+
                         accommodates+
                         bedrooms+
                         number_of_reviews+
                         review_scores_rating+
                         distance,
                       data = Airbnb_private)
tidy(OLS_model_private)
glance(OLS_model_private)
vif(OLS_model_private)

Airbnb_private <- Airbnb_private %>%
  mutate(OLS_model_res_private = residuals(OLS_model_private))
Airbnb_entire <- Airbnb_entire %>%
  mutate(OLS_model_res_entire = residuals(OLS_model_entire))


Figure4_en <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(Airbnb_entire)+
  tm_dots( col = "OLS_model_res_entire",
           palette = "RdYlBu",
           breaks = c(-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3,3.5,4),
           size = 0.1,
           midpoint = 0,
           popup.vars = TRUE)+
  tm_layout(title = "The Residual distribution of Entire home/apt", title.position = c("left","top"),title.size=3)
Figure4_en
tmap_save(Figure4_en, 'C:/Users/DeLL/Desktop/res_en.png',dpi = 300)



Figure4_pr <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(Airbnb_private)+
  tm_dots( col = "OLS_model_res_private",
           palette = "RdYlBu",
           breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3),
           size = 0.1,
           midpoint = 0,
           popup.vars = TRUE)+
  tm_layout(title = "The Residual distribution of Private room", title.position = c("left","top"),title.size=3)
Figure4_pr
tmap_save(Figure4_pr, 'C:/Users/DeLL/Desktop/res_pr.png',dpi = 300)


#GWR
#entire
st_crs(Airbnb_entire) = 27700

AirbnbSP_entire <- Airbnb_entire %>%
  as(., "Spatial")

st_crs(coordsW) = 27700

coordsWSP <- coordsW %>%
  as(., "Spatial")

GWRbandwidth_entire <- gwr.sel(log(price) ~
                                 bathrooms+
                                 accommodates+
                                 bedrooms+
                                 number_of_reviews+
                                 review_scores_rating+
                                 distance,
                               data = AirbnbSP_entire,
                               coords=coordsWSP,
                               adapt=T)

gwr.model_entire = gwr(log(price) ~
                         bathrooms+
                         accommodates+
                         bedrooms+
                         number_of_reviews+
                         review_scores_rating+
                         distance,
                       data = AirbnbSP_entire,
                       coords=coordsWSP,
                       adapt=GWRbandwidth_entire, 
                       hatmatrix=TRUE, 
                       se.fit=TRUE)
gwr.model_entire


#private
st_crs(Airbnb_private) = 27700

AirbnbSP_private<- Airbnb_private %>%
  as(., "Spatial")

st_crs(coordsWP) = 27700

coordsWPSP <- coordsWP %>%
  as(., "Spatial")

GWRbandwidth_private <- gwr.sel(log(price) ~
                                  bathrooms+
                                  accommodates+
                                  bedrooms+
                                  number_of_reviews+
                                  review_scores_rating+
                                  distance,
                                data = AirbnbSP_private,
                                coords=coordsWPSP,
                                adapt=T)

gwr.model_private = gwr(log(price) ~
                          bathrooms+
                          accommodates+
                          bedrooms+
                          number_of_reviews+
                          review_scores_rating+
                          distance,
                        data = AirbnbSP_private,
                        coords=coordsWPSP,
                        adapt=GWRbandwidth_private, 
                        hatmatrix=TRUE, 
                        se.fit=TRUE)


gwr.model_private   


#coef
results <- as.data.frame(gwr.model_entire$SDF)
names(results)

Airbnb_gwr_entire <- Airbnb_entire %>%
  mutate(coef_bathrooms_entire = results$bathrooms,
         coef_accommodates_entire = results$accommodates,
         coef_bedrooms_entire = results$bedrooms,
         coef_number_of_reviews_entire = results$number_of_reviews,
         coef_review_scores_rating_entire = results$review_scores_rating,
         coef_distance_entire = results$distance,
  )
write.csv(Airbnb_gwr_entire,"C:/Users/Dell/Desktop/entire.csv",row.names = FALSE)

tmap_mode("view")
Figure5_bath_en <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5)+
  tm_shape(Airbnb_gwr_entire) +
  tm_dots(col = "coef_bathrooms_entire", 
          palette = "RdYlBu",
          size = 0.1,
          alpha = 0.5)+
  tm_layout(title = "The bathrooms coeficient of Entire home/apt", title.position = c("left","top"),title.size=3)
Figure5_bath_en
tmap_save(Figure5_bath_en, 'C:/Users/DeLL/Desktop/bath_en.png',dpi = 300)



sigTest = abs(gwr.model_entire$SDF$"bathrooms")-2 * gwr.model_entire$SDF$"bathrooms_se"
Airbnb_gwr_entire <- Airbnb_gwr_entire %>%
  mutate(GWRbothroomshSig = sigTest)


tmap_mode("view")
Figure5_ac_en <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5)+
  tm_shape(Airbnb_gwr_entire) +
  tm_dots(col = "coef_accommodates_entire", 
          palette = "RdYlBu",
          size = 0.1,
          alpha = 0.5)+
  tm_layout(title = "The accommodates coeficient of Entire home/apt", title.position = c("left","top"),title.size=3)       
Figure5_ac_en
tmap_save(Figure5_ac_en, 'C:/Users/DeLL/Desktop/ac_en.png',dpi = 300)

Figure5_bath_en <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5)+
  tm_shape(Airbnb_gwr_entire) +
  tm_dots(col = "coef_bathrooms_entire", 
          palette = "RdYlBu",
          size = 0.1,
          alpha = 0.5)+
  tm_layout(title = "The bathrooms coeficient of Entire home/apt", title.position = c("left","top"),title.size=3)
Figure5_bath_en
tmap_save(Figure5_bath_en, 'C:/Users/DeLL/Desktop/bath_en.png',dpi = 300)

tmap_mode("view")
Figure5_bed_en <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5)+
  tm_shape(Airbnb_gwr_entire) +
  tm_dots(col = "coef_bedrooms_entire", 
          palette = "RdYlBu",
          size = 0.1,
          alpha = 0.5)+
  tm_layout(title = "The bedrooms coeficient of Entire home/apt", title.position = c("left","top"),title.size=3)
Figure5_bed_en
tmap_save(Figure5_bed_en, 'C:/Users/DeLL/Desktop/bed_en.png',dpi = 300)

tmap_mode("view")
Figure5_re_en <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5)+
  tm_shape(Airbnb_gwr_entire) +
  tm_dots(col = "coef_number_of_reviews_entire", 
          palette = "RdYlBu", 
          size = 0.1,
          alpha = 0.5)+
  tm_layout(title = "The number of reviews coeficient of Entire home/apt", title.position = c("left","top"),title.size=3)
Figure5_re_en
tmap_save(Figure5_re_en, 'C:/Users/DeLL/Desktop/re_en.png',dpi = 300)



tmap_mode("view")
Figure5_sc_en<- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5)+
  tm_shape(Airbnb_gwr_entire) +
  tm_dots(col = "coef_review_scores_rating_entire", 
          palette = "RdYlBu",
          size = 0.1,
          alpha = 0.5)+
  tm_layout(title = "The review scores rating coeficient of Entire home/apt", title.position = c("left","top"),title.size=3)
Figure5_sc_en
tmap_save(Figure5_sc_en, 'C:/Users/DeLL/Desktop/re_sc.png',dpi = 300)

tmap_mode("view")
Figure5_d_en<-tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5)+
  tm_shape(Airbnb_gwr_entire) +
  tm_dots(col = "coef_distance_entire", 
          palette = "RdYlBu",
          size = 0.1,
          alpha = 0.5)+
  tm_layout(title = "The distance coeficient of Entire home/apt", title.position = c("left","top"),title.size=3)
Figure5_d_en
tmap_save(Figure5_d_en, 'C:/Users/DeLL/Desktop/d_sc.png',dpi = 300)




outputs <- as.data.frame(gwr.model_private$SDF)
names(outputs)

Airbnb_gwr_private <- Airbnb_private %>%
  mutate(coef_bathrooms_private = outputs$bathrooms,
         coef_accommodates_private = outputs$accommodates,
         coef_bedrooms_private = outputs$bedrooms,
         coef_number_of_reviews_private = outputs$number_of_reviews,
         coef_review_scores_rating_private = outputs$review_scores_rating,
         coef_distance_private = outputs$distance,
  )
write.csv(Airbnb_gwr_private,"C:/Users/Dell/Desktop/private.csv",row.names = FALSE)


tmap_mode("view")
Figure5_bath_pr <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5)+
  tm_shape(Airbnb_gwr_private) +
  tm_dots(col = "coef_bathrooms_private", 
          palette = "RdYlBu", 
          size = 0.1,
          alpha = 0.5)+
  tm_layout(title = "The bathrooms coeficient of Private room", title.position = c("left","top"),title.size=3)
Figure5_bath_pr
tmap_save(Figure5_bath_pr, 'C:/Users/DeLL/Desktop/bath_pr.png',dpi = 300)

tmap_mode("view")
Figure5_ac_pr <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5)+
  tm_shape(Airbnb_gwr_private) +
  tm_dots(col = "coef_accommodates_private", 
          palette = "RdYlBu",
          size = 0.1,
          alpha = 0.5)+
  tm_layout(title = "The accommodates coeficient of Private room", title.position = c("left","top"),title.size=3)
Figure5_ac_pr
tmap_save(Figure5_ac_pr, 'C:/Users/DeLL/Desktop/ac_pr.png',dpi = 300)       

tmap_mode("view")
Figure5_bed_pr <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5)+
  tm_shape(Airbnb_gwr_private) +
  tm_dots(col = "coef_bedrooms_private", 
          palette = "RdYlBu",
          size = 0.1,
          alpha = 0.5)+
  tm_layout(title = "The bedrooms coeficient of Private room", title.position = c("left","top"),title.size=3)
Figure5_bed_pr
tmap_save(Figure5_bed_pr, 'C:/Users/DeLL/Desktop/bed_pr.png',dpi = 300)

tmap_mode("view")
Figure5_re_pr <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5)+
  tm_shape(Airbnb_gwr_private) +
  tm_dots(col = "coef_number_of_reviews_private", 
          palette = "RdYlBu", 
          size = 0.1,
          alpha = 0.5)+
  tm_layout(title = "The number of reviews coeficient of Private room", title.position = c("left","top"),title.size=3)
Figure5_re_pr
tmap_save(Figure5_re_pr, 'C:/Users/DeLL/Desktop/re_pr.png',dpi = 300)

tmap_mode("view")
Figure5_sc_pr <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5)+
  tm_shape(Airbnb_gwr_private) +
  tm_dots(col = "coef_review_scores_rating_private", 
          palette = "RdYlBu",
          size = 0.1,
          alpha = 0.5)+
  tm_layout(title = "The review scores rating coeficient of Private room", title.position = c("left","top"),title.size=3)
Figure5_sc_pr
tmap_save(Figure5_sc_pr, 'C:/Users/DeLL/Desktop/sc_pr.png',dpi = 300)

tmap_mode("view")
Figure5_d_pr <- tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5)+
  tm_shape(Airbnb_gwr_private) +
  tm_dots(col = "coef_distance_private", 
          palette = "RdYlBu", 
          size =0.1,
          alpha = 0.5)+
  tm_layout(title = "The distance coeficient of Private room", title.position = c("left","top"),title.size=3)
Figure5_d_pr
tmap_save(Figure5_d_pr, 'C:/Users/DeLL/Desktop/d.png',dpi = 300)

