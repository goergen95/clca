library(sf)
library(gdalcubes)
library(magrittr)




raster_cube(col, view) %>%
  zonal_statistics(aoi, "mean(precipitation)", as_stars = TRUE) -> result2
result2 = as.vector(result2$precipitation_mean)
y = ts(result2, start = c(1981,1), end = c(2019,12), frequency = 12)
ddata = decompose(y)
yy = ddata$trend
trd = Trend(yy, method = "SeasonalAdjusted",  funSeasonalCycle=MeanSeasonalCycle, mosum.pval = 0.01)
plot(trd)
trd

dates = seq(as.Date("1981-01-01"), as.Date("2019-12-31"), by = "months")
data = data.frame(anom = result, date = dates)
data %<>%
  mutate(sign =  ifelse(anom > 0, "pos", "neg"),
         year = str_sub(date, 1, 4)) 

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(forecast)
library(pastecs)
library(greenbrown)
data_y %>%  
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  ggplot(aes(date, anom, fill = sign)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-100, 100, 20)) +
  scale_fill_manual(values = c("#99000d", "#034e7b")) +
  labs(y = "Precipitation anomaly (mm)", x = "") +
  theme_classic()


file = "data/wdpa/bouhedma_stats.gpkg"
layers = rgdal::ogrListLayers(file)
layers = layers[grep("attr_", layers)]

data = lapply(layers, function(x) st_read(file, layer = x, quite = T))
data = do.call(rbind, data)
data$date =seq(as.Date("2000-01-01"), as.Date("2019-12-31"), by = "months")
data = as_tibble(data)
saveRDS(data, "docs/assets/data/spei.rds")

data %<>%
  mutate(year = str_sub(date, 1, 4)) 

data %>%
  as_tibble() %>%
  gather(scale, value, -date, -year) %>%
  separate(scale, into = c("var", "para"), sep = "_") %>%
  separate(var, into = c("var", "scale"), sep = "m") %>%
  dplyr::select(-para) %>%
  mutate(cap = paste0("SPEI - Scale of ", scale, " months"),
         scale = as.numeric(scale)) %>%
  mutate(cap = fct_reorder(cap, scale, min)) %>%
  mutate(sign =  ifelse(value > 0, "pos", "neg")) %>%
  ggplot(aes(date, value, fill = sign)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-10, 10, 1)) +
  scale_fill_manual(values = c("#99000d", "#034e7b")) +
  labs(y = "Precipitation anomaly (%)", x = "") +
  theme_classic() +
  facet_wrap(~cap, nrow = 4)

files = list.files("data/nbwp/", full.names = T)
dates = seq(as.Date("2010-01-01"), as.Date("2018-01-01"), by = "year")
col = create_image_collection(files, date_time =dates, band_names = c("S1", "S2"))

ext$t0 = "2010-01-01T00:00:00"
ext$t1 = "2018-01-01T00:00:00"
view = cube_view(extent = ext,
                 dx = 100, dy = 100, dt = "P1Y",
                 aggregation = "mean", resampling = "bilinear",
                 srs = "EPSG:32632")
nbwp_cube = raster_cube(col, view)
nbwp_cube %>%
  # select_bands("S1") %>%
  zonal_statistics(aoi, expr = c("mean(S1)", "mean(S2)"), as_stars = TRUE ) -> result
library(colorspace)
ndvi.col = function(n) {
  rev(sequential_hcl(n, "Green-Yellow"))
}

# nbwp_cube %>%
#   select_bands("S1") %>%
#   animate(col = ndvi.col)

s1 = as.vector(result$S1_mean)
s2 = as.vector(result$S2_mean)

data = data.frame(s1 = s1, s2 = s2, year = 2010:2018)
data %<>%
  gather(season, value, - year)

ggplot(data)+
  geom_bar(aes(y = value,x=year, fill = season, group = season), stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("darkseagreen", "darkolivegreen4")) +
  labs(y="Net Biomass Water Productivity (kg/m³)") +
  theme_classic()


files = list.files("data/wapor/L2_TBP_S/", full.names = T)
dates = seq(as.Date("2010-01-01"), as.Date("2018-01-01"), by = "year")
col = create_image_collection(files, format = "data/wapor/tbp.json")

ext$t0 = "2009-01-01T00:00:00"
ext$t1 = "2019-01-01T00:00:00"
view = cube_view(extent = ext,
                 dx = 100, dy = 100, dt = "P1Y",
                 aggregation = "mean", resampling = "bilinear",
                 srs = "EPSG:32632")
tbp_cube = raster_cube(col, view, mask = image_mask("tbp_s1", values = c(-9999, -1)))
tbp_cube %>%
  select_bands("tbp_s1") %>%
  zonal_statistics(aoi, expr = c("mean(tbp_s1)"), as_stars = TRUE ) -> result
s1 = as.vector(result$tbp_s1_mean)

tbp_cube = raster_cube(col, view, mask = image_mask("tbp_s2",  values = c(-9999, -1)))
tbp_cube %>%
  select_bands("tbp_s2") %>%
  zonal_statistics(aoi, expr = c("mean(tbp_s2)"), as_stars = TRUE ) -> result
s2 = as.vector(result$tbp_s2_mean)

data = data.frame(s1 = s1, s2 = s2, year = 2009:2019)
data %<>%
  gather(season, value, - year)

ggplot(data)+
  geom_bar(aes(y=value, x=year, fill = season, group = season), stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("darkseagreen", "darkolivegreen4")) +
  labs(y="Total Biomass Production (kg/ha)") +
  theme_classic()



aoi = st_read("data/wdpa/bouhedma.gpkg")
aoi = st_transform(aoi, st_crs("EPSG:32632"))
aoi = st_buffer(aoi, dist = 50000)
bbox = st_bbox(aoi)


files = list.files("data/chirps/", pattern =".tif$", full.names = T)
col = create_image_collection(files, format = "CHIRPS_v2_0_monthly_p05_tif")

ext = extent(col)
ext$left = bbox[1]
ext$right = bbox[3]
ext$top = bbox[4]
ext$bottom = bbox[2]
ext$t1 = "2019-12-31T00:00:00"

view = cube_view(extent = ext,
                 dx = 5000, dy = 5000, dt = "P1Y",
                 aggregation = "sum", resampling = "bilinear",
                 srs = "EPSG:32632")

raster_cube(col, view) %>%
  apply_time(names= "anomalie", FUN = function(x){
    y = x["precipitation",]
    result = rep(NA, length(y))
    z = matrix(y[1:30], ncol = 1, byrow = TRUE)
    z = colMeans(z)
    z = rep(z, (length(y)))
    result[] = y - z
    return(result)
  }) %>%
  zonal_statistics(aoi, "mean(anomalie)", as_stars = TRUE) -> result
result = as.vector(result$anomalie_mean)


library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(forecast)
library(pastecs)
library(greenbrown)
library(stringr)

raster_cube(col, view) %>%
  zonal_statistics(aoi, "mean(precipitation)", as_stars = TRUE) -> result2
result2 = as.vector(result2$precipitation_mean)
y = ts(result2, start = c(1981,1), end = c(2019,1), frequency = 1)
ddata = decompose(y, type = "add")
yy = ddata$trend
trd = Trend(y, method = "SeasonalAdjusted",  funSeasonalCycle=MeanSeasonalCycle, mosum.pval = 0.01)
plot(trd)
trd

dates = seq(as.Date("1981-01-01"), as.Date("2019-12-31"), by = "years")
data = data.frame(anom = result, date = dates)
data %<>%
  mutate(sign =  ifelse(anom > 0, "pos", "neg"),
         year = str_sub(date, 1, 4)) 
data %>%  
  #filter(year %in% 1981:2019) %>%
  ggplot(aes(date, anom, fill = sign)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-100, 100, 20)) +
  scale_fill_manual(values = c("#99000d", "#034e7b")) +
  labs(y = "Precipitation anomaly (mm)", x = "") +
  theme_classic()
