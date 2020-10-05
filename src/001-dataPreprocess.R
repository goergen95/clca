library(gdalcubes)
library(stringr)
library(magrittr)
library(dplyr)
library(tidyr)
library(sf)
library(lubridate)

# read in buffered polygon
aoi = st_read("data/wdpa/bouhedma_buffer.geojson")
aoi = st_transform(aoi, st_crs("EPSG:32632"))
bbox = st_bbox(aoi)

# create the image collection
files = list.files("data/chirps/", pattern =".tif$", full.names = T)
col = create_image_collection(files, format = "CHIRPS_v2_0_monthly_p05_tif")
gdalcubes_options(threads = 4, debug = T)
# make custom extent
ext = extent(col)
ext$left = bbox[1]
ext$right = bbox[3]
ext$top = bbox[4]
ext$bottom = bbox[2]
ext$t1 = "2019-12-31T00:00:00"
# create cube view
view_monthly = cube_view(extent = ext,
                         dx = 5000, dy = 5000, dt = "P1M",
                         aggregation = "mean", resampling = "bilinear",
                         srs = "EPSG:32632")

# calculate absolute precipitation data monthly
raster_cube(col, view_monthly) %>%
  zonal_statistics(aoi, "mean(precipitation)", as_stars = TRUE) -> prec_m
data_m = data.frame(date = seq(as.Date("1981-01-01"), as.Date("2019-12-31"), by = "month"),
                    prec =  as.vector(prec_m$precipitation_mean))

# calculate monthly anomalies
raster_cube(col, view_monthly) %>%
  apply_time(names= "anomalie", FUN = function(x){
    y = x["precipitation",]
    result = rep(NA, length(y))
    z = matrix(y[1:360], ncol = 12, byrow = TRUE)
    z = colMeans(z)
    z = rep(z, (length(y)/12))
    result[] = y - z
    return(result)
  }) %>%
  zonal_statistics(aoi, "mean(anomalie)", as_stars = TRUE) -> anom_m
data_m$anom = as.vector(anom_m$anomalie_mean)

# calculate yearly statistics
data_m %>%
  as_tibble() %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(prec = sum(prec)) -> data_y
annual_mean = mean(data_y$prec[1:30])
data_y %<>%
  mutate(anom = prec - annual_mean)
# add encodings for positive and negative values
data_m %<>%
  mutate(sign =  ifelse(anom > 0, "pos", "neg"))
data_y %<>%
  mutate(sign =  ifelse(anom > 0, "pos", "neg"))

# save results to disk
dir.create("docs/assets/data", recursive = TRUE, showWarnings = F)
saveRDS(data_m, "docs/assets/data/prec_monthly.rds")
saveRDS(data_y, "docs/assets/data/prec_yearly.rds")


# calculating wapor datasets with raster

library(raster)
library(rgdal)

# calculate yearly transpiration using phenologie and Transpiration data
years = 2010:2018
dir.create("data/t", recursive = T, showWarnings = F)
for (year in years){
  print(year)
  search = paste0(year-1,"|",year,"|", year+1)
  trans_files = list.files("data/wapor/L2_T_D/", full.names = T)
  trans_files = trans_files[grep(search, trans_files)]
  trans = stack(trans_files)
  
  phe_files = list.files("data/wapor/L2_PHE_S/", full.names = T)
  phe_files = phe_files[grep(year, phe_files)]
  phe = stack(phe_files)
  names(phe) = c("S1_EOS", "S1_SOS", "S2_EOS", "S1_SOS")
  phe = reclassify(phe, rcl = matrix(data=c(109,+Inf, NA), byrow = T, ncol = 3))
  data = stack(trans, phe)

  
  # calculate seasonal aeti for all years
  myfun = function(x){
    if(is.na(x[109])){
      s1 = NA
    } else {
      s1 = sum(x[x[110]:x[109]])
    }
    if(is.na(x[111])){
      s2 = NA
    } else {
      s2 = sum(x[x[112]:x[111]])
    }
    return(c(s1, s2))
  }
  
  start1 = Sys.time()
  aeti2 = calc(data, fun = myfun, filename = paste0("data/t//t_",year, ".tif"), overwrite = T)
  end1 = Sys.time()
  print(end1 - start1)
  removeTmpFiles(h = 0)
}


# calculate seasonal net biomass water productivity
dir.create("data/nbwp", recursive = T, showWarnings = F)
aeti = list.files("data/t/", full.names = T)
aeti = lapply(aeti, function(x){
  x = brick(x)
  names(x) = c("S1", "S2")
  x 
})
names(aeti) = years

tbp = list.files("data/wapor/L2_TBP_S/", full.names = T)
tbp = lapply(years, function(y){
  x = stack(tbp[grep(y, tbp)])
  names(x) = c("S1", "S2")
  x[x<0] = NA
  x
})

for (l in 1:length(years)){
  s1 = tbp[[l]]$S1  / aeti[[l]]$S1
  s2 = tbp[[l]]$S2  / aeti[[l]]$S2
  s = stack(s1, s2)
  s[s==Inf] = NA
  writeRaster(s, filename = paste0("data/nbwp/nbwp_",years[l], ".tif"), 
              overwrite = T)
}

# extract NBWP and TBP values
# NBWP
files = list.files("data/nbwp/", full.names = T)
dates = seq(as.Date("2010-01-01"), as.Date("2018-01-01"), by = "year")
col = create_image_collection(files, date_time =dates, band_names = c("S1", "S2"))

ext$t0 = "2010-01-01T00:00:00"
ext$t1 = "2018-01-01T00:00:00"
view = cube_view(extent = ext,
                 dx = 100, dy = 100, dt = "P1Y",
                 aggregation = "mean", resampling = "bilinear",
                 srs = "EPSG:32632")
raster_cube(col, view) %>%
  # select_bands("S1") %>%
  zonal_statistics(aoi, expr = c("mean(S1)", "mean(S2)"), as_stars = TRUE ) -> data_nbwp

data_s1_nbwp = data.frame(var = "NBWP", year = 2010:2018, season = "S1", value = as.vector(data_nbwp$S1_mean))
data_s2_nbwp = data.frame(var = "NBWP", year = 2010:2018,season = "S2", value = as.vector(data_nbwp$S2_mean))
data_nbwp = rbind(data_s1_nbwp,data_s2_nbwp)
saveRDS(data_nbwp, "docs/assets/data/nbwp.rds")

# TBP
files = list.files("data/wapor/L2_TBP_S/", full.names = T)
col = create_image_collection(files, format = "data/wapor/tbp.json")

ext$t0 = "2009-01-01T00:00:00"
ext$t1 = "2019-01-01T00:00:00"
view = cube_view(extent = ext,
                 dx = 100, dy = 100, dt = "P1Y",
                 aggregation = "mean", resampling = "near",
                 srs = "EPSG:32632")
raster_cube(col, view, mask = image_mask("tbp_s1", values = -9999:-1)) %>%
  select_bands("tbp_s1") %>%
  zonal_statistics(aoi, expr = c("mean(tbp_s1)"), as_stars = TRUE ) -> tbp_s1
tbp_s1 = as.vector(tbp_s1$tbp_s1_mean)

raster_cube(col, view, mask = image_mask("tbp_s2",  values = -9999:-1)) %>%
  select_bands("tbp_s2") %>%
  zonal_statistics(aoi, expr = c("mean(tbp_s2)"), as_stars = TRUE ) -> tbp_s2
tbp_s2 = as.vector(tbp_s2$tbp_s2_mean)

data_s1_tbp = data.frame(var = "TBP", year = 2009:2019, season = "S1", value = tbp_s1)
data_s2_tbp = data.frame(var = "TBP", year = 2009:2019, season = "S2", value = tbp_s2)
data_tbp = rbind(data_s1_tbp, data_s2_tbp)
saveRDS(data_tbp, "docs/assets/data/tbp.rds")

# MODIS calculation