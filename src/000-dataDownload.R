loadandinstall = function(mypkg) {
  if (!is.element(mypkg, installed.packages()[,1]))
    install.packages(mypkg)
  library(mypkg, character.only = TRUE)}
libs = c("sf", "stringr", "rnaturalearth")
for (lib in libs) loadandinstall(lib)
dir.create("data/chirps", recursive = T, showWarnings = F)


# Downloading CHIRPS precipitation data set

if(!file.exists("data/chirps/chirps-v2.0.monthly.nc")){
  download.file("https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/netcdf/chirps-v2.0.monthly.nc",
                destfile = "data/chirps/chirps-v2.0.monthly.nc")
}

# convert nc file to GTiffs so analysis with gdalcubes is possible (see 001_analysis.R)
chirps_raster = brick("data/chirps/chirps-v2.0.monthly.nc")
dates = str_sub(names(chirps_raster), 2, 8) 
filenames = paste("data/chirps/chirps-v2.0.", dates, ".tif", sep = "")
writeRaster(chirps_raster, filename = filenames, bylayer = TRUE, overwrite =FALSEs)


# downloading wapor dataset

if(!"wapoR" %in% installed.packages()[,1]){
  devtools::install_github("goergen95/wapoR")
}
library(wapoR)

# get extent object for tunisia
tun = ne_countries(country = "Tunisia", returnclass = "sf")
tun = st_buffer(tun, 0.5)

# define products and respective dimensions
products = c("L2_T_D", "L2_PHE_S", "L2_TBP_S")
dimensions = list(L2_T_D = list(), 
                  L2_PHE_S = list(SEASON = c("S1", "S2"), STAGE = c("SOS", "EOS")), 
                  L2_TBP_S = list(SEASON = c("S1", "S2")))
# create outdir
lapply(products, function(x) dir.create(file.path("data/wapor/test",x), 
                                        recursive = T, showWarnings = F))

# read in extent polygon
aoi = st_read("data/wdpa/bouhedma.gpkg")
aoi = st_transform(aoi, st_crs("EPSG:32632"))
aoi = st_buffer(aoi, dist = 50000)
aoi = st_transform(aoi, st_crs(4326))
st_write(aoi, "data/wdpa/bouhedma_buffer.geojson")

# loop to download different products from WAPOR
for(product in products){
  print(paste0("Starting download for product ", product))
  dimension = dimensions[product]
  
  wapor_queryRaster(collection = "WAPOR_2",
                    product = product,
                    dimensions = dimension[[1]],
                    aoi = aoi,
                    begin = "2009-01-01",
                    end = "2020-01-01",
                    APIkey = Sys.getenv("wapor-key"), # you will need your own API key for WAPOR
                    outdir = file.path("data/wapor/test", product),
                    cutline = FALSE,
                    tiled = TRUE,
                    compressed = TRUE,
                    overviews = TRUE,
                    sleep_time = 10)
}

# MODIS download