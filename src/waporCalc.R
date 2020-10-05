library(raster)
library(rgdal)

years = 2010:2018
for (year in years){
  print(year)
  
  search = paste0(year-1,"|",year,"|", year+1)
  aeti_files = list.files("data/wapor/L2_T_D/", full.names = T)
  aeti_files = aeti_files[grep(search, aeti_files)]
  aeti = stack(aeti_files)
  
  phe_files = list.files("data/wapor/L2_PHE_S/", full.names = T)
  phe_files = phe_files[grep(year, phe_files)]
  phe = stack(phe_files)
  names(phe) = c("S1_EOS", "S1_SOS", "S2_EOS", "S1_SOS")
  phe = reclassify(phe, rcl = matrix(data=c(109,+Inf, NA), byrow = T, ncol = 3))
  data = stack(aeti, phe)
  # aoi = readOGR("tunisia_test.shp")
  # data = crop(data, aoi)
  
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
  aeti2 = calc(data, fun = myfun, filename = paste0("results/t/t_",year, ".tif"), overwrite = T)
  end1 = Sys.time()
  print(end1 - start1)
  removeTmpFiles(h = 0)
}


aeti = list.files("results/t/", full.names = T)
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
  writeRaster(s, filename = paste0("results/nbwp/nbwp_",years[l], ".tif"), 
              overwrite = T)
}

