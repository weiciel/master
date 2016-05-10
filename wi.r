# set working directory
setwd("/Users/JackyHuang/Desktop/paper/newclimate/cmip5/")

# load required libraries
library(raster)
library(rgdal)
files = list.files(, pattern = "tif$", full.names = TRUE, recursive = TRUE)
nreserve= readOGR("../yushan NP/", 'Yushan_np')

#tw_bc85tx7012

RCP = c("26", "45", "60", "85")
fac = c("tx", "tn")
yer = c("50", "70")
# as. character 變成字串 處理 ﹍9 的問題
mon = as.character(1:12)

combn = apply(expand.grid(RCP, "(tx|tn)", yer, paste0(mon, ".tif")), 1, paste, collapse = "")
#第一迴圈 照TX TN分開
for (pattern in combn) {
  file = grep(pattern, files, value = TRUE)
  
  tx = grep("tx", file, value = TRUE)
  tn = grep("tn", file, value = TRUE)
  #第二迴圈 處理GCM
  for (iter in 1:11) {
    #切玉山
    tx_layer = mask(intersect(raster(tx[iter]), nreserve), nreserve)
    #指定（=）
    assign(letters[iter], tx_layer)
    
    #切玉山
    tn_layer = mask(intersect(raster(tn[iter]), nreserve), nreserve)
    #指定（=）
    assign(letters[iter + 11], tn_layer)
  }
  #平均GCM
  tx_result = sum(a, b, c, d, e, f, g, h, i, j, k) / 11
  tn_result = sum(l, m, n, o, p, q, r, s, t, u, v) / 11
  #月均溫
  result = (tx_result + tn_result) / 2
  #出圖
  writeRaster(result, sprintf("./step2/%s.tif", gsub("\\D", "", file[1])),overwrite=TRUE)
  plot(result)
}


#-----------------------------------

#4.算wi  Σ(12個月的月均溫 - 5) 
combn2 = apply(expand.grid(RCP, yer), 1, paste, collapse = "")
lemon = list.files("./step2", "tif$", full.names = TRUE)

for (pattern in combn2) {
  vaper = grep(pattern, combn2, value = TRUE)  
  strawberry = grep(vaper, lemon, value = TRUE)
  for (iter in 1:12) {
    blueberry = raster(strawberry[iter])
    blueberry[blueberry < 0] = 0
    assign(paste0('a',letters[iter]), blueberry)
  }
  blackberry = sum(aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al)/12 
  plot(blackberry)
  #出圖
  writeRaster(blackberry, sprintf("./step3/%s.tif", pattern), overwrite=TRUE)
}


#4.算增幅
apples = list.files("./step3", "tif$", full.names = TRUE)
microsoft =  mask(intersect(raster("../../worldclim_wi/tw_wi.asc"), nreserve), nreserve)  
for (apple in apples) {
  bug = gsub("\\D", "", apple)
  jobs = raster(apple)
  money = jobs - microsoft
  
  #-----------
  #出圖
  #-----------
  
  #增幅
  
  brks = seq(0,250, by=50)
  nb = length(brks)-1 
  cols = terrain.colors(nb)
  plot(money, breaks=brks, col=cols, lab.breaks=brks)
  
  writeRaster(money, sprintf("./step4/%s.tif", bug), overwrite=TRUE)
  jpeg(sprintf("./step4/jpg/%s.jpg", bug))
  
  
  #比例
  
  brks5 = seq(0,3, by=0.2)
  nb5 = length(brks5)-1 
  cols5 = terrain.colors(nb5)
  plot(money/microsoft, breaks=brks5, col=cols5, lab.breaks=brks5)
  
  writeRaster(money/microsoft, sprintf("./step5/%s.tif", bug), overwrite=TRUE)
  jpeg(sprintf("./step5/jpg/%s.jpg", bug))
  
  
}

