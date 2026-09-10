## ------------------------------------------------------------
## 0.  House-keeping
## ------------------------------------------------------------
# Recommended modern spatial stack
library(readxl)      # read Excel
library(dplyr)       # data wrangling
library(lubridate)   # date handling
library(sf)          # simple-features
library(gstat)       # variograms & kriging
library(sp)          # gstat still speaks sp
library(raster)      # to build a prediction grid
library(lattice)     # for spplot

setwd("C:/Users/User/Downloads/R QGIS/R GNR630/")

## ------------------------------------------------------------
## 1.  Read & pre-process the data
## ------------------------------------------------------------
# adjust the file name / sheet as needed
df_raw <- read_excel("USCRN_2015_airtemp.xlsx", sheet = 1)


# Explicit namespace for every dplyr verb
df <- df_raw %>% 
  dplyr::mutate(
    DATE  = as.Date(DATE),
    year  = lubridate::year(DATE),
    month = lubridate::month(DATE),
    day   = lubridate::day(DATE)
  ) %>% 
  dplyr::filter(DATE == as.Date("2015-01-01")) %>% 
  dplyr::select(LONGITUDE, LATITUDE, T_DAILY_MEAN) %>% 
  tidyr::drop_na()



## ------------------------------------------------------------
## 2.  Convert to sf / sp and pick a *projected* CRS
##     (kriging expects planar distances)
## ------------------------------------------------------------
pts_sf <- st_as_sf(df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Albers Equal-Area for CONUS; if you’re Alaska-only, try EPSG:3338 instead
target_crs <- 5070
pts_sf <- st_transform(pts_sf, target_crs)

# Convert to sp for gstat
pts_sp <- as_Spatial(pts_sf)

## ------------------------------------------------------------
## 3.  Build a prediction grid covering the stations
## ------------------------------------------------------------
# simple square buffer around points
buf   <- st_buffer(pts_sf, dist = 50e3)    # 50-km buffer
extent <- as(raster::extent(as_Spatial(st_union(buf))), "SpatialPolygons")
proj4string(extent) <- CRS(st_crs(pts_sf)$proj4string)





library(raster)   # for raster() & extent()
library(sp)       # for SpatialPixels*, gridded()

## 1. Build a blank raster at 10-km resolution ------------------------------
# (Assuming you already have an `extent` object called `ext`)
grd_r <- raster(ext, res = 10000)      # `ext` is your spatial extent

## 2. Convert to a SpatialPixels* object ------------------------------------
grd_sp <- as(grd_r, "SpatialPixels")   # or "SpatialPixelsDataFrame"
gridded(grd_sp) <- TRUE                # mark it explicitly as a grid

## 3. (Optional) add an empty data slot if you need a SpatialPixelsDataFrame
grd_spdf <- SpatialPixelsDataFrame(grd_sp, data = data.frame(id = 1:length(grd_sp)))



# 10-km grid (adjust as you like)
grd <- raster(extent, res = 10000)

# (continuing after you created `grd <- raster(extent, res = 10000)`)

library(sp)      # be sure sp is loaded

grd <- as(grd, "SpatialPixels")   # RasterLayer  -> SpatialPixels
gridded(grd) <- TRUE              # flag it as gridded
grd <- as(grd, "SpatialGrid")     # optional, if krige() prefers SpatialGrid


grd <- as.gridded(SpatialPixels(grd))
grd <- as(grd, "SpatialGrid")
gridded(grd) <- TRUE




## 10-km prediction grid -------------------------------------------
grd <- raster(extent, res = 10000)     # RasterLayer
grd <- as(grd, "SpatialPixels")        # → SpatialPixels
gridded(grd) <- TRUE                   # tell sp it’s gridded
grd <- as(grd, "SpatialGrid")          # → SpatialGrid (optional but tidy)

# quick sanity-check
class(grd)
## [1] "SpatialGrid"         "Spatial"






## ------------------------------------------------------------
## 3.  Build a prediction grid covering the stations
## ------------------------------------------------------------

# 3-A  Choose a projected CRS   --------------------------------
#      (skip if you've already transformed pts_sf earlier)
# target_crs <- 5070                 # CONUS Albers
# pts_sf     <- st_transform(pts_sf, target_crs)

# 3-B  Create a buffer around the outermost stations -----------
#      Adjust `dist` to widen / tighten the study envelope.
buf_dist <- 50 * 1000               # 50 km
buf      <- st_buffer(pts_sf, dist = buf_dist)

# 3-C  Build a simple rectangular extent -----------------------
#      If you prefer a concave hull or Voronoi envelope,
#      replace this with st_convex_hull() / st_union() etc.
ext_poly <- st_as_sf(st_bbox(buf))   # bbox → polygon
ext_sp   <- as_Spatial(ext_poly)     # sf → sp

# 3-D  Turn the extent into a raster template ------------------
#      `res` controls grid resolution (here: 10 km = 10 000 m)
grd_rst <- raster::raster(ext_sp, res = 10_000)

# 3-E  Convert RasterLayer → SpatialGrid for gstat ------------
grd_sp  <- as(grd_rst, "SpatialPixels")  # RasterLayer → SpatialPixels
gridded(grd_sp) <- TRUE                  # flag it as gridded
grd_sp  <- as(grd_sp, "SpatialGrid")     # SpatialPixels → SpatialGrid

# quick sanity-check
print(grd_sp)
#> class       : SpatialGrid
#> dimensions  : nrow x ncol, total cells
#> resolution  : 10000, 10000  (units: m)
#> extent      : …             (projected CRS units)
#> CRS         : +proj=aea …   (whatever target_crs is)

# 3-F  (Optional) Visual check with lattice --------------------
sp::spplot(grd_sp, col.regions = grey.colors(2),
           main = "Prediction grid (10-km cells)")








## ------------------------------------------------------------
## 4.  Empirical variogram
## ------------------------------------------------------------
vg_emp <- variogram(T_DAILY_MEAN ~ 1, pts_sp, cutoff = 2e6)  # cutoff ~ 2000 km
plot(vg_emp, plot.numbers = TRUE)

## ------------------------------------------------------------
## 5.  Variogram model fitting
## ------------------------------------------------------------
# quick spherical starter; tweak if needed
vg_mod0 <- vgm(psill = 20, model = "Sph", range = 500e3, nugget = 5)
vg_fit  <- fit.variogram(vg_emp, vg_mod0)
plot(vg_emp, vg_fit, plot.numbers = TRUE)

## ------------------------------------------------------------
## 6.  Ordinary kriging
## ------------------------------------------------------------
ok_pred <- krige(
  T_DAILY_MEAN ~ 1,
  locations = pts_sp,
  newdata   = grd,
  model     = vg_fit
)

## ------------------------------------------------------------
## 7.  Visualise
## ------------------------------------------------------------
# prediction
spplot(ok_pred, "var1.pred",
       main = "USCRN 1 Jan 2015 – T_DAILY_MEAN (°C)\nOrdinary Kriging",
       col.regions = rev(terrain.colors(100)),
       sp.layout = list("sp.points", pts_sp, pch = 20, col = "black")
)

# kriging variance
spplot(ok_pred, "var1.var",
       main = "Kriging variance (uncertainty)",
       col.regions = rev(heat.colors(100)),
       sp.layout = list("sp.points", pts_sp, pch = 20, col = "black")
)
