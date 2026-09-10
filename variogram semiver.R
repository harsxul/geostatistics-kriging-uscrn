

# ──────────────────────────────────────────────────────────────
# 0 ▸ libraries  (install once if needed)
# ──────────────────────────────────────────────────────────────
libs <- c(
  "readr", "dplyr", "sp", "fields", "gstat",
  "latticeExtra", "plotly", "viridis"
)
invisible(lapply(libs, \(pkg)
                 if (!require(pkg, character.only = TRUE)) install.packages(pkg)))

# ──────────────────────────────────────────────────────────────
# 1 ▸ read & prep the USCRN file  (change path if needed)
# ──────────────────────────────────────────────────────────────
crn <- read_csv("USCRN_2015_airtemp.csv",
                show_col_types = FALSE) |>
  select(LONGITUDE, LATITUDE, T_DAILY_MEAN, DATE) |>
  mutate(
    T_DAILY_MEAN = as.numeric(T_DAILY_MEAN),
    DATE         = as.Date(DATE)
  ) |>
  filter(!is.na(T_DAILY_MEAN))

# ▶ choose a single day (same as your TPS example)
crn_day <- filter(crn, DATE == as.Date("2015-01-01"))

# convert to sp object
coordinates(crn_day) <- ~ LONGITUDE + LATITUDE
proj4string(crn_day) <- CRS("+proj=longlat +datum=WGS84")

# ──────────────────────────────────────────────────────────────
# 2 ▸ thin-plate spline surface  (fields::Tps)
# ──────────────────────────────────────────────────────────────
tps_mod <- Tps(coordinates(crn_day), crn_day$T_DAILY_MEAN)

# build a prediction grid (0.25° lat/long resolution)
bb   <- bbox(crn_day)
grd  <- expand.grid(
  LONGITUDE = seq(bb[1,1], bb[1,2], by = 0.25),
  LATITUDE  = seq(bb[2,1], bb[2,2], by = 0.25)
)
pred <- predict.Krig(tps_mod, as.matrix(grd))

# wrap in SpatialPixelsDataFrame for easy plotting
grd_sp  <- SpatialPixelsDataFrame(points = grd,
                                  data   = data.frame(pred = pred))

# ──────────────────────────────────────────────────────────────
# 3 ▸ visualise: TPS raster + station dots
# ──────────────────────────────────────────────────────────────
cols <- viridis(256)
plt1 <- spplot(grd_sp, "pred",
               at   = pretty(pred, 50),
               col.regions = cols,
               main = "TPS Surface • 1 Jan 2015 (°C)") +
  layer(sp.points(crn_day,
                  col = "black", pch = 20, cex = 0.6))

print(plt1)   # shows in RStudio viewer

# ──────────────────────────────────────────────────────────────
# 4 ▸ omnidirectional & directional variograms
# ──────────────────────────────────────────────────────────────
# log-transform to stabilise variance (as in meuse demo)
crn_day$logT <- log(crn_day$T_DAILY_MEAN - min(crn_day$T_DAILY_MEAN) + 1)

vg      <- variogram(logT ~ 1, crn_day, cloud = FALSE)
vg_det  <- variogram(logT ~ LONGITUDE + LATITUDE, crn_day, cloud = FALSE)
vg_dir  <- variogram(logT ~ 1, crn_day,
                     alpha = c(0, 45, 90, 135), cloud = FALSE)

# static lattice plots
print(plot(vg,      main = "Omnidirectional Variogram"))
print(plot(vg_det,  main = "Variogram after 1st-order Trend Removal"))
print(plot(vg_dir,  main = "Directional Variograms (0/45/90/135°)",
           col = "darkgreen", pch = 20))

# ──────────────────────────────────────────────────────────────
# 5 ▸ interactive side-by-side variograms
# ──────────────────────────────────────────────────────────────
plotly::plot_ly(type = "scatter", mode = "markers") |>
  add_trace(data = vg,     x = ~dist, y = ~gamma,
            name = "Omnidirectional") |>
  add_trace(data = vg_det, x = ~dist, y = ~gamma,
            name = "Detrended (1st-order)") |>
  layout(title  = "Variogram Comparison • 1 Jan 2015",
         xaxis  = list(title = "Lag distance (deg)"),
         yaxis  = list(title = "Semivariance γ(h)"))



# ───────────────────────────────────────────────────────────────
# 6 ▸ SAVE ALL THE FIGURES AS PDF
# ───────────────────────────────────────────────────────────────

## 6·1  TPS map  -------------------------------------------------
pdf("TPS_surface_2015-01-01.pdf", width = 7, height = 7)
print(plt1)                   # lattice / spplot object
dev.off()

## 6·2  Omnidirectional variogram  ------------------------------
pdf("variogram_omni.pdf", width = 6, height = 4)
plot(vg, main = "Omnidirectional Variogram")
dev.off()

## 6·3  Detrended variogram  ------------------------------------
pdf("variogram_detrended.pdf", width = 6, height = 4)
plot(vg_det, main = "Variogram after 1st-order Trend Removal")
dev.off()

## 6·4  Directional variograms  ---------------------------------
pdf("variogram_directional.pdf", width = 6, height = 4)
plot(vg_dir, main = "Directional Variograms (0° / 45° / 90° / 135°)",
     col = "darkgreen", pch = 20)
dev.off()

## 6·5  Interactive plotly variograms  --------------------------
# –– save as a self-contained HTML file ––
library(htmlwidgets)
htmlwidgets::saveWidget(
  as_widget(
    plotly::plot_ly(type = "scatter", mode = "markers") |>
      add_trace(data = vg,     x = ~dist, y = ~gamma,
                name = "Omnidirectional") |>
      add_trace(data = vg_det, x = ~dist, y = ~gamma,
                name = "Detrended (1st-order)") |>
      layout(title  = "Variogram Comparison • 1 Jan 2015",
             xaxis  = list(title = "Lag distance (deg)"),
             yaxis  = list(title = "Semivariance γ(h)"))
  ),
  file = "variogram_comparison.html",
  selfcontained = TRUE
)

