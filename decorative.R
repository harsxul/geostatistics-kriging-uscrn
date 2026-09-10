setwd("C:/Users/User/Downloads/Temp Research/")

# ------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------
libs <- c(
  "tidyverse",   # dplyr, ggplot2, readr, etc.
  "readxl",      # Excel import
  "sf",          # modern spatial data
  "rnaturalearth", "rnaturalearthdata",
  "ggspatial",   # scalebars / north arrows
  "viridis",     # colour-blind-friendly palettes
  "patchwork",   # combine multiple ggplots
  "ggrepel"      # intelligent point labels
)
invisible(lapply(libs, \(pkg) if (!require(pkg, character.only = TRUE)) install.packages(pkg)))

theme_set(
  theme_minimal(base_size = 14, base_family = "serif") +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold"),
      panel.grid.major = element_line(colour = "grey85"),
      panel.grid.minor = element_blank()
    )
)

options(dplyr.summarise.inform = FALSE)




### 1  Quick EDA -- Histogram • Box-&-Violin
# ──────────────────────────────────────────────────────────────
# Data import (Excel ⇒ tibble)
# ──────────────────────────────────────────────────────────────
temp_df <- read_excel("USCRN_2015_airtemp.xlsx") |>
  mutate(T_DAILY_MEAN = as.numeric(T_DAILY_MEAN))

# Histogram
g_hist <- ggplot(temp_df, aes(T_DAILY_MEAN)) +
  geom_histogram(bins = 40, fill = "steelblue", colour = "white") +
  geom_vline(aes(xintercept = mean(T_DAILY_MEAN, na.rm = TRUE)),
             linetype = "dashed", linewidth = .6) +
  labs(title = "Distribution of Daily Mean Temperature (2015)",
       x = "Temperature (°C)", y = "Count")

# Box-&-Violin (side-by-side)
g_violin <- ggplot(temp_df, aes(x = "", y = T_DAILY_MEAN)) +
  geom_violin(fill = "lightblue", width = .8, colour = NA, alpha = .6) +
  geom_boxplot(width = .15, outlier.shape = 21, outlier.size = 2,
               outlier.fill = "white", linewidth = .4) +
  labs(x = NULL, y = "Temperature (°C)")


library(patchwork)  

#  Combine the plots
library(patchwork)  

(g_hist | g_violin) +              # side-by-side layout
  plot_annotation(tag_levels = "A")

library(patchwork)   # make sure it’s attached

# ── 1 ▸ assemble the plot WITHOUT facet tags ─────────────────────
combo_plot <- (g_hist | g_violin)            # no plot_annotation()

# ── 2 ▸ write to file ─────────────────────────────────────────────
ggsave(
  filename = "hist_violin_combined.png",  # or .pdf, .svg, .eps …
  plot     = combo_plot,
  width    = 10,      # inches
  height   = 5,
  dpi      = 320      # 300–600 is typical for print; 96–120 for web
)

print(combo_plot) 

# export
ggsave("hist_violin_combined.png", combo_plot,
       width = 10, height = 5, dpi = 320)

ggsave("hist_violin_combined.pdf", combo_plot,
       width = 10, height = 5, device = cairo_pdf)


##Spatial Trend for 1 Jan 2015
# 2.1 Prepare data
full_df <- read_csv("USCRN_2015_airtemp.csv",
                    show_col_types = FALSE) |>
  mutate(DATE        = as.Date(DATE),
         T_DAILY_MEAN = as.numeric(T_DAILY_MEAN)) |>
  filter(DATE == as.Date("2015-01-01")) |>
  drop_na(LONGITUDE, LATITUDE, T_DAILY_MEAN)

# Recast as sf
stations_sf <- st_as_sf(full_df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# 2.2 Linear trend surface (longitude + latitude)

# ──────────────────────────────────────────────────────────────
# 1.  Make sure required namespaces are attached
# ──────────────────────────────────────────────────────────────
library(dplyr)      # for mutate(), etc.
library(ggplot2)    # plotting core
library(sf)         # spatial objects
library(viridis)    # colour scale
library(ggspatial)  # scalebar / north arrow
library(patchwork)  # (only if you’re combining plots)

# ──────────────────────────────────────────────────────────────
# 2.  Re-fit the linear trend (if it isn’t in memory)
#     full_df comes from your earlier filter for 2015-01-01
# ──────────────────────────────────────────────────────────────
mod <- lm(T_DAILY_MEAN ~ LONGITUDE + LATITUDE, data = full_df)

# ──────────────────────────────────────────────────────────────
# 3.  Build the prediction grid
# ──────────────────────────────────────────────────────────────
grid_sf <- st_make_grid(stations_sf, n = c(100, 100), what = "centers") |>
  st_as_sf() |>
  rename(geometry = x) |>
  mutate(
    LONGITUDE = st_coordinates(geometry)[, 1],
    LATITUDE  = st_coordinates(geometry)[, 2],
    pred      = predict(mod, newdata = cur_data())   # <- now ‘mod’ exists
  )

# ──────────────────────────────────────────────────────────────
# 4.  Base map (contiguous USA)
# ──────────────────────────────────────────────────────────────
usa_sf <- rnaturalearth::ne_countries(country = "United States of America",
                                      scale = "medium", returnclass = "sf") |>
  st_transform(4326)

# ──────────────────────────────────────────────────────────────
# 5.  Plot
# ──────────────────────────────────────────────────────────────
g_pred <- ggplot() +
  geom_sf(data = usa_sf, fill = "grey97", colour = "grey60") +
  geom_tile(data = grid_sf,
            aes(x = LONGITUDE, y = LATITUDE, fill = pred),
            alpha = .9) +
  scale_fill_viridis_c(option = "plasma", name = "°C") +   # <- new helper
  geom_sf(data = stations_sf, shape = 21, fill = "white",
          size = 2, stroke = .3) +
  ggspatial::annotation_scale(location = "bl") +
  ggspatial::annotation_north_arrow(location = "tl") +
  labs(title = "Predicted Daily Mean Temperature\n1 Jan 2015")

# 6.  Preview in RStudio viewer
print(g_pred)

# 7.  Export if happy
ggsave("predicted_surface_2015-01-01.png", g_pred,
       width = 7, height = 5, dpi = 320)


# ── Choose a planar CRS: US National Albers (EPSG 5070) ───────────
library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(ggspatial)

# ── 1 ▸ choose a planar CRS (US National Albers) ───────────────
crs_albers <- 5070   # EPSG code

stations_sf_proj <- st_transform(stations_sf, crs_albers) |>
  mutate(
    X = st_coordinates(.)[, 1],   # easting  (metres)
    Y = st_coordinates(.)[, 2]    # northing (metres)
  )

# ── 2 ▸ refit the model in projected units ─────────────────────
mod_proj <- lm(T_DAILY_MEAN ~ X + Y,
               data = stations_sf_proj |> st_drop_geometry())

# ── 3 ▸ build a prediction grid in the same CRS ────────────────
grid_proj <- st_make_grid(stations_sf_proj, n = c(100, 100),
                          what = "centers") |>
  st_as_sf() |>
  mutate(
    X    = st_coordinates(.)[, 1],
    Y    = st_coordinates(.)[, 2],
    pred = predict(mod_proj, newdata = data.frame(X, Y))
  )

# ── 4 ▸ re-project USA outline for context ─────────────────────
usa_sf_proj <- st_transform(usa_sf, crs_albers)

# ── 5 ▸ plot  ──────────────────────────────────────────────────
g_pred <- ggplot() +
  geom_sf(data = usa_sf_proj,
          fill = "grey97", colour = "grey60") +
  geom_tile(data = grid_proj,
            aes(x = X, y = Y, fill = pred)) +
  scale_fill_viridis_c(option = "plasma", name = "°C") +
  geom_sf(data = stations_sf_proj,
          shape = 21, fill = "white", size = 2, stroke = .3) +
  ggspatial::annotation_scale(location = "bl",            # accurate now
                              width_hint = 0.35) +
  ggspatial::annotation_north_arrow(location = "tl") +
  labs(title = "Predicted Daily Mean Temperature\n1 Jan 2015",
       x = NULL, y = NULL)

print(g_pred)    # preview in the RStudio Plots pane

# ── 6 ▸ export  ────────────────────────────────────────────────
ggsave("predicted_surface_2015-01-01.png", g_pred,
       width = 7, height = 5, dpi = 320)

ggsave("predicted_surface_2015-01-01.pdf", g_pred,
       width = 10, height = 5, device = cairo_pdf)








###############


library(ggplot2)
library(dplyr)
library(sf)
library(viridis)
library(ggspatial)

# ── 0 ▸ Pick CRS & re-project stations ──────────────────────────
crs_albers <- 5070
stations_sf_proj <- st_transform(stations_sf, crs_albers)

# add X / Y to the stations object  (matrix → columns)
coords_s <- st_coordinates(stations_sf_proj)
stations_sf_proj$X <- coords_s[, 1]
stations_sf_proj$Y <- coords_s[, 2]

# ── 1 ▸ Fit the model in projected units ───────────────────────
mod_proj <- lm(T_DAILY_MEAN ~ X + Y,
               data = stations_sf_proj |> st_drop_geometry())

# ── 2 ▸ Build a prediction grid in the SAME CRS ────────────────
grid_proj <- st_make_grid(stations_sf_proj, n = c(100, 100),
                          what = "centers") |>
  st_as_sf()

coords_g <- st_coordinates(grid_proj)
grid_proj$X <- coords_g[, 1]
grid_proj$Y <- coords_g[, 2]

grid_proj$pred <- predict(mod_proj,
                          newdata = grid_proj |> st_drop_geometry())

# ── 3 ▸ USA outline for context ────────────────────────────────
usa_sf_proj <- st_transform(usa_sf, crs_albers)

# ── 4 ▸ Draw the map ───────────────────────────────────────────
g_pred <- ggplot() +
  g_pred <- ggplot() +
  # subtle base fill (kept, but optional)
  geom_sf(data = usa_sf_proj, fill = "grey97", colour = "grey60") +
  
  # ⬇ second pass – border only
  geom_sf(data = usa_sf_proj, fill = NA,
          colour = "black", linewidth = 0.6) +
  
  geom_tile(data = grid_proj, aes(x = X, y = Y, fill = pred)) +
  scale_fill_viridis_c(option = "plasma", name = "°C") +
  geom_sf(data = stations_sf_proj,
          shape = 21, fill = "white", size = 2, stroke = .3) +
  ggspatial::annotation_north_arrow(location = "tl") +
  labs(title = "Predicted Daily Mean Temperature\n1 Jan 2015",
       x = NULL, y = NULL)



#######
library(rnaturalearth)

states_sf   <- ne_states(returnclass = "sf",
                         country = "United States of America") |>
  st_transform(crs_albers)

g_pred <- ggplot() +
  geom_sf(data = usa_sf_proj, fill = "grey97", colour = NA) +       # base fill
  geom_sf(data = states_sf, fill = NA, colour = "black", linewidth = 0.4) +
  geom_tile(data = grid_proj, aes(x = X, y = Y, fill = pred)) +
  scale_fill_viridis_c(option = "plasma", name = "°C") +
  geom_sf(data = stations_sf_proj,
          shape = 21, fill = "white", size = 2, stroke = .3) +
  ggspatial::annotation_north_arrow(location = "tl") +
  labs(title = "Predicted Daily Mean Temperature\n1 Jan 2015",
       x = NULL, y = NULL)





print(g_pred)   # preview in the Plots pane




# ── 5 ▸ Export ─────────────────────────────────────────────────
ggsave("predicted_surface_2015-01-01.pdf", g_pred,
       width = 10, height = 5, device = cairo_pdf)





#############################



if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  # needs remotes first:  install.packages("remotes")
  remotes::install_github("ropensci/rnaturalearthhires")
}



library(rnaturalearth)
library(sf)

# hi-res US state boundaries (just omit `scale =`)
states_sf <- ne_states(country      = "United States of America",
                       returnclass = "sf") |>
  st_transform(crs_albers)   # project to Albers

# now draw or re-draw the map
g_pred <- ggplot() +
  geom_tile(data = grid_proj, aes(x = X, y = Y, fill = pred)) +
  
  geom_sf(data = usa_sf_proj, fill = "grey97", colour = NA) +          # base fill
  geom_sf(data = usa_sf_proj, fill = NA, colour = "black", linewidth = 0.6) +  # national border
  geom_sf(data = states_sf,   fill = NA, colour = "black", linewidth = 0.3) +  # state lines
  
  scale_fill_viridis_c(option = "plasma", name = "°C") +
  geom_sf(data = stations_sf_proj, shape = 21, fill = "white", size = 2, stroke = .3) +
  labs(title = "Predicted Daily Mean Temperature\n1 Jan 2015",
       x = NULL, y = NULL)

print(g_pred)    # preview
ggsave("predicted_surface_2015-01-01.pdf", g_pred,
       width = 10, height = 5, device = cairo_pdf)
