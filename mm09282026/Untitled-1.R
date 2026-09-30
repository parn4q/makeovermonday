

library(ggplot2)
library(gganimate)
library(dplyr)
library(sf)

df = readxl::read_xls("./World-Population.xls")

world = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |> 
  mutate(iso_code = case_when(
    iso_a3 == "-99" | is.na(iso_a3) ~ adm0_a3,
    TRUE ~ iso_a3
  )) |>
  select(iso_code, country_name = name, geometry) |>
  st_transform(crs = "+proj=eqearth") |>
  st_cast("MULTIPOLYGON")

df_long = df |> 
  tidyr::pivot_longer(
    cols = `1960`:`2025`, 
    names_to = "year", 
    values_to = "population"
  ) |>
  mutate(year = as.integer(year))

label_points <- st_coordinates(st_point_on_surface(world))
world$label_x <- label_points[, 1]
world$label_y <- label_points[, 2]

df_merge = left_join(
  world,
  df_long, 
  by = join_by("iso_code" == "Country Code")#, "Country Name" == "sovereignt")
) |>  filter(!is.na(year) & !is.na(population))

# as.data.frame(df_merge) |> 
#   select(year, population) |> 
#   group_by(year) |> 
#   summarise(mean_pop = mean(log(population)), med_pop = median(log(population))) |>
#   tail(5)

# ggplot() + 
#   geom_histogram(data= df_merge, mapping = aes(x = log(population)), bins = 100) +
#   geom_vline(data = df_merge, mapping = aes(xintercept = mean(log(population)), color = 'red')) +
#   facet_wrap(~year)





anim_map <- ggplot() +
  geom_sf(
    data = df_merge, 
    mapping = aes(fill = population, geometry = geometry), 
    color = "white", 
    linewidth = 0.05
  ) +
  geom_text(
    data = df_merge,
    mapping = aes(
      x = label_x,
      y = label_y,  
      label = ifelse(population > 30000000, country_name, "")
    ),
    size = 1.6,
    color = "black",
    fontface = "bold"
  ) +
  scale_fill_viridis_c(
    trans = "log10",
    name = "Population",
    labels = scales::label_comma(),
    na.value = "grey85",
    option = "viridis"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 1, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.title = element_text(vjust = 0.85, face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Global Population Trend: {frame_time}",
    subtitle = "Data source: World Bank Development Indicators (1960–2025)",
    caption = "Global Population Trend from 1960 to 2025.  Countries' name will population if they have more than 30 million people."
  ) +
  transition_time(year)


n_years <- length(unique(df_long$year))

animated_gif <- animate(
  anim_map, 
  nframes = n_years * 2,  # 2 frames per year for smooth movement
  fps = 10,               # 10 frames per second
  width = 1200, 
  height = 650, 
  res = 100,
  renderer = gifski_renderer()
)

# Display in RStudio Viewer
animated_gif

anim_save("./world_pop.gif", animation = animated_gif)
