#-------------------------------------------------------------------------------

library(dplyr)
library(purrr)
library(magick)
library(ggplot2)
library(gganimate)
library(abmi.themes)

root <- "G:/Shared drives/ABMI Camera Mammals/data/base/sample-images/"

moose_gif <- list.files(path = root, pattern = "*.jpg", full.names = TRUE) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps = 1) %>%
  image_write("./docs/moose1.gif")

img_length <- data.frame(seconds = c(0, 1,
                7,
                7,
                1,
                2,
                1,
                2,
                28,
                17,
                58,
                4,
                2,
                3,
                51,
                4,
                15,
                49,
                2),
                image = 1:19) %>%
  mutate(cumulative_time = cumsum(seconds))

p <- ggplot(data = img_length, aes(x = image, y = cumulative_time, group = 1)) +
  geom_point(color = "#2D415B", size = 4) +
  geom_line(color = "#2D415B", size = 1) +
  labs(y = "",
       x = "Image in series",
       title = "Cumulative time: {current_frame} seconds") +
  scale_x_continuous(breaks = seq(1, 18, 2)) +
  theme_abmi() +
  theme(panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16)) +
  transition_manual(frames = cumulative_time, cumulative = TRUE)

p

animate(p, nframes = 19, fps = 1)

anim_save("./docs/plot1.gif")

# Camera Locations

library(readr)
library(stringr)
library(sf)

cam_locations <- read_csv("S:/github-repos-data/SC-Camera-Mammals/data/lookup/camera-locations-public.csv")

bg <- cam_locations %>%
  filter(str_detect(Deployment, "BG"))

sf_bg <- st_as_sf(bg, coords = c("Public_Long", "Public_Lat"), crs = 4326)

st_write(sf_bg, "./data/bg_dep.shp")

cmu_dep <- read_csv("S:/github-repos-data/SC-Camera-Mammals/data/lookup/cmu-dep-abb.csv") %>%
  pull()

cmu <- cam_locations %>%
  filter(str_detect(Deployment, paste(cmu_dep, collapse = "|")))

sf_cmu <- st_as_sf(cmu, coords = c("Public_Long", "Public_Lat"), crs = 4326)

st_write(sf_cmu, "./data/cmu_dep.shp")

#-------------------------------------------------------------------------------

# Density

bg_density <- read_csv("G:/Shared drives/ABMI Camera Mammals/results/density/Marcus/all-density_2020-04-25.csv") %>%
  filter(str_detect(DeploymentYear, "BG"))

bg_deps <- sf_bg_selection %>%
  st_set_geometry(NULL) %>%
  select(deployment = Deployment, WMUNIT_NAM)

cmu_deps <- sf_cmu_selection %>%
  st_set_geometry(NULL) %>%
  select(deployment = Deployment, WMUNIT_NAM) %>%
  mutate(deployment = paste0("CMU", deployment))

bg_density_1 <- bg_density %>%
  filter(common_name == "Moose" | common_name == "Black Bear",
         seasonal > 20) %>%
  separate(DeploymentYear, into = c("deployment", "year"), sep = "_") %>%
  mutate(cpue_km2 = ifelse(cpue_km2 == "NaN", 0, cpue_km2)) %>%
  filter(cpue_km2 != Inf, !is.na(cpue_km2)) %>%
  group_by(deployment, common_name) %>%
  summarise(density = mean(cpue_km2)) %>%
  left_join(bg_deps, by = "deployment") %>%
  filter(!is.na(WMUNIT_NAM))

cmu_density <- read_csv("G:/Shared drives/ABMI Camera Mammals/results/density/Marcus/cmu-all-density_2020-04-17.csv")

cmu_density_1 <- cmu_density %>%
  filter(common_name == "Moose" | common_name == "Black Bear",
         seasonal > 20) %>%
  group_by(deployment, common_name) %>%
  summarise(density = mean(cpue_km2)) %>%
  filter(!is.na(density)) %>%
  mutate(deployment = str_remove_all(deployment, "-")) %>%
  left_join(cmu_deps, by = "deployment") %>%
  filter(!is.na(WMUNIT_NAM))

all <- density %>%
  st_set_geometry(NULL) %>%
  select(deployment, common_name, WMUNIT_NAM, density) %>%
  bind_rows(bg_density_1, cmu_density_1)

all_summary <- ace_summarise_dens(x = all,
                                 group_id = WMUNIT_NAM,
                                 agg_samp_per = TRUE,
                                 conflevel = 0.9) %>%
  mutate(projects = "ABMI, BG, and CMU")

summary <- summary %>%
  mutate(projects = "ABMI")

everything <- bind_rows(all_summary, summary)

ggplot(data = everything, aes(x = WMUNIT_NAM, y = density_avg,
                              ymin = density_lci_0.9, ymax = density_uci_0.9,
                              color = common_name)) +
  geom_errorbar(width = 0.3, size = 1.25) +
  geom_point(size = 4) +
  coord_flip() +
  facet_wrap(~ common_name, nrow = 4) +
  scale_color_abmi(palette = "main") +
  labs(x = "", y = expression(Density~(individuals~per~km^2)),
       title = "Cameras from: {closest_state}",
       subtitle = "Estimated density with 90% CI") +
  theme_abmi() +
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 13),
        plot.title = element_text(size = 16)) +
  transition_states(projects)

anim_save("./docs/plot2.gif")





