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






