##------------------------------------------------------------------------------##
##                    VISUALIZING DINOSAUR SPECIES OVER TIME                    ##
##------------------------------------------------------------------------------##


## R version 3.5.3 (2019-03-11)

## Author: Lisa Hehnke (dataplanes.org | @DataPlanes)


#-------#
# Setup #
#-------#

# Install and load packages using pacman
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(emoGG, extrafont, gganimate, magrittr, tidyverse)
loadfonts(device = "win")

# Import data
dino_df <- read.csv("pbdb_data.csv", skip = 21, header = TRUE, stringsAsFactors = FALSE)


#-----------#
# Set theme #
#-----------#

## Color scheme adapted from https://dinosaurstew.com/color-palettes/ (stormy seas palette).
dino_theme <- theme(axis.line = element_blank(), 
                   axis.text.x = element_text(colour = "darkgreen", family = "Open Sans"), 
                   axis.text.y = element_text(colour = "darkgreen", family = "Open Sans"), 
                   axis.ticks = element_blank(), 
                   axis.ticks.length = unit(0, "pt"),
                   axis.title.x = element_text(colour = "#484745", family = "Palatino Linotype", size = 12),
                   axis.title.y = element_text(colour = "#484745", family = "Palatino Linotype", size = 12),
                   legend.position = "none", 
                   panel.background = element_blank(),
                   panel.border = element_blank(), 
                   panel.grid.major = element_line(colour = "#eaead6"), 
                   panel.grid.minor = element_line(colour = "#eaead6"), 
                   plot.background = element_rect(fill = "#ccc7ad", colour = NA), 
                   plot.caption = element_text(colour = "#eaead6", family = "Palatino Linotype", size = 12), 
                   plot.margin = margin(5.5, 40, 5.5, 5.5),
                   plot.subtitle = element_text(colour = "#484745", family = "Palatino Linotype"), 
                   plot.title = element_text(face = "bold", colour = "#484745", size = 16, family = "Palatino Linotype"))


#--------------#
# Prepare data #
#--------------#

# Prepare data
dino_df %<>% 
  rename(species = accepted_name, begin = max_ma, end = min_ma) %>%
  mutate_at(vars(begin, end), funs(round(.))) %>%
  mutate(years = Map(seq, begin, end, -1)) %>%
  select(species, years) %>%
  unnest(years) %>%
  distinct() %>%
  group_by(years) %>%
  dplyr::count() %>%
  rename(Year = years, Count = n)

# Plot animated timeline
dino_timeline <- ggplot(dino_df, aes(x = Year, y = Count)) +
  geom_line(col = "darkgreen", size = 1.2) +  
  geom_emoji(emoji = "1f996") +
  geom_emoji(data = dino_df %>% filter(Year == 247), emoji = "1f95a") +
  geom_emoji(data = dino_df %>% filter(Year == 62), emoji = "1f525") +
  scale_x_reverse() + 
  transition_reveal(rev(Year)) + 
  labs(title = "Number of dinosaur species over time", subtitle = " ", 
       x = "Age in millions of years (Ma)", y = "Species count", caption = "Data source: \nPaleobiology Database") + 
  theme_minimal() + dino_theme

# Save GIF
anim_save("dino_timeline.gif", dino_timeline)