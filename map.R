setwd("~/Desktop/Lectures/Arabic20B_finalproject")

library(tidyverse)
library(scico)
library(sf)
library(ggtext)
library(ggstar)
library(cowplot)


cafes = read_csv('adeni_chai_bay_area.csv') %>%
  mutate(chain = case_when(name == "Heyma Yemeni Coffee" ~"'Heyma' قهوة يمنية ",
                           name == "Sana'a Cafe" ~ "'Sana'a' مقهوى",
                           name == "Delah Coffee" ~ "'Delah' بيت قهوة",
                           name == "Qamaria Yemeni Coffee Co." ~ "'Qamaria' قهوة يمنية ",
                           TRUE ~ "مقهى يمنية أُخرى"))

cafes_sf = cafes %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

cafe_count = cafes %>%
  group_by(chain) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

coastline = sf::st_read('region_water_area.shp') %>%
  sf::st_crop(sf::st_buffer(cafes_sf, 5000)) %>%
  sf::st_union()

box = sf::st_bbox(coastline)  %>%
  sf::st_as_sfc() 

land = sf::st_difference(box, coastline)[2] %>%
  sf::st_cast("POLYGON")

land = land[4:48] %>%
  sf::st_as_sf()

ocean_color = '#B8CCE0'
land_color = '#E8E2D4'

cafes$chain = factor(cafes$chain, levels = rev(cafe_count$chain))


text1 = "مع ستة دكاكين\nمقهوى «صَنْعَاء» أكبر\nعددً الأماكناً"
text2 = "يُوجَد  أكبر أعداد المقاهي\nفي شرق المنطقة"

ggplot() + theme_void() +
  geom_sf(data = box, fill = ocean_color) +
  geom_sf(data = coastline, fill = ocean_color, color = NA) +
  geom_sf(data = land, fill = land_color, color = 'black', linewidth = .1) +
  ggstar::geom_star(data = cafes, aes(x = longitude, y = latitude,
                                      starshape = chain, fill = chain),
                    color = 'black', size = 2) +
  scale_fill_scico_d(palette = 'lajolla', begin = .1, end = .9,
                     name = " اسم شركة مقهى") +
  ggstar::scale_starshape_manual(values = c(5, 1, 14, 2, 4),
                                 name = " اسم شركة مقهى") +
  annotate("curve",
           x = -122.2, y = 37.39,
           xend = -122.1, yend = 37.38,
           curvature = 0.25, linewidth = 0.4, color = "black",
           arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  annotate("text",
           x = -122.3, y = 37.45,
           label = text1, hjust = 0.5, vjust = 1, size = 3.5, fontface = "bold", color = "black") +
  annotate("curve",
           x = -122.05, y = 37.65,
           xend = -122.2, yend = 37.77,
           curvature = 0.2, linewidth = 0.4, color = "black",
           arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  annotate("text",
           x = -122.1, y = 37.6,
           label = text2, hjust = 0, vjust = 0, size = 3.5, fontface = "bold", color = "black") +
  labs(title = "مقاهي يمنية في منطقة خليج",
       subtitle = "طريقة: أماكن قد  بُحثت في «جوجل مابس» و تُناولت و قُدمت في اللعة «R»") +
  theme(plot.title = ggplot2::element_text(size = 20),
        legend.position = c(.8, .8),
        legend.background = element_rect(fill = 'white', color = 'black', linewidth = 0.5),
        legend.box.background = element_rect(fill = 'white', color = 'black', linewidth = 0.25),
        legend.title = element_text(margin = margin(b = 6)))


ggsave('extra_layritz.png', width = 6, bg = 'white')
