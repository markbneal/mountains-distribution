# Modified to plot NZ volcanoes

library(sf)
library(rvest)
library(ggdist)
# remotes::install_github("hrbrmstr/hrbragg") # or use another theme
#‘ragg’, ‘textshaping’ causing dramas
# install.packages("textshaping") # these require installs on the server, https://harfbuzz.github.io/building.html
# install.packages("ragg")
library(hrbragg) # 
#remotes::install_github("yutannihilation/ggsflabel")
library(ggsflabel) # 
library(tidyverse)
library(stringi)
#install.packages("lares")
library(lares)
library(rgdal)

# this uses the R native pipe, so if folks are on older R versions, swap that out
# for the {magrittr} pipe.

# Ísland shapefile
# https://geodata.lib.berkeley.edu/download/file/stanford-dq155fd3935-shapefile.zip
# ice <- st_read("./stanford-dq155fd3935-shapefile/dq155fd3935.shp")

# NZ shapefile
# https://geodata.lib.berkeley.edu/download/file/stanford-db272jt5872-shapefile.zip
# Run in terminal to download zip
# curl -s https://geodata.lib.berkeley.edu/download/file/stanford-db272jt5872-shapefile.zip -o stanford-db272jt5872-shapefile.zip

# unzip("stanford-db272jt5872-shapefile.zip", exdir = "stanford-db272jt5872-shapefile")
nz <- st_read("./stanford-db272jt5872-shapefile/db272jt5872.shp")


# Ísland volcano geo information
# https://kmlexport.toolforge.org/?article=List_of_volcanoes_in_Iceland
# vol <- st_read("doc.kml")

# New Zealand volcano geo information
# curl -s https://kmlexport.toolforge.org/?article=List_of_volcanoes_in_New_Zealand -o doc.kml
vol <- st_read("doc.kml") # Bad news, multiple layers to extract
class(vol)

# Stackoverflow to the rescue
# https://gis.stackexchange.com/a/315451/158084
layers <- ogrListLayers("doc.kml")
# vol <- lapply(layers, function(i) readOGR("doc.kml", i)) #base R
vol <- map(layers, function(i) st_as_sf(readOGR("doc.kml", i))) #tidy, make list elements sf objects
# convert list of sf objects to one sf object
# https://stackoverflow.com/a/66026716/4927395
vol <- bind_rows(vol) 

ggplot(vol)+ # note some volcanoes are east of Longitude 180 degrees, and some well south
  geom_sf()

# Let's fix the points just over the dateline
# https://stackoverflow.com/questions/60879460/plotting-around-the-180-dateline-using-sf-and-ggplot2

vol <- st_shift_longitude(vol)
ggplot(vol)+ # note, still some well south
  geom_sf()

#actually, lets remove the Ross Dependency volcanoes near Antarctica
#
# filter with a function, keep latitude >-60
#https://stackoverflow.com/a/61025270/4927395

filter_sf <- function(.data, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL) {
  bb <- sf::st_bbox(.data)
  if (!is.null(xmin)) bb["xmin"] <- xmin
  if (!is.null(xmax)) bb["xmax"] <- xmax
  if (!is.null(ymin)) bb["ymin"] <- ymin
  if (!is.null(ymax)) bb["ymax"] <- ymax
  sf::st_filter(.data, sf::st_as_sfc(bb), .predicate = sf::st_within)
}

vol <- vol %>% filter_sf(ymin = -60)


ggplot(vol)+ # neater!
  geom_sf()

## Ísland volcano height info
# pg <- read_html("https://en.wikipedia.org/wiki/List_of_volcanoes_in_Iceland") 
# 
# html_table(pg)[[2]] |> 
#   select(name=Name, elev=2) |> 
#   filter(elev != "(m)") |> 
#   mutate(
#     elev = as.numeric(stri_replace_all_regex(elev, "[^[:digit:]]", ""))
#   ) |> 
#   filter(elev > 0) -> height

# NZ volcano height info
pg <- read_html("https://en.wikipedia.org/wiki/List_of_volcanoes_in_New_Zealand") 

#NZ has 6 tables!

table_data <- bind_rows(
                      html_table(pg)[[1]],
                      html_table(pg)[[2]],
                      html_table(pg)[[3]],
                      html_table(pg)[[4]],
                      html_table(pg)[[5]]#,
                      #html_table(pg)[[6]]
                      )

table_data |> 
  select(name=Name, elev=2) |> 
  filter(name != "Name") |> 
  mutate(
    elev = as.numeric(elev)
    #elev = as.numeric(stri_replace_all_regex(elev, "[^[:digit:]]", ""))
  ) -> height 

#|> 
  # filter(elev > 0) 
  


# make the raincloud distribution chart

ggplot(
  data = height,
  aes(elev)
) +
  stat_halfeye(
    adjust = 0.5,
    justification = -0.1,
    .width = 0,
    point_colour = NA,
    fill = "#db7f33"
  ) +
  geom_boxplot(
    width = 0.1,
    fill = "#db7f33"
  ) +
  stat_dots(
    side = "bottom",
    justification = 1.125,
    dotsize = 1/6,
    fill = "#db7f33",
    stroke = 0.125,
    color = "black"
  ) +
  scale_x_comma(
  ) +
  scale_y_continuous(
    expand = c(0, 0, 0, 0)
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Volcano Height Distribution (m)"
  ) +
  #theme_cs(grid="XY", plot_title_size = 14) +
  theme(
    axis.text.y.left = element_blank(),
    plot.background =  element_rect(fill = "transparent", color = "transparent"),
    panel.background = element_rect(fill = "transparent", color = "transparent")
  ) -> gg
gg

# make the final map
gf <- ggplot() +
  geom_sf( # gray base layer for the outline
    data = nz,
    size = 2,
    fill = "white",
    color = "gray90"
  ) +
  geom_sf( # the ísland map
    data = nz,
    size = 0.125,
    fill = "white"
  ) +
  geom_sf_text( # the volcano emoji's (only the ones that are on the raised continent)
    data = vol, # |> filter(!(Name %in% c("Kolbeinsey", "Reykjaneshryggur", "Helgafell", "Jólnir", "Eldfell", "Surtsey"))),
    aes(label = "*"),  #"⛰")  #N eds  ragg etc to plot this
    colour = "#db7f33") +
  geom_sf_text_repel( # the volcano names (only the ones that are on the raised continent)
    data = vol, # |> filter(!(Name %in% c("Kolbeinsey", "Reykjaneshryggur", "Helgafell", "Jólnir", "Eldfell","Surtsey"))),
    aes(
      label = Name
    ),
    segment.alpha = 0.2,
    colour = "darkblue",
    max.overlaps = 20,
    size = 3
    #family = clear_sans_pkg$bold # if {hrbragg} is not usable, choose a diff font
  ) +
  annotation_custom( # place the raincloud distribution chart
    grob = ggplotGrob(gg),
    xmin = 162,
    xmax = 169,
    ymin = -29,
    ymax = -36
  ) +
  coord_sf( # crop the map and remove the graticules
    xlim = c(163, 185),
    ylim = c(-26, -50),
    datum = NA,
    #clip = "on"
  ) +
  annotate( # add the "title"
    geom = "text",
    x = 174,
    y = -27,
    vjust = 0.5,
    hjust = 0.5,
    label = "The Volcanoes of\n Aotearoa New Zealand",
    #family = clear_sans_pkg$bold,
    colour = "#db7f33",
    size = 9
  ) +
  annotate( # add the "footnote"
    geom = "text",
    x = 174,
    y = -47.75,
    vjust = 0.5,
    hjust = 0.5,
    label = "Code by Hrbrmstr, modified by Mark Neal \n Data: geodata.lib.berkeley.edu",
    #family = clear_sans_pkg$bold,
    colour = "black",
    size = 4
  ) +
  labs(
    x = NULL, y = NULL
  ) +
  # theme_cs(
  #   grid = ""
  # ) +
  theme(
    panel.background = element_rect( # "ocean blue"
      color = NA, 
      fill = "#9dcaca"
    )
  )

gf

ggsave("volcano.png", gf, height = 10, width = 7.5)
