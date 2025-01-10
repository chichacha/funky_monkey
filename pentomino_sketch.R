library(tidyverse)
library(sf)
library(tidyverse)
library(sf)
library(ggforce)

# Define pentomino pieces
pentomino_pieces <- list(
  F = list(c(0,0), c(0,1), c(1,1), c(1,2), c(2,1)),
  I = list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0)),
  L = list(c(0,0), c(1,0), c(2,0), c(3,0), c(3,1)),
  N = list(c(1,-1), c(1,0), c(1,1), c(2,1), c(2,2)),
  P = list(c(0,0), c(0,1), c(1,0), c(1,1), c(0,-1)),
  T = list(c(0,0), c(0,1), c(0,2), c(1,1), c(2,1)),
  U = list(c(0,0), c(1,0), c(2,0), c(0,1), c(2,1)),
  V = list(c(0,0), c(1,0), c(2,0), c(2,1), c(2,2)),
  W = list(c(0,0), c(1,-1), c(1,0), c(2,-1), c(2,-2)),
  X = list(c(0,0), c(1,0), c(1,-1), c(1,1), c(2,0)),
  Y = list(c(0,0), c(1,0), c(2,0), c(3,0), c(2,1)),
  Z = list(c(0,2), c(1,2), c(1,1), c(1,0), c(2,0))
)


# Convert pentomino pieces into a tibble
pentomino_tibble <- tibble(
  piece = names(pentomino_pieces),
  coords = pentomino_pieces
) %>%
  unnest(coords) %>%  # Expand list of coordinates into rows
  mutate(
    x = map_dbl(coords, ~ .x[1]),  # Extract x coordinate
    y = map_dbl(coords, ~ .x[2])   # Extract y coordinate
  ) %>%
  select(-coords)  # Remove the original list column


aligned_tibble <- pentomino_tibble %>%
  group_by(piece) %>%
  mutate(
    x = x - min(x),  # Shift x coordinates to start at 0
    y = y - min(y)   # Shift y coordinates to start at 0
  ) %>%
  ungroup()

aligned_tibble |> clipr::write_clip()



aligned_tibble |>
  group_by(piece) |>
  mutate(piece_idx=row_number(x*y)) |>
  ggplot() +
  geom_regon(aes(x0=x,y0=y,fill=factor(piece_idx),sides=4, 
                 r=sqrt(0.5), angle=0)) +
  geom_text(aes(label=piece,x=x,y=y), data = . %>% filter(piece_idx==3),
            family="Roboto Condensed", size=7, color="#fffff3") +
  geom_blank(aes(x=0,y=0)) +
  geom_blank(aes(x=5,y=5)) +
  facet_wrap(~piece, ncol=6) +
  coord_fixed() +
  theme_minimal() +
  scale_fill_manual(values=c(c("#00A0B0", "#6A4A3C", "#CC333F", "#EB6841", "#EDC951")
)) +
  cowplot::theme_nothing()

outpath <- path(here::here(),"output","pentomino")
dir_create(outpath)

ggsave("pentomino_retro.svg",device=svglite::svglite,
       bg="#fffffe", path=outpath, width=8, height=5)



# Function to create a square polygon from a coordinate
create_square <- function(x, y) {
  st_polygon(list(matrix(c(
    x, y,
    x + 1, y,
    x + 1, y + 1,
    x, y + 1,
    x, y  # Close the polygon
  ), ncol = 2, byrow = TRUE)))
}

### rowwise does some magic
pentomino_sf <-aligned_tibble |>
  rowwise() |>
  mutate(geometry=list(create_square(x,y))) |>
  ungroup() |>
  group_by(piece) |>
  summarise(geometry=st_union(st_sfc(geometry)),.groups="drop") |>
  st_sf() 

retro_col5 <- colorRampPalette(c("#00A0B0", "#6A4A3C", "#CC333F", "#EB6841", "#EDC951"))(5)
retro_col12 <- colorRampPalette(retro_col5)(12)

pento_col10 <- c("#001219ff", "#005f73ff", "#0a9396ff", "#94d2bdff", "#e9d8a6ff", "#ee9b00ff", "#ca6702ff", "#bb3e03ff", "#ae2012ff", "#9b2226ff")
pento_col <- colorRampPalette(pento_col10)(12)

names(pento_col) <- pentomino_sf$piece

pentomino_sf

### 
# Rotate 90 degrees around (0,0)
rot <- function(a) {
  a = a*(pi/180)
  matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
}
# Flip horizontally across x-axis
flip <- function() {
  # (x, y) -> (-x, y)
  matrix(c(-1, 0, 0, 1), nrow=2, ncol=2) 
}


piece_types <- tibble(
  piece=c("F","I","L","N","P","T","U","V","W","X","Y","Z"),
  asp = c(8,2,8,8,8,4,4,4,4,1,8,4),
  singleside=c(T,F,T,T,T,F,F,F,F,F,T,T)
)


pentomino_sf <-pentomino_sf |>
  left_join(piece_types) |>
  mutate(g=str_c(asp,if_else(singleside,"ab","a")))

piece_types |>
  count(asp,singleside)

### Plot them out
p1 <-pentomino_sf |>
  mutate(geometry=geometry+c(2,2)) |>
  ggplot() +
  geom_sf(aes(fill=piece), alpha=0.8, color="white") +
  geom_sf(aes(fill=piece, geometry=geometry*rot(90)), alpha=0.8, color="white") +
  geom_sf(aes(fill=piece, geometry=geometry*rot(180)), alpha=0.8, color="white") +
  geom_sf(aes(fill=piece, geometry=geometry*rot(270)), alpha=0.8, color="white") +
  facet_wrap(g~piece,ncol=6) +
  geom_text(aes(x=0,y=0,label=piece, color=piece), 
            family="Roboto Condensed",size=8) +
  cowplot::theme_nothing() +
  scale_fill_manual(values=pento_col) +
  scale_color_manual(values=pento_col) +
  coord_sf(expand=F)

### Plot them out
p2 <-pentomino_sf |>
  mutate(geometry=geometry*flip()+c(2,2)) |>
  ggplot() +
  geom_sf(aes(fill=piece), alpha=0.8, color="white") +
  geom_sf(aes(fill=piece, geometry=geometry*rot(90)), alpha=0.8, color="white") +
  geom_sf(aes(fill=piece, geometry=geometry*rot(180)), alpha=0.8, color="white") +
  geom_sf(aes(fill=piece, geometry=geometry*rot(270)), alpha=0.8, color="white") +
  geom_text(aes(x=0,y=0,label=piece, color=piece), 
            family="Roboto Condensed",size=8) +
  facet_wrap(g~piece,ncol=6) +
  cowplot::theme_nothing() +
  scale_fill_manual(values=pento_col) +
  scale_color_manual(values=pento_col) +
  coord_sf(expand=F)

library(patchwork)
p1+p2 + plot_layout(ncol=1)

pentomino_sf |> group_by(g) |>
  rowwise() |>
  mutate(geometry=geometry-st_centroid(geometry)) |> 
  ggplot() +
  geom_sf(aes(fill=g, geometry=geometry*rot(90)), color="black") +
  geom_sf(aes(fill=g,  geometry=geometry*rot(180)), color="black") +
  geom_sf(aes(fill=g, geometry=geometry*rot(270)), color="black") +
  geom_sf(aes(fill=g, geometry=geometry*rot(0)), color="black") +
  geom_sf(aes(fill=g, geometry=(geometry*rot(90))*flip()), color="black") +
  geom_sf(aes(fill=g,  geometry=(geometry*rot(180)*flip())), color="black") +
  geom_sf(aes(fill=g, geometry=(geometry*rot(270))*flip()), color="black") +
  geom_sf(aes(fill=g, geometry=(geometry*rot(0))*flip()), color="black") +
  geom_sf_text(aes(label=piece), color="black", family="Roboto Condensed") +
  facet_wrap(g~piece, ncol=6) +
  scale_color_manual(values=retro_col5) +
  scale_fill_manual(values=str_c(retro_col5,"30")) +
  cowplot::theme_nothing() 

pentomino_sf |>
  sf::st_write(path(here::here(),"pentomino_shp","pentomino.shp"))

pentomino_sf |>
  sf::st_write(path(here::here(),"output","pentomino.geojson"))

pentomino_sf |>
  sf::st_write(path(here::here(),"output","pentomino.geojson"))

?st_write

sol1 <- c("UUXIIIIINNNFTWYYYYZVUXXXPPLNNFFFTWWYZZZVUUXPPPLLLLFTTTWWZVVV")
sol2 <- c("UUXIIIIIZWWTTTFLLLLVUXXXPPZZZYWWTFFFNNLVUUXPPPZYYYYWTFNNNVVV")

sol_txt <- read_csv("https://isomerdesign.com/Pentomino/5x12/solutions.txt",
                    col_names=F)

sol_txt<-sol_txt |> 
  mutate(sol=str_remove_all(X2," ")) |>
  rowwise() |>
  mutate(sol=str_split(sol,""),
         row=5,
         col=12) 

sol_txt |> select(X1,sol) |>
  unnest(sol) |>
  group_by(X1) |>
  mutate(idx=row_number()-1) |>
  mutate(x=idx%%12,
         y=floor(idx/12)) |>
  ungroup() |>
  count(idx,x,y,sol,sort=T) |>
  mutate(max=max(n)) |>
  ggplot(aes(x=x,y=y)) +
  geom_tile(aes(fill=n/nrow(sol_txt))) +
  geom_text(aes(label=scales::percent(n/nrow(sol_txt),accuracy=1)),
            color="white",size=3) +
  geom_blank(aes(x=12,y=5)) +
  geom_blank(aes(x=-1,y=-1)) +
  #scale_color_manual(values=retro_col12) +
  facet_wrap(~sol,ncol=3) +
  coord_fixed() +
  cowplot::theme_minimal_grid() +
  scale_size_continuous(range=c(2,8)) +
  scale_fill_viridis_c(option="G", trans="sqrt")

?matrix
plot_sol <- function(sol){
  #sol <- sol_txt$sol[[1]]
  matrix(sol, nrow=5,byrow=T,dimnames=list(c(1:5),c(1:12))) |>
    as_tibble() |>
    mutate(y=row_number()) |>
    pivot_longer(-y) |>
    rename(x=name) |>
    mutate(x=as.numeric(x)) |>
    group_by(value) |>
    mutate(idx=row_number(x*y)) |>
    ggplot(aes(x=x,y=y)) +
    geom_tile(aes(fill=value, group=value)) +
    geom_text(aes(label=value, alpha=idx), color="#fffff3", family="Roboto Condensed") +
    #facet_wrap(~value) +
    scale_fill_manual(values=retro_col12) +
    scale_alpha_continuous(range=c(0.5,0.7)) +
    coord_fixed() +
    cowplot::theme_nothing()
}

plot_sol(sol_txt$sol[[1]]) +
  plot_sol(sol_txt$sol[[2]]) +
  plot_sol(sol_txt$sol[[3]]) +
  plot_sol(sol_txt$sol[[4]]) +
  plot_sol(sol_txt$sol[[5]]) +
  plot_sol(sol_txt$sol[[6]]) +
  plot_sol(sol_txt$sol[[7]]) +
  plot_sol(sol_txt$sol[[8]]) +
  plot_sol(sol_txt$sol[[9]]) +
  plot_sol(sol_txt$sol[[10]]) +
  plot_sol(sol_txt$sol[[11]]) +
  plot_sol(sol_txt$sol[[12]]) +
  plot_layout(ncol=3)


matrix(sol1 |> str_split("") |> pluck(1),nrow=3, byrow=T) |>
  as_tibble() |>
  mutate(y=row_number()) |>
  pivot_longer(-y) |>
  mutate(x=as.numeric(str_extract(name,"\\d+"))) |>
  ggplot(aes(x=x,y=y)) +
  geom_tile(aes(fill=value), color="white") +
  scale_fill_manual(values=pento_col) +
  coord_fixed() +
  cowplot::theme_nothing()

# Compute centroids and align geometries to the center
pentomino_sf <- pentomino_sf %>%
  mutate(
    cent = st_centroid(geometry),  # Compute centroid
    geometry2 = geometry - cent
    )  # Align to center


pentomino_sf

### function to rotate
rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
# Flip horizontally or vertically
flip <- function(direction = "h") {
  if (direction == "h") {
    matrix(c(-1, 0, 0, 1), 2, 2)  # Flip horizontally
  } else if (direction == "v") {
    matrix(c(1, 0, 0, -1), 2, 2)  # Flip vertically
  } else {
    stop("Invalid direction. Use 'h' or 'v'.")
  }
}


library(ggreveal)
# Visualize aligned pieces
pento_p <-ggplot(pentomino_sf) +
  geom_sf(aes(fill = piece, geometry=geometry2), 
          color = "black", alpha=1) 

pento_p +
  geom_sf(aes(fill = piece, geometry=geometry2*rot(pi/2)+c(0,-3)), 
          color = "black", alpha=0.8) +
  geom_sf(aes(fill = piece, geometry=geometry2*rot(pi)+c(3,0)), 
          color = "black", alpha=0.75) +
  geom_sf(aes(fill = piece, geometry=geometry2*rot(3*pi/2)+c(-3,0)), 
          color = "black", alpha=0.6) +
  geom_sf(aes(fill = piece, geometry=geometry2*flip("v")+c(5,5)), 
          color = "black", alpha=0.55) +
  geom_sf(aes(fill = piece, geometry=geometry2*flip("h")+c(-5,-5)), 
          color = "black", alpha=0.5) +
  scale_fill_manual(values=ggthemes::tableau_color_pal("Hue Circle")(12)) +
  labs(
    title = "Center-Aligned Pentomino Pieces",
    x = "X Coordinate",
    y = "Y Coordinate"
  ) 


pento_p +
  cowplot::theme_nothing() +
  facet_wrap(~piece) +
  geom_sf_text(aes(label=piece, geometry=geometry2), size=10, family="Roboto Condensed") +
  scale_fill_manual(values=pento_col)

ggsave(filename="pentomino.svg",path=fs::path(here::here(),"output"),
       width=10,height=10,device=svglite::svglite)

# Add a height to your 2D geometries (extrusion)
pentomino_3d <- pentomino_sf |>
  mutate(geometry2 = st_zm(st_cast(st_geometry(geometry), "POLYGON"),  zm = "Z"))

# View 3D geometries
pentomino_3d

pentomino_sf |> st_write("output/pentomino.geojson")

sf::st_drivers() |>
  as_tibble() |>
  clipr::write_clip()

