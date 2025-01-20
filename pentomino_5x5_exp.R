
library(tidyverse) # Easily Install and Load the 'Tidyverse' # Easily Install and Load the 'Tidyverse'
library(cowplot) # Streamlined Plot Theme and Plot Annotations for 'ggplot2'
library(sf) # Simple Features for R # Simple Features for R
library(arrow) # Integration to 'Apache' 'Arrow' # Integration to 'Apache' 'Arrow' # Integration to 'Apache' 'Arrow'
library(ggforce) # Accelerating 'ggplot2'

pentomino_solution_df <- read_csv("data/pentomino_solution_df.csv")

flower <-c("#4E5021", "#F298BF", "#FCCC13", "#E2D6A4", "#E6DBCE")
tapes <- c("#ef694d", "#dddb76", "#e4f2ef", "#388895", "#42403f")
vintage <- c("#669fb2", "#dadfe1", "#e8b877", "#dd802c", "#3e2a20")
warm <- c("#FF7043", "#FFB74D", "#A8D8B9", "#6B5B95", "#F7CAC9")
retro <-  c("#00A0B0", "#6A4A3C", "#CC333F", "#EB6841", "#EDC951")
retro12 <- colorRampPalette(retro)(12)
piece<-c("F","I","L","N","P","T","U","V","W","X","Y","Z")
names(retro12) <- piece

pento_df <- read_parquet("https://github.com/chichacha/pentomino/raw/refs/heads/main/pentomino_solution_sf.parquet")
pento_sf <- pento_df |> st_as_sf(wkt="wkt")
pento_sf <- pento_sf |> rename(geometry=wkt)

pento_sol_summary <-pento_sf |>
  st_drop_geometry() |>
  group_by(dim,sol_idx, piece_cnt) |>
  summarise(pieces=paste(sort(value),collapse="")) |>
  ungroup()

p_pal <-pento_sol_summary |> #count(dim)
  mutate(dim_g=str_split_i(dim,"(×| )",1)) |>
  mutate(dim_g2=str_split_i(dim,"(×| )",2)) |> 
  mutate(dim_g2=str_remove_all(dim_g2,"[a-z]")) |>
  count(dim_g,dim_g2,piece_cnt) |>
  mutate(across(1:3,as.character)) |>
  #select(piece_cnt,dim_g,dim_g2,n) |> 
  gather_set_data(1:3) |>
  group_by(x) |>
  mutate(n_share=n/sum(n)) |>
  ungroup() #|>



p_pal |> 
  mutate(facet=if_else(dim_g=="Triplicate","Triplicate","Rectangle")) |>
  mutate(x=if_else(x==1,1.5,if_else(x==2,1.8,2.4))) |>
  #mutate(x=if_else(piece_cnt==12,3,x)) #|>
  ggplot(aes(x, id = id, split = y, value = sqrt(n_share))) +
  #geom_parallel_sets(aes(fill = as.character(dim_g)), 
                     # = 0.9, axis.width = 0, strength=-0.5) +
  #geom_parallel_sets(aes(fill = as.character(dim_g)), 
                     #alpha = 0.9, axis.width = 0, strength=1.5) +
  #geom_parallel_sets(aes(fill = as.character(dim_g)), 
                     #alpha = 0.9, axis.width = 0, strength=-0.5) +
  geom_parallel_sets(aes(fill = as.character(dim_g2)), 
                     alpha = 0.9, axis.width = 0.1, strength=0.4) +
  geom_parallel_sets_axes(axis.width = 0.1, fill="black") +
  geom_parallel_sets_labels(colour = 'white', angle=0, family="Roboto Condensed") +
  scale_fill_manual(values=colorRampPalette(retro)(26)) +
  theme_nothing() +
  theme(plot.background=element_rect(fill="#303030")) +
  facet_wrap(~facet, ncol=2, scales="free") +
  coord_flip() +
  scale_x_reverse()

ggsave(filename="parallel_sets_visual_play.svg",
       path=fs::path(here::here(),"output"),
       width=18,height=9,device=svglite::svglite)

pento_sol_summary |> pull(dim) |> unique()

pento_sf |>
  filter(sol_idx==1 & piece_cnt==9) |>
  ggplot() +
  geom_sf(aes(fill=value),color="white") +
  facet_wrap(~dim) +
  theme_nothing() +
  scale_fill_manual(values=retro12)


pento_sf |> 
  filter(str_detect(dim,"8×8")) |> #count(sol_idx) |> arrange(desc(sol_idx))
  filter(sol_idx<13) |>
  ggplot() +
  geom_sf(aes(fill=value),color="white") +
  facet_wrap(~dim+sol_idx,ncol=12) +
  theme_nothing() +
  scale_fill_manual(values=retro12)

# Function to turn coordinate as center and conver to square
create_square_fence <- function(x, y) {
  st_polygon(list(matrix(c(
    x-0.5, y-0.5,  # Bottom Left
    x+0.5, y-0.5, #Bottom Right
    x+0.5, y+0.5, #Top Right
    x-0.5, y+0.5, #Top Left
    x-0.5, y-0.5  # Close the polygon by coming back to bottom left
  ), ncol = 2, byrow = TRUE)))
}

pento_df_to_sf <- function(df) {
  df |>
    rowwise() |>
    # For each row, create a square geometry from the x, y coordinate
    mutate(geometry=list(create_square_fence(x,y))) |>
    ungroup() |> # Remove rowwise grouping
    group_by(sol_idx,dim,value) |>
    summarise(geometry=st_union(st_sfc(geometry)),.groups="drop") |>
    st_sf() 
}

#pento_sol_sf <-pentomino_solution_df |>
  #filter(value!=".") |>
  #pento_df_to_sf()

library(tidyverse) # Easily Install and Load the 'Tidyverse' 
library(sf) # Simple Features for R 
library(arrow) # Integration to 'Apache' 'Arrow' 

##pento_sf |> glimpse()

pento_sol_sf |> write_rds(file="data/pentomino_solution_sf.rds")
#pento_sf <- read_rds("data/pentomino_solution_sf.rds")  
pento_sf |> write_sf("data/pentomino_solution_sf.geojson")

pento_sf |> write_sf("data/pentomino_solution_sf.parquet")
pento_sf |> write_parquet("data/pentomino_solution_sf.parquet")
### Above just gives you error 
### !Can't infer Arrow data type from object inheriting from XY / POLYGON / sfg

# data/pentomino_solution_sf.geojson     105.51M
# data/pentomino_solution_sf.parquet        695K
# data/pentomino_solution_sf.rds         110.62M

# Convert sf object to a data frame
pento_df <- pento_sf %>% 
  mutate(wkt = st_as_text(geometry)) |> # Convert geometry to WKT format
  st_drop_geometry() ## now I've converted geometry column, I don't need them

pento_df |> write_parquet("data/pentomino_solution_sf.parquet")
pento_df <- read_parquet("https://github.com/chichacha/pentomino/raw/refs/heads/main/pentomino_solution_sf.parquet")

pento_sf2<-pento_df |> st_as_sf(wkt="wkt")
pento_sf2 <-pento_sf2 |> rename(geometry=wkt)

pento_sf2 |> 
  st_drop_geometry() |>
  count(piece_cnt,dim,sol_idx) |>
  count(piece_cnt,dim) |>
  gt::gt()


pento_sf<-pento_sf |> 
  group_by(dim,sol_idx) |>
  mutate(piece_cnt=n_distinct(value)) |>
  ungroup()

library(ggreveal) # Reveal a 'ggplot' Incrementally

retro12 <- colorRampPalette(retro)(12)
names(retro12) <- unique(sort(pento_sf$value))

pento55 <-pento_sf |>
  filter(piece_cnt==5) |>
  add_count(value) |>
  arrange(value) |>
  ggplot() +
  geom_sf(aes(fill=value,
              group=fct_reorder(value,n,max,.desc=T)), color="#fffff3") +
  facet_wrap(~sol_idx) +
  scale_fill_manual(values=retro12) +
  theme_nothing()

pento55

pento_sf |>
  filter(piece_cnt==5) |>
  rowwise() |>
  mutate(geometry=geometry) |>
  count(value) |>
  ungroup() |>
  arrange(desc(n)) |>
  mutate(idx=row_number()) |>
  st_sf() |>
  ggplot() +
  geom_sf(aes(fill=value), color="#fffff3") +
  geom_sf_text(aes(label=str_c(value,n)), family="Roboto Condensed",
               color="white") +
  facet_wrap(~idx,ncol=6) +
  scale_fill_manual(values=retro12) +
  theme_nothing()

ggsave(filename="pentomino_5x5_piece_agg.svg",
       path=fs::path(here::here(),"output"),
       width=18,height=9,device=svglite::svglite)


pento55_sep <- ggreveal::reveal_aes(pento55)
ggreveal::reveal_save(pento55_sep,basename="output/pentomino/pentomino55.svg",
                      width=18, height=18, device=svglite::svglite)

pentomino_solution_df |>
  filter(!str_detect(dim,"Tripli")) |>
  filter(sol_idx<=12) |>
  filter(str_detect(dim,"8×")) |> 
  filter(value!=".") |>
  ggplot(aes(x=x,y=y)) +
  geom_tile(aes(fill=value)) +
  facet_wrap(dim~sol_idx, ncol=12) +
  scale_fill_manual(values=colorRampPalette(retro)(12)) +
  coord_fixed() +
  theme_nothing() 

ggsave(filename="pentomino_8x8_retro.svg",path=fs::path(here::here(),"output"),
       width=18,height=9,device=svglite::svglite)

flip <- function(direction = "h") {
  if (direction == "h") {
    matrix(c(-1, 0, 0, 1), 2, 2)  # Flip horizontally
  } else if (direction == "v") {
    matrix(c(1, 0, 0, -1), 2, 2)  # Flip vertically
  } else {
    stop("Invalid direction. Use 'h' or 'v'.")
  }
}

offset <- function(i,n=6){
  x <- (i-1)%%n
  y <- floor((i-1)/n)
  c(x,y)
}

offset(8)


### function to rotate
rot = function(a) {
  a <- (a/180)*pi
  matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
}
  
pento_sol_sf |>
  filter(sol_idx==12) |>
  ggplot() +
  geom_sf(aes(fill=value,geometry=geometry*flip("v")), color="white") +
  facet_wrap(~dim, ncol=6) +
  theme_nothing() +
  scale_fill_manual(values=colorRampPalette(retro)(12)) 

ggsave(filename="pentomino_3x20_retro_inception.svg",
       path=fs::path(here::here(),"output"),
       width=18,height=9,device=svglite::svglite)

pento_sol_sf |>
  filter(sol_idx==12) |>
  ggplot() +
  geom_sf(aes(fill=value,geometry=(geometry-st_centroid(geometry))*rot(45)), 
          color="white", alpha=0.3) +
  #facet_wrap(~dim, ncol=6) +
  theme_nothing() +
  scale_fill_manual(values=colorRampPalette(retro)(12)) 

pento_sol_sf |>
  filter(sol_idx<=6) |>
  mutate(dim_idx=dense_rank(str_c(dim,sol_idx))) |>
  mutate(val_idx=dense_rank(value)) |>
  mutate(val_idx=scales::rescale(val_idx,to=c(-45,45))) |>
  rowwise() |>
  mutate(geometry=if_else(str_detect(dim,"I"), geometry*rot(45),geometry)) |>
  mutate(geometry=geometry+offset(dim_idx,n=12)*13) |>
  ungroup() |>
  ggplot() +
  geom_sf(aes(fill=value,geometry=geometry*flip("v")), 
          color="white") +
  #facet_wrap(~dim, ncol=6) +
  theme_nothing() +
  scale_fill_manual(values=colorRampPalette(retro)(12)) 

ggsave(filename="pentomino_3x20_retro_offset_multi.svg",
       path=fs::path(here::here(),"output"),
       width=18,height=9,device=svglite::svglite)


pento_sf2 |>
  filter(dim=="5×4") |>
  ggplot() +
  geom_sf(aes(fill=value)) +
  scale_fill_manual(values=retro12, limits=names(retro12)) +
  facet_wrap(~sol_idx, ncol=12)

dim_vec<-unique(pento_sf2$dim)
dim_vec

?fct

test <-pento_sf2 |>
  head(5) |>
  mutate(
    coords = map(geometry, ~ st_coordinates(.)),
    top_left = map(coords, ~ .x[which.min(.x[, 1]) & which.max(.x[, 2]), ]),
    x = map_dbl(top_left, 1), # Extract x
    y = map_dbl(top_left, 2)  # Extract y
  )




pento_sf2 |>
  filter(str_detect(dim,"Tri")) |>
  count(value,dim,sort=T) |>
  mutate(value=fct(value,levels=sort(unique(pento_sf2$value)))) |>
  mutate(geometry=geometry*flip("v")) |>
  ggplot() +
  geom_sf(aes(fill=value, geometry=geometry, group=dim),color="white") +
  #geom_sf_text(aes(label=str_c(dim,"\n",n)),
               #color="#fffff3", family="Roboto Condensed") +
  scale_fill_manual(values=retro12, limits=names(retro12)) +
  facet_wrap(dim~value, drop=F, ncol=24) +
  #theme_map(font_family="Roboto Condensed") +
  theme_nothing()

ggsave(filename="pentomino_cryptic_triplicate.svg",
       path=fs::path(here::here(),"output"),
       width=18,height=9,device=svglite::svglite)


dim_vec
pento_sf2 |>
  filter(!str_detect(dim,"Tri")) |>
  filter(str_detect(dim,"(8).(8)")) |> 
  count(dim,value,sort=T) |>
  mutate(value=fct(value,levels=sort(unique(pento_sf2$value)))) |>
  mutate(geometry=geometry*flip("v")) |>
  ggplot() +
  geom_sf(aes(fill=value, geometry=geometry, group=dim),color="white") +
  #geom_sf_text(aes(label=str_c(dim,"\n",n)),
  #color="#fffff3", family="Roboto Condensed") +
  scale_fill_manual(values=retro12, limits=names(retro12)) +
  facet_wrap(~dim+value, drop=F, ncol=12,
             labeller = labeller(.multi_line=F)) +
  theme_map(font_family="Roboto Condensed") #+
  theme_nothing() 
  
?labeller
?install.packages

