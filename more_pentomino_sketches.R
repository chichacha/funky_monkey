retro_col5 <- c("#00A0B0", "#6A4A3C", "#CC333F", "#EB6841", "#EDC951")
sol_df <- read_csv("https://isomerdesign.com/Pentomino/6x10/solutions.txt",
                   col_names=F)

col10 <- c("#f72585ff", "#b5179eff", "#7209b7ff", "#560badff", "#480ca8ff", "#3a0ca3ff", "#3f37c9ff", "#4361eeff", "#4895efff", "#4cc9f0ff")
col12 <- c("#F72585", "#C11999", "#8A0EAD", "#6509B2", "#520BAB", "#460CA7", "#3B0CA3", "#3D2BBE", "#414DDD", "#4473EE", "#489EEF", "#4CC9F0")

col5 <- c("#355070ff", "#6d597aff", "#b56576ff", "#e56b6fff", "#eaac8bff")
colorRampPalette(col5)(12) |> cat(sep=",")

col12 <-c("#355070", "#495373", "#5D5677", "#735A79", "#8D5E78", "#A76276", "#BD6674", "#CF6872", "#E06A6F", "#E67C76", "#E89480", "#EAAC8B")


make_coord_df <- function(x){
  #x <- sol_df$X2[[1]]
  x <- str_split(str_remove_all(x," "),"",simplify=T)
  matrix(x,nrow=6,ncol=10, byrow=T) |>
    as_tibble() |>
    mutate(y=row_number()) |>
    pivot_longer(-y) |>
    mutate(x=as.integer(str_extract(name,"\\d+"))) |>
    select(x,y,piece=value) |>
    group_by(piece) |>
    mutate(x_min=min(x),
           y_min=min(y),
           x_max=max(x),
           y_max=max(y),
           idx=row_number(x+y),
           adj_x=x-x_min+1,
           adj_y=y-y_min+1) |>
    ungroup()
}


# Each coordinate represents the center of square
create_square_fence <- function(x, y) {
  st_polygon(list(matrix(c(
    x-0.5, y-0.5,  # Bottom Left
    x+0.5, y-0.5, #Bottom Right
    x+0.5, y+0.5, #Top Right
    x-0.5, y+0.5, #Top Left
    x-0.5, y-0.5  # Close the polygon by coming back to bottom left
  ), ncol = 2, byrow = TRUE)))
}

create_square_fence(1,1)

sol_df <- sol_df |> mutate(solution_df = map(X2,make_coord_df))
sol_df_unnest <- sol_df |> unnest(solution_df)
sol_sf <- sol_df_unnest |>
  rowwise() |>
  # For each row, create a square geometry from the x, y coordinate
  mutate(geometry=list(create_square_fence(x,y)),
         geometry_adj=list(create_square_fence(adj_x,adj_y))) |>
  ungroup() |> # Remove rowwise grouping
  group_by(X1,piece) |>
  summarise(geometry=st_union(st_sfc(geometry)),.groups="drop",
            geometry_adj=st_union(st_sfc(geometry_adj),.groups="drop")) |>
  st_sf() 

sol_sf |>
  count(piece,geometry_adj,sort=T) |>
  mutate(idx=row_number()) |>
  mutate(descr=str_c(piece," ",scales::comma(n))) |>
  st_sf() |>
  arrange(piece,desc(n)) |> 
  ggplot() +
  geom_sf(aes(fill=piece),color="transparent") +
  geom_sf(aes(fill=piece, geometry=st_buffer(geometry_adj,dist=n/nrow(sol_df))),
          color="black", linetype=3, alpha=0.2) +
  facet_wrap(~fct_inorder(descr),ncol=11) +
  scale_fill_manual(values=retro_col) +
  theme_map(font_family="Roboto Condensed") +
  theme(strip.text=element_text(family="Roboto Condensed")) +
  labs(title="63 Fixed Pentominos",
       subtitle="Number displayed above the puzzle indicate apperrence counts\nin 2,339 6x10 Rectangle Solutions")

library(cowplot)

retro_col <- c(retro_col5,colorspace::darken(retro_col5,amount=0.1),
              colorspace::lighten(retro_col5,amount=0.1))
retro_col |> scales::show_col()

make_coord_df(sol_df$X2[[1]]) |>
  ggplot() +
  geom_tile(aes(x=x,y=y,fill=piece), color="#fffff3", linetype=3) +
  coord_fixed() +
  scale_fill_manual(values=retro_col, guide="none") +
  theme_map(font_family="Roboto Condensed")


envelope <-sol_sf |> st_bbox() |> st_as_sfc()
sol_sf |> 
  filter(X1 %in% c(1)) |>
  ggplot() +
  geom_sf(aes(geometry=st_triangulate_constrained(geometry),
              fill=piece),
          linetype=3,color="#fffff2") +
  geom_sf(aes(group=piece),color="white", linewidth=1,fill="transparent") +
  scale_fill_manual(values=retro_col,guide="none") +
  theme_map(font_family="Roboto Condensed") +
  facet_wrap(~X1, ncol=6)

ggsave(filename="pentomino_6_10_sol_triangulate.svg",path=fs::path(here::here(),"output"),
       width=18,height=10,device=svglite::svglite)


make_coord_df(sol_df$X2[[4]]) |>
  group_by(piece) |>
  summarise(geometry = st_sfc(st_multipoint(as.matrix(cbind(x, y))))) |>
  st_as_sf() |>
  ungroup() |>
  ggplot() +
  geom_tile(aes(fill=piece,x=x,y=y), 
            data= make_coord_df(sol_df$X2[[4]]),alpha=0.95) +
  geom_sf(aes(fill=piece, 
              geometry=st_buffer(geometry,dist=0.3)),
          color="transparent") +
  geom_sf(aes(color=piece, 
              geometry=st_convex_hull(st_buffer(geometry,dist=0.15))),
          fill="#ffffff00", color="white") +
  geom_sf(aes(color=piece, 
              geometry=st_concave_hull(geometry,0.1)),
          fill="#ffffff00", color="black") +
  scale_fill_manual(values=retro_col) +
  scale_color_manual(values=retro_col) +
  theme_minimal_grid(font_family="Roboto Condensed")


make_coord_df(sol_df$X2[[4]]) |>
  rowwise() |>
  # For each row, create a square geometry from the x, y coordinate
  mutate(geometry=list(create_square_fence(x,y)),
         geometry_adj=list(create_square_fence(adj_x,adj_y))) |>
  ungroup() |> # Remove rowwise grouping
  group_by(piece) |>
  summarise(geometry=st_union(st_sfc(geometry)),.groups="drop",
            geometry_adj=st_union(st_sfc(geometry_adj),.groups="drop")) |>
  st_sf() |>
  ggplot() +
  geom_sf(aes(fill=piece)) +
  scale_fill_manual(values=retro_col) 


sol_df_unnest |>
  count(x,piece) |>
  ggplot(aes(x=x,y=n)) +
  geom_col(aes(fill=piece)) +
  scale_fill_manual(values=retro_col) 

sol_df_unnest |>
  count(y,piece) |>
  ggplot(aes(x=y,y=n)) +
  geom_col(aes(fill=piece)) +
  scale_fill_manual(values=retro_col) +
  coord_flip()

sol_df_unnest |>
  count(x,y,piece,sort=T) |>
  group_by(x,y) |>
  mutate(idx=row_number(desc(n))-1) |>
  mutate(x_jitter=idx%%4,
         y_jitter=floor(idx/4)) |>
  ungroup() |>
  ggplot(aes(x=x+x_jitter/5,y=y+y_jitter/4)) +
  geom_tile(aes(x=x+0.5,y=y+0.5), fill="#fffff3", color="#303030", linetype=3) +
  geom_text(aes(size=n,color=piece,label=piece),
            family="Futura", hjust=0, vjust=0) +
  scale_color_manual(values=retro_col) +
  theme_nothing() +
  scale_size_continuous(range=c(5,12),guide="none")

ggsave(filename="pentomino_6_10_sol_text.svg",path=fs::path(here::here(),"output"),
       width=18,height=10,device=svglite::svglite)

xopen::xopen(here::here())

dir_create("pentomino_6x10")
sol_sf |>
  st_write("pentomino_6x10/pentomino_6x10.shp")

sol_sf |>
  add_count(geometry_adj,piece) |>
  st_drop_geometry() |>
  select(X1,piece,n) |>
  group_by(piece) |>
  mutate(shape_type=dense_rank(desc(n))) |>
  ungroup() |>
  pull(shape_type) -> sol_shape_type
  
sol_sf |>
  mutate(shape_type=sol_shape_type) |>
  count(shape_type,piece)

sol_sf |>
  mutate(shape_type=sol_shape_type) |>
  filter(X1%%24==3) |> ## change the value here for filtering
  arrange(piece) |>
  ggplot() +
  geom_sf(aes(fill=piece,alpha=shape_type), color="white") +
  facet_wrap(~X1,ncol=11) +
  theme_nothing() +
  scale_fill_manual(values=retro_col) +
  scale_alpha_continuous(range=c(1,0.6))

ggsave(filename="pentomino_6_10_sol_mod3.svg",path=fs::path(here::here(),"output"),
       width=18,height=10,device=svglite::svglite)


sol_sf |>
  mutate(shape_type=sol_shape_type) |>
  filter(shape_type==1) |>
  filter(X1<100) |>
  arrange(piece) |>
  ggplot() +
  geom_sf(aes(fill=piece,alpha=shape_type), color="white") +
  facet_wrap(~X1,ncol=11) +
  theme_nothing() +
  scale_fill_manual(values=retro_col) +
  scale_alpha_continuous(range=c(1,0.6))

sol_sf |>
  mutate(shape_type=sol_shape_type) |>
  st_drop_geometry() |>
  mutate(piece_var=str_c(piece,shape_type)) |>
  select(piece_var,X1) |>
  group_by(X1) |>
  summarise(piece_var_comb=paste(sort(piece_var),collapse="|")) |>
  ungroup() |>
  group_by(piece_var_comb) |>
  summarise(n=n(),
            list=paste(X1,collapse=", ")) |>
  ungroup() |>
  arrange(desc(n))

sol_sf |>
  filter(X1 %in% c(1594,1595,1611,1612)) |>
  ggplot() +
  geom_sf(aes(fill=piece), color="#fffff3") +
  facet_wrap(~X1) +
  scale_fill_manual(values=retro_col)

sol_sf |>
  filter(X1 %in% c(1624, 1626, 1630, 1632)) |>
  ggplot() +
  geom_sf(aes(fill=piece), color="#fffff3") +
  facet_wrap(~X1) +
  scale_fill_manual(values=retro_col)

sol_sf |>
  filter(X1 %in% c(1708, 1709, 1715, 1716)) |>
  ggplot() +
  geom_sf(aes(fill=piece), color="#fffff3") +
  facet_wrap(~X1) +
  scale_fill_manual(values=retro_col)


sol_sf |>
  filter(X1 %in% c(822, 823, 918)) |>
  ggplot() +
  geom_sf(aes(fill=piece, group=X1), color="#fffff3") +
  facet_wrap(~X1) +
  scale_fill_manual(values=retro_col) +
  theme_nothing()

ggsave(filename="pentomino_6_10_sol_no_flip_facet.svg",path=fs::path(here::here(),"output"),
       width=18,height=10,device=svglite::svglite)

