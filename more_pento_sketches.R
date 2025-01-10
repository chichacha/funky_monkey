
library(sf)


pentomino_sf <- read_sf(path(here::here(),"output","pentomino.geojson"))
pentomino_sf
st_crs(pentomino_sf) <- NA

flip <- function() {
  # (x, y) -> (-x, y)
  matrix(c(-1, 0, 0, 1), nrow=2, ncol=2) 
}

rot <- function(a) {
  a = a*(pi/180)
  matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
}

## 18 One-sided Pentomino
onesided_pento <- bind_rows(
  pentomino_sf,
  pentomino_sf |> filter(singleside) |>
    mutate(geometry=geometry*flip()) 
)

## 63 Fixed Pentomino
fixed_pento <- bind_rows(
  onesided_pento,
  onesided_pento |>
    filter(asp==8|piece %in% c("T","I","Z","L","W","U","V")) |>
    mutate(geometry=geometry*rot(90)),
  onesided_pento |>
    filter(asp==8|piece %in% c("T","L","W","U","V")) |>
    mutate(geometry=geometry*rot(180)),
  onesided_pento |>
    filter(asp==8|piece %in% c("T","L","W","U","V")) |>
    mutate(geometry=geometry*rot(270))
) 


onesided_pento <-onesided_pento |>
  group_by(piece) |>
  mutate(piece_var=str_c(piece,row_number())) |>
  ungroup()

fixed_pento <- fixed_pento |>
  group_by(piece) |>
  mutate(piece_var=str_c(piece,row_number())) |>
  ungroup()


fixed_pento |>
  mutate(geometry=geometry-st_centroid(geometry)) |>
  arrange(piece) |>
  arrange(desc(g)) |>
  ggplot() +
  geom_sf(aes(fill=g)) +
  facet_wrap(~fct_inorder(piece_var)) +
  scale_fill_manual(values=retro_col5) +
  cowplot::theme_nothing()

ggsave(path(here::here(),"output","fixed_63_pentominos.svg"), width=16, height=9,
       device=svglite::svglite)


onesided_pento |>
  mutate(geometry=geometry-st_centroid(geometry)) |>
  arrange(piece) |>
  arrange(desc(g)) |>
  ggplot() +
  geom_sf(aes(fill=g)) +
  facet_wrap(~fct_inorder(piece_var)) +
  scale_fill_manual(values=retro_col5) +
  cowplot::theme_nothing()

ggsave(path(here::here(),"output","onesided_18_pentominos.svg"), width=16, height=9,
       device=svglite::svglite)

free_pento |>
  mutate(geometry=geometry-st_centroid(geometry)) |>
  arrange(piece) |>
  arrange(desc(g)) |>
  ggplot() +
  geom_sf(aes(fill=g)) +
  facet_wrap(~fct_inorder(piece),ncol=6) +
  scale_fill_manual(values=retro_col5) +
  cowplot::theme_nothing()

ggsave(path(here::here(),"output","free_12_pentominos.svg"), width=16, height=9,
       device=svglite::svglite)





