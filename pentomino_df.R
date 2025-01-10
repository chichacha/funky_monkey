
library(rvest)
pento <- read_html("https://isomerdesign.com/Pentomino/index.html")


pento_sol <- tibble(
  dim= pento |> rvest::html_elements(".solutionName a") |>
    html_text2()
)


pento_sol <-pento_sol |>
  mutate(url=str_glue("https://isomerdesign.com/Pentomino/{dim}/solutions.txt"))

pento_sol <-pento_sol |>
  mutate(dim_row=as.numeric(str_extract(dim,"\\d+(?=×)")),
         dim_col=as.numeric(str_extract(dim,"(?<=×)\\d+")),
         type=if_else(str_detect(dim,"×"),"grid","other")) |>
  mutate(url=str_replace(str_replace(url,"×","x")," ",""))

pento_sol <- pento_sol |> 
  mutate(solution=map(url,~read_csv(., col_names=F, col_types="cc")))

pento_sol <- pento_sol |>
  mutate(solution_cnt=map_int(solution,~dim(.)[[1]]))

pento_sol |> select(dim,solution_cnt)

pento_sol_df <- pento_sol |>
  unnest(solution) 

pento_sol_df <-pento_sol_df |> mutate(X3=str_split(X2," ")) |>
  mutate(row_cnt=lengths(X3),
         col_cnt=map_int(X3, ~ . |> pluck(1) |> str_count()))


pentomino_solution <- pento_sol_df |>
  select(dim,sol_text=X2,row_cnt,col_cnt) |>
  filter(dim!="3×20")

pen3x20 <- tibble(
  dim = c("3×20","3×20"),
  sol_text = c("UUXIIIIINNNFTWYYYYZV UXXXPPLNNFFFTWWYZZZV UUXPPPLLLLFTTTWWZVVV",
               "UUXIIIIIZWWTTTFLLLLV UXXXPPZZZYWWTFFFNNLV UUXPPPZYYYYWTFNNNVVV"),
  row_cnt=c(3,3),
  col_cnt=c(20,20))

pentomino_solution <- bind_rows(pentomino_solution, pen3x20)


pentomino_solution <- pentomino_solution |>
  arrange(dim) |>
  group_by(dim) |>
  mutate(sol_idx=row_number()) |>
  ungroup() 

fs::dir_create(path(here::here(),"data"))
pentomino_solution |> write_csv("data/pentomino_solution.csv")

create_matrix <- function(sol_text,row_cnt,col_cnt,...){
  sol_text <- str_remove_all(sol_text," ")
  matrix(sol_text |> str_split("") |> pluck(1),
         nrow=row_cnt,ncol=col_cnt, byrow=T) 
}

create_sol_df <- function(sol_text,row_cnt,col_cnt,...){
  sol_text <- str_remove_all(sol_text," ")
  matrix(sol_text |> str_split("") |> pluck(1),
         nrow=row_cnt,ncol=col_cnt, byrow=T) |>
    as_tibble() |>
    mutate(y=row_number()) |>
    pivot_longer(-y) |>
    mutate(x=as.numeric(str_extract(name,"\\d+"))) |>
    select(x,y,value) |>
    group_by(value) |>
    mutate(idx_within=row_number(pmin(x,y))) |>
    ungroup()
}

pentomino_solution[33008,] |> 
  pmap(create_sol_df) |>
  pluck(1) 

pento_col2 <- c(pento_col,"transparent")
names(pento_col2) <-c(names(pento_col),".")

pentomino_solution |> mutate(idx=row_number()) |>
  group_by(row_cnt,col_cnt) |>
  summarise(idx_min=min(idx),
            idx_max=max(idx),
            cnt=n(),
            dim_list=paste(unique(dim),collapse=" & ")) |>
  ungroup() |>
  gt::gt()

pentomino_solution[15379,] |> 
  pmap(create_sol_df) |>
  pluck(1) |>
  ggplot(aes(x=x,y=y)) +
  geom_tile(aes(fill=value)) +
  geom_text(aes(label=value), data = . %>% filter(idx_within==3),
            family="Roboto Condensed", color="white") +
  coord_fixed() +
  scale_fill_manual(values=pento_col2,name="") +
  theme_void(base_family="Roboto Condensed")

?theme_void

pentomino_solution_big <- pentomino_solution |>
  mutate(sol_df=pmap(list(sol_text,row_cnt,col_cnt),create_sol_df))

pentomino_solution_big_unnest <- pentomino_solution_big |>
  unnest(sol_df)

pentomino_solution_big_unnest |>
  write_csv("data/pentomino_solution_df.csv")


pentomino_solution_big_unnest <-pentomino_solution_big_unnest |>
  mutate(idx=dense_rank(str_c(dim,sol_idx))) 
pentomino_solution_big_unnest <-pentomino_solution_big_unnest |>
  select(idx,dim,sol_idx,sol_text,row_cnt,col_cnt,x,y,value,idx_within)

range(pentomino_solution_big_unnest$idx)

pentomino_solution_big_unnest |>
  count(value) |>
  mutate(n2=n/5) |>
  arrange(desc(n2))

pentomino_solution_big_unnest |>
  count(row_cnt,col_cnt,sort=T)

pentomino_solution_big_unnest |>
  filter(row_cnt==5 & col_cnt==10) |>
  count(x,y,value,sort=T) |>
  filter(value!=".") |>
  ggplot(aes(x=x,y=y)) +
  geom_tile(aes(fill=n)) +
  facet_wrap(~value) +
  scale_fill_viridis_c(option="G", trans="sqrt") +
  coord_fixed() +
  theme_no_axes()

pentomino_solution_big_unnest |> count(row_cnt,col_cnt) |> filter(row_cnt==6)

pentomino_solution_big_unnest |>
  filter(row_cnt==6&col_cnt==10) |> head(5)
  ggplot(aes(x=x,y=y)) +
  geom_tile(aes(fill=value)) +
  facet_wrap_paginate(~sol_idx,ncol=6,nrow=4) +
  coord_equal() +
  scale_fill_manual(values=pento_col2) +
  cowplot::theme_nothing()


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


sample_sf <-pentomino_solution_big_unnest |>
  filter(row_cnt==6&col_cnt==10) |>
  rowwise() |>
  mutate(geometry=list(create_square(x,y))) |>
  ungroup() |>
  group_by(value,idx) |>
  summarise(geometry=st_union(st_sfc(geometry)),.groups="drop") |>
  st_sf()  #|>
  
sample_sf |>
  filter(idx==26798) |>
  ggplot() +
  geom_sf(aes(geometry=geometry-st_centroid(geometry)-c(2,2),fill=value),
          color="white") +
  geom_sf(aes(geometry=geometry,fill=value),
          color="white") +
  facet_wrap(~idx) +
  scale_fill_manual(values=pento_col2) +
  cowplot::theme_nothing()

ggsave(path(here::here(),"output","pentomino.svg"), width=16, height=9,
       device=svglite::svglite)


pentomino_solution[26798,] |> 
  pmap(create_sol_df) |>
  pluck(1) |> 
  ggplot(aes(x=x,y=y,color=value)) +
  geom_point(size=10) +
  geom_path(aes(group=value)) +
  geom_text(aes(label=idx_within), color="white")

pentomino_solution[26797,] |> 
  pmap(create_sol_df) |>
  pluck(1) |> 
  group_by(value) |>
  arrange(value) |>
  mutate(x_offset=x-x[1]+1,
         y_offset=y-y[1]+1) |>
  ggplot(aes(x=x_offset,y=y_offset)) +
  geom_tile(aes(color=value,fill=value), alpha=0.5) +
  geom_tile(aes(x=x,y=y,fill=value)) +
  #facet_wrap(~value) +
  scale_fill_manual(values=pento_col2) +
  scale_color_manual(values=pento_col2) +
  coord_fixed()

pentomino_solution[26799,] |> 
  pmap(create_sol_df) |>
  pluck(1) |> 
  group_by(value) |>
  arrange(value) |>
  mutate(x_offset=x-x[1],
         y_offset=y-y[1]) |>
  ggplot(aes(x=x_offset,y=y_offset)) +
  geom_tile(aes(color=value,fill=value), alpha=0.5) +
  geom_tile(aes(x=x,y=y,fill=value)) +
  facet_wrap(~value) +
  scale_fill_manual(values=pento_col2) +
  scale_color_manual(values=pento_col2) +
  coord_fixed()


sol_nodes <-pentomino_solution[26768,] |> 
  pmap(create_sol_df) |>
  pluck(1) |>
  mutate(id=row_number())


library(tidygraph)
library(ggraph)

sol_edges <- sol_nodes |>
  mutate(key=paste(x,y,sep="_")) |>
  cross_join(sol_nodes) |>
  filter(id.x!=id.y) |>
  filter(value.x==value.y) |>
  filter(abs(x.x-x.y) + abs(y.x-y.y) == 1) |>
  select(from=id.x,to=id.y)

sol_g <- tbl_graph(nodes=sol_nodes,edges=sol_edges, directed=T)

sol_g <- sol_g |>
  mutate(deg=centrality_degree(),
         closeness = centrality_closeness(),
         btwnness = centrality_betweenness(),
         eigenvector=centrality_eigen(),
         comp=group_components())

sol_g |>
  ggraph(layout="manual", x=x,y=y) +
  #geom_edge_arc(linewidth=1, color="#303030",aes(width = betweenness)) +
  geom_edge_link(linewidth=15, aes(color = .N()$value[from]),
                 linejoin="round", lineend="square") +
  geom_edge_parallel(color="white") +
  geom_node_point(aes(color=value,size=btwnness),shape=16) +
  geom_node_text(aes(label=deg)) +
  coord_fixed() +
  scale_color_manual(values=pento_col2) +
  scale_size_continuous(range=c(3,10)) +
  scale_edge_color_manual(values=pento_col2) +
  cowplot::theme_nothing()

sol_nodes |>
  ggplot(aes(x=x,y=y)) +
  #geom_tile(aes(fill=factor(idx_within))) +
  geom_tile(aes(fill=factor(as.numeric(fct_reorder(value,x,sum))%%5))) +
  coord_fixed() +
  cowplot::theme_nothing() +
  scale_fill_manual(values=retro_col5)

ggsave(filename="pentomino_26798.svg",path=fs::path(here::here(),"output"),
       width=18,height=10,device=svglite::svglite)

ggsave(filename="pentomino_26798_2.svg",path=fs::path(here::here(),"output"),
       width=18,height=10,device=svglite::svglite)

ggsave(filename="pentomino_26768_2.svg",path=fs::path(here::here(),"output"),
       width=18,height=10,device=svglite::svglite)

