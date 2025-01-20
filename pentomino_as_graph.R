library(tidyverse)
library(ggraph)
library(tidygraph)
library(ggforce)
library(arrow)
library(sf)
library(cowplot)

#devtools::install_github('thomasp85/ggfx')
library(ggfx)

# Read Parquet file from GitHub
pento_df <- read_parquet("https://github.com/chichacha/pentomino/raw/refs/heads/main/pentomino_solution_sf.parquet")
pento_sol <- read_csv("https://raw.githubusercontent.com/chichacha/pentomino/refs/heads/main/pentomino_solution.csv")

# Just Prepping some color palette
retro <-  c("#00A0B0", "#6A4A3C", "#CC333F", "#EB6841", "#EDC951")
retro12 <- colorRampPalette(retro)(12)
retro13 <- c(retro12,"#ffffff")
piece<-c("F","I","L","N","P","T","U","V","W","X","Y","Z")
names(retro12) <- piece
names(retro13) <- c(piece,".")

# Convert WKT column back to geometry to recreate the spatial object
pento_sf <- pento_df |> 
  st_as_sf(wkt = "wkt") |>        # Convert WKT strings into spatial geometries
  rename(geometry = wkt)          # Rename for compatibility with sf conventions

pento_sol_df <-pento_sol |>
  mutate(solution_num=row_number()) |>
  mutate(sol_text=str_split(sol_text," ")) |>
  unnest(sol_text) |>
  group_by(solution_num) |>
  mutate(y=row_number()) |>
  ungroup() |>
  mutate(sol_text=str_split(sol_text,"")) |>
  unnest(sol_text) |>
  group_by(solution_num,y) |>
  mutate(x=row_number()) |>
  ungroup()

pento_sol_df |>
  filter(str_detect(dim,"6")) |>
  count(sol_text,dim,sort=T) |>
  mutate(piece_cnt=n/5) |>
  ggplot(aes(x=sol_text,y=piece_cnt)) +
  geom_col(aes(fill=dim))

### 2170 Solutions for 8x8d
pento_sol_df |>
  filter(str_detect(dim,"8d")) |> 
  ggplot(aes(x=x,y=y)) +
  geom_jitter(aes(color=sol_text), alpha=0.5, size=0.5) +
  scale_color_manual(values=c(retro13)) +
  coord_fixed() +
  facet_wrap(~solution_num%%6,ncol=3) +
  theme_nothing() 

pento_min <-pento_sol_df |>
  #filter(str_detect(dim,"8d")) |>
  filter(sol_text!=".") |>
  select(x,y,sol_text,solution_num,dim) |>
  nest(.by=c(solution_num,dim))


df_to_graph <- function(data) {
  ### cross join is computationally expensive, but I couldn't really think of alternatives
  #data <- pento_min$data[[333]]
  
  tmp <-data |>
    cross_join(data) |>
    filter((abs(x.x-x.y)+abs(y.x-y.y))==1) |>
    distinct()
  
  edges <- tmp |>
    transmute(
      from = paste(x.x, y.x, sol_text.x, sep = ","),
      to = paste(x.y, y.y, sol_text.y, sep = ","),
      w = if_else(sol_text.x==sol_text.y,1,0.1)
    )
  
  # -- Standardize so each pair is only listed once
  edges_unique <- edges %>%
    rowwise() %>%
    mutate(
      a = min(from, to),
      b = max(from, to)
    ) %>%
    ungroup() %>%
    distinct(a, b, .keep_all = TRUE) %>%
    select(-a,-b)
  
  graph <- tidygraph::as_tbl_graph(edges_unique, directed = FALSE)
  
  graph <- graph |>
    mutate(x=str_split_i(name,",",1),
           y=str_split_i(name,",",2),
           piece=str_split_i(name,",",3)) |>
    mutate(x=as.numeric(x),
           y=as.numeric(y)) |>
    mutate(deg=centrality_degree(weights=w),
           btwn=centrality_betweenness(weights=w),
           sub_g = centrality_subgraph())  |>
    mutate(idx=row_number())
  
  return(graph)
  
}

draw_graph <- function(df){
  
  df |>
    mutate(community = group_louvain(weights=w)) |>
    ggraph(layout="manual",x=x,y=y) +
    #geom_edge_fan2(aes(color=node.piece, linewidth=w)) +
    geom_edge_link(aes(color=.N()$piece[from], filter=w==1), 
                   linewidth=20, alpha=0.65, lineend="round", linejoin="bevel") +
    geom_node_point(aes(size=deg), color="#fffff330", shape=20) +
    #geom_node_text(aes(label=round(deg,1))) +
    # Highlight clusters with convex hulls
    #geom_mark_hull(aes(color=piece,x=x,y=y), concavity=5, linetype=3) +  
    theme_nothing() +
    scale_edge_color_manual(values=retro12) +
    #scale_color_manual(values=retro12) +
    scale_edge_width_continuous(range=c(0.1,1.2)) +
    scale_size_continuous(range=c(5,10), guide="none") +
    coord_fixed()+
    scale_x_continuous(expand=expansion(add=1.2)) +
    scale_y_reverse(expand=expansion(add=1.2))
  
}

library(patchwork)
df_to_graph(bind_rows(pento_min$data[[2342]])) |>
  draw_graph() +
  df_to_graph(bind_rows(pento_min$data[[2343]])) |>
  draw_graph() +
  df_to_graph(bind_rows(pento_min$data[[2344]])) |>
  draw_graph() +
  df_to_graph(bind_rows(pento_min$data[[2345]])) |>
  draw_graph() +
  patchwork::plot_layout(ncol=1)
  

ggsave("output/graph_viz_pentomino.svg", width=8, height=8, device=svglite::svglite)

df_to_graph(bind_rows(pento_min$data[[31647]])) |>
  draw_graph() +
  df_to_graph(bind_rows(pento_min$data[[31791]])) |>
  draw_graph() +
  df_to_graph(bind_rows(pento_min$data[[31772]])) |>
  draw_graph() +
  df_to_graph(bind_rows(pento_min$data[[31972]])) |>
  draw_graph() +
  plot_layout(ncol=4)

ggsave("output/pento_flip_graph.svg", width=16, height=8, device=svglite::svglite)
  

pento_min |> group_by(dim) |>
  summarise(min=min(solution_num)) |>
  gt::gt()

piece |> cat(sep=",")

test<-igraph::shortest_paths(df_to_graph(bind_rows(pento_min$data[[31647]])),
                       from=2)

plot_graph <- function(g){
  g |>
    activate("nodes") |>
    mutate(comm=group_louvain(weights=w)) |>
    ggraph(layout="manual",x=x,y=y) +
    geom_edge_link(lineend="round", linejoin="mitre", alpha=0.8,
                   aes(edge_color=.N()$piece[from], edge_width=I(if_else(w==1,10,w)))) +
    geom_edge_link(lineend="square", linejoin="round", alpha=0.8,
                   aes(edge_width=I(w)), color="#ffffff", linetype=3) +
    #geom_node_text(aes(label=deg), family="Roboto Condensed", color="white") +
    #geom_mark_hull(aes(group=comm,x=x,y=y,label=comm), concavity=1) +
    coord_fixed() +
    theme_nothing() +
    scale_edge_color_manual(values=retro12) +
    scale_x_continuous(expand=expansion(add=1)) +
    scale_y_reverse(expand=expansion(add=1))
}

pento_min$data[[111]] %>%
  df_to_graph() %>%
  plot_graph()

pento_min |> group_by(dim) |>
  summarise(min=min(solution_num),
            max=max(solution_num)) |>
  gt::gt()



indexes <- c(14369:15378) |> sample(size=12)

list_of_plots <- indexes %>%
  map(~ {
    pento_min$data[[.x]] %>%
      df_to_graph() %>%
      plot_graph()
  })

wrap_plots(list_of_plots, ncol = 3) +
  plot_annotation(title="Collection of Pentomino Puzzles",
                  subtitle="I didn't know there's wrap_plots function till today",
                  theme=theme(text=element_text(family="Roboto Condensed"))) +
  plot_layout()





df_to_graph(bind_rows(pento_min$data[[349]]))|> 
  activate("nodes") |>
  mutate(comm=group_louvain(weights=w)) |>
  ggraph(layout="manual",x=x,y=y) +
  geom_edge_link(lineend="round", linejoin="mitre", alpha=0.8,
                 aes(edge_color=.N()$piece[from], edge_width=I(if_else(w==1,10,w)))) +
  geom_node_text(aes(label=deg), family="Roboto Condensed", color="white") +
  geom_mark_hull(aes(group=comm,x=x,y=y,label=comm), concavity=1) +
  coord_fixed() +
  theme_nothing() +
  scale_edge_color_manual(values=retro12) +
  scale_x_continuous(expand=expansion(add=2)) +
  scale_y_reverse(expand=expansion(add=2))

ggsave("output/pento_349_graph.svg", width=16, height=8, device=svglite::svglite)
                    