library(imager)
library(magick)
library(patchwork)
library(cluster)

col5 <-c("#1d2f6f", "#8390fa", "#fac748", "#f9e9ec", "#f88dad")

library(dplyr)
library(cluster)
library(ggplot2)

col5 <- c("#1d2f6f", "#8390fa", "#fac748", "#f88dad","#f9e9ec")
col12 <-c("#1D2F6F", "#4252A1", "#6775D4", "#8D94E9", "#B9A9A9", "#E4BD68", "#F9BC5A", "#F8A77F", "#F892A3", "#F8A6BE", "#F8C7D5", "#F9E9EC")



pam_result$clustering
append_pam <- function(i) {
  
  #i <- 30000
  # Extract data for the given index
  data <- pento_min$data[[i]] |> ungroup()
  
  # Calculate centroids for each piece
  centroids <- data |> 
    group_by(sol_text) |> 
    summarize(
      centroid_x = mean(x),
      centroid_y = mean(y),
      .groups = "drop"
    )
  
  # Perform PAM clustering (4 clusters for 4 colors)
  pam_result <- pam(centroids[, c("centroid_x", "centroid_y")], k = 4)
  #pam_result$clustering
  
  # Add cluster assignments back to the original data
  data <- data |> 
    left_join(
      centroids |> mutate(cluster = pam_result$clustering),
      by = "sol_text"
    )
  
  data
  
}

append_pam(30000)


df_to_graph <- function(i) {
  ### cross join is computationally expensive, but I couldn't really think of alternatives
  
  data <- pento_min$data[[i]]
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
    mutate(idx=row_number()) |>
    left_join(append_pam(i))
  
  return(graph)
  
}

plot_g <- function(g) {
  
  g |>
    ggraph(layout="manual", x=x,y=y) +
    geom_tile(aes(x=x,y=y,fill=factor(cluster))) +
    geom_edge_link(color=col5[5], aes(linewidth=I(if_else(w==1,1,0.01))),
                   alpha=0.7, lineend="round") +
    scale_fill_manual(values=col5) +
    theme_nothing() +
    coord_fixed()
  
}



pento_min |> filter(dim=="8×8a") |> pull(solution_num)
list_g <- (pento_min |> filter(dim=="8×8a") |> pull(solution_num)) |>
  sample(size=10*6) |>
  map(~df_to_graph(.) |> plot_g())

ggsave("output/pentomino_8x8a_60.png", width=16, height=8,
       device=ragg::agg_png)


patchwork::wrap_plots(list_g, ncol=12)

abstract_plot <- function(i){
  pento_min$data[[i]] |>
    arrange(sol_text) |>
    group_by(sol_text) |>
    mutate(idx=row_number(),
           diff_x=x-lag(x,default=1),
           diff_y=y-lag(y,default=1)) |>
    ungroup() |>
    ggplot() +
    #geom_tile(aes(fill=sol_text,x=x,y=y), alpha=0.3) +
    geom_arc_bar(aes(x0=x+0.5,y0=y+0.5,r0=0, r=sqrt(1), start=3*pi/2, end=pi, 
                     fill=sol_text), color="transparent",
                 data = . %>% filter(idx==1)) +
    geom_arc_bar(aes(x0=x-0.5,y0=y-0.5,r0=0, r=sqrt(1), start=0, end=pi/2, 
                     fill=sol_text), color="transparent",
                 data = . %>% filter(idx==2)) +
    geom_arc_bar(aes(x0=x+0.5,y0=y-0.5,r0=0, r=sqrt(1), start=0, end=-pi/2, 
                     fill=sol_text), color="transparent",
                 data = . %>% filter(idx==3)) +
    geom_circle(aes(x0=x,y0=y, r=sqrt(0.25),
                    fill=sol_text), 
                data = . %>% filter(idx==5),
                color="transparent") +
    geom_arc_bar(aes(x0=x+0.5,y0=y+0.5,r0=0, r=sqrt(1), start=3*pi/2, end=pi, 
                     fill=sol_text), color="transparent",
                 data = . %>% filter(idx==4)) +
    coord_fixed() +
    scale_fill_manual(values=retro12) +
    #scale_color_manual(values=retro12) +
    theme_nothing()
  
}

pento_min |> filter(dim=="8×8a") |> pull(solution_num) |>
  sample(size=6) |>
  map(abstract_plot) |>
  wrap_plots() 

ggsave("output/pentomino_design_8x8d.svg", width=16, height=16,
       device=svglite::svglite)


?solve_TSP
library(TSP)

abstract_plot2 <- function(i) {
  my_tsp <-pento_min$data[[i]] |>
    mutate(sol_text=as.numeric(fct_reorder(sol_text,x*y,sum))*100) |>
    dist("manhattan") |>
    TSP::as.TSP()
  
  ### available TSP Method without installing extra
  methods <- c("identity", "random", "nearest_insertion",
               "cheapest_insertion", "farthest_insertion", "arbitrary_insertion",
               "nn", "repetitive_nn", "two_opt")
  
  my_tsp_path <- solve_TSP(my_tsp, method=methods[3]) ## nearest insertion 
  
  pento_min$data[[i]][as.integer(my_tsp_path),] |>
    mutate(idx=row_number()) |>
    ggplot() +
    geom_arc_bar(aes(x0=x+0.5,y0=y+0.5,r0=0, r=sqrt(1), start=3*pi/2, end=pi, 
                     fill=sol_text), color="transparent",
                 data = . %>% filter(idx%%8==1)) +
    geom_arc_bar(aes(x0=x-0.5,y0=y-0.5,r0=0, r=sqrt(1), start=0, end=pi/2, 
                     fill=sol_text), color="transparent",
                 data = . %>% filter(idx%%8==2)) +
    geom_arc_bar(aes(x0=x+0.5,y0=y-0.5,r0=0, r=sqrt(1), start=0, end=-pi/2, 
                     fill=sol_text), color="transparent",
                 data = . %>% filter(idx%%8==3)) +
    geom_circle(aes(x0=x,y0=y, r=sqrt(0.25),
                    fill=sol_text), 
                data = . %>% filter(idx%%8==4),
                color="transparent") +
    geom_arc_bar(aes(x0=x+0.5,y0=y+0.5,r0=0, r=sqrt(1), start=3*pi/2, end=pi, 
                     fill=sol_text), color="transparent",
                 data = . %>% filter(idx%%8==5)) +
    geom_regon(aes(x0=x,y0=y,r=sqrt(0.5),sides=4, fill=sol_text,angle=0), 
               color="transparent",
               data = . %>% filter(idx%%8==6)) +
    geom_regon(aes(x0=x,y0=y,r=sqrt(0.5),sides=4, fill=sol_text,angle=0), 
               color="transparent",
               data = . %>% filter(idx%%8==7)) +
    geom_regon(aes(x0=x,y0=y,r=sqrt(0.5),sides=4, fill=sol_text,angle=0), 
               color="transparent",
               data = . %>% filter(idx%%8==0)) +
    # geom_text(aes(x=x,y=y, label=idx), color="#fffff330",
    #           family="Futura") +
    coord_fixed() +
    scale_fill_manual(values=retro12) +
    theme_nothing()
  
}

abstract_plot2(29405) 

ggsave("output/pentomino_design_8x8d_single.svg", width=16, height=16,
       device=svglite::svglite)

c(29129:31646)|>
  sample(size=12) |>
  map(abstract_plot2) |>
  wrap_plots(ncol=4)

ggsave("output/pentomino_design_8x8.svg", width=16, height=9,
       device=svglite::svglite)


pento_min |> group_by(dim) |>
  summarise(min=min(solution_num),
            max=max(solution_num),
            cnt=n(),
            nrow=max(map_int(data,nrow))) |>
  gt::gt()





library(ggforce)
library(ggplot2)

# Sample data
data <- data.frame(
  x = c(0,0,0,0,0,0),
  y = c(0,0,0,0,0,0),
  r0=c(rep(0.5,times=4),0,0),
  r=c(rep(1,times=4),0.5,0.5),
  start = c(0, pi/2, pi, 3*pi/2,0,pi),
  end = c(pi/2, pi, 3*pi/2, 2*pi,pi,2*pi),
  group=c("A","B","C","D","E","F")
)

# Plot using geom_arc_bar
ggplot(data) +
  geom_arc_bar(aes(
    x0 = x+2, y0 = y, r0 = r0, r = r, 
    start = start, end = end, 
    fill = group
  ), color="#fffff3", n=4) +
  geom_arc_bar(aes(
    x0 = x, y0 = y, r0 = r0, r = r, 
    start = start, end = end, 
    fill = group
  ), color="#fffff3", n=360) +
  geom_arc_bar(aes(
    x0 = x-2, y0 = y, r0 = r0, r = r, 
    start = start, end = end, 
    fill = group
  ), color="#fffff3", expand=unit(-3,"mm")) +
  geom_arc_bar(aes(
    x0 = x+4, y0 = y, r0 = r0, r = r, 
    start = start, end = end, 
    fill = group
  ), color="#fffff3", radius=unit(5,"mm"), n=6) +
  theme_minimal_grid(font_family="Roboto Condensed") +
  labs(title = "Fun with geom_arc_bar", fill = "Group") +
  coord_fixed() +
  scale_fill_manual(values=c(retro,"#323433")) 

pento_min$data[[30000]] |>
  ungroup() |>
  arrange(sol_text) |>
  mutate(idx=floor((row_number()-1)/4)) |>
  ggplot() +
  geom_diagonal_wide(aes(x=x,y=y,group=idx, fill=sol_text), strength=1,
                     alpha=0.8) +
  scale_fill_manual(values=retro12) +
  theme_nothing() +
  coord_fixed()




img <- imager::load.image("https://www.felissimo.co.jp/on/demandware.static/-/Library-Sites-SharedFelissimo/default/dwa017390b/kuum/images/about-visual.jpg")
img <-img



img_df <-img |> 
  imager::imresize(scale=0.2) |>
  as.data.frame(wide="c") |>
  rename(red=c.1, green=c.2, blue=c.3) |>
  mutate(hex=rgb(red,green,blue)) |>
  filter(hex!="#FFFFFF")

img_hsv <- farver::convert_colour(farver::decode_colour(img_df$hex),from="rgb",to="hsv")
img_df <- bind_cols(img_df,img_hsv)

img_df |> 
  count(hex,h,s,sort=T) |>
  mutate(ha = scales::rescale(h,to=c(0,2*pi), from=c(0,360))) |>
  ggplot(aes(x=s*cos(ha),y=s*sin(ha))) +
  geom_point(aes(col=I(hex), size=n)) +
  theme_nothing() +
  coord_fixed()

  
img_df <-img_df |>
  add_count(hex) |>
  filter(hex!="#F8F8F8") |>
  #filter(x%%30==0 & y%%30==0) |> #count(hex,sort=T) |> filter(n>5) |> pull(hex) |> scales::show_col()
  #mutate(g=santoku::chop_equally(s,6)) |> 
  filter(y>100) |> arrange(desc(y)) |> as_tibble() |>
  filter(s>0.1) |>
  filter(n<3000) |> as_tibble() 

img_df |>
  ggplot(aes(x=x,y=y)) +
  geom_point(aes(color=I(hex), size=n)) +
  scale_y_reverse() +
  coord_fixed() +
  theme_minimal() 

img_df |> 
  mutate(s=santoku::chop_deciles(s),
         v=santoku::chop_deciles(v)) |>
  count(h,s,v,hex) |>
  mutate(idx=dense_rank(h)-1) |>
  mutate(x=floor(idx/100),
         y=idx%%100) |>
  ggplot(aes(x=x,y=y)) +
  geom_tile(aes(fill=I(hex))) +
  theme_nothing()



img_df |>
  filter(y>90) |>
  ggplot(aes(x=x,y=y)) +
  geom_tile(aes(fill=I(hex))) +
  scale_y_reverse() +
  coord_fixed() +
  scale_x_continuous(breaks=scales::pretty_breaks(n=50))


# Example Dataset
set.seed(123)
data <- data.frame(
  x = c(rnorm(10, mean = 5), rnorm(10, mean = 15), 100), # Outlier at 100
  y = c(rnorm(10, mean = 5), rnorm(10, mean = 15), 100)  # Outlier at 100
)

# k-means Clustering
kmeans_result <- kmeans(data, centers = 2)

# PAM Clustering
library(cluster)
pam_result <- pam(data, k = 2)

# Visualization
library(ggplot2)
data$kmeans_cluster <- factor(kmeans_result$cluster)
data$pam_cluster <- factor(pam_result$clustering)

# Plot k-means
ggplot(data, aes(x = x, y = y, color = kmeans_cluster)) +
  geom_point(size = 3) +
  labs(title = "k-means Clustering (Outlier Affects Centers)") +
  theme_minimal()

# Plot PAM
ggplot(data, aes(x = x, y = y, color = pam_cluster)) +
  geom_point(size = 3) +
  labs(title = "PAM Clustering (Robust to Outlier)") +
  theme_minimal()


