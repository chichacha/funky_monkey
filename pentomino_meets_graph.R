
pento_sol <- read_csv("https://raw.githubusercontent.com/chichacha/pentomino/refs/heads/main/pentomino_solution.csv")



# Just Prepping some color palette
retro <-  c("#00A0B0", "#6A4A3C", "#CC333F", "#EB6841", "#EDC951")
retro12 <- colorRampPalette(retro)(12)
retro13 <- c(retro12,"#ffffff")
piece<-c("F","I","L","N","P","T","U","V","W","X","Y","Z")
names(retro12) <- piece
names(retro13) <- c(piece,".")




# Create a data frame with row, column, and block labels

pento_sol_grid <- function(sol_list){
  tibble(
    row = rep(1:length(sol_list), each = nchar(sol_list[1])),
    col = rep(1:nchar(sol_list[1]), times = length(sol_list)),
    block = unlist(str_split(sol_list, ""))
  )
  
}

pento <- pento_sol |>
  mutate(sol_list=str_split(sol_text," ")) |>
  mutate(grid_df=map(sol_list,pento_sol_grid))


pento_edges <- function(grid_df){
  
  # Shift `col` values by +1 in a copy of the data
  grid_shifted_c <- grid_df %>%
    mutate(col = col + 1)  # Shift column indices to the right
  
  # Perform the join to find horizontal adjacency
  horizontal_edges <- grid_df %>%
    inner_join(grid_shifted_c, by = c("row", "col")) %>%
    select(from = block.x , to = block.y )
  
  # Shift `row` values by +1 in a copy of the data
  grid_shifted_r <- grid_df %>%
    mutate(row = row + 1)  # Shift row indices down
  
  # Perform the join to find vertical adjacency
  vertical_edges <- grid_df %>%
    inner_join(grid_shifted_r, by = c("col", "row")) %>%
    select(from = block.x , to = block.y )
  
  
  # Diagonal Edges
  # Create shifted grids for each diagonal direction
  bottom_right <- grid_df %>%
    mutate(row = row - 1, col = col - 1) %>%
    rename(block_shifted = block)
  
  bottom_left <- grid_df %>%
    mutate(row = row - 1, col = col + 1) %>%
    rename(block_shifted = block)
  
  top_right <- grid_df %>%
    mutate(row = row + 1, col = col - 1) %>%
    rename(block_shifted = block)
  
  top_left <- grid_df %>%
    mutate(row = row + 1, col = col + 1) %>%
    rename(block_shifted = block)
  
  
  # Bottom-right diagonal
  edges_bottom_right <- grid_df %>%
    inner_join(bottom_right, by = c("row", "col")) %>%
    select(from = block, to = block_shifted)
  
  # Bottom-left diagonal
  edges_bottom_left <- grid_df %>%
    inner_join(bottom_left, by = c("row", "col")) %>%
    select(from = block, to = block_shifted)
  
  # Top-right diagonal
  edges_top_right <- grid_df %>%
    inner_join(top_right, by = c("row", "col")) %>%
    select(from = block, to = block_shifted)
  
  # Top-left diagonal
  edges_top_left <- grid_df %>%
    inner_join(top_left, by = c("row", "col")) %>%
    select(from = block, to = block_shifted)
  
  
  # Combine horizontal and vertical edges
  edges <- bind_rows(horizontal_edges |> mutate(edge_type="h"), 
                     vertical_edges |> mutate(edge_type="v"),
                     edges_bottom_right |> mutate(edge_type="d"), 
                     edges_bottom_left |> mutate(edge_type="d"), 
                     edges_top_right |> mutate(edge_type="d"), 
                     edges_top_left|> mutate(edge_type="d"))
  edges <- edges |> filter(!from==".") |>
    filter(!to==".")
  
  return(edges)
  
}



i <- 23
pento$grid_df[[i]] |>
  ggplot(aes(x=row,y=col)) +
  geom_tile(aes(fill=block)) +
  geom_text(aes(label=block), family="American Typewriter", color="#fffff3") +
  coord_fixed() +
  scale_fill_manual(values=retro13, guide="none") +
  theme_nothing() +
  as_tbl_graph(pento_edges(pento$grid_df[[i]])) |>
  inner_join(pento$grid_df[[i]] |>
               group_by(name=block) |>
               summarise(y=mean(col),x=mean(row))) |>
  #ggraph("manual", x=x,y=y) +
  #ggraph("stress") +
  ggraph("nicely") +
  geom_edge_fan2(aes(edge_colour=node.name, 
                     edge_width=I(if_else(edge_type %in% c("h","v"),0.7,0.1)))) +
  geom_node_point(aes(color=name, size=I(centrality_degree(loops=F)))) +
  geom_node_text(aes(size=I(centrality_degree(loops=F)-2), 
                     label=str_c(name)), family="American Typewriter") +
  scale_color_manual(values=retro13, guide="none") +
  scale_edge_color_manual(values=retro13, guide="none") +
  theme_nothing()


