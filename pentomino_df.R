
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
    mutate(idx_within=row_number(x*y)) |>
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
  filter(row_cnt==6&col_cnt==10) |>
  ggplot(aes(x=x,y=y)) +
  geom_tile(aes(fill=value)) +
  facet_wrap_paginate(~sol_idx,ncol=6,nrow=4) +
  coord_equal() +
  scale_fill_manual(values=pento_col2) +
  cowplot::theme_nothing()

?facet_wrap_paginate

