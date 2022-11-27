names_to_date <- function(file_rast) {

dats <- strsplit(names(file_rast), "=") %>% do.call(rbind, .) |> as_tibble() |> select(V2) |> unlist() |>
         as.numeric() |> as.Date(origin = "1971-01-31")

return(dats)

}
