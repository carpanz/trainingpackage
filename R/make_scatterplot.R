make_scatterplot <- function(tsv_file) {
  dataset <- read_tsv(tsv_file)
  sample_vars <- sample(
    names(dataset %>% select(where(is.numeric))),
    2
  )

  plot <- dataset %>%
    ggplot(aes(x = .data[[sample_vars[[1]]]], y = .data[[sample_vars[[2]]]])) +
    geom_point()

  return(plot)
}
