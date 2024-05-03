summarize_dataset <- function(tsv_file) {
  data <- read_tsv(tsv_file)
  summary_table <- tibble()
  for (var in names(data)) {
    if (is.numeric(data[[var]])) {
      summary_table <- bind_rows(
        summary_table,
        bind_cols(
          variable = var,
          average = mean(data %>% pull(var)),
          minimum = min(data %>% pull(var)),
          maximum = max(data %>% pull(var))
        )
      )
    }
  }
  return(summary_table)
}
