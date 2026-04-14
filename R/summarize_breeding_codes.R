summarize_breeding_codes <- function(x) {
  x |>
    dplyr::select(Confirmed, Probable, Possible, Observed) |>
    pivot_longer(everything(), names_to = "Code", values_to = "Count") |>
    gt()
}
