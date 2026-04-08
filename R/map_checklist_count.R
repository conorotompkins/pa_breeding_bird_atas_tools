map_checklist_count <- function(x) {
  exp_seq <- function(start, end, length) {
    if (start <= 0 || end <= 0) {
      stop("start and end must be > 0")
    }
    if (length < 1 || length != as.integer(length)) {
      stop("length must be a positive integer")
    }

    x <- exp(seq(log(start), log(end), length.out = length))

    round(x, 0)
  }

  count_range <- exp_seq(
    min(x$checklist_count),
    max(x$checklist_count),
    length = 10
  )

  count_range

  circle_sizes <- seq(2, 10, length.out = length(count_range))

  maplibre() |>
    fit_bounds(x, animate = FALSE) |>
    add_circle_layer(
      id = "count-circles",
      source = x,
      circle_radius = step_expr(
        column = "checklist_count",
        base = circle_sizes[1],
        values = count_range,
        stops = circle_sizes
      ),
      circle_color = "#1f78b4",
      circle_opacity = 0.8,
      circle_stroke_color = "#ffffff",
      circle_stroke_width = 1,
      tooltip = "checklist_count"
    ) |>
    add_legend(
      legend_title = "Checklists",
      values = count_range,
      colors = rep("#1f78b4", length(circle_sizes)),
      type = "categorical",
      sizes = circle_sizes,
      position = "top-right"
    )
}
