plot_checklist_coords <- function(x, buffer = 1000) {
  x <- x |>
    distinct(checklist_id, longitude, latitude) |>
    count(longitude, latitude, name = "count") |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    st_transform(3857)

  checklist_ext <- st_as_sfc(st_bbox(x), crs = 3857) |> st_buffer(1000)

  ggplot() +
    basemap_gglayer(checklist_ext, verbose = FALSE) + # basemap layer (e.g. OpenStreetMap)
    geom_sf(data = x, aes(size = count, alpha = count), color = "black") +
    coord_sf(crs = st_crs(3857)) +
    scale_fill_identity() +
    scale_alpha_continuous(range = c(1, .5)) +
    labs(size = "Checklists", alpha = "Checklists") +
    theme_void()
}
