generate_buildings <- function(sidewalks, palette) {
  map_dfr(
    sidewalks,
    function(sidewalk) {
      n_buildings <- 30

      sidewalk_buildings_sf <- vector("list", length = n_buildings)

      buildings <- map_dfr(1:n_buildings, function(building_num) {
        sidewalk_main <- sidewalk[["sidewalk_main"]]
        sidewalk_lines <- sidewalk[["sidewalk_lines"]]
        overlaps <- TRUE
        attempts <- 0

        while (overlaps & attempts < 20) {
          # while (overlaps) {
          building_size <- sample(c("s", "m", "l"), 1, prob = c(0.25, 0.5, 0.25))

          if (building_size == "s") {
            size_right <- runif(1, 6, 8)
            size_left <- runif(1, 6, 8)
            height <- runif(1, 3, 4)
          } else if (building_size == "m") {
            size_right <- runif(1, 4, 6)
            size_left <- runif(1, 4, 6)
            height <- runif(1, 4, 6)
          } else if (building_size == "l") {
            size_right <- runif(1, 3, 4)
            size_left <- runif(1, 3, 4)
            height <- runif(1, 10, 15)
          }

          # The y should be along a sidewalk line - one of the negative sloped ones
          # Amazingly enough I stored the x and xend here so we can see the range of placing the bottom point, by looking at the left and right sidewalks
          sidewalk_sides <- sidewalk_lines %>%
            filter(sign(m) == -1) %>%
            mutate(sidewalk_side = ifelse(x < xend, "right", "left"))

          sidewalk_sides <- sidewalk_sides %>%
            filter(sidewalk_side == "left") %>%
            select(xend, m, b) %>%
            mutate(b_join = round(b, 2)) %>%
            left_join(
              sidewalk_sides %>%
                filter(sidewalk_side == "right") %>%
                select(x, m, b) %>%
                mutate(b_join = round(b, 2)),
              by = c("m", "b_join")
            )

          sidewalk_along_line <- sidewalk_sides %>%
            sample_n(1) %>%
            select(x_left = xend, m, b = b.x, x_right = x)

          # The bottom point of it needs to be between the bottom and left points
          # And within size_left of the left point
          building_x <- runif(
            1,
            sidewalk_along_line[["x_left"]] + size_left,
            sidewalk_along_line[["x_right"]]
          )

          building_y <- sidewalk_along_line[["m"]] * building_x + sidewalk_along_line[["b"]]

          colour <- sample(palette, 1)

          building <- generate_building(building_x, building_y, size_right, size_left, height, colour = colour) %>%
            mutate(
              sidewalk_id = first(sidewalk[["id"]]),
              building_x = building_x,
              building_y = building_y
            )

          # Check if it's out of bounds - if it intersects the sidewalk

          building_sf <- building %>%
            filter(section == "roof") %>%
            mutate(y = y - height) %>%
            st_as_sf(coords = c("x", "y")) %>%
            group_by(id) %>%
            summarise(geometry = st_combine(geometry)) %>%
            st_cast("POLYGON") %>%
            st_make_valid()

          sidewalk_sf <- sidewalk_main %>%
            slice(1:5) %>%
            st_as_sf(coords = c("x", "y")) %>%
            summarise(geometry = st_combine(geometry)) %>%
            st_cast("POLYGON") %>%
            st_make_valid()

          combined <- bind_rows(sidewalk_sf, building_sf)

          building_within_sidewalks <- st_contains_properly(sidewalk_sf, building_sf)[[1]] %>% length() != 0

          if (!building_within_sidewalks) {
            # cat("building overlaps sidewalk \n")
            overlaps <- TRUE
            attempts <- attempts + 1
          }

          if (building_within_sidewalks) {

            if (building_num == 1) {
              sidewalk_buildings_sf[[building_num]] <<- building_sf
              overlaps <- FALSE

              return(building)
            } else {
              overlaps <- st_intersection(
                st_make_valid(
                  sidewalk_buildings_sf %>%
                    bind_rows()
                ),
                building_sf
              ) %>%
                nrow() != 0

              if (overlaps) {
                attempts <- attempts + 1
                # cat("overlaps, on attempt", attempts, "\n")
              } else {
                # cat("wow, no overlap! \n")

                sidewalk_buildings_sf[[building_num]] <<- building_sf
                return(building)
              }
            }
          }
        }
      })

      building_placement_order <- buildings %>%
        distinct(id, building_x, building_y) %>%
        arrange(desc(building_y), building_x) %>%
        mutate(placement = row_number()) %>%
        select(id, placement)

      buildings <- buildings %>%
        left_join(building_placement_order, by = "id") %>%
        mutate(id = forcats::fct_reorder(id, placement))

      buildings
    }
  )
}
