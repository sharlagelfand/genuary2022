generate_buildings <- function(sidewalks) {
  map_dfr(
    sidewalks,
    function(sidewalk) {
      n_buildings <- 5

      sidewalk_buildings_sf <- vector("list", length = n_buildings)

      buildings <- map_dfr(1:n_buildings, function(building_num) {
        sidewalk_main <- sidewalk[["sidewalk_main"]]
        sidewalk_lines <- sidewalk[["sidewalk_lines"]]
        overlaps <- TRUE
        attempts <- 0

        while (overlaps & attempts < 10) {
          building_size <- sample(c("s", "m", "l"), 1)

          if (building_size == "s") {
            size_right <- runif(1, 8, 10)
            size_left <- runif(1, 8, 10)
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

          building <- generate_building(building_x, building_y, size_right, size_left, height) %>%
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
            summarise() %>%
            st_cast("POLYGON") %>%
            st_make_valid()

          sidewalk_sf <- sidewalk_main %>%
            slice(1:4) %>%
            st_as_sf(coords = c("x", "y")) %>%
            summarise() %>%
            st_cast("POLYGON") %>%
            st_make_valid()

          building_overlaps_sidewalk <- st_intersection(sidewalk_sf, building_sf) %>%
            nrow() != 0

          if (building_overlaps_sidewalk) {
            cat("building overlaps sidewalk \n")
            overlaps <- TRUE
            attempts <- attempts + 1
          }

          if (!building_overlaps_sidewalk) {
            browser()
            # Check if it overlaps any of the other buildings

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
                cat("overlaps, on attempt", attempts, "\n")
              } else {
                cat("wow, no overlap! \n")

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
