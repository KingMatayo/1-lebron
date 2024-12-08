# Functions we'll source to qmd

# plot function
create_scatterplot <- function(data, selected_species, selected_island) {
  # Filter the data for the specified species and island
  filtered_data <- data %>%
    na.omit() %>%
    filter(species == selected_species, island == selected_island)
  
  # Create the scatterplot
  plot <- ggplot(
    filtered_data,
    aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species)
  ) +
    geom_point() +
    labs(
      x = "Bill Length (mm)",
      y = "Bill Depth (mm)",
      title = paste("Penguin Bill Dimensions -", selected_species, "-", selected_island)
    )
  
  return(plot)
}


create_shotchart <- function(data, selected_player, selected_date) {
  # Filter the data for the specified player and date
  filtered_data <- data |>
    # Player
    filter(namePlayer == selected_player) |> 
    # Game data
    filter(dateGame == selected_date) |>
    # Scale x and y coordinates (court dimensions adjusted manually)
    mutate(x = (locationX / 10) - 0,
           y = (locationY / 10) - 41.75) |>
    # Remove shots from beyond half-court range
    filter(y < 0) |> 
    mutate(
      # Categorize shots based on distance from the basket
      court_zone = case_when(
        distanceShot <= 8 ~ "Paint",               # Shots close to the basket
        distanceShot > 8 & distanceShot < 23 ~ "Mid-Range",  # Shots outside the paint but inside the 3PT line
        distanceShot >= 23 ~ "3PT",                # Shots beyond the 3-point line
        TRUE ~ "Other"                            # Catch-all for undefined cases
      )
    )
  
  # Create the shot chart plot
  plot <- # draws the NBA half court
    BasketballAnalyzeR::drawNBAcourt(
      ggplot(data = filtered_data), size = 0.5, col = "grey20") +
    
    # shot data
    geom_point(aes(x = x, y = y, fill = court_zone), shape = 21, color = "white", size = 2.5, alpha = 0.8)
  
  return(plot)
  
  
}