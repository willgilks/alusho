# Load required libraries
library(httr)
library(jsonlite)

#' Function to pull live flight positions
#' @param api_token Your FR24 API token
#' @param bounds A string representing geographical boundaries "N,S,W,E"
#' @return A data frame of live flight data
get_fr24_live_data <- function(api_token, bounds = "50.682,46.218,14.422,22.243") {
  
  # Base URL for live flight positions (v1)
  url <- "https://fr24api.flightradar24.com/api/live/flight-positions/full"
  
  # Construct request headers as per official docs
  headers <- c(
    "Authorization" = paste("Bearer", api_token),
    "Accept" = "application/json",
    "Accept-Version" = "v1"
  )
  
  # Set query parameters
  query_params <- list(bounds = bounds)
  
  # Execute GET request
  response <- GET(url, add_headers(.headers = headers), query = query_params)
  
  # Handle the response
  if (status_code(response) == 200) {
    data <- content(response, "text", encoding = "UTF-8")
    return(fromJSON(data))
  } else if (status_code(response) == 402) {
    stop("Insufficient credits. Check your subscription or top-up your account.")
  } else {
    stop(paste("API Request failed with status:", status_code(response), 
               "\nMessage:", content(response)$message))
  }
}


# --- Execution ---
my_token <- "your_actual_token_here"
# Example bounds for a specific region (North, South, West, East)
search_bounds <- "50.682,46.218,14.422,22.243"

# Pull the data
live_flights <- get_fr24_live_data(my_token, search_bounds)

# View results
print(head(live_flights$data))
