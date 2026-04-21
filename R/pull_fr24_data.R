# Load required libraries
library(httr) # pull openskies flights
library(jsonlite) # pull openskies flights
library(rvest) # pull wiki table
library(rworldmap) # for country assignment
library(sp) # for country assignment
library(countrycode)
library(tidyverse)

# Functions #


#' Function to pull ALL current live flights globally
get_all_live_flights <- function(url = "https://opensky-network.org/api/states/all") {
  ft1=Sys.time()
  
  response <- GET(url)
  
  if (status_code(response) == 200) {
    raw_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Convert matrix to a readable Data Frame
    flights_df <- as.data.frame(raw_data$states)
    colnames(flights_df) <- c("icao24", "callsign", "origin_country", "time_position", 
                              "last_contact", "longitude", "latitude", "baro_altitude", 
                              "on_ground", "velocity", "true_track", "vertical_rate", 
                              "sensors", "geo_altitude", "squawk", "spi", "position_source")
    flights_df$pull_timestamp<-Sys.time()
    
    ft2=Sys.time()
    dur=round(ft2-ft1,2)
    nrow_flights_df = nrow(flights_df)
    print(paste0("Retreived ",nrow_flights_df," rows of data in ",dur," seconds"))
    return(flights_df)
    
  } else {
    stop("Failed to fetch data. Status code: ", status_code(response))
  }
}



# make country groups references for squawk codes
get_external_country_groups <- function(easa_url,eurocontrol_url) {
  
  # EASA Member States (27 EU + 4 EFTA)
  # Source: Official EASA 'By Country' page
  
  easa_list <- easa_url %>%
    read_html() %>%
    html_elements(".views-field-title a") %>% # Selector for country names in the list
    html_text2() %>%
    paste(collapse = ", ")
  
  # EUROCONTROL Member States (41 current states)
  # Source: Official Eurocontrol 'Our Member States' page
  
  eurocontrol_list <- eurocontrol_url %>%
    read_html() %>%
    html_elements(".field--name-title") %>% # Standard title selector for these pages
    html_text2() %>%
    # Filter for valid country names (removes header text)
    keep(~ .x != "Our member and comprehensive agreement states") %>%
    paste(collapse = ", ")
  
  # 3. ICAO Member States (193 countries)
  # Because the ICAO list is massive and rarely changes, a standard world list is best
  icao_list <- paste(countrycode::codelist$country.name.en, collapse = ", ")
  
  list(
    "EASA countries" = easa_list,
    "EUROCONTROL"= eurocontrol_list,
    "Europe"= eurocontrol_list,
    "ICAO"= icao_list,
    "ICAO countries" = icao_list)
}



# pull and format squawk code lookup
make_squawk_lookup = function(
    transpoder_wiki_url = TRANSPONDER_WIKI_URL,
    country_groups = SQUAWK_CODE_COUNTRY_GROUPS) {
  
  # 1. Scrape and initial rename
  transponder_dat <- transpoder_wiki_url |>
    read_html() |>
    html_element(".wikitable") |>
    html_table(fill = TRUE) |>
    dplyr::rename(squawk = Code, sq_country = Countries, sq_usage = `Allocated use`) |> 
    mutate(sq_usage=gsub("\\[.*?\\]", "", sq_usage))
  
  # 2. Explode the Squawk codes (ranges and commas)
  exploded_transponder_dat <- transponder_dat |>
    separate_longer_delim(squawk, delim = ",") |>
    mutate(squawk = str_trim(squawk)) |>
    mutate(squawk = map(squawk, function(x) {
      # Matches any non-digit (hyphen, en-dash, em-dash)
      if(grepl("\\D", x)) {
        vals <- as.numeric(unlist(strsplit(x, "\\D")))
        # Generate sequence and pad with leading zeros
        sprintf("%04d", seq(min(vals, na.rm = TRUE), max(vals, na.rm = TRUE)))
      } else {
        # Ensure single codes are also padded strings
        sprintf("%04d", as.numeric(x))
      }
    })) |>
    unnest(squawk)
  
  # 3. Handle Country Groups and Redundancy
  output_df <- exploded_transponder_dat |>
    mutate(new_sq_country = sq_country) |> 
    
    # Map Group Names (e.g., "EUROCONTROL") to lists
    mutate(new_sq_country = ifelse(new_sq_country %in% names(country_groups), 
                                   country_groups[new_sq_country], 
                                   new_sq_country)) |> 
    
    # Explode the countries into individual rows
    separate_longer_delim(new_sq_country, delim = ",") |> 
    mutate(new_sq_country = str_trim(new_sq_country)) |> 
    
    # Standardise both columns to ISO3 for comparison
    mutate(
      orig_iso = countrycode(sq_country, "country.name", "iso3c", warn = FALSE),
      new_iso  = countrycode(new_sq_country, "country.name", "iso3c", warn = FALSE)
    ) |> 
    
    # REDUNDANCY LOGIC:
    # Within each squawk code, if an exploded ISO code already exists 
    # as a primary 'orig_iso', remove the redundant exploded row.
    group_by(squawk) |> 
    filter(!( !is.na(new_iso) & 
                new_iso %in% orig_iso & 
                sq_country != new_sq_country )) |> 
    ungroup() |> 
    
    # 4. Final Formatting
    group_by(squawk, sq_country, new_sq_country) |> 
    summarise(
      squawk_info = trimws(paste0(sq_usage, collapse = " :: ")), 
      .groups = 'drop'
    ) |>
    # Ensure squawk remains a character with 4 digits
    mutate(squawk = sprintf("%04d", as.numeric(squawk))) |>
    arrange(squawk) |> 
    mutate(iso_code = countrycode(new_sq_country, 
                                  origin = 'country.name', 
                                  destination = 'iso3c')) |> 
    distinct()
  
  return(output_df)
}



# get country name from geo co-ordinates
get_countries_vectorised=function(lon, lat, map = countries_sp) {
  
  out <- rep(NA_character_, length(lon))
  
  # Find indices where both coordinates are NOT NA
  ok <- !is.na(lon) & !is.na(lat)
  
  if (any(ok)) {
    pts <- SpatialPoints(cbind(lon[ok], lat[ok]), proj4string = CRS(proj4string(map)))
    res <- over(pts, map)
    out[ok] <- as.character(res$ADMIN)
  }
  
  return(out)
}









## Constants #


EASA_URL = "https://www.easa.europa.eu/en/domains/international-cooperation/easa-by-country"
EUROCONTROL_URL = "https://www.eurocontrol.int/our-member-and-comprehensive-agreement-states"
OPENSKY_NETWORK_URL = "https://opensky-network.org/api/states/all"
TRANSPONDER_WIKI_URL = "https://en.wikipedia.org/wiki/List_of_transponder_codes"


LM_VARIABLE_PAIRS = list(
  c("baro_altitude", "geo_altitude"),
  c("geo_altitude", "velocity"),
  c("baro_altitude", "velocity"),
  c("time_position", "last_contact")
)

COUNTRIES_SP=getMap(resolution = 'low')


# define country groups for squawk codes
SQUAWK_CODE_COUNTRY_GROUPS = get_external_country_groups(
  easa_url = EASA_URL,
  eurocontrol_url = EUROCONTROL_URL)


SQUAWK_LOOKUP = make_squawk_lookup(transpoder_wiki_url = TRANSPONDER_WIKI_URL,country_groups = SQUAWK_CODE_COUNTRY_GROUPS)

squawk_ref1 = SQUAWK_LOOKUP |> 
  select(iso_code,squawk,squawk_info) |> 
  filter(!is.na(iso_code))

squawk_ref2 = SQUAWK_LOOKUP |> 
  filter(is.na(iso_code))|> 
  select(squawk,extra_squawk_info=squawk_info) |> 
  distinct() |>
  ungroup() |> 
  mutate(sq_priority = 1)

# should be one row per country and squawk
SQUAWK_LOOKUP |> 
  group_by(squawk,new_sq_country) |> 
  summarise(nr=n(),.groups='drop')





# Install from GitHub
# devtools::install_github("luisgasco/openskyr")
pak::pak("luisgasco/openskyr")
renv::snapshot()
library(openskyr)

# Retrieve states for a specific Unix timestamp (requires authentication)
historical_states <- tibble(data.frame(get_state_vectors(
)))


as_datetime(1776783910)


usethis::edit_r_environ()

state_vectors_df <- get_state_vectors(username="your_username",password="your_password")


# Run #

raw_data=get_all_live_flights(url = OPENSKY_NETWORK_URL)




prepped_data=raw_data |> 
  as_tibble() |> 
  mutate(across(where(is.character), ~type.convert(., as.is = TRUE))) |> 
  select(icao24,callsign,origin_country,time_position,last_contact,longitude,latitude,baro_altitude,geo_altitude,on_ground,velocity,true_track,vertical_rate,squawk,spi)|> 
  mutate(callsign=trimws(callsign)) |> 
  mutate(callsign=if_else(callsign==""|is.na(callsign),icao24,callsign)) |>
  group_by(callsign) |> 
  mutate(n_icoa24 = length(unique(icao24))) |> 
  ungroup() |> 
  mutate(callsign = if_else(n_icoa24>1,paste0(callsign,"--",icao24),callsign)) |> 
  select(-n_icoa24) |> #view()
  mutate(on_ground=as.character(on_ground)) |> 
  mutate(geo_country = get_countries_vectorised(longitude, latitude,COUNTRIES_SP)) |> 
  mutate(assigned_country = if_else(is.na(geo_country),origin_country,geo_country)) |> 
  mutate(iso_code = suppressWarnings(countrycode(assigned_country, 
                                                 origin = 'country.name', 
                                                 destination = 'iso3c'))) |> 
  mutate(squawk = sprintf("%04d", as.numeric(squawk))) |>
  left_join(squawk_ref1) |> 
  left_join(squawk_ref2,relationship = "many-to-many") |>
  mutate(squawk_info = if_else(is.na(squawk_info),extra_squawk_info,squawk_info)) |>
  mutate(sq_priority = if_else(!is.finite(sq_priority),9,sq_priority)) |> 
  mutate(sq_priority = if_else(sq_priority==9 & !is.na(squawk_info),4,sq_priority)) |> 
  select(-extra_squawk_info) |> 
  mutate(squawk_info=trimws(squawk_info)) |> 
  ungroup()|> 
  select(icao24,callsign,time_position, last_contact, origin_country,assigned_country,iso_code,
         on_ground,spi,
         longitude,latitude,baro_altitude,geo_altitude,velocity,true_track,vertical_rate,squawk,squawk_info,
         sq_priority) |> 
  distinct()


# for a pair of variables, make a linear model and identify outliers
get_multiple_bivariate_outliers = function(input_df, variable_pairs, split_var = "on_ground") {
  
  master_df <- input_df
  n <- nrow(input_df)
  master_df$.id <- seq_len(n)
  
  for (pair in variable_pairs) {
    v1 <- pair[1]
    v2 <- pair[2]
    new_col_name <- paste0(v1, "_vs_", v2, "_centile")
    
    master_df[[new_col_name]] <- NA_integer_
    groups <- unique(na.omit(master_df[[split_var]]))
    
    for (grp in groups) {
      temp_df <- master_df %>%
        filter(.data[[split_var]] == grp) %>%
        filter(if_all(all_of(c(v1, v2)), is.finite))
      
      if (nrow(temp_df) < 15) {
        warning(paste("Skipping group", grp, "for", new_col_name, "- too few rows."))
        next
      }
      
      form <- as.formula(paste(v1, "~", v2))
      mod <- try(lm(form, data = temp_df), silent = TRUE)
      if (inherits(mod, "try-error")) next
      
      # Use Studentised Residuals (measures 'distance' from the expected line)
      # take the absolute value because being far 'above' or 'below' the line is equally abnormal
      dist_from_norm <- abs(rstudent(mod))
      
      # Convert to Centiles (1 to 100) using ntile
      # 100 = most abnormal/distant from the expected value
      centiles <- ntile(dist_from_norm, 100)
      
      group_ids <- temp_df$.id
      master_df[[new_col_name]][master_df$.id %in% group_ids] <- centiles
    }
    
    # Fill cases that couldn't be modeled with 0 or NA
    master_df[[new_col_name]][is.na(master_df[[new_col_name]])] <- 0
  }
  
  return(master_df %>% select(-.id) %>% as_tibble())
}




univariate_centiles_data = prepped_data |> 
  select(callsign,time_position,on_ground,baro_altitude,geo_altitude,velocity,vertical_rate) |> 
  gather(metric,value,-c(callsign,time_position,on_ground)) |> 
  mutate(metric = paste0(metric,"_centile")) |> 
  filter(is.finite(value)) |> 
  group_by(metric, on_ground) |> 
  mutate(centile = ntile(value, 100)) |> 
  ungroup() |> 
  select(-value) |>
  spread(metric,centile) |>
  ungroup()


bivariate_centiles_data=get_multiple_bivariate_outliers(prepped_data,LM_VARIABLE_PAIRS,"on_ground")

bivariate_centiles_data_redux = bivariate_centiles_data |> 
  select(callsign,time_position,baro_altitude_vs_geo_altitude_centile,baro_altitude_vs_velocity_centile,geo_altitude_vs_velocity_centile,time_position_vs_last_contact_centile)

data_with_centiles = prepped_data |> 
  left_join(univariate_centiles_data) |> 
  left_join(bivariate_centiles_data_redux)

data_with_centiles_redux = data_with_centiles |> 
  select(callsign,time_position,on_ground,contains("centile"))

data_with_centiles_redux_long = data_with_centiles_redux |> 
  gather(metric,centile,-c(callsign,time_position,on_ground)) |> 
  filter(is.finite(centile)) |> 
  # filter(on_ground=='FALSE') |>
  # group_by(callsign) |>
  # mutate(mean_centile = mean(centile,na.rm=T)) |>
  ungroup()


# ggplot(data_with_centiles_redux_long,aes(metric,reorder(callsign,mean_centile)))+
#   geom_tile(aes(fill = centile))+
#   scale_fill_viridis_b(direction=-1)


ggplot(
  data_with_centiles_redux_long,
  aes(centile))+
  geom_density()+
  facet_wrap(on_ground~metric,scales='free')

# wide_data <- data_with_centiles_redux_long %>%
#   select(-c(time_position,on_ground)) |>
#   pivot_wider(names_from = callsign, values_from = centile)

# wide_data_no_names = wide_data |> 
#   select(-metric)
# 
# cor_matrix <- cor(wide_data_no_names, use = "pairwise.complete.obs")




final_data = prepped_data |> 
  left_join(data_with_centiles_redux) |>
  mutate(max_time=max(time_position,na.rm=T))


# predict_3d_position
predict_3d_position <- function(callsign, lat, lon, alt, speed_kmh, climb_rate_ms, 
                                initial_time, target_time, bearing) {
  earths_radius <- 6371000
  time_diff_sec <- target_time - initial_time
  dist_m <- (speed_kmh / 3.6) * time_diff_sec
  
  lat1 <- lat * pi / 180
  lon1 <- lon * pi / 180
  brng <- bearing * pi / 180
  
  lat2 <- asin(sin(lat1) * cos(dist_m/earths_radius) + 
                 cos(lat1) * sin(dist_m/earths_radius) * cos(brng))
  
  lon2 <- lon1 + atan2(sin(brng) * sin(dist_m/earths_radius) * cos(lat1),
                       cos(dist_m/earths_radius) - sin(lat1) * sin(lat2))
  
  tibble(
    callsign = callsign,
    predicted_time = target_time,
    predicted_latitude = lat2 * 180 / pi,
    predicted_longitude = lon2 * 180 / pi,
    predicted_altitude = alt + (climb_rate_ms * time_diff_sec),
    elapsed_seconds = time_diff_sec
  )
}

# predict positions
pred_dat <- final_data %>%
  group_by(callsign) %>%
  # Slice(1) ensures we only take the first row of each group
  slice(1) %>% 
  # Use reframe to execute the function per row and return the result
  reframe(predict_3d_position(
    callsign = callsign,
    lat = latitude,
    lon = longitude,
    alt = baro_altitude,
    speed_kmh = velocity,
    climb_rate_ms = vertical_rate,
    initial_time = time_position,
    target_time = max_time,
    bearing = true_track
  ))



pred_dat_plus = final_data |> 
  select(callsign,time_position,on_ground,longitude,latitude,baro_altitude,velocity,vertical_rate,true_track) |> 
  left_join(pred_dat)


library(geosphere) # For accurate lat/long distance

# Helper function for 3D distance between segments
# p1, p2 are start/end of path A; p3, p4 are start/end of path B
dist_3d_segments <- function(p1, p2, p3, p4) {
  # Early exit if any point has missing data to avoid the 'if (NA)' error
  if (any(is.na(c(p1, p2, p3, p4)))) return(NA_real_)
  
  u <- p2 - p1
  v <- p4 - p3
  w <- p1 - p3
  
  a <- sum(u * u); b <- sum(u * v); c <- sum(v * v)
  d <- sum(u * w); e <- sum(v * w)
  D <- a * c - b * b
  
  # Now D is guaranteed to be non-NA
  if (D < 1e-8) {
    sc <- 0.0
    tc <- if (b > c) d / b else e / c
  } else {
    sc <- (b * e - c * d) / D
    tc <- (a * e - b * d) / D
  }
  
  sc <- max(0, min(1, sc))
  tc <- max(0, min(1, tc))
  
  sqrt(sum(((p1 + sc * u) - (p3 + tc * v))^2))
}




# grid_size <- 1.0 
grid_size <- 0.2


# Helper: Check if B is in front of A
is_in_front <- function(lat_a, lon_a, bearing_a, lat_b, lon_b) {
  # Convert bearing to a unit vector (0 deg is North/Y-axis)
  rad <- bearing_a * pi / 180
  vh_x <- sin(rad)
  vh_y <- cos(rad)
  
  # Vector from A to B
  vrel_x <- lon_b - lon_a
  vrel_y <- lat_b - lat_a
  
  # Dot product: Positive = In Front, Negative = Behind
  (vh_x * vrel_x + vh_y * vrel_y) > 0
}

# Assign a grid ID to every aircraft
indexed_data <- pred_dat_plus %>%
  mutate(grid_x = floor(longitude / grid_size),
         grid_y = floor(latitude / grid_size))

# Create a "Neighbor Map"
# This tells R that grid (10,10) should look at (9,9), (9,10), (10,11), etc.
neighbor_offsets <- expand.grid(dx = -1:1, dy = -1:1)

neighbor_map=indexed_data %>%
  select(grid_x, grid_y) %>%
  distinct() %>%
  cross_join(neighbor_offsets) %>%
  mutate(neighbor_x = grid_x + dx,
         neighbor_y = grid_y + dy) %>%
  select(grid_x, grid_y, neighbor_x, neighbor_y)


# Join via the map
# This avoids the memory issue because it only pairs planes in nearby cells
results=indexed_data %>%
  # Link aircraft_a to the map
  inner_join(neighbor_map, by = c("grid_x", "grid_y"), relationship = "many-to-many") %>%
  # Link the map to aircraft_b using the neighbor coordinates
  inner_join(indexed_data, by = c("neighbor_x" = "grid_x", "neighbor_y" = "grid_y"), suffix = c("_a", "_b")) %>%
  # Filter to remove duplicates and self-joins
  # filter(callsign_a < callsign_b) %>%
  filter(is.finite(latitude_a),is.finite(longitude_a),is.finite(baro_altitude_a),
         is.finite(latitude_b),is.finite(longitude_b),is.finite(baro_altitude_b)) |>
  # Filter: Keep only if Aircraft B is in front of Aircraft A
  filter(is_in_front(latitude_a, longitude_a, true_track_a, latitude_b, longitude_b)) %>%
  # 3D Math (only for surviving candidates)
  rowwise() %>%
  mutate(min_sep_m = dist_3d_segments(
    c(longitude_a, latitude_a, baro_altitude_a),
    c(predicted_longitude_a, predicted_latitude_a, predicted_altitude_a),
    c(longitude_b, latitude_b, baro_altitude_b),
    c(predicted_longitude_b, predicted_latitude_b, predicted_altitude_b)
  )) |> 
  group_by(callsign_a) |> 
  mutate(nr = n()) |> 
  ungroup()


ggplot(results,aes(baro_altitude_a,min_sep_m))+
  geom_point()

library("maps")
world <- map_data("world")
head(world)

# geo_lims = list(
#   long=list(min = -30, max = 10),
#   lat=list(min = 25, max = 60))
#   
geo_lims = list(
  long=list(min = -1, max = 0),
  lat=list(min = 51, max = 52))

results_filtered = results |> 
  group_by(callsign_a) |> 
  mutate(callsign_min_sep = min(min_sep_m,na.rm=T)) |> 
  ungroup() |> 
  # filter(callsign_min_sep == min_sep_m) |> 
  # filter(min_sep_m<5e-2) |>
  mutate(facet_col = paste0(callsign_a,"\n",round(baro_altitude_a)," ",round(callsign_min_sep))) |> 
  ungroup() |> 
  # filter(
  #   longitude_a>geo_lims$long$min,
  #   longitude_a<geo_lims$long$max,
  #   latitude_a>geo_lims$lat$min,
  #   latitude_a<geo_lims$lat$max) |>
  ungroup()

ggplot(results_filtered,aes(baro_altitude_a,min_sep_m))+
  geom_point(aes(colour=on_ground_a))

filtered_world =  world |>
  filter(
    long>geo_lims$long$min,
    long<geo_lims$long$max,
    lat>geo_lims$lat$min,
    lat<geo_lims$lat$max)

# Plot world map
ggplot(filtered_world, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="gray40") +
  theme_minimal()+
  geom_point(data = results_filtered,
             aes(longitude_b,latitude_b,
                 colour=min_sep_m<1e-4,group=1))+
  geom_point(data = results_filtered,
             aes(predicted_longitude_b,predicted_latitude_b,
                 colour=min_sep_m<1e-4,group=1))+
  geom_point(data = results_filtered,
             aes(longitude_a,latitude_a,
                 colour=min_sep_m<1e-4,group=1))+
  geom_text(data = results_filtered |> 
              filter(min_sep_m<1e-4) |> ungroup(),
            aes(longitude_a,latitude_a,label = callsign_a,group=1))+
  geom_point(data = results_filtered,
             aes(predicted_longitude_a,predicted_latitude_a,
                 colour=min_sep_m<1e-4,group=1))+
  theme()


# filter(baro_altitude_a>5e3) |> 
# filter(nr>10) |>
# sample_frac(0.01) |> 
# filter(nr==max(nr,na.rm = T)) |> 
# filter(callsign_a == "GBHOR") |>
# filter(grepl(callsign_a)) |>

plot_data = results_filtered |> 
  # filter(min_sep_m<1e-2) |> 
  # filter(callsign_a=='BAW493U') |> 
  ungroup()
plot_data
ggplot(plot_data)+
  geom_segment(aes(x = longitude_b,
                   y= latitude_b,
                   xend = predicted_longitude_b ,
                   yend = predicted_latitude_b))+
  geom_point(aes(x=longitude_b,latitude_b),size=1)+
  geom_text(aes(x=longitude_b,latitude_b,label = callsign_b),size=3)+
  geom_point(aes(x=predicted_longitude_b,predicted_latitude_b),colour='grey',size=1)+
  geom_text(aes(x=predicted_longitude_b,predicted_latitude_b,label = predicted_altitude_b),colour='grey',size=3)+
  geom_segment(data = plot_data |> select(facet_col,callsign_a,
                                          baro_altitude_a,
                                          longitude_a,
                                          latitude_a,
                                          predicted_longitude_a,
                                          predicted_latitude_a),
               aes(x = longitude_a,
                   y = latitude_a,
                   xend = predicted_longitude_a,
                   yend = predicted_latitude_a),colour='blue',linetype = 2)+
  geom_point(data = plot_data |> select(facet_col,callsign_a,
                                        baro_altitude_a,
                                        longitude_a,
                                        latitude_a,
                                        predicted_longitude_a,
                                        predicted_latitude_a),
             aes(x = longitude_a,
                 y = latitude_a),colour='blue')+
  geom_point(data = plot_data |> select(facet_col,callsign_a,
                                        baro_altitude_a,
                                        longitude_a,
                                        latitude_a,
                                        predicted_longitude_a,
                                        predicted_latitude_a),
             aes(x = predicted_longitude_a,
                 y = predicted_latitude_a),colour='blue',alpha=.5)+
  # coord_fixed(ratio = 1)+
  facet_wrap(facet_col~.,scales='free')+
  theme_bw()





library("plotly")
make_3d_plot = function(data,x,y,z,group){
  
  # Plot: mode 'lines+markers' connects points within the same group
  fig <- plot_ly(data, x = ~x, y = ~y, z = ~z, color = ~group, 
                 type = 'scatter3d', mode = 'lines+markers')
  
  fig
}

pdat = plot_data |> 
  # filter(callsign_a=="WZZ58NF")
filter(callsign_a=="DAL1540")
pdat

# DAL1540
# DAL931

pdat2 = pdat |> 
  select(callsign_a,
         longitude_a,latitude_a,
         baro_altitude_a,
         predicted_latitude_a,
         predicted_longitude_a,
         predicted_altitude_a,
         callsign_b,longitude_b,latitude_b,baro_altitude_b,
         predicted_latitude_b,
         predicted_longitude_b,
         predicted_altitude_b) |> 
  gather(metric,value,-c(callsign_a,callsign_b)) |> #view()
  mutate(metric = gsub("baro_","",metric)) |>
  mutate(timestate=if_else(grepl("predicted",metric),1,0)) |> 
  mutate(callsign=if_else(grepl("_a$",metric),callsign_a,callsign_b)) |> 
  mutate(metric = gsub("predicted_","",metric)) |> 
  mutate(metric = gsub("_a$","",metric)) |> 
  mutate(metric = gsub("_b$","",metric)) |> 
  ungroup() |> 
  select(callsign,timestate,metric,value) |> 
  # view()
  distinct() |> 
  spread(metric,value)
pdat2$longitude[1]
pdat2$latitude[1]
# -83.3641,42.2292

pdat2 |> 
  filter(callsign %in% c("DAL1540","DAL931")) |> 
  view()


make_3d_plot(data=pdat2,
             x= pdat2$longitude,
             y=pdat2$latitude ,
             z=pdat2$altitude ,
             group=pdat2$callsign)

ggplot(final_data |> 
         filter(is.finite(geo_altitude),is.finite(velocity),is.finite(geo_altitude_vs_velocity_centile)),aes(geo_altitude,velocity))+
  geom_point(aes(colour=geo_altitude_vs_velocity_centile>99))+
  scale_color_manual(values=c('grey','red2'))+
  geom_text(aes(label = callsign,alpha = geo_altitude_vs_velocity_centile>99))+
  scale_alpha_manual(values=c(0,1))+
  facet_wrap(on_ground~.,scales='free')


ggplot(final_data |> 
         filter(is.finite(geo_altitude),is.finite(baro_altitude),is.finite(baro_altitude_vs_geo_altitude_centile)),aes(baro_altitude,geo_altitude))+
  geom_point(aes(colour=baro_altitude_vs_geo_altitude_centile>99))+
  scale_color_manual(values=c('grey','red2'))+
  geom_text(aes(label = callsign,alpha = baro_altitude_vs_geo_altitude_centile>99))+
  scale_alpha_manual(values=c(0,1))+
  facet_wrap(on_ground~.,scales='free')



# final_data = data_with_centiles_redux_long |> 
#   full_join(prepped_data |> 
#   select(callsign,time_position,on_ground,latitude,longitude,origin_country,assigned_country,sq_priority,squawk_info)) |> 
#   filter(centile>99) |>
#   ungroup()
# final_data
# 
names(final_data)

plot_data = final_data |> 
  # mutate(txt=if_else(sq_priority==1,squawk_info,paste0(metric," (",centile,")"))) |> 
  mutate(txt=paste0(metric," (",centile,")")) |> 
  select(callsign,time_position,on_ground,txt,centile,latitude,longitude,origin_country,assigned_country,sq_priority) |> 
  group_by( callsign,time_position,on_ground,latitude,longitude,origin_country,assigned_country,sq_priority) |>
  summarise(txtsum = paste0(sort(unique(txt)),collapse="\n"),.groups='drop') |>
  ungroup()
plot_data


plot_data |> select(
  txtsum,callsign
) |> 
  group_by(txtsum) |> 
  summarise(nr=n(),callsigns = paste0(sort(callsign),collapse = ", "),
            .groups='drop') |> 
  
  ggplot(plot_data,aes(longitude,latitude))+
  geom_text(aes(label=callsign,colour=txtsum))

# Generate the heatmap
# clustering_distance_cols defines how similar groups are
# clustering_method "complete" or "ward.D2" are common choices
library(pheatmap)
pheatmap(cor_matrix, 
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         clustering_method = "ward.D2",
         main = "Pairwise Group Correlations (Clustered)",
         display_numbers = TRUE) # Optional: show cor values


# %>%
#   select(-observation_id) # Remove ID column before math

# Create correlation matrix
cor_matrix <- cor(wide_data, use = "pairwise.complete.obs")
