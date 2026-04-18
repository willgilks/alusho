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



# for a pair of variables, make a linear model and identify outliers
get_multiple_bivariate_outliers <- function(input_df, variable_pairs, split_var = "on_ground") {
  
  master_df <- input_df
  n <- nrow(input_df)
  master_df$.id <- seq_len(n)
  
  for (pair in variable_pairs) {
    v1 <- pair[1]
    v2 <- pair[2]
    new_col_name <- paste0(v1, "_vs_", v2)
    
    # Initialize the column in master with 0s
    master_df[[new_col_name]] <- 0
    
    # Get unique values of the split variable (e.g., TRUE and FALSE)
    groups <- unique(na.omit(master_df[[split_var]]))
    
    for (grp in groups) {
      
      # filter data for specific group and pair
      temp_df <- master_df %>%
        filter(.data[[split_var]] == grp) %>%
        filter(if_all(all_of(c(v1, v2)), is.finite))
      
      if (nrow(temp_df) < 15) {
        warning(paste("Skipping group", grp, "for", new_col_name, "- too few rows."))
        next
      }
      
      # Build the simple bivariate model for this group only
      form <- as.formula(paste(v1, "~", v2))
      mod <- try(lm(form, data = temp_df), silent = TRUE)
      
      if (inherits(mod, "try-error")) next
      
      # Calculate Cook's D and levels
      threshold_levels <- (1:5) / n
      cooks_vals <- cooks.distance(mod)
      cooks_vals[is.na(cooks_vals)] <- 0
      
      # Assign scores back to the master_df using IDs
      group_ids <- temp_df$.id
      scores <- findInterval(cooks_vals, threshold_levels)
      
      master_df[[new_col_name]][master_df$.id %in% group_ids] <- scores
    }
  }
  
  return(master_df %>% select(-.id) %>% as_tibble())
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


# Run #

raw_data=get_all_live_flights(url = OPENSKY_NETWORK_URL)




prepped_data=raw_data |> 
  as_tibble() |> 
  mutate(across(where(is.character), ~type.convert(., as.is = TRUE))) |> 
  select(icao24,callsign,origin_country,time_position,last_contact,longitude,latitude,baro_altitude,geo_altitude,on_ground,velocity,true_track,vertical_rate,squawk,spi)|> 
  mutate(callsign=trimws(callsign)) |> 
  mutate(callsign=if_else(callsign==""|is.na(callsign),icao24,callsign)) |>
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
         sq_priority)


# find outliers in pairs of variables
res_dat=get_multiple_bivariate_outliers(prepped_data,LM_VARIABLE_PAIRS,"on_ground")



# plot
ggplot(res_dat,aes(baro_altitude, geo_altitude,fill = baro_altitude_vs_geo_altitude))+
  geom_point(shape=21,stroke=.1,size=3.5)+
  scale_fill_viridis_c()+
  facet_wrap(on_ground~.,scales='free')+
  theme()

ggplot(res_dat,aes(baro_altitude, velocity,fill = baro_altitude_vs_velocity))+
  geom_point(shape=21,stroke=.1,size=3.5)+
  scale_fill_viridis_c()+
  facet_wrap(on_ground~.,scales='free')+
  theme()

ggplot(res_dat,aes(geo_altitude, velocity,fill = geo_altitude_vs_velocity))+
  geom_point(shape=21,stroke=.1,size=3.5)+
  scale_fill_viridis_c()+
  facet_wrap(on_ground~.,scales='free')+
  theme()


ggplot(res_dat,aes(time_position, last_contact,fill = time_position_vs_last_contact))+
  geom_point(shape=21,stroke=.1,size=3.5)+
  scale_fill_viridis_c()+
  facet_wrap(on_ground~.,scales='free')+
  theme()





hard_limits_in_air = list(
  altitude_disparity=c(-5e2,7e2),
  baro_altitude = c(0,14e3),
  geo_altitude = c(0,14e3),
  last_contact_delay = c(0,300),
  time_pos_delay = c(0,300),
  velocity = c(0,300),
  vertical_rate = c(-15,15)
)


hard_limits_on_ground = list(
  baro_altitude = c(0,5e3),
  last_contact_delay = c(0,300),
  time_pos_delay = c(0,300),
  velocity = c(0,50),
  vertical_rate = c(-3.5,0)
)

{
  
  vlines_dat_hard_limits_in_air = bind_rows(lapply(names(hard_limits_in_air), function(z){
    tibble(metric = z, lims = unlist(hard_limits_in_air[z])) |> 
      mutate(on_ground = "FALSE")
  }))
  
  
  vlines_dat_hard_limits_on_ground = bind_rows(lapply(names(hard_limits_on_ground), function(z){
    tibble(metric = z, lims = unlist(hard_limits_on_ground[z]))|> 
      mutate(on_ground = "TRUE")
  }))
  
  vlines_dat_wide = vlines_dat_hard_limits_in_air |>
    bind_rows(vlines_dat_hard_limits_on_ground) |> 
    group_by(on_ground,metric) |> 
    mutate(lab = if_else(lims == max(lims),"max_lim","min_lim")) |> 
    ungroup() |> 
    spread(lab,lims)
  # vlines_dat_wide
  
  
  
  # alternative method
  # assign centiles to data and take e.g. first two centiles etc.
  # Assuming your tibble is named 'my_data'
  # grouping_col1/2 are the categories, value_col is what you're ranking
  
  
  long_data = prepped_data |> 
    select(callsign,time_position,on_ground,baro_altitude,geo_altitude,velocity,vertical_rate) |> 
    gather(metric,value,-c(callsign,time_position,on_ground)) |> 
    filter(is.finite(value)) |> 
    group_by(metric, on_ground) %>%
     mutate(centile = ntile(value, 100)) %>%
    ungroup()
  
  
  ## distributions plot data
  
  
