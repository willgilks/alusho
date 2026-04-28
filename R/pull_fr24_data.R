# Load required libraries
library(httr) # pull openskies flights
library(jsonlite) # pull openskies flights
library(rvest) # pull wiki table
library(rworldmap) # for lat/long country assignment
library(sp) # for country assignment
library(countrycode)
library(tidyverse)
library(geosphere) # For accurate lat/long distance
library("maps")
# install.packages("tidycountries")

library(fuzzyjoin) # Required for range-based matching


# renv::snapshot()
# usethis::edit_r_environ()

# Functions #

#' Function to pull ALL current live flights globally
get_all_live_flights=function(url = "https://opensky-network.org/api/states/all") {
  ft1=Sys.time()
  
  response <- GET(url)
  
  if (status_code(response) == 200) {
    raw_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Convert matrix to a readable Data Frame
    flights_df <- as.data.frame(raw_data$states)
    colnames(flights_df) = c(
      "icao24", "callsign", "origin_country", "time_position", 
      "last_contact", "longitude", "latitude", "baro_altitude", 
      "on_ground", "velocity", "true_track", "vertical_rate", 
      "sensors", "geo_altitude", "squawk", "spi", "position_source")
    
    flights_df$pull_timestamp<-Sys.time()
    
    ft2=Sys.time()
    dur=as.numeric(round(ft2-ft1,2))
    nrow_flights_df = nrow(flights_df)
    print(paste0("Retreived ",nrow_flights_df," rows of data in ",dur," seconds (",
                 round(nrow_flights_df/dur,2)," per second"))
    return(flights_df)
    
  } else {
    stop("Failed to fetch data. Status code: ", status_code(response))
  }
}



# get country name from geo co-ordinates. used in prep
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




## boiler plat function to prep raw positions data from opensky

prep_raw_positions_data=function(input_df,countries_sp = COUNTRIES_SP){
  
  temp_df=input_df
  
  output_df=temp_df |> 
    as_tibble() |> 
    # force any numerics
    mutate(across(where(is.character), ~type.convert(., as.is = TRUE))) |> 
    dplyr::select(pull_timestamp,pull_iteration,icao24,callsign,origin_country,time_position,last_contact,longitude,latitude,baro_altitude,geo_altitude,on_ground,velocity,true_track,vertical_rate,squawk,spi)|> 
    # if callsign is missing, replace with icao24.
    mutate(callsign=trimws(callsign)) |> 
    mutate(callsign=if_else(callsign==""|is.na(callsign),icao24,callsign)) |>
    # if there is more than one icoa code per callsign, then concatenate the two as callsign.
    group_by(callsign,pull_timestamp) |> 
    mutate(n_icoa24 = length(unique(icao24))) |> 
    ungroup() |> 
    mutate(callsign = if_else(n_icoa24>1,paste0(callsign,"--",icao24),callsign)) |> 
    dplyr::select(-n_icoa24) |>
    mutate(on_ground=as.character(on_ground)) |> 
    # get country the aircraft is in, from log and lat.
    mutate(geo_country = get_countries_vectorised(longitude, latitude,countries_sp)) |> 
    # assign iso codes and a default country name, which is the country the ac is assumed to be in at the time.
    mutate(origin_iso_code = suppressWarnings(countrycode(origin_country, origin = 'country.name', destination = 'iso3c'))) |>
    mutate(origin_country = suppressWarnings(countrycode(origin_iso_code, origin = 'iso3c', destination = 'country.name'))) |>
    mutate(geo_iso_code = suppressWarnings(countrycode(geo_country, origin = 'country.name', destination = 'iso3c'))) |>
    mutate(geo_country = suppressWarnings(countrycode(geo_iso_code, origin = 'iso3c', destination = 'country.name'))) |>
    mutate(assigned_country = if_else(is.na(geo_country),origin_country,geo_country)) |>
    mutate(assigned_iso_code = if_else(is.na(geo_iso_code),origin_iso_code,geo_iso_code)) |>
    mutate(squawk = sprintf("%04d", as.numeric(squawk))) |>
    mutate(time_position_dt=as_datetime(time_position),last_contact_dt=as_datetime(last_contact)) |> 
    dplyr::select(pull_timestamp,pull_iteration,icao24,callsign,
                  time_position,time_position_dt,
                  last_contact, last_contact_dt,
                  origin_country,origin_iso_code,
                  geo_country,geo_iso_code,
                  assigned_country,assigned_iso_code,
                  on_ground,spi,longitude,latitude,baro_altitude,geo_altitude,velocity,true_track,vertical_rate,squawk) |> 
    distinct()
  
  return(output_df)
}



# add_squawk_info add info column on squawk codes from lookup 
add_squawk_info=function(data, squawk_col = "squawk", country_col = "assigned_country", squawk_ref) {
  
  data_clean <- data %>%
    mutate(join_sq = stringr::str_pad(as.character(.data[[squawk_col]]), 4, "left", "0"))
  
  result <- fuzzyjoin::fuzzy_left_join(
    data_clean, squawk_ref,
    by = c("join_sq" = "range_start", "join_sq" = "range_end"),
    match_fun = list(`>=`, `<=`)) %>%
    filter(
      is.na(country_match) | country_match == "Global" | .data[[country_col]] == country_match |
        (country_match == "Europe" & 
           countrycode::countrycode(.data[[country_col]], "country.name", "continent") == "Europe")) %>%
    mutate(
      squawk_description = ifelse(is.na(usage_description), "Discrete / Local ATC Assignment", usage_description),
      squawk_priority = ifelse(is.na(squawk_priority), 3, squawk_priority)) %>%
    dplyr::select(-join_sq, -range_start, -range_end, -country_match, -usage_description) %>%
    distinct()
  
  return(result)
}





# for a pair of variables, make a linear model and identify outliers
make_bivariate_outlier_score = function(input_df, variable_pairs, split_var = "on_ground") {
  
  master_df <- input_df
  n <- nrow(input_df)
  master_df$.id <- seq_len(n)
  
  for (pair in variable_pairs) {
    v1 <- pair[1]
    v2 <- pair[2]
    # new_col_name <- paste0(v1, "_vs_", v2, "_centile")
    new_col_name <- paste0(v1, "_vs_", v2, "_outlier_score")
    
    master_df[[new_col_name]] <- NA_integer_
    groups <- unique(na.omit(master_df[[split_var]]))
    
    for (grp in groups) {
      temp_df <- master_df |>
        filter(.data[[split_var]] == grp) |>
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
      
      group_ids <- temp_df$.id
      master_df[[new_col_name]][master_df$.id %in% group_ids] <- dist_from_norm
    }
    
    # Fill cases that couldn't be modeled with 0 or NA
    master_df[[new_col_name]][is.na(master_df[[new_col_name]])] <- 0
  }
  
  return(master_df |> dplyr::select(-.id) |> as_tibble() |> 
           mutate(altitude_ratio=baro_altitude/geo_altitude,
                  geo_alt_velo_ration=geo_altitude/velocity,
                  baro_alt_velo_ratio=baro_altitude/velocity,
                  time_ratio=time_position/last_contact))
}




# predict_3d_position
predict_3d_position=function(
    callsign,
    lat,lon,
    alt,
    speed_kmh,
    climb_rate_ms,
    initial_time,
    target_time,
    bearing) {
  
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
    elapsed_seconds = time_diff_sec) |> 
    mutate(predicted_altitude = if_else(predicted_altitude<0,0,predicted_altitude))
}





# dist_3d_segments calculate 3D distance between segments
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





## Constants #
OPENSKY_NETWORK_URL = "https://opensky-network.org/api/states/all"

WORLD <- map_data("world")


LM_VARIABLE_PAIRS = list(
  c("baro_altitude", "geo_altitude"),
  c("geo_altitude", "velocity"),
  c("baro_altitude", "velocity"),
  c("time_position", "last_contact"))


# countries spatial dataframe for geo-position based country assignment.
COUNTRIES_SP=getMap(resolution = 'low')


TIDYCOUNTRIES_DATA <- get_country_info(country_value = "all", geometry = FALSE)


COUNTRY_AREAS=TIDYCOUNTRIES_DATA |>
  dplyr::select(assigned_iso_code=cca3,area,population)


SQUAWK_LOOKUP=tibble::tribble(
  ~range_start,~range_end,~country_match,~usage_description,~squawk_priority,
  "7500","7500","Global","Unlawful Interference (Hijack)",1,
  "7600","7600","Global","Radio Communication Failure",1,
  "7700","7700","Global","General Emergency",1,
  "0030","0030","United Kingdom","FIR Lost Aircraft",2,
  "0020","0020","United Kingdom","Air Ambulance / Medivac",2,
  "0001","0001","United Kingdom","Height Monitoring Unit",2,
  "0033","0033","United Kingdom","Parachute Dropping",2,
  "0025","0025","Germany","Parachute Dropping",2,
  "1200","1200","United States","Standard VFR Conspicuity",3,
  "1200","1200","Canada","Standard VFR Conspicuity",3,
  "7000","7000","Europe","Standard VFR Conspicuity",3,
  "2000","2000","Global","IFR Conspicuity (Non-SSR area)",3,
  "1000","1000","Europe","Mode S IFR (Multiple Countries)",3,
  "0101","0117","Belgium","Transit (ORCAM) Brussels",3,
  "0601","0637","Germany","Transit (ORCAM) Germany",3,
  "0640","0677","France","Transit (ORCAM) Paris",3,
  "0701","0727","Maastricht","Transit (ORCAM) Maastricht",3,
  "7501","7537","Switzerland","Transit (ORCAM) Geneva",3,
  "7540","7547","Germany","Transit (ORCAM) Bremen",3,
  "7550","7577","France","Transit (ORCAM) Paris",3,
  "1177","1177","United Kingdom","London Information FIS",3,
  "3000","3000","Australia","Civil IFR Class G Airspace",3,
  "4000","4000","United States","VFR Military Training Routes",3,
  "7777","7777","Global","Military Intercept / Ground Test",3)


write_csv_timestamped = function(x, filename = "data", path = "~/Desktop") {
  ts = format(Sys.time(), "%Y%m%d%H%M%S")
  full_name = paste0(filename, "_", ts, ".csv")
  full_path = file.path(path, full_name)
  readr::write_csv(x, full_path)
}


# Run
raw_data=get_all_live_flights(url = OPENSKY_NETWORK_URL)

raw_data=bind_rows(lapply(1:3,function(z){
  Sys.sleep(2)
  get_all_live_flights(url = OPENSKY_NETWORK_URL) |> 
    mutate(pull_iteration = z)
}))


# write to file
write_csv_timestamped(x = raw_data,filename = "opensky_data")


# repair callsigns, assign countries.
prepped_data = prep_raw_positions_data(input_df=raw_data,countries_sp = COUNTRIES_SP)


# add squawk info and urgency ranking
data_with_squawk_info = add_squawk_info(data = prepped_data,squawk_col = "squawk",country_col = "assigned_country",squawk_ref = SQUAWK_LOOKUP)

# get urgent squaks data
data_with_squawk_info |> 
  filter(squawk_priority==1) |>
  dplyr::select(
    icao24,callsign,squawk,squawk_description,
    time_position_dt,
    last_contact_dt,
    origin_country,geo_country,on_ground,
    longitude,latitude,baro_altitude,geo_altitude,
    velocity,true_track,vertical_rate,
    time_position,last_contact) |> 
  distinct()

# calculate outlier scores for pairs of measures
# e.g. barometric and geo altitude should be broadly correlated
bivariate_outlier_scores_data=make_bivariate_outlier_score(data_with_squawk_info,LM_VARIABLE_PAIRS,"on_ground")


make_scatter_plot = function(
    input_df = bivariate_outlier_scores_data, 
    xvar = "baro_altitude",
    yvar = "geo_altitude",
    colour_var = "baro_altitude_vs_geo_altitude_outlier_score",
    colour_thr = 1,
    txt_thr = 1,
    show_txt = FALSE){
  
  plot_df = input_df |> 
    mutate(colour_col = if_else(.data[[colour_var]] > colour_thr, "outlier", "in bounds"))
  
  ggplot(plot_df, aes(x = !!sym(xvar), y = !!sym(yvar))) +
    geom_point(aes(colour = colour_col)) +
    theme(
      legend.position = ""
    )
}

# make_scatter_plot(
#   bivariate_outlier_scores_data, 
#   "baro_altitude","geo_altitude",
#   "baro_altitude_vs_geo_altitude_outlier_score",
#   colour_thr = 1,txt_thr = 1)



#   # geom_text(data = bivariate_outlier_scores_data |> filter(1.5<baro_altitude_vs_geo_altitude_outlier_score),
#             # aes(label = callsign),size=3)+
#   theme(
#     legend.position = ""
#   )


cowplot::plot_grid(
  
  make_scatter_plot(
    bivariate_outlier_scores_data, 
    "baro_altitude","geo_altitude",
    "baro_altitude_vs_geo_altitude_outlier_score",
    colour_thr = 1.5,txt_thr = 1)
  ,
  
  make_scatter_plot(
    bivariate_outlier_scores_data, 
    "geo_altitude","velocity",
    "geo_altitude_vs_velocity_outlier_score",
    colour_thr = 2,txt_thr = 1)
  ,
  
  make_scatter_plot(
    bivariate_outlier_scores_data, 
    "baro_altitude","velocity",
    "baro_altitude_vs_velocity_outlier_score",
    colour_thr = 2.5,txt_thr = 1)
  ,
  
  make_scatter_plot(
    bivariate_outlier_scores_data, 
    "time_position","last_contact",
    "time_position_vs_last_contact_outlier_score",
    colour_thr = 2,txt_thr = 1),align = "hv"
)



library(tidyverse)
library(MASS) ## masks dplyr::select !

assign_density_rings = function(data, x_var, y_var, n_bins = 50) {
  
  clean_data = data |> 
    filter(if_all(all_of(c(x_var, y_var)), is.finite))
  
  # Compute 2D Kernel Density
  dens = MASS::kde2d(clean_data[[x_var]], clean_data[[y_var]], n = n_bins)
  
  # Map densities
  ix = findInterval(clean_data[[x_var]], dens$x)
  iy = findInterval(clean_data[[y_var]], dens$y)
  point_densities = dens$z[cbind(ix, iy)]
  
  # Assign Ring Levels (5 breaks create 5 intervals)
  clean_data |> 
    mutate(
      density_val = point_densities,
      ring_level = factor(
        findInterval(density_val, quantile(density_val, probs = seq(0, 1, by = 0.01)))
        # ,
        # labels = c("Low", "Medium-Low", "Medium-High", "High", "Highest")
      )
    )
}


# df_with_rings = assign_density_rings(data_with_squawk_info,"baro_altitude", "velocity")


df_with_rings = bind_rows(
  lapply(split(
    data_with_squawk_info,data_with_squawk_info$pull_iteration),
    function(z_df){
      assign_density_rings(z_df,"baro_altitude", "velocity")
    }))

ggplot(df_with_rings,aes(baro_altitude,velocity))+
  geom_point(aes(colour = as.numeric(ring_level)>2))+
  facet_wrap(pull_iteration~.,ncol=2)

## prep world plot data
world_density_pdat = WORLD |> 
  mutate(assigned_iso_code = suppressWarnings(countrycode(region, origin = 'country.name', destination = 'iso3c')))

world_map_isos = sort(unique(world_density_pdat$assigned_iso_code))

names(bivariate_outlier_scores_data)
aircraft_density=bivariate_outlier_scores_data |> 
  filter(pull_iteration==max(pull_iteration,na.rm = T)) |> 
  group_by(pull_iteration,pull_timestamp,assigned_country,assigned_iso_code) |>
  summarise(n=n(),.groups = 'drop')

aircraft_density_plus = tibble(assigned_iso_code = world_map_isos) |> 
  left_join(aircraft_density) |>
  mutate(assigned_country = suppressWarnings(countrycode(assigned_iso_code,origin='iso3c',destination = 'country.name'))) |> 
  left_join(COUNTRY_AREAS) |>
  distinct() |> 
  mutate(density = n/area,per_head = n/population) |>
  ungroup() |> 
  arrange(desc(density),assigned_country,assigned_iso_code)






# ggplot(aircraft_density |> 
#          filter(per_head<1e-5,
#                 density<2e-4) |> 
#          ungroup(),aes(density,per_head))+
#   geom_text(aes(label=assigned_iso_code)) +
#   facet_wrap(on_ground~.,scales='free',ncol=1)



geo_lims = list(
  long=list(min = -120, max = 180),
  lat=list(min = -50, max = 65))



world_density_pdat_plus=world_density_pdat|>
  full_join(aircraft_density_plus)|>
  mutate(density = if_else(!is.finite(density),0,density),
         per_head = if_else(!is.finite(per_head),0,per_head)) |> 
  mutate(log10_density = log10(density),
         log10_per_head = log10(per_head)) |> 
  mutate(log10_density = if_else(!is.finite(log10_density),as.numeric(NA),log10_density),
         log10_per_head = if_else(!is.finite(log10_per_head),as.numeric(NA),log10_per_head)) |> 
  mutate(min_log10_density=min(log10_density,na.rm = T),
         min_log10_per_head=min(log10_per_head,na.rm = T)) |> 
  ungroup() |> 
  mutate(log10_density=if_else(!is.finite(log10_density),min_log10_density,log10_density),
         log10_per_head=if_else(!is.finite(log10_per_head),min_log10_per_head,log10_per_head)) |> 
  ungroup()


map_colours3 = c(low="skyblue",mid = "forestgreen",high = 'red3')

# world map, aircraft density, per skm.
make_map_plot = function(
    
  input_df=world_density_pdat_plus,
  colour_col="log10_per_head",
  map_colours=map_colours3,
  map_geo_lims = geo_lims,
  map_title){
  
  plot_df = input_df
  
  # colour_midpoint = ceiling(median(world_density_pdat_plus[[colour_col]],na.rm = T))
  # colour_midpoint = median(world_density_pdat_plus[[colour_col]],na.rm = T)
  colour_midpoint = mean(world_density_pdat_plus[[colour_col]],na.rm = T)
  
  print(colour_midpoint)
  
  plot_timestamp_text = paste0(round(sort(unique(plot_df$pull_timestamp))),collapse=',')
  
  
  range_long = range(plot_df$long,na.rm = T)
  range_lat = range(plot_df$lat,na.rm = T)
  
  len_lon = map_geo_lims$long$max-map_geo_lims$long$min
  len_lat = map_geo_lims$lat$max- map_geo_lims$lat$min
  
  adj_lon = .05 * len_lon
  adj_lat = .025 * len_lat
  
  txt_long = map_geo_lims$long$max - adj_lon
  txt_lat = map_geo_lims$lat$min + adj_lat
  
  ggplot(plot_df, aes(x=long, y=lat, group=group)) +
    geom_polygon(aes_string(colour = colour_col,fill = colour_col),linewidth = 0) +
    geom_polygon(fill=NA,colour='grey30',linewidth = .05) +
    scale_colour_gradient2(name="",low=map_colours[1],mid=map_colours[2],high = map_colours[3],
                           midpoint = colour_midpoint,
                           na.value = map_colours[1])+
    scale_fill_gradient2(name="",low=map_colours[1],mid=map_colours[2],high = map_colours[3],
                         midpoint = colour_midpoint,
                         na.value = map_colours[1])+
    annotate("text",x = txt_long,y=txt_lat,label=plot_timestamp_text,size=3,alpha=.8)+
    coord_cartesian(
      xlim = c(
        map_geo_lims$long$min,
        map_geo_lims$long$max),
      ylim = c(map_geo_lims$lat$min,
               map_geo_lims$lat$max))+
    labs(title = map_title)+
    theme_minimal(base_size = 18)+
    theme(
      axis.text=element_text(size = 8),
      axis.title = element_blank(),
      legend.position = "right",
      legend.justification = 'top',
      panel.spacing.x = unit(2,'lines'),
      panel.spacing.y = unit(2,'lines'),
      panel.grid = element_blank(),
      panel.background = element_rect(linewidth = .1),
      plot.background = element_rect(linewidth = .1),
      plot.title = element_text(hjust = .5))
}


# hist(world_density_pdat_plus$log10_density)
# world map, density of aircraft, per sq km
make_map_plot(
  input_df=world_density_pdat_plus,
  colour_col="log10_density",
  # colour_col="density",
  map_colours=map_colours3,
  map_geo_lims = geo_lims,
  map_title = "Aircraft per square kilometer")

# world map, aircraft by population size.
make_map_plot(
  input_df=world_density_pdat_plus,
  colour_col="log10_per_head",
  map_colours=map_colours3,
  map_geo_lims = geo_lims,
  map_title = "Aircraft per person population")





plist = lapply(split(aircraft_density_plus,aircraft_density_plus$on_ground),function(z){
  pdat=z |> 
    slice_max(order_by = density, n = 10) |> 
    mutate(facet_col = if_else(on_ground=="TRUE","On ground","Airbourne"))
  
  plot_timestamp_text = paste0(round(sort(unique(pdat$pull_timestamp))),collapse=',')
  
  
  ggplot(pdat,aes(density,reorder(assigned_country,density)))+
    geom_col()+
    scale_x_continuous(expand = c(0,NA))+
    facet_wrap(facet_col~.,scales='free')+
    theme_minimal()+
    theme(
      axis.title.y = element_blank()
    )
})
cowplot::plot_grid(plotlist = plist,nrow = 2)


# standaridise positions to max time
baseline_standardised_positions=bivariate_outlier_scores_data|>
  mutate(max_time=max(time_position,na.rm=T)) |>
  group_by(callsign) |>
  # take the first row of each group
  slice(1) |> 
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
    bearing = true_track))



baseline_standardised_data = bivariate_outlier_scores_data|>
  mutate(max_time=max(time_position,na.rm=T)) |> 
  left_join(baseline_standardised_positions) |> 
  dplyr::select(icao24,callsign,time_position=predicted_time,contains("_country"),contains("iso_code"),
                on_ground,
                predicted_longitude,
                predicted_latitude,
                predicted_altitude,
                velocity,vertical_rate,true_track,max_time) |> 
  dplyr::rename(
    latitude=predicted_latitude,
    longitude=predicted_longitude,
    baro_altitude=predicted_altitude)




predicted_positions = baseline_standardised_data|>
  mutate(max_time=max_time+1000) |>
  group_by(callsign) |>
  # take the first row of each group
  slice(1) |> 
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
    bearing = true_track))

baseline_and_predicted_positions = baseline_standardised_data |> 
  left_join(predicted_positions)


ggplot(baseline_and_predicted_positions |>
         ungroup(),
       aes(longitude,latitude))+
  geom_polygon(data = WORLD, aes(x=long, y=lat, group=group),fill="white", color="gray40",linewidth = .2) +
  geom_point(size=.1,aes(colour=baro_altitude))+
  geom_point(aes(predicted_longitude,predicted_latitude,colour=predicted_altitude),size=.1)+
  geom_segment(aes(x=longitude,
                   y=latitude,
                   xend = predicted_longitude,
                   yend = predicted_latitude,
                   colour=predicted_altitude))+
  scale_colour_viridis_c()+
  coord_cartesian(
    xlim = c(-100,-150),
    ylim = c(30,85))+
  theme_minimal()

grid_size <- 0.1


# is_in_front Check if aircraft B is in front of aircraft A
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



# Assign grid ID to every aircraft
indexed_data <- baseline_and_predicted_positions |>
  mutate(grid_x = floor(longitude / grid_size),
         grid_y = floor(latitude / grid_size))

# Create Neighbor Map
neighbor_offsets <- expand.grid(dx = -1:1, dy = -1:1)

neighbor_map=indexed_data |>
  dplyr::select(grid_x, grid_y) |>
  distinct() |>
  cross_join(neighbor_offsets) |>
  mutate(neighbor_x = grid_x + dx,
         neighbor_y = grid_y + dy) |>
  dplyr::select(grid_x, grid_y, neighbor_x, neighbor_y)


# Join via the map
# This avoids the memory issue because it only pairs planes in nearby cells
results=indexed_data |>
  # Link aircraft_a to the map
  inner_join(neighbor_map, by = c("grid_x", "grid_y"), relationship = "many-to-many") |>
  # Link the map to aircraft_b using the neighbor coordinates
  inner_join(indexed_data, by = c("neighbor_x" = "grid_x", "neighbor_y" = "grid_y"), suffix = c("_a", "_b")) |>
  # Filter to remove duplicates and self-joins
  filter(callsign_a != callsign_b) |>
  filter(is.finite(latitude_a),is.finite(longitude_a),is.finite(baro_altitude_a),
         is.finite(latitude_b),is.finite(longitude_b),is.finite(baro_altitude_b)) |>
  # Filter: Keep only if Aircraft B is in front of Aircraft A
  filter(is_in_front(latitude_a, longitude_a, true_track_a, latitude_b, longitude_b)) |>
  # minimum separation calculation
  rowwise() |>
  mutate(min_sep_m = dist_3d_segments(
    c(longitude_a, latitude_a, baro_altitude_a),
    c(predicted_longitude_a, predicted_latitude_a, predicted_altitude_a),
    c(longitude_b, latitude_b, baro_altitude_b),
    c(predicted_longitude_b, predicted_latitude_b, predicted_altitude_b)
  )) |> 
  group_by(callsign_a) |> 
  mutate(nr = n()) |> 
  ungroup()





geo_lims_small = list(
  long=list(min = -100, max = -80),
  lat=list(min = 20, max = 40))


results_filtered = results |> 
  group_by(callsign_a) |> 
  mutate(callsign_min_sep = min(min_sep_m,na.rm=T)) |> 
  ungroup() |>
  mutate(facet_col = paste0(callsign_a,"\n",round(baro_altitude_a)," ",round(callsign_min_sep))) |> 
  ungroup() |> 
  # filter(
  #   longitude_a>geo_lims_small$long$min,
  #   longitude_a<geo_lims_small$long$max,
  #   latitude_a>geo_lims_small$lat$min,
  #   latitude_a<geo_lims_small$lat$max) |>
  ungroup()



plot_sep_lim = 1e-4
# Plot world map
ggplot(WORLD, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="gray40") +
  theme_minimal()+
  geom_point(data = results_filtered,
             aes(longitude_b,latitude_b,
                 colour=min_sep_m<plot_sep_lim,size =min_sep_m<plot_sep_lim,group=1))+
  geom_point(data = results_filtered,
             aes(predicted_longitude_b,predicted_latitude_b,
                 colour=min_sep_m<plot_sep_lim,size =min_sep_m<plot_sep_lim,group=1))+
  geom_point(data = results_filtered,
             aes(longitude_a,latitude_a,
                 colour=min_sep_m<plot_sep_lim,size =min_sep_m<plot_sep_lim,group=1))+
  geom_point(data = results_filtered,
             aes(predicted_longitude_a,predicted_latitude_a,
                 colour=min_sep_m<plot_sep_lim,group=1))+
  geom_text(data = results_filtered |>
              filter(min_sep_m<plot_sep_lim) |> ungroup(),
            aes(longitude_a,latitude_a,label = callsign_a,group=1))+
  coord_cartesian(
    xlim = c(
      geo_lims$long$min,
      geo_lims$long$max),
    ylim = c(geo_lims$lat$min,geo_lims$lat$max)
  )+
  theme()



ggplot(results_filtered,aes(min_sep_m,velocity_a))+
  geom_point()

plot_data_prep = results_filtered |> 
  filter(min_sep_m<1e-3) |>
  # filter(on_ground_a=="FALSE",on_ground_b=="FALSE",
  # baro_altitude_a!=0,baro_altitude_b!=0,
  # predicted_altitude_a!=0,predicted_altitude_b!=0,
  # velocity_a>10,velocity_b>10) |>
  # filter(callsign_a=='UAE334') |>
  filter(callsign_a=='FHLIX') |>
  ungroup()

# plot_data_prep
plot_data_prepA=plot_data_prep|> 
  dplyr::select(callsign_a,
                assigned_iso_code_a,on_ground_a,
                time_position_a,
                velocity_a,
                longitude_a,latitude_a,baro_altitude_a,
                predicted_time_a,predicted_longitude_a,predicted_latitude_a,predicted_altitude_a) |> 
  gather(measure,value,-callsign_a) |> 
  mutate(grp="a") |> 
  mutate(measure=gsub("_a$","",measure)) |> 
  dplyr::rename(callsign=callsign_a)


plot_data_prepB=plot_data_prep|> 
  dplyr::select(callsign_b,
                assigned_iso_code_b,on_ground_b,
                time_position_b,
                velocity_b,longitude_b,latitude_b,baro_altitude_b,
                predicted_time_b,predicted_longitude_b,predicted_latitude_b,predicted_altitude_b) |> 
  gather(measure,value,-callsign_b) |> 
  mutate(grp="b") |> 
  mutate(measure=gsub("_b$","",measure)) |> 
  dplyr::rename(callsign=callsign_b)

plot_data=plot_data_prepA |> bind_rows(plot_data_prepB) |> 
  distinct() |>
  spread(measure,value) |> 
  mutate(across(where(is.character), ~type.convert(., as.is = TRUE))) |> 
  mutate(plab1 = paste0(callsign,"\n",baro_altitude,"\n",velocity),
         plab2 = paste0(callsign,"\n",predicted_altitude,"\n",velocity))


ggplot(plot_data,aes(longitude,latitude,colour=grp))+
  geom_polygon(data = WORLD, aes(x=long, y=lat, group=group),fill="gray90", color="gray40") +
  geom_segment(aes(x = longitude,y=latitude,xend=predicted_longitude,yend = predicted_latitude))+
  geom_point(size = .3, alpha = .8)+
  geom_point(aes(predicted_longitude,predicted_latitude), size = .2, alpha = .5)+
  geom_text(aes(label=plab1))+
  geom_text(aes(predicted_longitude,predicted_latitude,label=plab2))+
  scale_x_continuous(expand = c(.2,.7))+
  scale_y_continuous(expand = c(.2,.7))+
  coord_cartesian(expand = T,
                  xlim = range(c(plot_data$longitude,plot_data$predicted_longitude),na.rm=T),
                  ylim = range(c(plot_data$latitude,plot_data$predicted_latitude),na.rm=T))+
  theme_bw()







# library("plotly")
make_3d_plot = function(data,x,y,z,group){
  fig <- plot_ly(data, x = ~x, y = ~y, z = ~z, color = ~group, 
                 text = ~timestate,
                 type = 'scatter3d', mode = 'lines+markers')
  fig
}

pdat = plot_data |> 
  # filter(callsign_a=="DAL1150") |> 
  filter(callsign_a=="BOX444")

pdat2 = pdat |> 
  dplyr::select(callsign_a,
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
  dplyr::select(callsign,timestate,metric,value) |> 
  distinct() |> 
  spread(metric,value)

make_3d_plot(data=pdat2,
             x= pdat2$longitude,
             y=pdat2$latitude ,
             z=pdat2$altitude ,
             group=pdat2$callsign)


