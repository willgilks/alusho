# Load required libraries
library(httr)
library(jsonlite)
library(tidyverse)

my_token <- Sys.getenv("FR24_SANDBOX")


#' Function to pull ALL current live flights globally
get_all_live_flights <- function() {
  ft1=Sys.time()
  # OpenSky 'all' endpoint for global state vectors
  url <- "https://opensky-network.org/api/states/all"
  
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

# Execute the pull
raw_data <- get_all_live_flights()
raw_data[1:3,1:10]
prepped_data=raw_data |> 
  as_tibble() %>%
  mutate(across(where(is.character), ~type.convert(., as.is = TRUE))) |> 
  select(icao24,callsign,origin_country,time_position,last_contact,longitude,latitude,baro_altitude,geo_altitude,on_ground,velocity,true_track,vertical_rate,squawk,spi)|> 
  mutate(callsign=trimws(callsign)) |> 
  mutate(callsign=if_else(callsign==""|is.na(callsign),icao24,callsign)) |> 
  mutate(max_time_pos=max(time_position,na.rm=T),
         max_last_contact=max(last_contact,na.rm=T)) |> 
  # mutate(time_pos_delay = max_time_pos - time_position,
  # last_contact_delay = max_last_contact - last_contact,
  # altitude_disparity = (geo_altitude - baro_altitude)) |> 
  mutate(on_ground=as.character(on_ground)) |> 
  ungroup()
prepped_data


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




my_pairs <- list(
  c("baro_altitude", "geo_altitude"),
  c("geo_altitude", "velocity"),
  c("baro_altitude", "velocity"),
  c("time_position", "last_contact")
)


res_dat = get_multiple_bivariate_outliers(prepped_data,my_pairs,"on_ground")
names(res_dat)


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

squawk_sum = res_dat |> 
  group_by(squawk) |> 
  summarise(
    nr=n(),callsigns=paste0(sort(unique(callsign)),collapse=','),.groups='drop'
  ) |> 
  mutate(squawk=as.character(squawk)) |> 
  # mutate(squawk=fct_reorder(squawk,nr)) |> 
  mutate(squawk=as.integer(squawk)) 

squawk_sum




library(rvest)

# Define the URL
url <- "https://en.wikipedia.org/wiki/List_of_transponder_codes"

# Read the HTML and extract the table
transponder_dat_raw <- url %>%
  read_html() %>%
  html_element(".wikitable") %>%
  html_table(fill = TRUE)

transponder_dat = transponder_dat_raw |> 
  dplyr::rename(squawk=Code,sq_country=Countries,sq_usage=`Allocated use`) |> 
  # mutate(sq_full=paste(squawk,sq_country,sq_usage)) |> 
  ungroup()


exploded_transponder_dat <- transponder_dat %>%
  separate_rows(squawk, sep = ",") %>%
  mutate(squawk = trimws(squawk)) %>%
  rowwise() %>%
  mutate(squawk = list(
    
    # Matches any non-digit (hyphen, en-dash, em-dash)
    if(grepl("\\D", squawk)) {
      
      vals <- as.numeric(unlist(strsplit(squawk, "\\D")))
      sprintf("%04d", seq(min(vals, na.rm = TRUE), max(vals, na.rm = TRUE)))
    } else {
      squawk
    }
  )) %>%
  unnest(squawk) %>%
  ungroup()


exploded_transponder_dat
compressed_transponder_dat = exploded_transponder_dat |> 
  mutate(sq_lab = paste0(sq_country," (",sq_usage,")")) |> 
  group_by(squawk) |> 
  summarise(squawk_info = paste0(sort(unique(sq_lab)),collapse = ", "),.groups = 'drop') |> 
  mutate(squawk=as.integer(squawk))
compressed_transponder_dat


# get country from coords
library(rworldmap)
library(sp)

countries_sp <- getMap(resolution = 'low')

get_countries_vectorised <- function(lon, lat, map = countries_sp) {
  # Create a placeholder vector of NAs
  out <- rep(NA_character_, length(lon))
  
  # Find indices where both coordinates are NOT NA
  ok <- !is.na(lon) & !is.na(lat)
  
  if (any(ok)) {
    # Only process the valid points
    pts <- SpatialPoints(cbind(lon[ok], lat[ok]), proj4string = CRS(proj4string(map)))
    res <- over(pts, map)
    out[ok] <- as.character(res$ADMIN)
  }
  
  return(out)
}

# assign countries
res_dat_plus <- res_dat %>%
  mutate(geo_country = get_countries_vectorised(longitude, latitude)) |> 
  mutate(assigned_country = if_else(is.na(geo_country),origin_country,geo_country)) |> 
  # mutate()
  left_join(compressed_transponder_dat)

sort(unique(res_dat_plus$assigned_country))
sort(unique(transponder_dat$sq_country))
transponder_dat




expanded_df <- transponder_dat %>%
  # 1. Split comma-separated values into individual rows
  separate_longer_delim(squawk, delim = ",") %>%
  mutate(squawk = str_trim(squawk)) %>%
  
  # 2. Identify and expand ranges (0005-0007)
  mutate(squawk = map(squawk, function(x) {
    if (str_detect(x, "-")) {
      # Split by dash, convert to numeric, create sequence
      range_limits <- as.numeric(str_split_1(x, "-"))
      seq_nums <- seq(range_limits[1], range_limits[2])
      # Format back to 4-digit string with leading zeros
      sprintf("%04d", seq_nums)
    } else {
      # Keep single codes as they are
      x
    }
  })) %>%
  
  # 3. Flatten the lists created by map() into new rows
  unnest(squawk)

# View results
print(expanded_df)




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
  
  ## distributions plot data
  
  ## join basic limits to main data
  ## assign outlier status
  long_data = prepped_data |> 
    select(callsign,time_position,on_ground,baro_altitude,geo_altitude,velocity,vertical_rate,time_pos_delay,last_contact_delay,altitude_disparity) |> 
    gather(metric,value,-c(callsign,time_position,on_ground)) |> 
    filter(is.finite(value)) |> 
    left_join(vlines_dat_wide) |> 
    mutate(status = if_else(value>max_lim,"high","normal"))|> 
    mutate(status = if_else(value<min_lim,"low",status))
  # long_data
  
  # get callsigns and metrics which are odd.
  long_data_outliers = long_data |> 
    filter(status != "normal") |> 
    select(callsign,time_position,on_ground,metric,value,status) |> 
    mutate(metric_status = paste0(metric,":",value," (",status,")")) 
  
  long_data_outliers_sum=long_data_outliers |> 
    select(callsign,time_position,metric,status) |> 
    distinct()
  
  
  multiworld_plot_data = bind_rows(lapply(split(long_data_outliers_sum,long_data_outliers_sum$metric), function(z){
    prepped_data |>
      select(icao24, callsign, origin_country, time_position, on_ground, last_contact, longitude, latitude) |> 
      left_join(z) |> 
      fill(metric,.direction = 'updown') |> 
      mutate(status=if_else(is.na(status),"normal",status))
  }))
  
  multiworld_plot_data_filtered = multiworld_plot_data |> 
    filter(status !="normal")
  
  
}

ggplot(multiworld_plot_data_filtered,aes(longitude,latitude,colour=status))+
  geom_point(data = multiworld_plot_data,aes(longitude,latitude),size = .1,colour='black')+
  geom_point(size = .5)+
  facet_wrap(on_ground~metric,scales='fixed')


# # long_data_outliers |> 
# #   select(callsign,time_position,on_ground,metric,status) |> 
# #   distinct() |> 
# #   spread(metric,status)
# 
# outlier_callsigns_summary = long_data_outliers |> 
#   group_by(callsign,time_position,on_ground) |>
#   summarise(cs_status = paste0(metric_status,collapse=" <br> "),
#             cs_status = paste0(metric_status,collapse=" <br> "),.groups='drop') |>
#   ungroup()
# outlier_callsigns_summary




distr_plot_data = long_data |> 
  # filter(
  #   ( on_ground == "TRUE" & 
  #       (metric %in% c("baro_altitude","velocity","last_contact_delay","vertical_rate"))|
  #       (metric == "time_pos_delay" & value <10))
  #   |
  #     ( on_ground == "FALSE" & (
  #       (metric %in% c("baro_altitude","geo_altitude","velocity"))| 
  #         (metric == "altitude_disparity" & value >(-1e3) & value < (1e3)) |
  #         (metric == "last_contact_delay" & value <10) |
  #         (metric == "time_pos_delay" & value <10) |
  #         (metric == "vertical_rate" & value >(-10) & value < (10 ))))
  # )|>
  filter(is.finite(value)) |> 
  droplevels() |> 
  ungroup()

ggplot(distr_plot_data,aes((value),colour=on_ground))+
  geom_density()+
  geom_vline(data = vlines_dat_wide,aes(xintercept = max_lim))+
  geom_vline(data = vlines_dat_wide,aes(xintercept = min_lim))+
  facet_wrap(on_ground~metric,scales='free')



library(cluster)
library(dbscan)

pca_input_data = prepped_data |> 
  select(callsign,baro_altitude,geo_altitude,
         on_ground,
         velocity,vertical_rate,
         # time_pos_delay,
         time_position, last_contact,
         # last_contact_delay,
         altitude_disparity
  ) %>%
  filter(on_ground == "FALSE") |> select(-on_ground) |> 
  mutate(across(where(is.character), as.factor))
# pca_input_data


gower_dist <- daisy(pca_input_data %>% select(-callsign), metric = "gower")


# Calculate Outlier Scores based on Gower distance
# minPts is usually set to k + 1, where k is expected cluster size
lof_scores <- lof(gower_dist, minPts = 5)

# Add scores back to your data and pick the top outliers
pca_input_data$outlier_score <- lof_scores
top_outliers <- pca_input_data %>% arrange(desc(outlier_score)) %>% head(10)

# Calculate the mean/mode for the whole dataset using modern dplyr syntax
global_summary <- pca_input_data %>%
  summarise(
    # Use \(x) for the anonymous function
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    # Get the most common level for factors
    across(where(is.factor), \(x) names(sort(table(x), decreasing = TRUE))[1])
  )

top_callsigns = top_outliers$callsign

comparison <- pca_input_data %>%
  filter(callsign %in% top_callsigns) %>%
  bind_rows(global_summary) %>%
  # Label rows dynamically: if callsign is NA, it's the Global Average
  mutate(row_type = if_else(is.na(callsign), "Global_Average", "Outlier")) %>%
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(
    cols = -c(callsign, row_type, outlier_score), 
    names_to = "variable", 
    values_to = "value"
  ) |>
  mutate(value = round(as.numeric(value),4)) |>
  mutate(outlier_score = round(as.numeric(outlier_score),4)) |> 
  group_by(variable) |> 
  mutate(max_val = max(value,na.rm=T),
         min_val = min(value,na.rm=T)) |> 
  ungroup() |> 
  rowwise() |> 
  mutate(value2 = if_else(value == max_val,paste0(value," *"),as.character(value)))|> 
  mutate(value2 = if_else(value == min_val,paste0(value," !"),value2)) |>
  ungroup()

comparison |> 
  select(-c(value,min_val,max_val)) |> 
  spread(variable,value2) |> 
  arrange(desc(outlier_score)) |> 
  mutate(rn = row_number()) |> 
  view()
# Now you can easily see the side-by-side comparison
# To see a nice side-by-side table for ALL top outliers:
# comparison_table <- comparison %>%
# pivot_wider(names_from = c(row_type, callsign), values_from = value)


# View the differences
print(comparison_table)


# Convert to matrix and find the minimum distance (excluding 0s on the diagonal)
mat <- as.matrix(gower_dist)
diag(mat) <- NA
which(mat == min(mat, na.rm = TRUE), arr.ind = TRUE)
as_tibble(data.frame(mat))

# Calculate clusters using PAM (k-medoids)
pam_fit <- pam(gower_dist, k = 3)

# tibble(data.frame(
sum_pam_fit = summary(pam_fit)
# ))
# sum_pam_fit$

# 
# summary(gower_dist)
# 
# 
# image(as.matrix(gower_dist), main = "Pairwise Dissimilarity Heatmap")
# 
# # Or use a more advanced heatmap (recommended)
# library(gplots)
# heatmap.2(as.matrix(gower_dist), trace = "none", col = rev(heat.colors(100)))
#                                                            
#                                                           
# hclust_obj = hclust(gower_dist, method = "ward.D2")
# 
# # hclust_obj$merge
# # plot(hclust_obj, 
# #      labels = pca_input_data$callsign, 
# #      main = "Flight Relatedness Dendrogram")
# #      
# dend <- as.dendrogram(hclust_obj)
# 
# # 2. Adjust graphical parameters (margins)
# # You need a wider right margin (4th value) to fit the labels
# par(mar = c(5, 4, 4, 8)) 
# 
# # 3. Plot with rotation and smaller text
# plot(dend, 
#      horiz = TRUE,      # Rotates the plot (labels on the right)
#      nodePar = list(lab.cex = 0.6, pch = NA), # lab.cex makes label text smaller
#      main = "Flight Relatedness")
# 
# 
# library(ape)
# 
# # 1. Convert hclust to a 'phylo' object
# phylo_tree <- as.phylo(hclust(gower_dist, method = "ward.D2"))
# 
# # 2. Plot as a fan
# plot(phylo_tree, 
#      type = "fan",       # This creates the circular "fan" layout
#      cex = 0.5,          # Adjust text size (0.5 = 50% of default)
#      tip.color = "blue", # Optional: change label color
#      no.margin = TRUE)   # Maximizes space for the plot
# 
# BiocManager::install("ggtree")
# library(ggtree)
# 
# ggtree(phylo_tree, layout = "circular") + 
#   geom_tiplab(size = 2, offset = 0.01) + # Smaller labels, slightly offset from branches
#   theme_tree()




filtered_data = prepped_data |> 
  filter(latitude>52,latitude<55,longitude>(0),longitude<3) |>
  ungroup()
ggplot(filtered_data,aes(longitude,latitude))+
  geom_point(alpha = .5, aes(colour=baro_altitude))




# View results
print(head(live_flights$data))
