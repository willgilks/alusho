
library("tidyverse")
library("rvest") ## pull url
library("maps") # world map ggplot
library("countrycode") ## match cities to countries




## dat scrape and analysis
end_of_first_period="2022-07-15"
start_of_second_period="2022-08-15"
end_of_second_period="2023-08-01"
earliest_overall_date="1994-03-01"
earliest_test_date=Sys.Date()-365


## pull data ####

loop_dates=seq.Date(from=base::as.Date(earliest_overall_date),to=Sys.Date(),by = "1 year")
# loop_dates=seq.Date(from=base::as.Date(Sys.Date()-365),to=Sys.Date(),by = "1 year")


deduped_data=raw_data |> 
  group_by(orig_text) |> 
  filter(reporting_date==min(reporting_date)) |> 
  ungroup()


## for event classifcation
make_label_column=function(input_df,new_column_name,str_list1,str_list2){
  
  if (!is.null(str_list2)){
    names(str_list2)<-str_list2
  }
  comb_list_temp=append(str_list1,str_list2)
  comb_list=lapply(comb_list_temp, function(z){
    paste0(z,collapse="|")
  })
  
  df_with_new_column=input_df |> 
    mutate({{new_column_name}}:=as.character("")) 
  
  for (kw in names(comb_list)) {
    print(kw)
    df_with_new_column=df_with_new_column |> 
      rowwise() |> 
      mutate({{new_column_name}}:=ifelse(str_detect(orig_text,comb_list[[kw]]),trimws(paste0(c({{new_column_name}},kw),collapse=";")),{{new_column_name}})) |> 
      ungroup()
  }
  df_with_new_column
}




## Flatten ac_types1
flatten_lookup=function(input_list) {
  
  map_dfr(names(input_list), function(manuf) {
    families=input_list[[manuf]]
    
    map_dfr(names(families), function(fam_name) {
      fam=families[[fam_name]]
      
      map_dfr(names(fam), function(ac_code) {
        ac=fam[[ac_code]]
        tibble(
          manufacturer=manuf,
          ac_family=fam_name,
          standardised_ac_name=ac_code,
          reported_ac_name=unlist(ac))
      })
    })
  })
}


make_ac_type_lookup=function(ac_type_input_list=AIRCRAFT_SEARCH_STRINGS) {
  
  pancake=flatten_lookup(ac_type_input_list)
  
  output_df=pancake |>
    mutate(pattern=reported_ac_name) |> 
    mutate(pattern=paste0(" ",pattern," ")) |> 
    mutate(p2=gsub(" $",",",pattern)) |> 
    mutate(p3=gsub(" $","$",pattern)) |> 
    mutate(pattern=paste0(pattern,"|",p2,"|",p3)) |> 
    select(-c(p2,p3))
  return(output_df)
}

## Build ac lookup
ac_type_lookup_table=make_ac_type_lookup(ac_type_input_list=AIRCRAFT_SEARCH_STRINGS)



## Extractor for aircraft type
extract_aircraft=function(text_row,lookup_tbl) {
  
  if(length(text_row)!=1 || is.na(text_row)) {
    return(tibble(
      manufacturer=NA_character_,
      ac_family=NA_character_,
      standardised_ac_name=NA_character_,
      reported_ac_name=NA_character_,
      num_aircraft=0
    ))
  }
  
  hits=lookup_tbl[str_detect(text_row,lookup_tbl$pattern),]
  
  if(nrow(hits)==0) {
    return(tibble(
      manufacturer=NA_character_,
      ac_family=NA_character_,
      standardised_ac_name=NA_character_,
      reported_ac_name=NA_character_,
      num_aircraft=0
    ))
  }
  
  tibble(
    manufacturer=paste(unique(hits$manufacturer),collapse="/"),
    ac_family=paste(unique(hits$ac_family),collapse="/"),
    standardised_ac_name=paste(unique(hits$standardised_ac_name),collapse="/"),
    reported_ac_name=paste(unique(hits$reported_ac_name),collapse="/"),
    num_aircraft=n_distinct(hits$standardised_ac_name)
  )
}



## Do extract ac type
data_with_ac_info=deduped_data |>
  mutate(match=purrr::map(orig_text,~extract_aircraft(.x,lookup_tbl=ac_type_lookup_table))) |>
  unnest(match)



world_cities=tibble(maps::world.cities)

world_cities_filtered=world_cities |> 
  group_by(name) |> 
  filter(pop==max(pop)) |> 
  ungroup() |> 
  mutate(name2=gsub("^'","",name))





## functions for location extraction

## trims location trail
trim_location_tail=function(x){
  month_pattern=paste(c(month.abb,month.name),collapse="|")
  sub(paste0("(?i)(\\son\\s|\\b(",month_pattern,")\\b|:|\\(|\\s\\d).*?$"),"",x,perl=TRUE)
}

## clean location words
clean_location_words=function(loc){
  if(is.na(loc) || loc=="") return(loc)
  words=str_split(loc,"\\s+")[[1]]
  keep=words[str_detect(words,"^[A-Z]")|
               str_detect(words,"^el(-|$)") |
               str_to_lower(words)%in%c("and","enroute","el","de","la","of")
  ]
  str_squish(paste(keep,collapse=" "))
}


extract_one_location=function(x){
  if(is.na(x)||x=="")return(tibble(location="",location_ind=""))
  
  indicators=c("at","near","between","over","overhead")
  trimmed=trim_location_tail(x)
  
  loc_idx=map_dbl(indicators,\(kw){
    m=str_locate(tolower(trimmed),paste0("\\b",kw,"\\b"))[1,1]
    ifelse(is.na(m),Inf,m)
  })
  
  if(all(is.infinite(loc_idx))){
    location=""
    indicator=""
  }else{
    indicator=indicators[which.min(loc_idx)]
    location=sub(paste0(".*?\\b",indicator,"\\b\\s*"),"",trimmed,ignore.case=TRUE)
    location=str_remove_all(location,"^[,;.:\\-\\s]+|[,;.:\\-\\s]+$")
  }
  
  # if missing and enroute exist, then assign enroute
  if(location==""&&grepl("\\benroute\\b",x,ignore.case=TRUE)){
    location="enroute"
    indicator="enroute"
  }
  
  ## clean words (if not enroute)
  if(indicator!="enroute"&&location!=""&&location!="enroute"){
    location=clean_location_words(location)
  }
  
  tibble(location=location,location_ind=indicator)
}



## location extraction wrapper
extract_location=function(input_df,text_col="orig_text",city_names=NULL){
  input_df|>
    mutate(tmp=purrr::map(.data[[text_col]],extract_one_location))|>
    unnest_wider(tmp)
}



## Inital extract locations
locs_data=extract_location(deduped_data,city_names=NULL)


## split up multi-location events
locs_data_n_check=locs_data|>
  mutate(comma_count=str_count(location,","),
         and_count=str_count(location," and "))|>
  mutate(comma_and_and_count=comma_count+and_count)|> 
  mutate(do_split=if_else(and_count>0|comma_count>0,TRUE,FALSE))|>
  mutate(max_split=max(comma_and_and_count)+1) 


max_locs_split=locs_data_n_check$max_split[1]

## make long
# count number of locations per event
locs_banana=locs_data_n_check|>
  mutate(location2=gsub(" and ",", ",location))|> 
  filter(do_split==TRUE) |> 
  separate(location2,into = paste0("loc",1:max_locs_split),sep =",",remove = FALSE) |> 
  select(orig_text,reporting_date,location,location_ind,location2,starts_with("loc")) |> 
  gather(loc_grp,subloc,-c(orig_text,reporting_date,location,location_ind,location2)) |> 
  mutate(subloc=trimws(subloc)) |> 
  filter(!is.na(subloc)) |> 
  group_by(orig_text,reporting_date) |> 
  mutate(nlocations=n()) |> 
  ungroup()


locs_banana_fin=locs_banana |> 
  select(orig_text,reporting_date,location,location_ind,location2=subloc,nlocations) |> 
  ungroup()


# join splitted multi-location events to single-location events
locs_with_multi_split=locs_data_n_check |> 
  filter(do_split==FALSE) |> 
  mutate(location2=gsub("[,].*","",location)) |> 
  select(orig_text,reporting_date,location,location_ind,location2) |> 
  mutate(nlocations=1) |> 
  ungroup() |> 
  bind_rows(locs_banana_fin)



# airport locations reference data
airports_tbl=airportr::airports|>
  select(airport=Name,
         city=City,
         iso_code=`Country Code (Alpha-3)`,
         country=Country,
         lat=Latitude,
         long=Longitude)


airport_level_lookup=airports_tbl |> 
  mutate(location2=trimws(gsub("Airport","",airport))) |> 
  select(location2,country2=country,iso_code2=iso_code,lat2=lat,long2=long)|> 
  group_by(location2,country2,iso_code2) |> 
  summarise(lat2=mean(lat2),long2=mean(long2),.groups='drop')


city_airport_info=airports_tbl |>
  mutate(location2=city) |>
  select(location2,country2=country,iso_code2=iso_code,lat2=lat,long2=long) |> 
  group_by(location2,country2,iso_code2) |> 
  summarise(lat2=mean(lat2),long2=mean(long2),.groups='drop')


## for events missing geo assignments such as city country lat long
## left join city and airport tables and see if they match on variations of names
## this is cheaper than exhaustive coordinate matching
data_with_locs_and_geos=locs_with_multi_split |> 
  left_join(world_cities_filtered |>
              select(location=name2,country=country.etc,lat,long)) |>
  left_join(world_cities_filtered |>
              select(location2=name2,country2=country.etc,lat2=lat,long2=long)) |>
  mutate(country=if_else(is.na(country)&!is.na(country2),country2,country),
         lat=if_else(is.na(lat)&!is.na(lat2),lat2,lat),
         long=if_else(is.na(long)&!is.na(long2),long2,long)) |> 
  select(-c(country2,lat2,long2)) |> 
  left_join(airport_level_lookup)|>
  mutate(country=if_else(is.na(country)&!is.na(country2),country2,country),
         lat=if_else(is.na(lat)&!is.na(lat2),lat2,lat),
         long=if_else(is.na(long)&!is.na(long2),long2,long)) |> 
  select(-c(country2,lat2,long2))|>
  mutate(iso_code=countrycode(country,origin = "country.name",destination = "iso3c")) |> 
  mutate(iso_code=if_else(is.na(iso_code),iso_code2,iso_code)) |> 
  select(-iso_code2) |> 
  left_join(city_airport_info)|>
  mutate(country=if_else(is.na(country),country2,country),
         iso_code=if_else(is.na(iso_code),iso_code2,iso_code),
         lat=if_else(!is.finite(lat),lat2,lat),
         long=if_else(!is.finite(long),long2,long)) |>
  select(-c(iso_code2,country2,lat2,long2)) |>
  ungroup() |> 
  distinct() |> 
  mutate(iso_code2=countrycode(location2,origin = "country.name",destination = "iso3c")) |> 
  mutate(iso_code=if_else(is.na(iso_code),iso_code2,iso_code)) |>
  select(-iso_code2) |>
  mutate(country2=countrycode(iso_code,origin = "iso3c",destination = "country.name")) |>
  mutate(country=if_else(is.na(country),country2,country)) |>
  select(-country2) |>
  ungroup();data_with_locs_and_geos

# write_csv(data_with_locs_and_geos,"~/Desktop/data_with_locs_and_geos.csv")

final_missing_locs=data_with_locs_and_geos |> 
  filter(location!="" & (is.na(country)|country=="")) |> 
  filter(!grepl("Sea|Bay|Gulf",location2)) |> 
  filter(!location2 %in% c("Atlantic","Pacific","enroute")) |> 
  select(location2) |> 
  arrange(location2) |> 
  distinct() 



deduped_data |> 
  mutate(mo=extract_month_numbers(orig_text))



extract_day_numbers <- function(x) {
  # match 1–2 digit numbers, possibly followed by st/nd/rd/th
  pattern <- "\\b(\\d{1,2})(?:st|nd|rd|th)?\\b"
  
  # extract first match per string
  match <- stringr::str_extract(x, regex(pattern, ignore_case = TRUE))
  
  # remove any ordinal suffix and convert to integer
  as.integer(gsub("(st|nd|rd|th)$", "", match, ignore.case = TRUE))
}


deduped_data |> 
  mutate(day=extract_day_numbers(orig_text))


extract_dates=function(x) {
  
  months_all=c(month.name,month.abb)
  months_pattern=paste(months_all,collapse="|")
  current_year=as.integer(format(Sys.Date(),"%Y"))
  
  # Match formats: Month Year, Month Day Year, Day Month Year
  date_pattern=paste0(
    "(?i)",
    "(\\b(",months_pattern,")\\b\\s*(\\d{1,2}(?:st|nd|rd|th)\\b)?\\s*,?\\s*(199\\d|20\\d{2}|",current_year,")?)|",
    "(\\b\\d{1,2}(?:st|nd|rd|th)?\\s+(",months_pattern,")\\b\\s*,?\\s*(199\\d|20\\d{2}|",current_year,")?)")
  
  matches=str_extract_all(x,regex(date_pattern, ignore_case = TRUE))
  
  purrr::map(matches,function(ms){
    if(length(ms)==0){
      return(tibble(month=NA_integer_,day=NA_integer_,year=NA_integer_,date=as.Date(NA),n_dates=0))
    }
    
    purrr::map_dfr(ms,function(mtxt){
      # Extract month
      m_match<-str_extract(mtxt,regex(months_pattern,ignore_case=TRUE))
      month_num<-match(tolower(m_match),tolower(months_all))
      month_num<-ifelse(!is.na(month_num)&month_num>12,month_num-12,month_num)
      
      # Extract day (ignore if part of a 4-digit year)
      d_match=str_extract(mtxt, "(?<!\\d)\\b\\d{1,2}(?:st|nd|rd|th)?\\b(?!\\d{2})")
      day_num<-as.integer(gsub("(st|nd|rd|th)$", "", d_match, ignore.case = TRUE))
      
      # Extract year
      y_match=str_extract(mtxt,"\\b(199\\d|20\\d{2})\\b")
      year_num<-as.integer(y_match)
      if (!is.na(year_num) && (year_num < 1990 || year_num > current_year)) year_num <- NA_integer_
      
      # Use 15 if only month+year are present but no day
      if (is.na(day_num) && !is.na(month_num) && !is.na(year_num)) {
        day_num <- 15L
      }
      
      # make date
      date_std<-suppressWarnings(make_date(year_num,month_num,day_num))
      
      tibble(month=month_num,day=day_num,year=year_num,date=date_std)
    })|>
      mutate(n_dates=n())
  })
}



## initial data with dates
data_with_dates=deduped_data |> 
  mutate(date_parts = extract_dates(orig_text)) |>
  unnest(date_parts)

## estimate missing dates based on reported date
data_with_estimated_dates=data_with_dates |>
  mutate(reporting_lag=as.numeric(reporting_date-date)) |>
  mutate(reporting_lag=if_else(reporting_lag<1,as.numeric(NA),reporting_lag)) |> 
  mutate(avg_reporting_lag=median(reporting_lag,na.rm=T)) |> 
  mutate(est_date=reporting_date-avg_reporting_lag) |> 
  mutate(date_source=if_else(is.na(date)&!is.na(est_date),"Estimated","Extracted")) |> 
  mutate(date=if_else(is.na(date),est_date,date)) |> 
  select(orig_text,reporting_date,date,date_source) |> 
  group_by(orig_text,reporting_date) |> 
  filter(date==min(date)) |> 
  ungroup()




extract_airline <- function(x) {
  
  if (is.na(x) || x == "") return(NA_character_)
  
  ## Normalise spacing
  x_clean <- str_squish(x)
  
  ## Define strong airline keywords
  airline_keywords <- c("Airlines?", "Airways?", "Air ", "Flight", "Cargo", "Express",
                        "Aviation", "Cargo", "Jet", "Fly", "Lines", "Sky", "Wings")
  airline_pattern <- paste(airline_keywords, collapse = "|")
  
  ## Extract airline candidates (word groups before/around keyword)
  match <- str_extract(
    x_clean,
    paste0(
      "(?i)\\b([A-Z][a-zA-Z]+(?:\\s+[A-Z][a-zA-Z]+){0,2}\\s+(?:", airline_pattern, "))\\b"
    )
  )
  
  ## If no match, try uppercase short airline identifiers (e.g., KLM, UPS, SAS)
  if (is.na(match)) {
    match <- str_extract(x_clean, "\\b[A-Z]{2,4}\\b(?=\\s+(?:flight|aircraft|A\\d|B\\d))")
  }
  
  ## Clean up result
  match <- str_remove_all(match, "\\b(Flight|Airlines?|Airways?|Air|Cargo|Aviation|Express|Jet|Fly|Lines|Wings)\\b$")
  match <- str_squish(match)
  if (is.na(match) || match == "") return(NA_character_)
  
  ## Reattach keyword if relevant
  keyword <- str_extract(x_clean, "(?i)(Airlines?|Airways?|Air|Cargo|Aviation|Express|Jet|Fly|Lines|Wings)")
  airline_full <- if (!is.na(keyword) && !str_detect(match, keyword)) paste(match, keyword) else match
  
  airline_full
}

## extract airlines
deduped_data_with_airline <- deduped_data %>%
  mutate(airline = map_chr(orig_text, extract_airline))




# Parse fields and infer airline / country
pig_parsed=deduped_data |>
  filter(orig_text!="") |>
  mutate(
    airline=str_extract(orig_text,"^[A-Za-z\\s]+?(?=\\s[A-Z0-9]{3,4})") |> str_squish(),
    date_raw=str_extract(orig_text,"on\\s[A-Za-z]+\\s\\d{1,2}[a-z]{2}\\s\\d{4}") |> str_remove("^on\\s"),
    date=parse_date_time(date_raw,orders="b dY"),
    incident_summary=str_extract(orig_text,",\\s.*$") |> str_remove("^,\\s"),
    across(everything(),str_squish)) |> 
  mutate(incident_summary=if_else(
    is.na(incident_summary),
    str_extract(orig_text,":|-\\s.*$") |> str_remove("^:|-\\s"),incident_summary))
pig_parsed


# write_csv(pig_parsed,"~/Desktop/pig_parsed.csv")

# get city coords
city_coords=as_tibble(maps::world.cities) |> 
  group_by(name) |> 
  filter(pop==max(pop,na.rm=T)) |> 
  ungroup() |> 
  select(city_name=name,maps_country=country.etc,lat,long) |> 
  distinct()

## assign geo coords tp cities ad filter
locs_unique=pig_parsed |>
  filter(!is.na(location),location!="") |>
  distinct(location) |> 
  mutate(city_name=location) |> 
  mutate(city_name2=location) |> 
  left_join(city_coords)|> 
  left_join(city_coords |> mutate(city_name2=gsub("^'","",city_name)) |> select(-city_name)) |> 
  select(location,maps_country,lat,long) |> 
  distinct()


# Repair dates, assign countries and geo coords
pig_geo=pig_parsed |> 
  mutate(date=base::as.Date(date),reporting_date=base::as.Date(reporting_date,format="%Y%m%d")) |> 
  left_join(locs_unique)|> 
  mutate(month_end=ceiling_date(date,unit = 'months')-1) |> 
  mutate(reporting_delay=as.numeric(reporting_date-date)) |> 
  mutate(event_id=row_number()) |> 
  mutate(airline=if_else(is.na(airline),"unknown",airline)) |> 
  group_by(incident_summary) |> 
  mutate(nr_incident_summary=n()) |> 
  ungroup() |> 
  mutate(incident_sum_short=if_else(nr_incident_summary<10,'other',incident_summary)) |> 
  group_by(airline) |> 
  mutate(airline_total=n()) |> 
  ungroup() |> 
  group_by(incident_sum_short) |> 
  mutate(incident_total=n()) |> 
  ungroup() |> 
  group_by(airline,incident_sum_short) |> 
  mutate(airline_with_incident=n()) |> 
  ungroup() |> 
  mutate(overall_total_events=n())

# pig_geo |> 
#   group_by(incident_summary) |> 
#   summarise(n=n(),.groups='drop') |> 
#   ungroup() |> 
#   view()


## define terms

{
  times1=list(
    approach=c("approach","descent","go around","initial climb","landed","on landing","hard landing",
               "touch down","touchdown","touched down","roll out","rollout"),
    on_ground=c("apron","at stand","ground worker","push back","taxi","line up","turn off","on runway","runway excursion"),
    departure=c("departure","departed","climb out","takeoff","take off","could not retract landing gear"),
    enroute=c("in flight","midair","enroute"))
  
  animals1=list(
    bird=c("birds","bird","goose","geese"),
    other=c("dog","coyote"),
  )
  
  
  ac_parts1=list(
    electric=c("electric","electronic"),
    navigation=c("nav","navigation"),
    toilet=c("lavatory","toilet"),
    FMS=c("FMS","FMSs"),
    GPS=c("GPS","EGPWS","GPWS"),
    engine=c("engine","propeller"),
    oil=c("^oil"," oil"),
    pressure=c("pressurization","pressure"))
  
  ac_parts2=c(
    "hydraulic", "instrument",
    "MCP speed selector",
    "flight control",
    "pneumatic",
    "communication",
    "configuration",
    "aircraft",
    "airframe",
    "air conditioning",
    "altitude sensor",
    "APU",
    "autopilot", 
    "battery",
    "bleed",
    "brake",
    "cabin",
    "charger",
    "cockpit",
    "cargo",
    "door", 
    "elevator",
    "computer",
    "flight deck",
    "flap",
    "fuel", 
    "galley",
    "gear",
    "tyre","wheel",
    "on board",
    "oxygen","panel",
    "phone",
    "power bank",
    "radar altimeter",
    "RAT","radio","radome",
    "slat","spoiler","stairs",
    "tail",
    "weather radar",
    "rudder",
    "slat",
    "water system",
    "windshield","window","wing","wing tip")
  
  
  
  
  people1=list(
    pilot=c("captain","copilot","pilot","^pilot","first officer"),
    cabin_crew=c("flight attendant","attendant","cabin crew"),
    atc=c("ATC|tower"),
    ground_worker=c("ground worker"),
    passenger=c("passenger","people"))
  
  
  
  events1=list(
    activation=c("activation","activates"),
    alert=c("alert","alarm"),
    decsent=c("descent","descend"),
    fire=c("flames","fire"),
    injury=c("injuries","injures","injured"),
    noise=c("noisy","noise"),
    overrun=c("overran","overrun"),
    smell=c("odour","smell"),
    return=c("return"),
    divert=c("divert","diversion"))
  
  events2=list(
    "asymmetry",
    "beeping",
    "bang",
    "bird",
    "blew",
    "burst",
    "breaks",
    "burning",
    "clogged",
    "collapse",
    "collision",
    "contact",
    "could not retract",
    "cracked","crashed",
    "damage","detached","deployed",
    "died",
    "disabled","disagree",
    "discrepancy",
    "dislodged",
    "dropped",
    "emergency","error","evacuation","excursion",
    "exposed",
    "failure","fault","fire","flamed out",
    "generator",
    "go around",
    "fell",
    "flames",
    "fumes",
    "hail strike",
    "heaviness",
    " hit",
    "impacted","incapacitated","incursion","indication",
    "issue",
    "ill",
    "jammed",
    "killed",
    "leak",
    "lightning",
    "locked",
    "loss of",
    "loss of separation",
    "lost power",
    "lost height",
    "malfunction",
    "near collision",
    "opened",
    "overflew","overheat",
    "pressure","pressurize",
    "problem","rejected","returned",
    "separated","shot","shut down","smoke","stall",
    "stick shaker",
    "TCAS",
    "touched down short of runway",
    "trouble",
    "turbulence",
    "thermal runaway",
    "scrape",
    "sparks",
    "strike",
    "veered off","vibrations",
    "wake turbulence",
    "warning")
  
  adjectives1=list(instability=c("stabilisation","unstable"))
  
  adjectives2=list(
    "hard","incorrect","insufficient",
    "unidentified","unreliable","unsafe",
    "unusual","wrong")
  
  
  ## ac_condition
  ac_condition1=list(
    ice=" ice", altitude=c("altitude","height"),
    speed=c("airspeed","speed"),
    attitude="attitude",
    angle="angle",thrust="thurst")
  }



## make columns
pig_with_times=make_label_column(input_df=pig_geo,new_column_name=flight_stage,str_list1=times1,str_list2=NULL)
pig_with_ac_parts=make_label_column(input_df=pig_with_times,new_column_name=ac_part,str_list1=ac_parts1,str_list2=ac_parts2)
pig_with_people=make_label_column(input_df=pig_with_ac_parts,new_column_name=persons,str_list1=people1,str_list2=NULL)
pig_with_verb=make_label_column(input_df=pig_with_people,new_column_name=verb,str_list1=events1,str_list2=events2)
pig_with_adjectives=make_label_column(input_df=pig_with_verb,new_column_name=adjective,str_list1=adjectives1,str_list2=adjectives2)
pig_with_ac_condition=make_label_column(input_df=pig_with_adjectives,new_column_name=ac_condition,str_list1=ac_condition1,str_list2=NULL)
pig_with_ac_type=make_label_column(input_df=pig_with_ac_condition,new_column_name=ac_type,str_list1=NULL,str_list2=ac_types2)



pig_with_ac_condition |> 
  select(incident_summary,flight_stage,ac_part,persons,verb,ac_condition,adjective) |>
  rowwise() |> 
  mutate(event_lab=paste0(c(flight_stage,ac_part,persons,verb,ac_condition,adjective),collapse=";")) |> 
  ungroup() |> 
  mutate(event_lab=gsub("[;][;][;]",";",event_lab)) |>
  mutate(event_lab=gsub("[;][;]",";",event_lab)) |>
  mutate(event_lab=gsub("^[;]|[;]$","",event_lab))|>
  select(incident_summary,event_lab) |> 
  group_by(event_lab) |> 
  mutate(nr=n()) |> 
  ungroup() |> 
  distinct() |> 
  view()
res


times


pig_fin=pig_with_verbs |> 
  rowwise() |> 
  mutate(event_lab=paste0(c(
    flight_stage,ac_part,persons,verb),collapse=";")) |> 
  ungroup() |> 
  mutate(event_lab=gsub("[;][;][;]",";",event_lab)) |> 
  mutate(event_lab=gsub("[;][;]",";",event_lab)) |> 
  mutate(event_lab=gsub("^[;]|[;]$","",event_lab))|> 
  select(date,airline,aircraft,location,maps_country,lat,long,
         event_lab,incident_summary)
pig_fin |> 
  select(event_lab) |> distinct() |> arrange(event_lab) |> view()
filter(event_lab=="")










## tail strike or wing tip strike



dog_prep=pig_geo |> 
  select(airline,incident=incident_sum_short,airline_total,incident_total,
         airline_with_incident
         ,overall_total_events
  ) |> 
  distinct() |> 
  mutate(airline_not_incident=airline_total-airline_with_incident) |> 
  mutate(nonairline_total=overall_total_events-airline_total) |> 
  mutate(nonairline_with_incident=incident_total-airline_with_incident) |>
  mutate(nonairline_not_incident=nonairline_total-nonairline_with_incident) |> 
  mutate(prop_in_airline=airline_with_incident/airline_total)|> 
  mutate(prop_not_in_airline=nonairline_with_incident/nonairline_total) |> 
  mutate(prop_diff=prop_in_airline-prop_not_in_airline)

dog_fin=dog_prep|>
  select(airline,
         incident,
         airline_with_incident,
         airline_not_incident,
         nonairline_with_incident,
         nonairline_not_incident)



# write_csv(dog_fin,"~/Desktop/dog_fin.csv")


chi2dat = dog_fin |>
  rowwise() |>
  mutate(
    ## Components of the 2x2 table
    a = airline_with_incident,
    b = airline_not_incident,
    c = nonairline_with_incident,
    d = nonairline_not_incident,
    
    ## Chi-squared test
    chi_test = list(chisq.test(matrix(c(a,b,c,d), nrow=2, byrow=TRUE), correct=FALSE)),
    
    ## Extract test values
    chi2 = chi_test$statistic,
    p_value = chi_test$p.value,
    expected_min = min(chi_test$expected),
    
    ## Odds ratio and CI (manual calculation)
    odds_ratio = (a * d) / (b * c),
    log_or = log(odds_ratio),
    se_log_or = sqrt(1/a + 1/b + 1/c + 1/d),
    ci_lower = exp(log_or - 1.96 * se_log_or),
    ci_upper = exp(log_or + 1.96 * se_log_or),
    
    ## Significance flags
    sig_p = p_value < 0.05,
    sig_or = (ci_lower > 1 | ci_upper < 1)) |>
  ungroup() |>
  select(airline, incident, chi2, p_value, odds_ratio, ci_lower, ci_upper, sig_p, sig_or)

chi2dat
# write_csv(dog,"~/Desktop/dog.csv")

## join chi2 results to base
cow=dog_prep|>
  select(airline,
         incident,
         airline_with_incident,
         airline_not_incident,
         nonairline_with_incident,
         nonairline_not_incident,prop_in_airline,prop_not_in_airline,
         prop_diff) |> 
  left_join(chi2dat) |> 
  mutate(log10p=-log10(p_value)) |> 
  arrange(p_value) |> 
  mutate(tag=str_wrap(paste(airline,incident,round(prop_diff,2)),30)) |> 
  mutate(tag=if_else(p_value<.05,tag,"")) |> 
  filter(is.finite(odds_ratio))
cow

ggplot(cow,aes(prop_diff,log10p))+
  geom_point()+
  geom_text(aes(label=tag),size=3)+
  scale_x_continuous(expand=c(.1,.1))






# Reporting-lag time series
pdat=pig_geo |> 
  filter(date>=Sys.Date()-365) |> 
  select(event_id,date,reporting_delay) |> 
  distinct() |> 
  ungroup()
ggplot(pdat,aes(date,reporting_delay))+
  geom_point()+
  labs(x="",y="Reporting lag",
       title="Time delay from event date to reporting date",
       caption=Sys.Date())


## Events map
pdat=pig_geo |> 
  filter(date>=Sys.Date()-365) |> 
  group_by(lat,long) |> 
  summarise(nr=n(),.groups='drop')
ggplot(pdat,aes(long,lat))+
  geom_point(aes(size=nr))+
  labs(x="",y="",
       title="Events map",
       caption=Sys.Date())


# plot monthly time series of events by e.g. airline, country
plot_by_group=function(input_df=pig_geo,grouping_col=airline,pooling_threshold=5){
  
  df_prep=input_df |> 
    mutate(gcol={{grouping_col}}) |> 
    filter(!is.na(gcol)) |>
    filter(date>=Sys.Date()-365) |>
    group_by(gcol,month_end) |>
    summarise(nr=n(),.groups='drop') |>
    group_by(gcol) |>
    mutate(total_nr=sum(nr,na.rm=T)) |>
    ungroup() |>
    mutate(gcol2=if_else(total_nr<pooling_threshold,'others',gcol)) |>
    group_by(gcol2,month_end) |>
    summarise(nr=sum(nr),.groups='drop') |>
    group_by(gcol2) |>
    mutate(total_nr=sum(nr,na.rm=T)) |>
    ungroup()
  
  ## all combs
  all_mos_df=df_prep |>
    select(month_end) |>
    distinct() |>
    crossing(df_prep |>
               filter(!is.na(gcol2)) |>
               select(gcol2) |>
               distinct())
  
  
  plot_df=all_mos_df |>
    left_join(df_prep) |>
    group_by(gcol2) |>
    fill(total_nr,.direction='updown') |>
    ungroup() |>
    mutate(nr=if_else(!is.finite(nr),0,nr))
  
  ggplot(plot_df,aes(month_end,nr))+
    geom_line()+
    geom_point()+
    facet_wrap(fct_reorder(gcol2,total_nr,.desc = T)~.,scales='free_y')+
    labs(x="",y="Events",
         title=str_to_title(deparse(substitute(grouping_col))),
         caption=Sys.Date())
}


names(pig_geo)
plot_by_group(input_df = pig_geo,grouping_col = airline,pooling_threshold = 10)
plot_by_group(input_df = pig_geo,grouping_col = aircraft,pooling_threshold = 10)
plot_by_group(input_df = pig_geo,grouping_col = maps_country,pooling_threshold = 10)
plot_by_group(input_df = pig_geo,grouping_col = location,pooling_threshold = 10)
plot_by_group(input_df = pig_geo,grouping_col = incident_summary,pooling_threshold = 10)



# test


## make columns by splitting on generalised key words.
dat=suppressWarnings(
  rdat2|>
    mutate(x2=X1)|>
    mutate(x2=sub(" at ",splitwords$loc,x2,fixed=TRUE))|>
    mutate(x2=sub(" over ",splitwords$loc,x2,fixed=TRUE))|>
    mutate(x2=sub(" near ",splitwords$loc,x2,fixed=TRUE))|>
    mutate(x2=sub(" enroute ",splitwords$loc,x2,fixed=TRUE))|>
    separate(x2,c("careq","x4"),sep=splitwords$loc,remove=T)|>
    ## assign location classes, at on near enroute.
    rowwise()|>
    mutate(loclass=trimws(gsub(careq,"",X1)))|>
    ungroup()|>
    mutate(loclass=gsub("[ ].*","",loclass))|>
    mutate(x4=if_else(grepl(" enroute",X1),paste("enroute",x4),x4))|>
    mutate(x4=sub(" on ",splitwords$date,x4))|>
    separate(x4,c("loc","x6"),sep=splitwords$date,remove=T)|>
    ## split first comma from after date column
    mutate(x6=sub(", ",splitwords$comma,x6))|>
    separate(x6,c("dhum","descr"),sep=splitwords$comma,remove=T)|>
    ungroup()|>
    mutate_if(is.character,trimws)|>
    ## indicate if there is more than one date.
    mutate(extra_dates=if_else(grepl(" and ",dhum),as.numeric(1),as.numeric(0)))|>
    # mutate(dhum=gsub(" and .*","",dhum))|>
    # mutate(dhum=gsub("[,]","",dhum))|>
    ## formalise date
    # mutate(edate=mdy(dhum))|>
    mutate(nac=if_else(grepl(" and ",careq),as.numeric(2),as.numeric(1)))|>
    mutate(car=sub("\\s+[^ ]+$", "", careq))|>
    mutate(eq=sub(".*\\s","",careq))|>
    mutate(eq=if_else(eq=="aircraft","",eq))|>
    ## assign manufacturers
    mutate(lab=trimws(gsub("[0-9]"," ",eq)))|>
    mutate(lab=sub("\\s+[^ ]+$","",lab))|>
    left_join(manuf_lookup,by=join_by(lab))|>
    mutate(nm=if_else(is.na(nm),"unknown",nm)))|>
  mutate(enum= substr(gsub("[A-Z]","",eq),start=1,stop=2))|>
  mutate(etype=paste0(lab,enum)) |>
  mutate(loc=gsub("WInnipeg","Winnipeg",loc))



## assign event id numbers and rename columns
dat_temp=dat|>
  filter(!is.na(event_date))|>
  distinct()




##
## description laws
## trying to assign groups to free text descriptions
## if flaps, then all are flaps problem unless they also have fuel emergency as well. also slat problem.
## loss of cabin pressure
## bird strike
## tail strike or wingtip/let strike
## loss of communication
## brakes/gear
## cracked windshield
## computer problems


## summarising by carrier or type
psum=dat_fin  |>
  mutate(ind_comb=if_else(grepl("fire",ind_comb),"fire",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("engine",ind_comb),"engine",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("bird",ind_comb),"bird",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("pilots",ind_comb),"pilots",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("fumes",ind_comb),"fumes",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("fuel",ind_comb),"fuel",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("pressure",ind_comb),"pressure",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("sensor",ind_comb),"sensor",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("hydraulic",ind_comb),"hydraulic",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("passengers",ind_comb),"passengers",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("odour",ind_comb),"odour",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("weather",ind_comb),"weather",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("computer",ind_comb),"computer",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("door",ind_comb),"door",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("window",ind_comb),"gear",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("gear",ind_comb),"gear",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("flaps",ind_comb),"flaps",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("other",ind_comb),"other",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("approach",ind_comb),"approach",ind_comb)) |>
  mutate(ind_comb=if_else(grepl("departure",ind_comb),"approach",ind_comb)) |>
  mutate(ind_comb=if_else(is.na(ind_comb),"other",ind_comb)) |>
  filter(ind_comb!="other")

