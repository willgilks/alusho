
library("tidyverse")
library("rvest") ## pull url
library("maps") # world map ggplot
library("countrycode") ## match cities to countries

library("tidyverse")
library("httr")


## loop through likely page names and pull the text from the url
pull_data=function(date_start,date_end) {
  
  current_date=Sys.Date()
  
  all_dates=seq.Date(
    from=as.Date(date_start),
    to=date_end,
    by="1 day")
  
  df_res=map_dfr(all_dates,function(i) tryCatch({
    
    message("Fetching: ",i)
    base_url=paste0(c("https://","a",
                      "v",
                      "h",
                      "er",
                      "ald",".com","/h?","list=&opt=0&offset="),collapse="")
    
    full_url=paste0(base_url,gsub("[-]","",i),"120000")
    resp=GET(full_url,add_headers(`Accept-Language`="en-GB,en;q=0.5"))
    
    if(status_code(resp)!=200) {
      message("Skipped ",i," (HTTP ",status_code(resp),")")
      return(NULL)
    }
    
    doc=read_html(resp)
    tables=html_table(doc,fill=TRUE)
    
    # print(length(tables))
    
    if(length(tables)<6) return(NULL)
    
    # combine tables
    tables_short=tables[6:length(tables)]
    combined=map_dfr(tables_short,\(z) if(ncol(z)>1) z else NULL)
    
    # print(names(combined))
    if(!"X1" %in% names(combined)) return(NULL)
    
    vecs=sort(unique(na.omit(unlist(combined))))
    tibble(orig_text=vecs,reporting_date=base::as.Date(i))
  },error=function(e)NULL))
  # })
  
  if (!is.null(df_res)){
    if (nrow(df_res)>0){
      df_out=df_res|>
        group_by(orig_text)|>
        filter(reporting_date==min(reporting_date,na.rm=T)) |>
        ungroup()
      df_out
    }
  }
}






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
flatten_ac_type_lookup=function(input_list) {
  
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
  
  pancake=flatten_ac_type_lookup(ac_type_input_list)
  
  output_df=pancake |>
    mutate(pattern=reported_ac_name) |> 
    mutate(pattern=paste0(" ",pattern," ")) |> 
    mutate(p2=gsub(" $",",",pattern)) |> 
    mutate(p3=gsub(" $","$",pattern)) |> 
    mutate(pattern=paste0(pattern,"|",p2,"|",p3)) |> 
    select(-c(p2,p3))
  return(output_df)
}




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





## Extract dates

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




## Run functions #####



## dat scrape and analysis
end_of_first_period="2022-07-15"
start_of_second_period="2022-08-15"
end_of_second_period="2023-08-01"
earliest_overall_date="1994-03-01"
earliest_test_date=Sys.Date()-365


## pull data ####

# loop_dates=seq.Date(from=base::as.Date(earliest_overall_date),to=Sys.Date(),by = "1 year")
loop_dates=seq.Date(from=base::as.Date(earliest_test_date),to=Sys.Date(),by = "1 year")


raw_data=bind_rows(lapply(loop_dates,function(loop_date){
  loop_date_end=loop_date+365
  rdf=pull_data(date_start = loop_date,date_end=loop_date_end)
  rdf
}))


deduped_data=raw_data |> 
  mutate(orig_text=gsub("  "," ",orig_text)) |> 
  group_by(orig_text) |> 
  filter(reporting_date==min(reporting_date)) |> 
  ungroup()


## prep city and airport info for geo assignment
world_cities=tibble(maps::world.cities) 

world_cities_filtered=world_cities |> 
  group_by(name) |> 
  filter(pop==max(pop)) |> 
  ungroup() |> 
  mutate(name2=gsub("^'","",name))



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






# # Reporting-lag time series #####
# pdat=pig_geo |> 
#   filter(date>=Sys.Date()-365) |> 
#   select(event_id,date,reporting_delay) |> 
#   distinct() |> 
#   ungroup()
# ggplot(pdat,aes(date,reporting_delay))+
#   geom_point()+
#   labs(x="",y="Reporting lag",
#        title="Time delay from event date to reporting date",
#        caption=Sys.Date())
# 
# 
# ## Events map #####
# pdat=pig_geo |> 
#   filter(date>=Sys.Date()-365) |> 
#   group_by(lat,long) |> 
#   summarise(nr=n(),.groups='drop')
# ggplot(pdat,aes(long,lat))+
#   geom_point(aes(size=nr))+
#   labs(x="",y="",
#        title="Events map",
#        caption=Sys.Date())


## Aircraft type #####

## Build ac type lookup
ac_type_lookup_table=make_ac_type_lookup(ac_type_input_list=AIRCRAFT_SEARCH_STRINGS)


## Run extract ac type information
data_with_ac_info=deduped_raw_data  |>
  mutate(match=purrr::map(orig_text,~extract_aircraft(.x,lookup_tbl=ac_type_lookup_table))) |>
  unnest(match)

# write_csv(data_with_ac_info,"~/Desktop/data_with_ac_info.csv")


## Event location #####
## Run inital extract locations
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



## Event date #####

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






# write_csv(pig_parsed,"~/Desktop/pig_parsed.csv")


## set terms for event type and ac part etc.

## Event details #####

## make columns summarising common events
pig_with_times=make_label_column(input_df=pig_geo,new_column_name=flight_stage,str_list1=times1,str_list2=NULL)
pig_with_ac_parts=make_label_column(input_df=pig_with_times,new_column_name=ac_part,str_list1=ac_parts1,str_list2=ac_parts2)
pig_with_people=make_label_column(input_df=pig_with_ac_parts,new_column_name=persons,str_list1=people1,str_list2=NULL)
pig_with_verb=make_label_column(input_df=pig_with_people,new_column_name=verb,str_list1=events1,str_list2=events2)
pig_with_adjectives=make_label_column(input_df=pig_with_verb,new_column_name=adjective,str_list1=adjectives1,str_list2=adjectives2)
pig_with_ac_condition=make_label_column(input_df=pig_with_adjectives,new_column_name=ac_condition,str_list1=ac_condition1,str_list2=NULL)


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





## statistical testing ####
## on prepped data set
## for deviation from norm by group

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






# plot monthly time series of events by e.g. airline, country
## time series plot #####
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


