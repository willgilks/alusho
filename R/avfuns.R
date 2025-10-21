
library("tidyverse")
# library("rvest") ## pull url
# library("countrycode") ## match cities to countries
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
      ungroup() |> 
      mutate({{new_column_name}}:=gsub("$;|;$","",{{new_column_name}}))
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

extract_dates=function(df_vec) {
  
  months_all=c(month.name,month.abb)
  months_pattern=paste(months_all,collapse="|")
  current_year=as.integer(format(Sys.Date(),"%Y"))
  
  # Match formats: Month Year, Month Day Year, Day Month Year
  date_pattern=paste0(
    "(?i)",
    "(\\b(",months_pattern,")\\b\\s*(\\d{1,2}(?:st|nd|rd|th)\\b)?\\s*,?\\s*(199\\d|20\\d{2}|",current_year,")?)|",
    "(\\b\\d{1,2}(?:st|nd|rd|th)?\\s+(",months_pattern,")\\b\\s*,?\\s*(199\\d|20\\d{2}|",current_year,")?)")
  
  matches=str_extract_all(df_vec,regex(date_pattern, ignore_case = TRUE))
  
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


extract_and_estimate_dates=function(input_df=deduped_data){
  
  extracted_dates_df=input_df |> 
    mutate(date_parts = extract_dates(input_df$orig_text)) |>
    unnest(date_parts)
  
  ## estimate missing dates based on reported date
  output_df=extracted_dates_df|>
    mutate(reporting_lag=as.numeric(reporting_date-date))|>
    mutate(reporting_lag=if_else(reporting_lag<1,as.numeric(NA),reporting_lag))|>
    mutate(avg_reporting_lag=median(reporting_lag,na.rm=T))|>
    mutate(est_date=reporting_date-avg_reporting_lag)|>
    mutate(date_source=if_else(is.na(date)&!is.na(est_date),"Estimated","Extracted"))|>
    mutate(date=if_else(is.na(date),est_date,date))|>
    select(orig_text,reporting_date,date,date_source)|>
    group_by(orig_text,reporting_date)|>
    filter(date==min(date))|>
    ungroup() |> 
    dplyr::rename(event_date=date)
  return(output_df)
}




cleanup_location_extraction=function(
    input_df=locs_data,
    cities_df=world_cities_filtered,
    airports_df=airports_tbl){
  
  
  airport_level_lookup=airports_df|>
    mutate(location2=trimws(gsub("Airport","",airport)))|>
    select(location2,country2=country,iso_code2=iso_code,lat2=lat,long2=long)|>
    group_by(location2,country2,iso_code2)|>
    summarise(lat2=mean(lat2),long2=mean(long2),.groups='drop')
  
  
  city_airport_info=airports_df|>
    mutate(location2=city)|>
    select(location2,country2=country,iso_code2=iso_code,lat2=lat,long2=long)|>
    group_by(location2,country2,iso_code2)|>
    summarise(lat2=mean(lat2),long2=mean(long2),.groups='drop')
  
  
  
  
  ## split up multi-location events
  locs_data_n_check=locs_data|>
    mutate(comma_count=str_count(location,","),
           and_count=str_count(location,"[:space:]]and[:space:]]"))|>
    mutate(comma_and_and_count=comma_count+and_count)|> 
    mutate(do_split=if_else(and_count>0|comma_count>0,TRUE,FALSE))|>
    mutate(max_split=max(comma_and_and_count)+1) 
  
  
  max_locs_split=locs_data_n_check$max_split[1]
  
  
  ## make long
  # count number of locations per event
  locs_banana_prep=suppressWarnings(
    locs_data_n_check|>
      mutate(location2=gsub("[:space:]]and[:space:]]",",[:space:]]",location))|> 
      filter(do_split==TRUE)|>
      separate(location2,into=paste0("loc",1:max_locs_split),sep=",",remove=FALSE))
  
  locs_banana=locs_banana_prep|>
    select(orig_text,reporting_date,location,location_ind,location2,starts_with("loc"))|>
    gather(loc_grp,subloc,-c(orig_text,reporting_date,location,location_ind,location2))|>
    mutate(subloc=trimws(subloc))|>
    filter(!is.na(subloc))|>
    group_by(orig_text,reporting_date)|>
    mutate(nlocations=n())|>
    ungroup()
  
  
  locs_banana_fin=locs_banana|>
    select(orig_text,reporting_date,location,location_ind,location2=subloc,nlocations)
  
  
  # join splitted multi-location events to single-location events
  locs_with_multi_split=locs_data_n_check|>
    filter(do_split==FALSE)|>
    mutate(location2=gsub("[,].*","",location))|>
    select(orig_text,reporting_date,location,location_ind,location2)|>
    mutate(nlocations=1)|>
    bind_rows(locs_banana_fin)
  
  
  
  ## for events missing geo assignments such as city country lat long
  ## left join city and airport tables and see if they match on variations of names
  ## this is cheaper than exhaustive coordinate matching
  output_df=locs_with_multi_split|>
    left_join(world_cities_filtered|>
                select(location=name2,country=country.etc,lat,long),
              relationship="many-to-many",by = join_by(location))|>
    left_join(world_cities_filtered|>
                select(location2=name2,country2=country.etc,lat2=lat,long2=long),
              relationship="many-to-many",by = join_by(location2))|>
    mutate(country=if_else(is.na(country)&!is.na(country2),country2,country),
           lat=if_else(is.na(lat)&!is.na(lat2),lat2,lat),
           long=if_else(is.na(long)&!is.na(long2),long2,long))|>
    select(-c(country2,lat2,long2))|>
    left_join(airport_level_lookup,relationship="many-to-many",by = join_by(location2))|>
    mutate(country=if_else(is.na(country)&!is.na(country2),country2,country),
           lat=if_else(is.na(lat)&!is.na(lat2),lat2,lat),
           long=if_else(is.na(long)&!is.na(long2),long2,long))|>
    select(-c(country2,lat2,long2))|>
    mutate(iso_code=countrycode(country,origin="country.name",destination="iso3c",warn=FALSE))|>
    mutate(iso_code=if_else(is.na(iso_code),iso_code2,iso_code))|>
    select(-iso_code2)|>
    left_join(city_airport_info,relationship="many-to-many",by = join_by(location2))|>
    mutate(country=if_else(is.na(country),country2,country),
           iso_code=if_else(is.na(iso_code),iso_code2,iso_code),
           lat=if_else(!is.finite(lat),lat2,lat),
           long=if_else(!is.finite(long),long2,long))|>
    select(-c(iso_code2,country2,lat2,long2))|>
    ungroup()|>
    distinct()|>
    mutate(iso_code2=countrycode(location2,origin="country.name",destination="iso3c",warn=FALSE))|>
    mutate(iso_code=if_else(is.na(iso_code),iso_code2,iso_code))|>
    select(-iso_code2)|>
    mutate(country2=countrycode(iso_code,origin="iso3c",destination="country.name"))|>
    mutate(country=if_else(is.na(country),country2,country))|>
    select(-country2)|>
    ungroup()
}

