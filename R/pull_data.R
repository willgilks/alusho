
library("tidyverse")
library("maps")
library("httr")
library("tidygeocoder")


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



raw_data=bind_rows(lapply(loop_dates,function(loop_date){
  loop_date_end=loop_date+365
  rdf=pull_data(date_start = loop_date,date_end=loop_date_end)
  rdf
}))



dedupe_raw_data=raw_data |> 
  # mutate(reporting_date=as.numeric(reporting_date)) |> 
  group_by(orig_text) |> filter(reporting_date==min(reporting_date)) |> 
  ungroup()

# write_csv(dedupe_raw_data,"~/Desktop/avh_raw_data.csv")

# dedupe_raw_data=read_csv("~/Desktop/avh_raw_data.csv",show_col_types = F)






avh_raw=read_csv("~/Desktop/avh_raw_data.csv")|>tibble() |> 
  mutate(rn=row_number())



## helper: take substring from start up to "at"/"near" and before "on <Month ...>"
prep_candidate_region=function(txt) {
  x=txt
  if (is.na(x)||x=="") return("")
  xu=toupper(x)
  
  ## cut at "on <Month ...>"
  cut_date=str_locate(xu,"\\bON\\s+(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|SEPT|OCT|NOV|DEC)\\b")[1,1]
  if (!is.na(cut_date)) xu=substr(xu,1,cut_date-1)
  
  ## cut at first " at " or " near "
  m1=str_locate(xu,"\\bAT\\b")[1,1]
  m2=str_locate(xu,"\\bNEAR\\b")[1,1]
  m=min(c(m1,m2),na.rm=TRUE)
  if (is.infinite(m)) m=NA_integer_
  if (!is.na(m)) xu=substr(xu,1,m-1)
  
  xu
}


## literal extractor (no rewriting)
extract_aircraft_names=function(txt) {
  if (is.na(txt)||txt=="") return(NA_character_)
  
  region=prep_candidate_region(txt)
  if (region=="") return(NA_character_)
  
  ## phrase patterns (makers + model)
  phrase_pat=paste0(
    "\\b(",
    "BOEING\\s?\\d{3}(?:-\\d{2,3})?",
    "|AIRBUS\\s?A?\\d{3}(?:-\\d{2,3})?",
    "|EMBRAER\\s?\\d{2,3}",
    "|FOKKER\\s?\\d{2,3}",
    "|SAAB\\s?\\d{2,4}",
    "|JETSTREAM\\s?\\d{2,3}",
    "|DASH\\s?-?8(?:\\s?Q?\\s?\\d{0,3})?",
    "|BAE\\s?146(?:-\\d{3})?",
    "|AVRO\\s?RJ\\d{2,3}",
    "|SHORTS\\s?(?:330|360)",
    "|DORNIER\\s?(?:228|328)",
    "|TWIN\\s+OTTER",
    ")\\b"
  )
  
  ## compact codes (must start with letters to avoid dates)
  code_pat=paste0(
    "\\b(",
    "B\\d{3}[A-Z]?|B7\\d{2}(?:-\\d{2,3})?|BLCF",
    "|A\\d{3}[A-Z]{0,2}|A\\d{2}[A-Z]{1,2}",
    "|DC-?\\d{1,3}[A-Z]?|MD-?\\d{2,3}",
    "|CRJ-?\\d{1,3}|RJ\\d{2,3}|RJ1H",
    "|ERJ-?\\d{2,3}|E\\d{3}|CL\\d{3,4}",
    "|ATR-?\\d{2,3}|AT(?:42|72)",
    "|DHC-?\\d{1,3}|DH8[ABCD]|Q400",
    "|AN-?\\d{2,3}|IL-?\\d{2,3}|SU\\d{2,3}|SSJ-?100",
    "|JS\\d{2}|SH\\d{2}|SH36",
    "|C212|C208(?:GC)?|C\\d{3}",
    "|F\\d{2,3}|YK\\d{2}",
    ")\\b"
  )
  
  ## find matches in uppercased region for stable matching
  reg_up=toupper(region)
  hits_up=c(
    str_extract_all(reg_up,phrase_pat)[[1]],
    str_extract_all(reg_up,code_pat)[[1]]
  )
  if (length(hits_up)==0) return(NA_character_)
  
  ## clean: drop trailing punct, plural 'S'
  hits_up=hits_up |>
    str_replace_all("[,.;:)]+$","") |>
    str_replace("(?<=[A-Z0-9])S$","") |>
    unique()
  
  ## filter: must contain both letters and digits; not years/ordinals
  hits_up=hits_up[str_detect(hits_up,"[A-Z]") & str_detect(hits_up,"\\d")]
  hits_up=hits_up[!str_detect(hits_up,"^(19|20)\\d{2}$")]
  hits_up=hits_up[!str_detect(hits_up,"^\\d{1,2}(ST|ND|RD|TH)$")]
  
  if (length(hits_up)==0) return(NA_character_)
  
  ## map back to original-case substrings from the original text
  res=map_chr(hits_up,\(h){
    m=str_extract(txt,regex(h,ignore_case=TRUE))
    ifelse(is.na(m),h,m)
  }) |> unique()
  
  if (length(res)==0) return(NA_character_)
  res
}


# avh_raw=read_csv("avh_raw_data.csv")|>tibble()

ac_names_temp=unlist(lapply(split(avh_raw,avh_raw$rn), function(z){
  tryCatch({extract_aircraft_names(z[1])},error=function(e)NULL)
}))
ac_names=sort(unique(ac_names_temp))
ac_names=ac_names[!grepl("[,]",ac_names)]
ac_names


aircraft_found=avh_raw|>
  mutate(aircraft_candidates=map_chr(orig_text,extract_aircraft_names))

aircraft_found|>select(orig_text,aircraft_candidates)


aircraft_found|>select(orig_text,aircraft_candidates) |> 
  filter(is.na(aircraft_candidates)) |> 
  view()

## MISSING
## MD-10, DC-9-10, MD-82, AN-140, TU-154M, CRJ-100, MD-88, Tu-134, MD-83, DC-9, CRJ, MD-10


parse_aircraft_text=function(x) {
  
  if (is.na(x)||x=="") return(tibble(
    aircraft_types=NA_character_,
    event_date=NA_Date_,
    airlines=NA_character_,
    location=NA_character_,
    number_of_aircraft=NA_integer_,
    event_description=NA_character_
  ))
  
  ## ---------- aircraft extraction ----------
  ## 1) Phrase-level patterns (multiword, hyphenated, with makers)
  phrase_terms=c(
    "Fokker\\s?(?:50|70|100)",
    "Saab\\s?(?:340|2000)",
    "Jetstream\\s?(?:31|32|41)",
    "BAe\\s?146(?:-?\\d{3})?",
    "Avro\\s?RJ\\d{2,3}",
    "Twin\\s+Otter",
    "Dash\\s?-?8(?:\\s?Q?\\s?\\d{0,3})?",
    "Embraer\\s?(?:145|170|175|190|195)",
    "Boeing\\s?\\d{3}(?:-\\d{2,3})?",
    "Airbus\\s?A?\\d{3}(?:-\\d{2,3})?",
    "Dornier\\s?(?:228|328)",
    "Shorts\\s?(?:330|360)",
    "Cessna\\s?(?:208|172|152)"
  )
  phrase_pat=paste0("(?i)\\b(",paste(phrase_terms,collapse="|"),")\\b")
  phrase_hits=str_extract_all(x,phrase_pat)|>unlist()
  
  ## 2) Code-level patterns (short ICAO/IATA and common aliases)
  code_pat=paste0(
    "\\b(?:",
    ## Boeing short/long
    "BLCF|B7\\d{2}(?:-\\d{2,3})?|B\\d{3}[A-Z]?|\\d{3}(?:-\\d{2,3})?(?=\\b)",  ## 747, 767-300
    "|",
    ## Airbus incl. neo codes
    "A\\d{3}[A-Z]{0,2}|A\\d{2}[A-Z]{1,2}",                                   ## A333,A20N,A21N
    "|",
    ## Douglas / MD
    "DC-?\\d{1,3}[A-Z]?|DC3T|MD-?\\d{2,3}",
    "|",
    ## CRJ / RJ
    "CRJ-?\\d{1,3}|RJ\\d{2,3}|RJ1H",
    "|",
    ## ERJ / E-jets / Challenger
    "ERJ-?\\d{2,3}|E\\d{3}|CL\\d{3,4}",
    "|",
    ## ATR / AT short
    "ATR-?\\d{2,3}|AT(?:42|72)",
    "|",
    ## De Havilland / DHC / DH8 codes
    "DHC-?\\d{1,3}|DH8[ABCD]s?|Q400|DASH-?8",
    "|",
    ## Saab short codes
    "SF34|SB20|SW4",
    "|",
    ## Fokker short codes
    "F\\d{2,3}",
    "|",
    ## Dornier/Do
    "DO\\s?\\d{2,3}|D\\d{3}|D228|D328|J328",
    "|",
    ## Antonov
    "AN-?\\d{2,3}",
    "|",
    ## Ilyushin
    "IL-?\\d{2,3}",
    "|",
    ## Sukhoi
    "SU\\d{2,3}|SSJ-?100",
    "|",
    ## Jetstream
    "JS\\d{2}",
    "|",
    ## Shorts short code
    "SH\\d{2}|SH36",
    "|",
    ## CASA/C-series
    "C212|C208GC|C208|C\\d{3}",
    "|",
    ## Yakovlev
    "YK\\d{2}",
    "|",
    ## Misc numerics we want to keep when stand-alone (752,738,763 etc.)
    "(?<![A-Z])7(?:3[57]|4[7]|6[37]|8[7])(?:\\d)?",                          ## 737/738/752/763/787 etc.
    "(?<![A-Z])\\d{3}(?:-\\d{2,3})?",                                        ## generic 767-300 case
    ")\\b"
  )
  code_hits=str_extract_all(toupper(x),code_pat)|>unlist()
  
  ## Combine + clean tokens
  raw_tokens=c(phrase_hits,code_hits)
  tok=raw_tokens|>
    str_replace_all("[,.;:)]+$","")|>
    str_replace("(?<=[A-Z0-9])S$","")|>   ## B763s->B763, DH8Ds->DH8D
    str_squish()|>
    unique()
  
  ## Remove pure years
  tok=tok[!str_detect(tok,"^(19|20)\\d{2}$")]
  
  ## Keep only plausible aircraft prefixes or known phrases
  keep_pat=paste0(
    "(?i)^(",
    "B|B7|BOEING|A|AIRBUS|DC|MD|CRJ|RJ|ERJ|E|CL|ATR|AT|DHC|DH8|DASH|AN|IL|SU|",
    "JS|BAE|AVRO|F|FOKKER|DO|DORNIER|SH|SHORTS|C|CESSNA|CASA|YK|SF34|SB20|",
    "Q400|BLCF|TWIN\\s+OTTER|JETSTREAM|SAAB|DASH|\\d{3}(-\\d{2,3})?",
    ")"
  )
  tok=tok[str_detect(tok,keep_pat)]
  
  aircraft=ifelse(length(tok)==0,NA_character_,paste(tok,collapse=", "))
  
  ## ---------- airline ----------
  airline=str_extract(x,"^[A-Z][A-Za-z\\s&']+(?=\\s+[A-Za-z]*\\d)") %||% ""
  
  ## ---------- location (at|near) ----------
  location=str_extract(
    x,
    "(?<=\\b(?:at|near)\\s)[A-Za-z0-9'./()\\-\\s,]+?(?=(\\s+(on|when|after|around)\\s)|[,.;]|$)"
  )|>str_trim()
  if (is.na(location)||location=="") {
    location=str_extract(x,",\\s*([^,]+?)\\s+(?=(on|when|after)\\s)")|>
      str_replace("^,\\s*","")|>str_trim()
  }
  if (is.na(location)||location=="") location=NA_character_
  
  ## ---------- date ----------
  date_raw=str_extract(x,"\\b(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\\s+\\d{1,2}(st|nd|rd|th)?(\\s+\\d{4})?")
  event_date=parse_date_time(date_raw,orders=c("b d Y","b d","bdY","bd"),quiet=TRUE)|>as_date()
  
  ## ---------- number of aircraft ----------
  number_of_aircraft=ifelse(is.na(aircraft),NA_integer_,str_count(aircraft,",")+1)
  
  ## ---------- description ----------
  event_desc=str_extract(x,"(?<=\\b\\d{4}[.,;:]?\\s)(.*)$")
  if (is.na(event_desc)||event_desc=="") {
    event_desc=str_extract(x,"(?<=\\b(?:at|near)\\s)[^,]+(.*)$")
  }
  event_desc=ifelse(is.na(event_desc)||event_desc=="",NA_character_,str_trim(event_desc))
  
  tibble(
    aircraft_types=aircraft,
    event_date=event_date,
    airlines=airline,
    location=location,
    number_of_aircraft=number_of_aircraft,
    event_description=event_desc
  )
}

parsed_data=avh_raw|>
  filter(!is.na(orig_text))|>
  mutate(orig_text=str_replace_all(orig_text,"\\s+"," "))|>
  rowwise()|>
  mutate(tmp=list(parse_aircraft_text(orig_text)))|>
  unnest(cols=c(tmp))

parsed_data|>
  select(orig_text,aircraft_types,event_date,airlines,location,number_of_aircraft,event_description)




parsed_data|>
  # select(orig_text,aircraft_types,event_date,airline,location,number_of_aircraft,event_description) |> 
  select(orig_text,aircraft_types,event_date,airlines,location) |> 
  filter(is.na(aircraft_types)) |>
  arrange(orig_text) |> 
  view()


sort(unique(avh_raw$orig_text))


parsed_data|>
  select(orig_text,aircraft_types,event_date,airlines,location,number_of_aircraft,event_description)


parsed_data|>
  # select(orig_text,aircraft_types,event_date,airline,location,number_of_aircraft,event_description) |> 
  select(orig_text,aircraft_types,event_date,airline,location) |> 
  # filter(is.na(aircraft_types)) |> 
  arrange(orig_text) |> 
  view()

####



# parsed_data|>
# select(orig_text,aircraft_types,event_date,airlines,location,number_of_aircraft,event_description)

# 1. issues with airlines being mis-assigned. e.g. there's no such airline as just 'Air'.


avh_parsed|>
  # select(orig_text,aircraft_types,event_date,airline,location,number_of_aircraft,event_description) |> 
  select(orig_text,aircraft_types,event_date,airline,location) |> 
  # filter(is.na(aircraft_types)) |> 
  arrange(orig_text) |> 
  view()



## missing CL600, Shorts 360  as an aircraft type
## DH8Ds, B763s, A20N, A21N, B78X, D328, IL76, SU95
##  A20N, A21N, A30B, AN12, AN26, AT42, AT72, B78X, B763s, B73G, CRJ9, DH8C, DH8Ds, DC3T, JS31, JS32, 

repair_airline_names=function(df){
  df |>
    mutate(airline=if_else(grepl("Aeromexcio",airline,ignore.case = F),"Aeromexico",airline)) |> 
    mutate(airline=if_else(grepl("Airblue",airline),"AirBlue",airline)) |>
    mutate(airline=if_else(grepl("American",airline),"American",airline)) |>
    mutate(airline=if_else(grepl("^Argentina ",airline),"Argentinas",airline)) |>
    mutate(airline=if_else(grepl("ASL",airline),"ASL",airline)) |>
    mutate(airline=if_else(grepl("Atlanta",airline),"Atlanta",airline)) |>
    mutate(airline=if_else(grepl("ANA",airline),"ANA",airline)) |>
    mutate(airline=if_else(grepl("BAW",airline),"British Airways",airline)) |>
    mutate(airline=if_else(grepl("British Airways",airline),"British Airways",airline)) |>
    mutate(airline=if_else(grepl("Canada ",airline),"Canada",airline)) |>
    mutate(airline=if_else(grepl("^Canada$",airline),"Air Canada",airline)) |>
    mutate(airline=if_else(grepl("Cathay",airline),"Cathay",airline)) |>
    mutate(airline=if_else(grepl("Eastern Airways",airline),"Eastern",airline)) |>
    mutate(airline=if_else(grepl("Easyjet",airline),"Easyjet",airline)) |>
    mutate(airline=if_else(grepl("Eurowings",airline),"Eurowings",airline)) |>
    mutate(airline=if_else(grepl("^France$",airline),"Air France",airline)) |>
    mutate(airline=if_else(grepl("Iceland |^ICE$",airline),"Iceland",airline)) |>
    mutate(airline=if_else(grepl("Jetblue",airline),"Jetblue",airline)) |>
    mutate(airline=if_else(grepl("Jetstar",airline),"Jetstar",airline)) |>
    mutate(airline=if_else(grepl("KLM",airline),"KLM",airline)) |>
    mutate(airline=if_else(grepl("LATAM",airline),"LATAM",airline)) |>
    mutate(airline=if_else(grepl("Lingus",airline),"Aer Lingus",airline)) |>
    mutate(airline=if_else(grepl("Lufthansa",airline),"Lufthansa",airline)) |>
    mutate(airline=if_else(grepl("Norwegian",airline),"Norwegian",airline)) |>
    mutate(airline=if_else(grepl("Ryanair",airline),"Ryanair",airline)) |>
    mutate(airline=if_else(grepl("Smartwings",airline),"Smartwings",airline)) |>
    mutate(airline=if_else(grepl("SriLankan",airline,ignore.case = T),"SriLankan",airline)) |> 
    mutate(airline=if_else(grepl("Transavia",airline),"Transavia",airline)) |>
    mutate(airline=if_else(grepl("TUI",airline),"TUI",airline)) |>
    mutate(airline=if_else(grepl("Transavia",airline),"Transavia",airline)) |>
    mutate(airline=if_else(grepl("Virgin",airline),"Virgin",airline)) |>
    mutate(airline=if_else(grepl("Westjet",airline),"Westjet",airline)) |>
    ungroup()
}




# get city coords
city_coords=as_tibble(maps::world.cities) |> 
  group_by(name) |> 
  filter(pop==max(pop,na.rm=T)) |> 
  ungroup() |> 
  select(city_name=name,maps_country=country.etc,lat,long) |> 
  distinct()


# Parse fields and infer airline / country
raw_data_parsed=dedupe_raw_data |>
  filter(orig_text!="") |> 
  mutate(
    airline=str_extract(orig_text,"^[A-Za-z\\s]+?(?=\\s[A-Z0-9]{3,4})") |> str_squish(),
    aircraft=str_extract(orig_text,"[A-Z0-9]{3,4}(?=\\s(?:at|near))"),
    location=str_extract(orig_text,"(?<=at\\s|near\\s)[A-Za-z\\s]+(?=\\son\\s)") |> str_squish(),
    date_raw=str_extract(orig_text,"on\\s[A-Za-z]+\\s\\d{1,2}[a-z]{2}\\s\\d{4}") |> str_remove("^on\\s"),
    date=parse_date_time(date_raw,orders="b dY"),
    incident_summary=str_extract(orig_text,",\\s.*$") |> str_remove("^,\\s"),
    airline_code=str_extract(airline,"\\b[A-Z]{2}\\b"),
    across(everything(),str_squish)) |> 
  repair_airline_names()



## assign geo coords tp cities ad filter
locs_unique_temp=raw_data_parsed |>
  filter(!is.na(location),location!="") |>
  distinct(location) |> 
  mutate(city_name=location) |> 
  mutate(city_name2=location) |> 
  left_join(city_coords)|> 
  left_join(city_coords |> mutate(city_name2=gsub("^'","",city_name)) |> select(-city_name)) |> 
  select(location,maps_country,lat,long) |> 
  distinct()


# locs_missing_info=locs_unique_temp |> 
#   filter(is.na(maps_country))|>
#   mutate(location=trimws(location))|>
#   geocode(address=location,method="osm",lat=lat,long=long,full_results=TRUE)|>
#   mutate(map_country=if_else(is.na(maps_country)&!is.na(country),country,maps_country))|>
#   select(location,map_country,lat,long)

locs_unique=locs_unique_temp |> 
  filter(!is.na(maps_country))|>
  # bind_rows(locs_missing_info) |> 
  ungroup()



# Repair dates, assign countries and geo coords
data_with_geo_info=raw_data_parsed |> 
  # mutate(date=base::as.Date(date),reporting_date=base::as.Date(reporting_date,format="%Y%m%d")) |> 
  left_join(locs_unique)
data_with_geo_info
