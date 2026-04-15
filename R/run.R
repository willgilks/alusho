library("tidyverse")
library("countrycode") ## match cities to countries
# library("maps") # world map ggplot
library("tidyverse")
library("httr")

source("./R/constants.R")
source("./R/avfuns.R")


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


## make lookup table for aircraft type from hardcoded list
ac_type_lookup_table=make_ac_type_lookup(ac_type_input_list=AIRCRAFT_SEARCH_STRINGS)


## pull data ####

# from_date=earliest_overall_date
from_date=earliest_test_date
loop_dates=seq.Date(from=base::as.Date(from_date),to=Sys.Date(),by="1 year")

raw_data=read_csv("./avh_raw_data.csv",show_col_types=FALSE)

# raw_data=bind_rows(lapply(loop_dates,function(loop_date){
#   loop_date_end=loop_date+365
#   rdf=pull_data(date_start = loop_date,date_end=loop_date_end)
#   rdf
# }))


## keep earliest reporting data for duplicated reports
deduped_data=raw_data|>
  mutate(orig_text=gsub("  "," ",orig_text))|>
  mutate(orig_text=gsub("  "," ",orig_text))|>
  group_by(orig_text)|>
  filter(reporting_date==min(reporting_date))|>
  ungroup() |> 
  mutate(lwr_orig_text = str_to_lower(orig_text))|> 
  arrange(lwr_orig_text) |> 
  mutate(rn=row_number()) 


# AIRLINES
# panama occurs both as an airline and as a city airport name.


with_airlines_data = 
  bind_rows(
    lapply(split(deduped_data,deduped_data$rn)[1:100], function(z){
      
      print(paste(Sys.time(),z$lwr_orig_text[1]))
      bind_rows(
        lapply(AIRLINES, function(i){
          
          # print(i)
          search_term = paste0("^",sort(unique(str_to_lower(i)))," ")
          
          if ( isTRUE(grepl(search_term,z$lwr_orig_text[1])) ){
            z |>
              mutate(carrier = i[1]) |>
              ungroup()
          }
          
        })
      ) |> 
        distinct()
    })
  )
with_airlines_data

# deduped_data
## alternative approach
## insert start and end brackets around what the item is, e.g. carrier, ac type, location adjective, location, date, event text.


# Download the OpenFlights airline database
# Columns: ID, Name, Alias, IATA, ICAO, Callsign, Country, Active
url <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airlines.dat"
airlines_full <- read_csv(url, col_names = c("id", "name", "alias", "iata", "icao", "callsign", "country", "active"))

# Filter for relevant columns and clean data
airlines_clean <- airlines_full %>%
  filter(!is.na(name)) %>%
  select(standard_name = name, iata, icao, alias) %>%
  mutate(across(everything(), as.character))




## initial approach
## 
## extract aircraft type info #####
data_with_ac_info=deduped_data|>
  mutate(match=purrr::map(orig_text,~extract_aircraft(.x,lookup_tbl=ac_type_lookup_table)))|>
  unnest(match)


## make provisional carrier names
initial_carrier_info=data_with_ac_info|>
  rowwise()|>
  mutate(carrier=trimws(sub(paste0(reported_ac_name[1],".*"),"",orig_text)))|>
  ungroup()|>
  select(carrier,reported_ac_name,orig_text)|>
  mutate(carrier=str_to_lower(carrier))|>
  #   mutate(carrier=gsub("[[:space:]]at[[:space:]].*|
  # [[:space:]]on[[:space:]].*|
  # [[:space:]]in[[:space:]].*|
  # [[:space:]]over[[:space:]].*|
  # [[:space:]]near[[:space:]].*","",carrier))|>
  mutate(carrier=gsub(" at .*|
   on .*| in .*| over .*| near .*","",carrier))|>
  mutate(carrier = gsub("\\[\\[:space:\\]\\]"," ",carrier)) |> 
  group_by(carrier)|>
  mutate(carrier_n=n())|>
  ungroup()


initial_carrier_info
## make list of believable carrier names
filtered_carrier_dat=initial_carrier_info|>
  filter(carrier_n>5|grepl("airlines|airways",carrier,ignore.case = T))|>
  select(carrier)|>distinct()|>pull()

## grepl for likely carrier names.
## test the original text for occurrence of the patterns
reassigned_carriers_dat=bind_rows(lapply(split(initial_carrier_info,initial_carrier_info$carrier), function(row_df){
  # print(row_df)
  bind_rows(lapply(filtered_carrier_dat,function(z){
    if (grepl(paste0(paste0(z,"$"),"|",paste0(z," ")),row_df$carrier[1])){
      row_df |> 
        mutate(assigned_carrier=z)
    }
  }))
})) |> 
  select(orig_text,assigned_carrier) |> 
  distinct()

reassigned_carriers_dat |> 
  group_by(assigned_carrier) |> 
  mutate(carrier_n=n()) |> 
  ungroup() |> 
  select(assigned_carrier,carrier_n) |> 
  distinct() |> 
  view()
select()


## CHECK ISSUES WITH ASSIGNED CARRIER
## 1.  'wings' should not be a carrier name. german wings ? smart wings ?
## 2. 'thy' should probably be changed to 'turkish'
## 3. 'united' vs 'united airlines'.
## 4. 'delta' vs 'delta airlines'
## 5. 'france' vs 'air france'
## 6. virgin vs virgin atlantic vs virgin america
## 7. germanwings vs german wings  
## 8. ba vs british airways
## 9. turkish airlines vs turkish
## 10. aer lingus vs lingus
## 11. easyjet vs easyjet europe vs easyjet switzerland
## 12. avianca braSil vs braZil

## extract event location info #####
## separate out location prepositions near/at/overhead, and likely location string
locs_data=extract_location(deduped_data,city_names=NULL)


## extract remaining info and assign geo coords and country
data_with_locs_and_geos=cleanup_location_extraction(
  input_df=locs_data,
  cities_df=world_cities_filtered,
  airports_df=airports_tbl)


## extract event dates #####
## if they are unknown, then estimated from lag time.
data_with_estimated_dates=extract_and_estimate_dates(deduped_data)


## extract details #####
## extract stage of flight info, aircraft parts, people involved, verbs, adjectives, aicraft condition.
## make columns summarising common events
{
  dat_with_times=make_label_column(input_df=deduped_data,new_column_name=flight_stage,str_list1=times1,str_list2=NULL)
  dat_with_ac_parts=make_label_column(input_df=dat_with_times,new_column_name=ac_part,str_list1=ac_parts1,str_list2=ac_parts2)
  dat_with_people=make_label_column(input_df=dat_with_ac_parts,new_column_name=persons,str_list1=people1,str_list2=NULL)
  dat_with_verb=make_label_column(input_df=dat_with_people,new_column_name=verb,str_list1=events1,str_list2=events2)
  dat_with_adjectives=make_label_column(input_df=dat_with_verb,new_column_name=adjective,str_list1=adjectives1,str_list2=adjectives2)
  dat_with_ac_condition=make_label_column(input_df=dat_with_adjectives,new_column_name=ac_condition,str_list1=ac_condition1,str_list2=NULL)
}

## maybe for each word, assess the possibility that it's an aircraft., e.g. by length, and composition of letters, numbers and special.

# word_compos_dat=bind_rows(lapply(split(deduped_data,deduped_data$orig_text), function(df_row){
#   
#   text_vec=df_row$orig_text
#   print(text_vec)
#   banana=strsplit(text_vec,"\\s+")
#   
#   bind_rows(lapply(banana,function(z){
#     tibble(word=z,
#            nalpha=str_count(z,"[[:alpha:]]"),
#            npunc=str_count(z,"[[:punct:]]"),
#            nnum=str_count(z,"[[:number:]]"))|>
#       mutate(pos=row_number(),
#              orig_text=text_vec[1]) |> 
#       mutate(wlen=nalpha+npunc+nnum) |> 
#       mutate(prop_alpha=nalpha/wlen,prop_num=nnum/wlen) |> 
#       mutate(is_day=if_else(wlen>2&wlen<10&grepl("1st$|1nd$|2nd$|3nd$|3rd$|4rd$|[0-9]th$",word)&!grepl("-",word),TRUE,FALSE))
#     
#   }))
# }))
# filterd_ac_dat=word_compos_dat |> 
#   filter(prop_alpha>.05,prop_alpha<.75,prop_num>.2,prop_num<.9) |> 
#   filter(is_day==FALSE) |> 
#   filter(npunc==0|grepl("-",word)) |> 
#   select(orig_text,word,prop_alpha,prop_num,word,wlen,pos) |> 
#   distinct() |> 
#   group_by(word) |> 
#   mutate(word_n=n()) |> 
#   ungroup()


## make an event label
## join other extraction tables.
## make nice column names
dat_fin=dat_with_ac_condition|>
  mutate(flight_stage=gsub("^;|;$","",flight_stage),
         ac_part=gsub("^;|;$","",persons),
         verb=gsub("^;|;$","",verb),
         adjective=gsub("^;|;$","",adjective),
         ac_condition=gsub("^;|;$","",ac_condition)) |> 
  rowwise()|>
  mutate(event_lab=paste0(c(flight_stage,ac_part,persons,verb,adjective,ac_condition),collapse=";"))|>
  ungroup()|>
  mutate(event_lab=gsub("[;][;][;]",";",event_lab))|>
  mutate(event_lab=gsub("[;][;]",";",event_lab))|>
  mutate(event_lab=gsub("^[;]|[;]$","",event_lab))|>
  full_join(data_with_estimated_dates,by=join_by(orig_text,reporting_date),relationship="many-to-many")|>
  full_join(reassigned_carriers_dat,by = join_by(orig_text),relationship = "many-to-many") |>
  full_join(data_with_ac_info,by=join_by(orig_text,reporting_date),relationship="many-to-many") |> 
  full_join(data_with_locs_and_geos,by=join_by(orig_text,reporting_date),relationship="many-to-many")|>
  ungroup()|>
  select(report_date=reporting_date,
         flight_stage,
         report_text=orig_text,
         event_date,date_source,
         assigned_carrier,
         ac_manufacturer=manufacturer,
         ac_family,
         ac_standard_name=standardised_ac_name,
         ac_reported_name=reported_ac_name,
         ac_number_of_ac=num_aircraft,
         geo_loc=location,
         geo_loc_ind=location_ind,
         geo_loc2=location2,
         geo_nlocs=nlocations,
         geo_iso_code=iso_code,
         geo_country=country,
         geo_lat=lat,geo_long=long,
         descr_stage=flight_stage,
         descr_part=ac_part,
         descr_action=verb,
         descr_adjective=adjective,
         descr_condition=ac_condition)|>
  ungroup()

# dat_fin
# write_csv(dat_fin,"./avh_processed_20251021.csv")

# write_csv(pig_parsed,"~/Desktop/pig_parsed.csv")


names(dat_fin)



# Reporting-lag time series #####
pig_geo=dat_fin |> 
  mutate(reporting_delay = report_date - event_date) |> 
  select(geo_lat,geo_long,event_date,reporting_delay,report_text)
pdat=pig_geo |>
  # filter(event_date>=Sys.Date()-365) |>
  select(event_date,reporting_delay,report_text) |>
  distinct() |>
  ungroup()
ggplot(pdat,aes(event_date,reporting_delay))+
  geom_point()+
  labs(x="",y="Reporting lag",
       title="Time delay from event date to reporting date",
       caption=Sys.Date())


## Events map #####
pdat=pig_geo |>
  filter(event_date>=Sys.Date()-365) |>
  mutate(tt_text_part = paste0(c(event_date,"\n",report_text),collapse="")) |> 
  group_by(geo_lat,geo_long) |>
  summarise(nr=n(),tt_text = paste0(tt_text_part,collapse="\n"),.groups='drop')
ggplot(pdat,aes(geo_long,geo_lat))+
  geom_point(aes(size=nr))+
  labs(x="",y="",
       title="Events map",
       caption=Sys.Date())


## airlines
airlines_dat = dat_fin |> 
  select(event_date,assigned_carrier,report_text) |> 
  distinct() |> 
  mutate(assigned_carrier=if_else(is.na(assigned_carrier),"unknown",assigned_carrier)) |> 
  group_by(assigned_carrier) |> 
  arrange(event_date,.by_group = TRUE) |> 
  mutate(event_interval = as.numeric(event_date-lag(event_date))) |>
  mutate(airline_n = n()) |> 
  ungroup()

airlines_dat

airlines_pdat = airlines_dat |> 
  filter(airline_n>290) |> 
  filter(event_date > "2019-01-01")

ggplot(airlines_pdat,aes(event_date,event_interval))+
  geom_line()+
  facet_wrap(fct_reorder(assigned_carrier,airline_n)~.,scales = 'free_y')




## statistical testing ####
## on prepped data set
## for deviation from norm by group

dog_prep=pig_geo |> 
  select(airline,incident=incident_sum_short,airline_total,incident_total,
         airline_with_incident,overall_total_events) |> 
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
