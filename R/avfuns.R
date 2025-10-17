
library("tidyverse")
library("rvest") ## pull url
library("maps") # world map ggplot
library("countrycode") ## match cities to countries



## av herald dat scrape and analysis
splitwords=list(loc=" LOCS_PLIT ",date=" DATE_SPLIT ",comma="COMMA_SPLIT ")
end_of_first_period="2022-07-15"
start_of_second_period="2022-08-15"
end_of_second_period="2023-08-01"
earliest_overall_date="1994-03-01"
earliest_test_date=Sys.Date()-365






## pull data ####

loop_dates=seq.Date(from=base::as.Date(earliest_overall_date),to=Sys.Date(),by = "1 year")
# loop_dates=seq.Date(from=base::as.Date(Sys.Date()-365),to=Sys.Date(),by = "1 year")
# loop_dates




pig=raw_data


dedupe_pig=pig |> 
  # mutate(reporting_date=as.numeric(reporting_date)) |> 
  group_by(orig_text) |> filter(reporting_date==min(reporting_date)) |> 
  ungroup()



dedupe_pig


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



ac_types2=sort(unique(c(
  "501", # citatation
  "A3ST", ## airbus beluga
  "A124","A140","A148","A20n","A20N","A21N","A225","A300","A306","A30B","A310","A312",
  "A313","A318","A319","A320","A321","A322","A330","A332","A333","A337","A339","A340",
  "A342","A343","A345","A346","A359","A35k","A35K","A380","A388","A748","Airbus A306","Airbus,A320",
  "Airbus A321","Airbus A330","Airbus A332","Airbus A337","Airbus A346","Airbus A359","Airbus A380","AN-12","AN-140","AN-32","AN12","AN24",
  "AN26","AN28","AN30","AN32","AN38","AN74",
  "ATP", ## british aerospace
  ## ATR
  "AT42",
  "AT43",
  "AT45",
  "AT72",
  "AT76",
  "ATR42","ATR72",
  "BCS1", ## airbus a220-100
  "BCS2", ## airbus a220-200
  "BCS3", ## airbus a220-300
  "B190","B461",
  "B462","B463",
  "B703","B707","B712","B717","B721","B722","B723","B727","B732","B733",
  "B734","B735","B736","B737","B738",
  "738",
  "B73W",
  "B38M",
  "B39M", ## 737 max 9.
  "B73H", ## 737 737-9 with winglets
  "B78X",
  "BLCF", ## boeing dreamlifter
  "B72F", ## boeing 727 frieght
  "B739","B73G","B741","B742","B743","B744","B744F","B747","B74S",
  "747",
  "B748","B752","B753","B757","B762","B763","B764","B767","B772","B773","B777","B77W","B788",
  "B789","BAe 146","BAe 146-200","BAe146","Boeing 737","Boeing 737-600","Boeing 752","Boeing 767",
  "763",
  "Boeing 767-300","Boeing 777","Boeing 787",
  "B77F",
  "BE99", ## beechcraft
  "BN2P", ## Britten-Norman
  "TRIS",  ## Britten-Norman
  "Buffalo C46",
  "Caravan", ## cessna
  "CVLT", ## convair
  "CVLP", ## convair
  "CL-600", ## bombadier challenger
  "C123",
  "C130","C208GC","C212","C402","C421","C525","CL600","CRJ-100","CRJ-200","CRJ1","CRJ100","CRJ2",
  "CRJ",
  "CRJ200","CRJ4","CRJ7","CRJ700","CRJ9","CRJ900","Dash8","DC-10","DC-9","DC10","DC3","DC3T",
  "DC4","DC6","DC8","DC85","DC86","DC87","DC9","DC91","DC93","DC94","DC95","DC9E",
  "D228", ## dornier
  "D328",
  "Do328",
  "DH82",# de Havilland Canada
  "DH8A","DH8B","DH8C","DH8D","DHC6","DHC7","Dornier 328","E110","E120","E135","E140","E145",
  "EMB145",
  "EMB120",
  "EMB170",
  "EMB195",
  "E170","E175","E190","E195","E290","E295","Embraer 145","ERJ-190","ERJ135","ERJ145","F100","F27",
  "F28","F50","F70","Fokker 100","Fokker 27","Fokker 50","Fokker 70","IL18","IL62","IL76","IL86","IL96",
  "Jetstream 32","JS31","JS32","JS41",
  "J328", ## dornier
  "J31",
  "J32",
  "L188", ## lockheed
  "L101", ## Lockheed,
  "L410", ## Let
  "MA60", ## Xi'an
  "MD-10","MD-80","MD-82","MD-83","MD-87","MD-88","MD10","MD11",
  "MD80","MD81","MD82","MD83","MD87","MD88","MD90",
  "Q400",
  "RJ100","RJ1H","RJ70","RJ85",
  "Saab 340",
  "SF34", # saab
  "SB20", ## saab
  "SH33","SH36","Shorts 360","SW4","SU95","Superjet-100",
  "T134","Tu-134","TU154M","TU-154","T154","TU-154M","TU-154","T204","T-204","T214",
  "Vulcan",
  "Twin Otter", # de Havilland Canada
  "YK40","YK42")))





# dat_with_incident_si

pig_with_ac_type=make_label_column(input_df=dedupe_pig,new_column_name=ac_type,str_list1=NULL,str_list2=ac_types2)

pig_with_ac_type |> 
  filter(is.na(ac_type)|ac_type=='') |> 
  view()

# Parse fields and infer airline / country
pig_parsed=dedupe_pig |>
  filter(orig_text!="") |> 
  mutate(
    # airline=str_extract(orig_text,"^[A-Za-z\\s]+?(?=\\s[A-Z0-9]{3,4})") |> str_squish(),
    # aircraft=str_extract(orig_text,"[A-Z0-9]{3,4}(?=\\s(?:at|near))"),
    location=str_extract(orig_text,"(?<=at\\s|near\\s)[A-Za-z\\s]+(?=\\son\\s)") |> str_squish(),
    date_raw=str_extract(orig_text,"on\\s[A-Za-z]+\\s\\d{1,2}[a-z]{2}\\s\\d{4}") |> str_remove("^on\\s"),
    date=parse_date_time(date_raw,orders="b dY"),
    incident_summary=str_extract(orig_text,",\\s.*$") |> str_remove("^,\\s"),
    # airline_code=str_extract(airline,"\\b[A-Z]{2}\\b"),
    across(everything(),str_squish)) 
# |> 
#   repair_airline_names()



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

