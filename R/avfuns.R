## https://dcl-wrangle.stanford.edu/rvest.html
library("tidyverse")
library("rvest")
library("maps")
library("countrycode") ## match cities to countries
library("zoo")


base_url="https://avherald.com/"
# base_url="https://www.tidyverse.org/packages/"
### alt dev scrape ####
website <- base_url %>%
  read_html()

a_elements <- website %>%
  html_elements(
    # css="",xpath="",
    # css = "div.package > a"
  )
a_elements

links <- a_elements %>%
  html_attr(name = "href")
links
pages <- links %>%
  purrr::map(read_html)
pages


pages %>%
  purrr::map(html_element, css = "a.navbar-brand") %>%
  map_chr(html_text)

pages %>%
  purrr::map(html_element, css = "small.nav-text.text-muted.me-auto") %>%
  map_chr(html_text)




### and another
library(httr)
library(jsonlite)

# Using Chrome Inspector we can see that this is the API link
example_url <-"https://doctorisysearchprod.search.windows.net/indexes/profiles-index/docs?api-version=2017-11-11&&search=*&$filter=search.ismatch('Guatemala', 'country', 'full', 'all')&$top=10&$skip=10&facet=&$count=true&$orderby=orderPay desc, plid desc, orderNoPay desc, aleatory asc"

# Make a function that creates a valid URL based on inputs
doctorisy_url <- function(filter1 = 'Guatemala',
                          filter2 = 'country',
                          filter3 = 'full',
                          filter4 = 'all',
                          top = 10,
                          skip = 0){
  example_url <- paste0(
    "https://doctorisysearchprod.search.windows.net/indexes/profiles-index/docs?api-version=2017-11-11&",
    "&search=*&$filter=search.ismatch('",filter1,"', '",filter2,"', '",filter3,"', '",filter4,"')",
    "&$top=",top,"&$skip=",skip,
    "&facet=&$count=true&$orderby=orderPay desc, plid desc, orderNoPay desc, aleatory asc"
  )
  
  example_url <- URLencode(example_url,repeated = TRUE)
}

# Make a custom GET function that adds the headers that the site expects to receive so we don't get a 404 or 400 error.
doctorisy_GET <- function(url,...){
  GET(url,
      add_headers(# Override default headers
        authority = "doctorisysearchprod.search.windows.net",
        # method = "GET",
        # path = "/indexes/profiles-index/docs?api-version=2017-11-11&&search=*&$filter=search.ismatch(%27Guatemala%27,%20%27country%27,%20%27full%27,%20%27all%27)&$top=10&$skip=10&facet=&$count=true&$orderby=orderPay%20desc,%20plid%20desc,%20orderNoPay%20desc,%20aleatory%20asc",
        # scheme = "https",
        accept = "application/json, text/plain, */*",
        `accept-encoding` = "gzip, deflate, br",
        `accept-language` = "en-US,en;q=0.9",
        `api-key` = "A7A0E69A1BB9C015591C62298F330840",
        application = "WEB_Hlp5E88Gpj",
        origin = "https://www.doctorisy.com",
        referer = "https://www.doctorisy.com/",
        # sec-ch-ua: "Google Chrome";v="95", "Chromium";v="95", ";Not A Brand";v="99"
        # sec-ch-ua-mobile: ?0
        # sec-ch-ua-platform: "Windows"
        # sec-fetch-dest: empty
        # sec-fetch-mode: cors
        # sec-fetch-site: cross-site
        # time-zone: America/New_York
        `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.69 Safari/537.36"
      ),
      ...)
}

# Pull the data from the API starting on page 1
page <- doctorisy_GET(doctorisy_url(skip = 0))
page_json <- jsonlite::fromJSON(rawToChar(page$content))
page_json$value$firstSurName
# [1] "Berganza" "De León"  "Barreda"  "Pozuelos" "Morales"  "Pacheco"  "Mayén"    "Claverie" "Osorio"   "Vitale"

# Pull the data from the API starting on page 2 (assuming we are getting top 10)
page <- doctorisy_GET(doctorisy_url(skip = 10))
page_json <- jsonlite::fromJSON(rawToChar(page$content))
page_json$value$firstSurName
# [1] "Quijada"     "Astorga"     "Asturias"    "Arévalo"     "López"       "Piche"       "Estrada"     "Castellanos" "Liuti"
# [10] "Cabrera"

# Change the default top 10 to top 100 and start at 0.
# WARNING: API will max out at some unkown value of top. Going over top = 100 is risky.
page <- doctorisy_GET(doctorisy_url(top = 100, skip = 0))
page_json <- jsonlite::fromJSON(rawToChar(page$content))
page_json$value$firstSurName
# [1] "Berganza"    "De León"     "Barreda"     "Pozuelos"    "Morales"     "Pacheco"     "Mayén"       "Claverie"    "Osorio"
# [10] "Vitale"      "Quijada"     "Astorga"     "Asturias"    "Arévalo"     "López"       "Piche"       "Estrada"     "Castellanos"
# [19] "Liuti"       "Cabrera"     "Ochoa "      "Lengua"      "Hernández"   "Rodriguez"   "García"      "Guillén"     "Valencia"
# [28] "Choc"        "Turcios"     "Quan"        "Saucedo B"   "Villatoro"   "López"       "Amato"       "Barillas"    "Mendoza"
# [37] "Portillo"    "Díaz"        "MacDonald"   "Barrios"     "Garcia"      "Araujo"      "Carranza"    "Estrada"     "Bauer"
# [46] "Mayén"       "De la Cruz"  "Chacón"      "Contreras"   "Obregón"     "Matta"       "Ranchos"     "Torres"      "Labbé"
# [55] "Andrade"     "Gutiérrez"   "Paredes "

# Convert the JSON to a dataframe
df <- page_json$value
tibble(df)


# library(rare)
# library(Matrix)
# # Design matrix = document-term matrix
# pig=data.dtm[1:15,1:15]
# pig=as_tibble(data.frame(as.matrix(data.dtm))) |>
#   mutate(rn1=row_number()) |>
#   gather(grp,val,-rn1) |>
#   filter(val>0) |>
#   # spread(grp,val) |>
#   ungroup()
# pig
#
# names(pig)
# dim(data.dtm)
# #> [1] 500 200
# # Ratings for the reviews in data.dtm
# length(data.rating)
# hist(colMeans(sign(data.dtm)) * 100, breaks = 50, main = "Histogram of Adjective Rarity in the TripAdvisor Sample",
#      xlab = "% of Reviews Using Adjective")
#
# par(cex=0.35)
# plot(as.dendrogram(data.hc),center=F)
#
#
# ## Fit the Model
# set.seed(100)
#
# ## training set
# ts <- sample(1:length(data.rating), 400) # Train set indices
#
# ## let the program to determine lambda and alpha sequence.
# ## after setting length of sequences to be nlam = 20 and nalpha = 10.
# ## fit the model on the training set over the two-dimensional grid of regularization parameters
# ## The rarefit function implements the model fit alongside alpha,
# ## i.e., at each α the model is fitted over the entire sequence of lambda values.
# ourfit <- rarefit(y = data.rating[ts], X = data.dtm[ts, ], hc = data.hc, lam.min.ratio = 1e-6,
#                   nlam = 20, nalpha = 10, rho = 0.01, eps1 = 1e-5, eps2 = 1e-5, maxite = 1e4)
#
# ## Perform K-Fold Cross Validation
# ourfit.cv <- rarefit.cv(ourfit, y = data.rating[ts], X = data.dtm[ts, ],
#                         rho = 0.01, eps1 = 1e-5, eps2 = 1e-5, maxite = 1e4)
#
#
# # Make Predictions for New Observations
# # After choosing optimal (λ,α)
# # using CV, we evalute our model’s performance on the hold-out test set (100 reviews and ratings from the sample). The function rarefit.predict is the one-click function for making new predictions, based on model fit object ourfit and CV object ourfit.cv (for choosing optimal (λ,α)).
#
# # Prediction on test set
# pred <- rarefit.predict(ourfit, ourfit.cv, data.dtm[-ts, ])
# pred.error <- mean((pred - data.rating[-ts])^2)
# pred.error
#
# # Visualize Aggregated Groups in a Colored Tree
# # In addition to the prediction performance of the model, we may also be interested in seeing how the model aggregates rare adjectives into groups. We provide two functions to allow user view recovered groups at given (β̂ ,γ̂ )
# # : group.recover and group.plot.
# # The function group.recover determines aggregated groups of leaf indices (i.e., β
# # elements) based on sparsity in γ.
# # In particular, we iterate over all non-zero γ elements in postorder; at every non-zero γ, we make its descendant leaves a set after excluding all leaves that have appeared in previous groups. For example, suppose v1 and v2 are the only two children nodes of some node u with γv1≠0, γv2=0, γu≠0 and γw=0 for all w∈descendant(v1)∪descendant(v2). At node v1, we recover (v1) (the leaf set of subtree rooted at v1) as a group. Then we move to u and recover (u)∖(v1) as a group. The postorder traversal across nodes with non-zero γ
# # ensures us recover the correct groups.
# # Since rarefit returns NA for γ̂
# # when solving at α=0, group.recover (and the following group.plot) will only work for α≠0 cases.
# # In the following, we find the groups aggregated at (β̂ 0(λ̂ CV,α̂ CV),β̂ (λ̂ CV,α̂ CV))
#
#
# set.seed(100)
# ts <- sample(1:length(data.rating), 400) # Train set indices
# # Fit the model on train set
# ourfit <- rarefit(y = data.rating[ts], X = data.dtm[ts, ], hc = data.hc, lam.min.ratio = 1e-6,
# nlam = 20, nalpha = 10, rho = 0.01, eps1 = 1e-5, eps2 = 1e-5, maxite = 1e4)
# # Cross validation
# ourfit.cv <- rarefit.cv(ourfit, y = data.rating[ts], X = data.dtm[ts, ],
# rho = 0.01, eps1 = 1e-5, eps2 = 1e-5, maxite = 1e4)
# # Visualize the groups at optimal beta and gamma
# ibest.lambda <- ourfit.cv$ibest[1]
# ibest.alpha <- ourfit.cv$ibest[2]
# beta.opt <- ourfit$beta[[ibest.alpha]][, ibest.lambda]
# gamma.opt <- ourfit$gamma[[ibest.alpha]][, ibest.lambda] # works if ibest.alpha > 1
# # Visualize the groups at optimal beta and gamma
# group.plot(beta.opt, gamma.opt, ourfit$A, data.hc)



{
  ## av herald dat scrape and analysis
  dow=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  dow_plus=paste0("^",dow)
  other_skip_terms=c("avherald","aviation herald")
  all_skip_terms=c(dow_plus,other_skip_terms)
  all_skip_terms_squished=paste0(all_skip_terms,collapse="|")
  archive_urls_path="~/Desktop/data/av_urls.txt"
  # archive_urls_path="~/Downloads/google8e8924d74df89636.html"
  av_url="https://avherald.com/"
  splitwords=list(loc=" LOCS_PLIT ",date=" DATE_SPLIT ",comma="COMMA_SPLIT ")
  end_of_first_period="2022-07-15"
  start_of_second_period="2022-08-15"
  end_of_second_period="2023-08-01"
}
manuf_list=list(
  "A"="Airbus","AN"="Antonov","ATR"="ATR","AT"="ATR",
  "ATP"="British Aerospace",
  "RJ"="British Aerospace",
  "B"="Boeing","BLCF"="Boeing",
  "BCS"="Airbus",
  "D"="Dornier",
  "DH"="De Havilland",
  "DHC"="De Havilland",
  "E"="Embraer",
  "CR"="Bombardier",
  "CRJ"="Bombardier",
  "CRJX"="Bombardier",
  "DC"="Basler",
  "F"="Fokker",
  "IL"="Ilyushin",
  "J"="Fairchild-Dornier",
  "JS"="British Aerospace",
  "L"="Lockheed",
  "MD"="McDonnell Douglas",
  "SF"="Saab",
  "SH"="Short",
  "SSLH"="Santa",
  "SW"="Swearingen")



fun_repair_carrier_names=function(df){
  df |>
    mutate(carrier=if_else(grepl("Aeromexico",carrier),"Aeromexico",carrier)) |>
    mutate(carrier=if_else(grepl("Airblue",carrier),"AirBlue",carrier)) |>
    mutate(carrier=if_else(grepl("American",carrier),"American",carrier)) |>
    mutate(carrier=if_else(grepl("^Argentina ",carrier),"Argentinas",carrier)) |>
    mutate(carrier=if_else(grepl("ASL",carrier),"ASL",carrier)) |>
    mutate(carrier=if_else(grepl("Atlanta",carrier),"Atlanta",carrier)) |>
    mutate(carrier=if_else(grepl("ANA",carrier),"ANA",carrier)) |>
    mutate(carrier=if_else(grepl("BAW",carrier),"British Airways",carrier)) |>
    mutate(carrier=if_else(grepl("British Airways",carrier),"British Airways",carrier)) |>
    mutate(carrier=if_else(grepl("Canada ",carrier),"Canada",carrier)) |>
    mutate(carrier=if_else(grepl("^Canada$",carrier),"Air Canada",carrier)) |>
    mutate(carrier=if_else(grepl("Cathay",carrier),"Cathay",carrier)) |>
    mutate(carrier=if_else(grepl("Eastern Airways",carrier),"Eastern",carrier)) |>
    mutate(carrier=if_else(grepl("Easyjet",carrier),"Easyjet",carrier)) |>
    mutate(carrier=if_else(grepl("Eurowings",carrier),"Eurowings",carrier)) |>
    mutate(carrier=if_else(grepl("^France$",carrier),"Air France",carrier)) |>
    mutate(carrier=if_else(grepl("Iceland |^ICE$",carrier),"Iceland",carrier)) |>
    mutate(carrier=if_else(grepl("Jetblue",carrier),"Jetblue",carrier)) |>
    mutate(carrier=if_else(grepl("Jetstar",carrier),"Jetstar",carrier)) |>
    mutate(carrier=if_else(grepl("KLM",carrier),"KLM",carrier)) |>
    mutate(carrier=if_else(grepl("LATAM",carrier),"LATAM",carrier)) |>
    mutate(carrier=if_else(grepl("Lingus",carrier),"Aer Lingus",carrier)) |>
    mutate(carrier=if_else(grepl("Lufthansa",carrier),"Lufthansa",carrier)) |>
    mutate(carrier=if_else(grepl("Norwegian",carrier),"Norwegian",carrier)) |>
    mutate(carrier=if_else(grepl("Ryanair",carrier),"Ryanair",carrier)) |>
    mutate(carrier=if_else(grepl("Smartwings",carrier),"Smartwings",carrier)) |>
    mutate(carrier=if_else(grepl("Transavia",carrier),"Transavia",carrier)) |>
    mutate(carrier=if_else(grepl("TUI",carrier),"TUI",carrier)) |>
    mutate(carrier=if_else(grepl("Transavia",carrier),"Transavia",carrier)) |>
    mutate(carrier=if_else(grepl("Virgin",carrier),"Virgin",carrier)) |>
    mutate(carrier=if_else(grepl("Westjet",carrier),"Westjet",carrier)) |>
    ungroup()
}



mymonths=paste0(" ",month.abb," ")
mydays=paste0(" ",gsub("^1th","1st",
                       gsub("^2th","2nd",
                            gsub("^3th","3rd",
                                 gsub("21th","21st",
                                      gsub("22th","22nd",
                                           gsub("23th","23rd",
                                                gsub("31th","31st",
                                                     paste0(as.character(1:31),"th"))))))))," ")



## world cities
data(world.cities)
city_raw<-as_tibble(world.cities)
city_dat=city_raw |>
  select(location2=name,country=`country.etc`,lat,long,pop) |>
  group_by(location2) |>
  filter(pop==max(pop,na.rm=T)) |>
  filter(row_number()==1) |>
  ungroup() |>
  mutate(location2=gsub("Addis Abeba","Addis Ababa",location2))


## load urls list. until dynamic soln can be found
archive_urls=unique(scan(archive_urls_path,what='char'))


library(RCurl)
url <- 'https://avherald.com/'

list.files(pattern=url)
filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)


# Deal with newlines as \n or \r\n. (BDR)
# Or alternatively, instruct libcurl to change \n’s to \r\n’s for us with crlf = TRUE
# filenames = getURL(url, ftp.use.epsv = FALSE, ftplistonly = TRUE, crlf = TRUE)
filenames = paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")




# load_urls=c(av_url,archive_urls)
load_urls=c(archive_urls)

## suffix for archive pages
# arch_suff="h?list=&opt=0&offset="
# arch_suff="h?list=&opt=0&offset="
# av_url_arch_root=paste0(av_url,arch_suff)

## define split level for url html parsing
css_selector="tr"

## pull html from url and make as table.
rdat=bind_rows(lapply(load_urls,function(z){
  print(paste(Sys.time(),z))
  z|>
    read_html()|>
    # read_html(options=)|>
    html_element(css=css_selector)|> ## function has incorrect warning
    html_table()|>
    as_tibble()|>
    select(`X1`)|>
    filter(!grepl(all_skip_terms_squished,X1,ignore.case=T))|>
    filter(X1!="")|>
    mutate(source_url=z)
}))|>mutate(event_id=row_number())



## extract event dates
rdat2=bind_rows(lapply(mymonths,function(z){
  # print(z)
  df=rdat|>
    mutate(yr=str_extract(pattern="[0-9]{4}",X1))|>
    mutate(mo=if_else(grepl(z,X1,ignore.case=FALSE),z,as.character(NA)))|>
    filter(!is.na(mo))
  
  out=bind_rows(lapply(mydays,function(i){
    df|>
      mutate(dm=if_else(grepl(i,X1,ignore.case=FALSE),i,as.character(NA)))|>
      filter(!is.na(dm))|>
      mutate(dm=trimws(dm))|>
      mutate(event_date=lubridate::as_date(paste0(yr,"-",mo,"-",gsub("[^0-9.-]","",dm))))|>
      select(event_date,yr,mo,dm,everything())
  }))
  out
}))


rdat2sum=rdat2 |>
  group_by(event_date) |>
  summarise(n=n(),.groups='drop')

# ggplot(rdat2sum,aes(event_date,n),colour='blue')+
# geom_point(size=1,colour='blue',alpha=.5)



make_col=function(df,mystr){
  df |>
    filter(grepl(mystr,descr)) |>
    mutate(ind=mystr)
}


## make manufacturer lookup table
manuf_lookup=bind_rows(lapply(names(manuf_list),function(z){
  tibble(lab=z)|>
    mutate(nm=manuf_list[[z]])|>
    ungroup()
}))


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



## grouped lists of common terms
people=list(pilots=c("captain","copilot"," pilot","^pilot","first officer"),
            `cabin crew`=c("flight attendant","crew"),
            `airport staff`="tower","ground worker",
            passengers=c("passenger"),
            people=c("people"))

times=list(approach=c("approach","descent","landed","landing","touch down","touchdown","touched down"),
           on_ground=c("apron","at stand","runway"),
           departure=c("departure","takeoff","take off","roll out","line up"),
           enroute=c("in flight","midair"))

events=list(sensor=c("airspeed","altitude","descended","go around","indication"),
            computer=c("comm","disconnected","disagree","uncommanded"),
            dropped="dropped",
            odour=c("overspeed","smell","odour"),
            weather=c("hail","lightning","weather"),
            flight=c("speed","stall","turbulence","MSAW","loss of control","loss of thrust","impacted terrain","below safe height","ditched"),
            # animals=c("bird"),
            birds=c("bird"),
            icing=c(" icing","^icing"),
            pressue=c("pressure","pressurize"),
            vibrations=c("vibrations"),
            explosion=c("explosion"),
            fire=c("burn","fire"),
            fumes=c("fumes","smoke"),
            warning="warning",
            other_events=c("bang","bomb","hoax","broke","contact","collision",
                           "collided","control","conflict","collapse","cigarette",
                           "excursion","emergency","evacuation",
                           "damage",
                           "dropped","fault",
                           "deviation",
                           "failure","failure","failed","fire","fumes",
                           "issues","injur",
                           "incapacitated","incursion","icing","issue",
                           "killed",
                           "leak",
                           "lost ","loose","landed short"," shot ",
                           "malfunction",
                           "overheating",
                           "problem",
                           "rejected",
                           "separation","shot down","smok","slip","steam",
                           "tail strike",
                           "TCAS",
                           "pressur","unwell","vibrations"))


ac_parts=list(computer=c("ACARS","ADI","ADR","autopilot","computer","electric","GPWS"),
              airframe=c("radome","airframe","fuselage"," tail ","^tail "," wing "," wings ","^wing ","^wings "),
              controls="controls",
              internal=c("toilet","flight deck","coffee","oven","on board","lavator","food cart","cabin","cargo","cockpit","elevator"),
              door_window=c("door","windshield","windscreen","window"),
              flaps=c("flaps","rudder","slat"),
              gear=c("brakes","gear"," tyre"," tyres","^tyre","^tyres"),
              engine=c("engine"),
              fuel=c("fuel"),
              hydraulic=c("hydraulic","hydraulics"),
              other_ac_part=c(
                "instruments",
                "FMS",
                "mechanical",
                "oil ",
                "navigation",
                "QNH",
                "radar","sensor","technical"," trim",
                "turbine","water"))


descr_levels=list(
  people=unlist(people),
  events=unlist(events),
  ac_parts=unlist(ac_parts),
  times=unlist(times))



## assign labels if string found, and maintain group labels
snake=bind_rows(lapply(names(descr_levels),function(z)tryCatch({
  z2=descr_levels[[z]]
  bind_rows(lapply(names(z2),function(i)tryCatch({
    i2=z2[[i]]
    dat_temp|>
      select(event_id,descr,full_descr=X1)|>
      filter(grepl(i2,descr))|>
      mutate(ind1=gsub("[0-9]","",trimws(i)),ind2=trimws(i2))
  },error=function(e)NULL)))|>
    mutate(gname=trimws(z))
},error=function(e)NULL)))



snake2=snake |>
  group_by(event_id,descr,full_descr) |>
  reframe(ind=paste0(sort(unique(ind1)),collapse=";")) |>
  ungroup() |>
  group_by(ind) |>
  mutate(nind=n()) |>
  ungroup()


## make overall group label
dolph=snake |>
  group_by(event_id,descr,full_descr) |>
  summarise(ind_comb=paste0(sort(unique(ind1)),collapse=" ; "),.groups='drop') |>
  ungroup()


dat_fin_temp=dat_temp |>
  left_join(dolph) |>
  mutate(two_loc=if_else(grepl(" and ",loc),TRUE,FALSE)) |>
  mutate(two_eq=if_else(grepl(" and ",eq),TRUE,FALSE)) |>
  mutate(location2=gsub(" and .*","",loc)) |>
  select(event_id=enum,
         event_date=event_date,
         carrier=car,
         equipment=eq,
         extra_dates,
         location=loc,
         location2,
         two_loc,
         two_eq,
         description=descr,
         full_descr,
         ind_comb,
         numb_aircraft=nac,
         manufacturer=nm,
         eq_type=etype) |>
  left_join(city_dat) |>
  mutate(country=if_else(is.na(country),location,country)) |>
  ungroup() |>
  fun_repair_carrier_names()|>
  distinct()


cdf=data.frame(country=sort(unique(dat_fin_temp$country)))
cdf$continent=countrycode(
  sourcevar= cdf[,"country"],
  origin="country.name",
  destination="continent")

## cleaned data ####
dat_fin=dat_fin_temp |>
  filter(event_date>start_of_second_period) |>
  left_join(cdf) |>
  distinct()

dat_fin |>
  group_by(description) |>
  summarise(n=n(),.groups='drop') |>
  view()

ggplot(dat_fin |> group_by(event_date) |> summarise(n=n(),.groups='drop'),aes(event_date,n),colour='blue')+
  geom_line()



dat_filt=dat_fin |>
  filter(country=="Germany") |>
  # filter(carrier=="Air Canada") |>
  # filter(carrier=="Air Canada") |>
  filter(
    !grepl("being shot at|security threat|bomb",description,ignore.case=T)) |>
  # !grepl("bird",description,ignore.case=T),
  # !grepl("bird strike",description,ignore.case=T),
  # !grepl("hail strike",description,ignore.case=T),
  # !grepl("bomb",description,ignore.case=T))
  # !grepl("lightning",description,ignore.case=T)) |>
  ungroup()


dat_rev=dat_fin |>
  filter(
    grepl("bird strike|lightning|loss of separation|turbulence|hail",description,ignore.case=T)) |>
  mutate(description=if_else( grepl("bird",description),"bird",description)) |>
  mutate(description=if_else( grepl("lightning",description),"lightning",description)) |>
  mutate(description=if_else( grepl("hail",description),"hail",description)) |>
  mutate(description=if_else( grepl("loss of separation",description),"loss of separation",description)) |>
  # mutate(description=if_else( grepl("collision",description),"collision or near collision",description)) |>
  mutate(description=if_else( grepl("turbulence",description),"turbulence",description)) |>
  mutate(mo=month(event_date)) |>
  mutate(moch=as.character.Date(event_date,"%b")) |>
  group_by(description,location2,lat,long,moch,mo) |>
  mutate(descrn=n(),.groups='drop') |>
  ungroup() |>
  arrange(desc(descrn)) |>
  mutate(moch=fct_reorder(moch,mo))
# dat_rev
# unique(dat_rev$description)


# world_coordinates <- map_data("world")
lims=list(long=c(-10,40),lat=c(30,70))

wcdat=world_coordinates |>
  filter(region!="Antarctica")|>
  # filter(
  #   long>=lims$long[1],
  #   long<=lims$long[2],
  #        lat>=lims$lat[1],
  #        lat<=lims$lat[2]) |>
  ungroup()

pdat_rev=dat_rev |>
  filter(is.finite(lat)) |>
  # filter(
  #   long>=lims$long[1],
  #   long<=lims$long[2] ,
  #        lat>=lims$lat[1],
  #        lat<=lims$lat[2]) |>
  ungroup()

cowplot::plot_grid(
  plotlist=lapply(split (pdat_rev, pdat_rev$description), function(z){
    ggplot(z,aes(long,lat))+
      geom_map(
        data = wcdat, map = wcdat,
        fill='grey90',colour='grey50',linewidth=.1,
        aes(long, lat, map_id = region)
      )+
      # geom_density2d_filled(alpha=.4)+
      # geom_density2d_filled(alpha=.4,contour_var='density')+
      # geom_density2d_filled(alpha=.4,contour_var='count')+
      geom_point(aes(size=descrn),alpha=.5,show.legend=F)+
      scale_size(range=c(2,1+max(z$descrn)))+
      coord_cartesian(expand=0)+
      # scale_fill_viridis_d(option="A")+
      facet_wrap(description~.,ncol=1)+
      theme_minimal()+
      labs(x="",y="")+
      theme(legend.position='',
            panel.grid=element_blank(),
            panel.border=element_rect(linewidth=.25,colour='grey',fill=NA))
  }))






fun_make_ests=function(df=dat_filt,search_term="fumes"){
  print(search_term)
  
  df2temp=df |>
    select(event_id,carrier,location,eq_type,country,description) |>
    mutate(descr=if_else(grepl(search_term,description),"yes","no")) |>
    gather(grp,glabel,-c(event_id,description,descr)) |>
    filter( ! ( grp %in% c("country","location") & glabel=="enroute")) |>
    group_by(grp,glabel,descr) |>
    summarise(n=n(),.groups='drop') |>
    spread(descr,n)
  
  df2=df2temp%>%
    { if (!"yes" %in% names(df2temp)) mutate(.,yes=as.numeric(0)) else .}%>%
    replace_na(list(yes=0,no=0)) |>
    mutate(sumcar=yes+no) |>
    mutate(propcar=yes/sumcar) |>
    mutate(margin=qnorm(0.975)*sqrt(propcar*(1-propcar)/sumcar)) |>
    mutate(lwr=propcar-margin,upr=propcar+margin) |>
    mutate(lwr=if_else(lwr<0,0,lwr),upr=if_else(upr>1,1,upr)) |>
    # filter(sumcar>10) |>
    # filter(propcar>0) |>
    # filter(propcar<1) |>
    ungroup()
  
  
  df2_sum=df2 |>
    group_by(grp) |>
    summarise(no=sum(no),yes=sum(yes),sumcar=sum(sumcar),.groups='drop') |>
    mutate(propcar=yes/sumcar) |>
    mutate(margin=qnorm(0.975)*sqrt(propcar*(1-propcar)/sumcar)) |>
    mutate(lwr=propcar-margin,upr=propcar+margin) |>
    mutate(lwr=if_else(lwr<0,0,lwr),upr=if_else(upr>1,1,upr))
  
  df2_sum_long=df2_sum |>
    select(grp,propcar,lwr,upr,sumcar)|>
    gather(lgrp,val,-grp) |>
    mutate(searchterm=search_term)
  
  outab=df2|>
    left_join(df2_sum |> select(grp,all_prop=propcar,all_lwr=lwr,all_upr=upr,all_sumcar=sumcar),by = join_by(grp)) |>
    mutate(stat1=if_else(propcar>all_prop,'hi','lo')) |>
    mutate( stat2 = if_else( propcar > all_upr|propcar < all_lwr,'marg','ns' )) |>
    mutate(stat2=if_else( lwr > all_upr|upr < all_lwr,'sig',stat2)) |>
    mutate(stat=paste(stat1,stat2)) |>
    mutate(stat=if_else(grepl("ns",stat),"ns",stat)) |>
    mutate(searchterm=search_term)
  outab
  # list(main=outab,margins=df2_sum_long)
}

key_terms=c(
  "bird strike",
  "lightning strike",
  "altitude|altimeter",
  "brake",
  "bleed",
  "cabin pressure|cabin did not pressurize|pressurization",
  "captain",
  "cabin",
  "cargo",
  "cockpit|flight deck",
  "cracked windshield",
  "dropped",
  "descended below",
  "door",
  "engine|propeller",
  "electric",
  "failure",
  "fire",
  "first officer",
  "flaps|slat",
  "fumes|odour|smoking|smoke|smell",
  "fuel",
  "gear|tyre|wheel|breaks",
  "hydraulic",
  "incapacitated",
  "on landing",
  "rejected takeoff",
  "runway",
  "tail strike|tail scrape",
  "turbulence",
  "passenger",
  "autopilot|uncommanded|computer|unreliable|APU|flight control",
  "loss of separation|conflict",
  "takeoff|departure",
  "flight attendant|cabin crew",
  "landing|approach|landed|cleared to land|touchdown|touch down|short final",
  "enroute|in flight| cruise flight")

# ktst=paste0(key_terms,collapse="|")
# dat_filt |>
#   # filter(!grepl(ktst,description)) |>
#   group_by(description) |>
#   summarise(n=n(),.groups='drop') |>
#   view()

res=bind_rows(
  lapply( key_terms, function(z) {
    fun_make_ests(df=dat_filt,search_term=z)
  }))

res_chi=res |>
  mutate(all_yes=round(all_prop*all_sumcar)) |>
  mutate(other_sum=all_sumcar-sumcar) |>
  mutate(other_yes=round((all_prop*all_sumcar))-yes)|>
  mutate(other_no=other_sum-other_yes) |>
  filter(yes>3) |>
  filter(is.finite(yes),is.finite(no),is.finite(other_no),is.finite(other_yes)) |>#view()
  rowwise() |>
  mutate(pval=fisher.test(matrix(nrow=2,c(yes,other_yes,no,other_no)))$p.value) |>
  mutate(estimate=fisher.test(matrix(nrow=2,c(yes,other_yes,no,other_no)))$estimate) |>
  ungroup() |>
  select(searchterm,grp,glabel,prop=propcar,prop_all=all_prop,pval,estimate,yes,tot=sumcar,all_yes,tot_all=all_sumcar) |>
  mutate(reldiff=(prop-prop_all)/prop) |>
  mutate(plab=paste0(gsub("[|].*","",searchterm),"\n",glabel)) |>
  # mutate(plab=if_else(pval<.05,plab,as.character(NA))) |>
  ungroup()
# res_chi|>
#   filter(pval<.05) |>
#   view()



ggplot(res_chi|>
         # filter(is.finite(estimate)) |>
         # filter(estimate<400) |>
         # filter(-log10(pval)<10) |>
         # filter(pval<.05) |>
         filter(is.finite(estimate)) |>
         ungroup(),aes(estimate,-log10(pval),colour=grp))+
  geom_point()+
  geom_text(aes(label=plab),size=3)+
  # scale_x_log10()+scale_y_log10()+
  facet_wrap(grp~.,scales="free")+
  theme(legend.position="")

# pig=fisher.test(matrix(nrow=2,c(1,2,3,40)))$est
# pig$estimate
# $p.value



# res_pdat=res |>
#   filter(yes>0) |>
#   filter(sumcar>30) |>
#   filter(!grepl("ns",stat)) |>
#   ungroup()

# ggplot(res_pdat,aes(propcar,glabel,colour=stat))+
#   # geom_vline(data=res$margins,aes(xintercept=val,linetype=lgrp),colour='grey')+
#   # geom_errorbarh(aes(xmin=lwr,xmax=upr),height=.1)+
#   geom_point(aes(size=sumcar))+
#   # scale_linetype_manual(values=c(2,1,2))+
#   labs(x="",y="",title=myword)+
#   # facet_wrap(grp~searchterm,scales='free_y')+
#   facet_wrap(grp~searchterm,scales='free_y')+
#   theme(legend.position='')
#
#


tdat_prep=res |>
  mutate(diff=(propcar-all_prop)/propcar) |>
  select(grp,glabel,sumcar,propcar,all_prop,searchterm,diff,stat) |>
  ungroup() |>
  mutate(diff=if_else(!is.finite(diff),as.numeric(0),diff)) |>
  filter(is.finite(diff)) |>
  # filter(propcar>0) |>
  # filter(sumcar>5) |>
  arrange(diff)

tdat_prep |>
  filter(propcar>0) |>
  # filter(sumcar>5) |>
  # filter(grepl("sig",stat)) |>
  view()

tdat=tdat_prep |>
  filter(propcar>0) |>
  filter(sumcar>5) |>
  filter(grepl("sig",stat)) |>
  # filter(grepl("landing",searchterm)) |>
  select(grp,glabel,stat,searchterm)
tdat

ggplot(tdat,aes(grp,glabel,fill=stat))+
  geom_tile()+
  # scale_fill_continuous()+
  facet_wrap(searchterm~.,scales='free')

tdat_prep2=crossing(searchterm=unique(tdat_prep$searchterm),
                    tdat_prep |>
                      select(grp,glabel) |> distinct()) |>
  left_join(tdat_prep)


modin=tdat_prep2 |>
  select(searchterm,grp,glabel,propcar) |>
  mutate_if(is.character,as.factor)
#

# mod=lm(propcar~1+searchterm+grp+glabel,data=modin)

# modin$pred<-predict(mod,modin)
# modin$pred
mods=lapply(split (modin,modin$searchterm),function(z){
  print(unique(z$searchterm))
  lm(propcar~1+glabel,data=z)
})

modout=bind_rows(lapply(split (modin,modin$searchterm),function(z){
  zst=unique(z$searchterm)
  print(zst)
  
  # crossing(grp=unique(z$grp,
  
  z |>
    mutate(pred=predict(mods[[zst]]))
}))

ggplot(modout,aes(propcar,pred,colour=grp))+
  geom_point()+
  facet_wrap(searchterm~.)


# dev.off()
# gc()
# sumbird=sum(bird),sumnot=sum(nothing),
# ,sumall=sumbird+sumnot

pig |>
  group_by(event_id) |>
  summarise_all(funs(chisq.test(pig$carrier,pig$descr)))


nbase=nrow(dat_fin)

ncar=nrow(dat_fin |>
            filter(carrier==mycarrier))

nword=nrow(dat_fin |>
             filter(grepl(myword,description)))

nboth=nrow(dat_fin|>
             filter(carrier==mycarrier) |>
             filter(grepl(myword,description)))

nneither=nrow(dat_fin|>
                filter(carrier!=mycarrier) |>
                filter(!grepl(myword,description)))
nneither



mutate(ind1=if_else(carrier==mycarrier,as.numeric(1),as.numeric(0)),
       ind2=if_else(grepl(myword,description),as.numeric(1),as.numeric(0))) |>
  group_by(ind1,ind2) |>
  summarise(val1=sum(ind1),val2=sum(ind2))
# group_by(ind1,ind2) |>
# summarise(val1=sum(ind1),val2=sum(ind2))




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
  filter(ind_comb!="other") |>
  mutate(carrier=equipment) |>
  group_by(carrier,ind_comb) |>
  summarise(n=n(),.groups='drop')|>
  group_by(carrier) |>
  mutate(carn=sum(n)) |>
  ungroup() |>
  group_by(ind_comb) |>
  mutate(icn=sum(n)) |>
  ungroup() |>
  mutate(prop=n/carn) |>
  # mutate(idx=icn*n) |>
  # group_by(carrier) |>
  # mutate(sumidx=sum(idx)) |>
  ungroup()|> filter(carn>10) |>
  group_by(carrier) |>
  mutate(nr=n()) |>
  ungroup() |>
  mutate(plab=paste(ind_comb,round(prop*100))) |>
  mutate(engval=if_else(ind_comb=="engine",prop,as.numeric(NA))) |>
  mutate(birval=if_else(ind_comb=="bird",prop,as.numeric(NA))) |>
  mutate(geaval=if_else(ind_comb=="gear",prop,as.numeric(NA))) |>
  group_by(carrier) |>
  fill(c('engval','birval','geaval'),.direction='updown') |>
  ungroup() |>
  replace_na(list(engval=0,birval=0,geaval=0)) |>
  mutate(cark=(10*engval)+(1*birval)+(.9*geaval)) |>
  mutate(ind_comb=fct_reorder(ind_comb,icn,.desc=T),
         carrier=fct_reorder(carrier,cark))
# psum

col_labs=unique(psum$ind_comb)
pcols0<-rcartocolor::carto_pal(name="Vivid")
pcols=rep(pcols0,ceiling(length(col_labs)/length(pcols0)))
names(pcols)<-col_labs

# ggplot(psum,aes( prop,reorder(carrier,carn),fill=reorder(ind_comb,icn)),colour='blue')+
ggplot(psum ,aes( prop,carrier,nr,fill=ind_comb),colour='blue')+
  geom_col(position=position_stack(),colour='black',linewidth=.2)+
  geom_text(position=position_stack(vjust=.5),aes(group=ind_comb,label=plab),size=3)+
  scale_fill_manual(values=pcols)+
  theme(legend.position="")


pwide=psum |>
  filter(carn>10) |>
  select(carrier,ind_comb,prop) |>
  spread(carrier,prop) |>
  gather(carrier,prop,-ind_comb) |>
  replace_na(list(prop=0)) |>
  spread(ind_comb,prop)

pwide

pca_input=as.data.frame(pwide %>% select(-carrier))
rownames(pca_input)<-pwide$carrier
head(pca_input)
pca_output <- prcomp(pca_input,center=T,scale.=T)
head(pca_output$x)

pca_plot_data=as_tibble(pca_output$x) %>%
  mutate(country=rownames(pca_input)) %>%
  select(country,everything())
pca_plot_data

ggplot(pca_plot_data,aes(PC1,PC2),colour='blue')+
  geom_point(alpha=.5,size=.5,colour="dodgerblue2")+
  geom_text(aes(label=country),size=3)






###
date_range=range(dat_fin$event_date)
hours_range=as.numeric(24*(max(date_range)-min(date_range)))

dat_fin_sum=dat_fin|>
  group_by(carrier,eq_type,country)|>
  summarise(n=n(),.groups='drop')|>
  # mutate(events_per_hour=n/hours_range)|>
  group_by(carrier)|>
  mutate(carsum=sum(n))|>
  ungroup()|>
  group_by(eq_type)|>
  mutate(eqsum=sum(n))|>
  ungroup()|>
  group_by(country)|>
  mutate(countrysum=sum(n))|>
  ungroup()|>
  mutate(carrier=if_else(carsum<=2,'Other',carrier),
         eq_type=if_else(eqsum<=2,'Other',eq_type),
         country=if_else(countrysum<=2,'Other',country))|>
  group_by(carrier,eq_type,country)|>
  summarise(n=n(),.groups='drop')|>
  group_by(carrier)|>
  mutate(carsum=sum(n))|>
  ungroup()|>
  group_by(eq_type)|>
  mutate(eqsum=sum(n))|>
  ungroup()|>
  group_by(country)|>
  mutate(countrysum=sum(n))|>
  ungroup()


dat_fin_extra=dat_fin_sum |>
  gather(grp,events_per_hour,-c(carrier,eq_type,country)) |>
  mutate(events_per_hour=events_per_hour/hours_range)
#
#   mutate(evint=(1/events_per_hour)/24)

#
# ggplot(dat_fin_sum,aes(events_per_hour,country),colour='blue')+
#   geom_col(aes(fill=eq_type),position=position_fill(),colour='blue')+
#   facet_wrap(carrier~.,scales='free')+
#   theme(legend.position='')



# carrier_groups=stack(list(
#   Virgin=c("Delta","New York","Indigo","A319"),
#   pg2=c("CRJ9","Canada","B772"),
#   pg3=c("Munich","American"),
#   pg4=c("Southwest","B737"))) |>
#   dplyr::rename(label=values,grp=ind)




# pig=dat_fin |>
#   select(event_id,event_date,carrier,equipment,location2,two_loc,ind_comb,numb_aircraft,
#          manufacturer,eq_type,country,lat,long,pop,continent)




## count time interval between events in each group?
## all events


sort(unique(dat_fin$carrier))

finx=dat_fin|>
  select(event_id,event_date,location2,equipment,carrier,ind_comb,full_descr)|>
  gather(metric,label,-c(event_id,event_date,full_descr))|>
  mutate(mlab=paste(metric,label))


## summarise ind comb by carrier, eq or loc

## get proportions and plot for any two group combinations
fun_group_sums=function(df,v1,v2){
  ggplot(df |>
           group_by(val1={{v1}},val2={{v2}})|>
           summarise(n=n(),.groups='drop')|>
           group_by(val1)|>
           mutate(sumn=sum(n))|>
           ungroup()|>
           mutate(prop=n/sumn)|>
           group_by(val2)|>
           mutate(icsum=sum(n))|>
           ungroup()|>
           filter(sumn>15) |>
           mutate(val1=fct_reorder(val1,sumn,.desc=F),
                  val2=fct_reorder(val2,icsum,.desc=T)),aes( prop,val1),colour='blue')+
    geom_col(position=position_fill(),
             colour='grey',
             aes(fill=val2,group=val2),colour='blue')+
    geom_text(position=position_fill(vjust=.5),
              aes(label=val2,group=val2),size=3.5)+
    labs(x="",y="")+
    theme(legend.position="")
}


plot_group_sums=list(
  p1=fun_group_sums(dat_fin,manufacturer,equipment),
  p2=fun_group_sums(dat_fin,carrier,equipment),
  p3=fun_group_sums(dat_fin,carrier,location2),
  p4=fun_group_sums(dat_fin,location2,carrier))
cowplot::plot_grid(plotlist=plot_group_sums)




## count events per dat and get cucmulative
datsum1=bind_rows(lapply(split(finx,finx$mlab),function(z){
  met=unique(z$metric)
  lab=unique(z$label)
  mla=unique(z$mlab)
  print(mla)
  left_join(tibble(event_date=seq.Date(
    from=min(z$event_date),to=max(z$event_date),by='1 day')),
    z|>
      group_by(event_date)|>
      summarise(n=n(),.groups='drop'),by=join_by(event_date))|>
    mutate(n=if_else(!is.finite(n),0,n))|>
    mutate(data_period=if_else(event_date>start_of_second_period,"Second","First"))|>
    mutate(data_period=if_else(data_period=="First"&event_date>end_of_first_period,"Intermediate",data_period))|>
    mutate(data_period=if_else(event_date>end_of_second_period,"Third",data_period))|>
    # mutate(country=cntry,continent=cont)|>
    mutate(metric=met,label=lab,mlab=mla)|>
    arrange(event_date)|>
    mutate(cumn=cumsum(n))|>
    ungroup()
}))



## hrs til event
pig=datsum1|>
  #filter(data_period=="Second")|>
  group_by(mlab)|>
  mutate(mlabtot=sum(n))|>
  ungroup()|>
  mutate(mend=ceiling_date(event_date,unit='months')-1,
         wend=ceiling_date(event_date,unit='weeks')-1)|>
  filter(mlabtot>15)|>
  mutate(hpd=24)|>
  group_by(mlab)|>
  arrange(event_date)|>
  mutate(dn=row_number())|>
  mutate(cumn=cumsum(n),cumh=cumsum(hpd))|>
  ungroup()|>
  mutate(ceph=cumn/cumh)|>
  mutate(hte=1/ceph)|>
  group_by(mlab)|>
  arrange(event_date)|>
  mutate(deacc_hte=1/(ceph-lag(ceph)))|>
  ungroup()|>
  group_by(mlab,mend)|>
  mutate(epm=sum(n))|>
  ungroup()|>
  group_by(mlab,wend)|>
  mutate(epw=sum(n))|>
  ungroup()


pigd=pig|>
  # filter(cumn>0) |>
  select(mlab,metric,label,event_date,n,data_period)|>
  filter(data_period=="Second") |>
  distinct()



cdate=base::as.Date(sort(unique(pigd$event_date)))
pigm=bind_rows(lapply(cdate,function(i){
  print(i)
  out=NULL
  out=pigd|>
    filter(event_date<=i+14,
           event_date>=base::as.Date(i-14))|>
    group_by(mlab,metric,label)|>
    summarise(tot=sum(n),meann=mean(n),nr=n(),.groups='drop')|>
    mutate(rdate=i)|>
    #filter(nr>15)|>
    mutate(eph=meann/24)|>
    mutate(dpe=1/meann)|>
    ungroup()|>
    mutate(dpe=if_else(is.infinite(dpe),30,dpe))|>
    ungroup()
}))


pigm_pdat=pigm |>
  group_by(mlab) |>
  arrange(rdate) |>
  mutate(rmt=rollmean(tot,k=20,fill=NA)) |>
  ungroup() |>
  ungroup()



# ggplot(pigm_pdat |> filter(metric=='carrier'),aes(rdate,tot,colour=label),colour='blue')+
#   geom_line(aes(y=rmt,colour=label),linewidth=.5)+
#   # coord_cartesian(ylim=c(0,NA),colour='blue')+
#   facet_wrap(label~.,scales='free_y')+
#   theme_gray()+
#   theme(legend.position="bottom")



library(ggcorrplot)
cutoff_date="2023-01-01"
pigx=pigm_pdat |>
  # filter(rdate<cutoff_date,rdate>base::as.Date(cutoff_date)-35) |>
  select(rdate,label,rmt) |>
  filter(is.finite(rmt))|>
  distinct() |>
  group_by(rdate,label) |>
  summarise(rmt=mean(rmt,na.rm=T),.groups='drop') |>
  filter(rmt>0) |>
  spread(label,rmt) |>
  mutate_if(is.numeric,replace_na, replace=0)


mtcars_dat<-data.frame(pigx[,-1])
corr<-cor(mtcars_dat)
ggcorrplot(corr,hc.order=TRUE)

cortib=as_tibble(corr)|>
  mutate(grp1=rownames(corr))|>
  select(grp1,everything())|>
  gather(grp2,val,-grp1)|>
  filter(grp1!=grp2) |>
  mutate(grp1=gsub("[.]"," ",grp1),grp2=gsub("[.]"," ",grp2)) |>
  mutate(grp1=gsub("   "," ; ",grp1),
         grp2=gsub("   "," ; ",grp2))


corhi=cortib|>
  # filter(val<(-.65),grp1!=grp2) |>
  filter(grp1!=grp2) |>
  # filter(val>.95) |>
  rowwise() |>
  mutate(grpc=paste0( sort(c(grp1,grp2)),collapse=',')) |>
  ungroup() |>
  group_by(val) |>
  filter(row_number()==1) |>
  ungroup() |>
  slice_max(val,n=24) |>
  ungroup()



corev=pigm_pdat |>
  filter(label %in% c(corhi$grp1,corhi$grp2)) |>
  ungroup()


plot_list=lapply( unique(corhi$grpc), function (z) {
  df1=corhi |> filter(grpc==z)
  df2=corev |> filter(label %in% c(df1$grp1,df1$grp2))
  mets=unique(df2$metric)
  ggplot(df2,aes(rdate,tot,colour=label),colour='blue')+
    geom_line(aes(y=rmt,colour=label),linewidth=.5)+
    rcartocolor::scale_colour_carto_d(name=NULL,palette='Vivid',direction=-1)+
    guides(colour=guide_legend(ncol=1),colour='blue')+
    labs(title=NULL,x=NULL,y=NULL)+
    theme_gray()+
    theme(
      axis.text=element_text(size=6),
      plot.margin=unit(rep(.15,4),'lines'),
      legend.position='top',
      legend.justification='left',
      legend.text=element_text(size=7),
      legend.key.size=unit(.5,'lines'),
      legend.background=element_rect(fill='white'),
      legend.box.margin=margin(rep(.15,4)))
})
cowplot::plot_grid(plotlist=plot_list)

## what are the intermediate values, e.g. b763 and fuel reports?
dat_fin
pig=bind_rows(lapply( split (corhi, corhi$grpc), function(z){
  finx |>
    filter(grepl(z$grp1,full_descr)&grepl(z$grp2,full_descr)) |>
    select(event_id) |>arrange(event_id) |>
    distinct() |> pull()
}))


# ggplot(cortib,aes(grp1,grp2,fill=val),colour='blue')+
#   geom_tile(size=0,linewidth=1)+
#   scale_fill_viridis_b()


# ggplot(pigd,aes(dval,val,colour=grp),colour='blue')+
#   geom_line()+
#   facet_wrap(country~.,scales='free_y')
# ggplot(pig,aes(event_date,ceph,colour=country),colour='blue')+
#   # geom_line()+
#   geom_smooth(se=F)+
#   scale_y_log10()+
#   theme_grey()


ggplot(pig,aes(event_date,hte,colour=country),colour='blue')+
  geom_smooth(se=F)+
  facet_wrap(country~.,scales='free_y')+
  theme_grey()+
  theme(
    legend.position="")


## make models
scountries=sort(unique(pig$country))

growth_mods=lapply( scountries , function(z) {
  df1=pig|>filter(country==z)
  mod=lm(cumn~dn,data=df1)
  pred=tryCatch({predict(mod)})
  out=df1 |> mutate(pred=pred)
  return(list(mod=mod,pred=pred,dat=out))
})
names(growth_mods)<-scountries

## apply model to make average
## identify area over or under expected
clean_pdat=bind_rows(lapply( growth_mods , function(z) {
  z$dat
})) |>
  mutate(over_val=cumn-pred) |>
  mutate(is_over=if_else(cumn>pred,1,0)) |>
  ungroup() |>
  filter(countrytot>5) |>
  # clean_pdat |>
  group_by(country) |>
  mutate(rc=cumn/max(cumn)) |>
  mutate(rp=pred/max(pred)) |>
  ## mean abs over val.
  mutate(mao=max(abs(over_val))) |>
  ungroup() |>
  mutate(rov=abs(over_val)/mao) |>
  mutate(rov=if_else(over_val<0,rov*-1,rov)) |>
  group_by(country) |>
  arrange(event_date) |>
  mutate(rm1=rollmean(rov,k=1,fill=NA)) |>
  fill(rm1,.direction='updown') |>
  ungroup()


ggplot(clean_pdat,aes(event_date,rp,colour=country),colour='blue')+
  # geom_line(aes(group=country),colour='blue')+
  geom_point(aes(y=rc),size=.3,alpha=.5)+
  # facet_wrap(country~.,scales='fixed')+
  theme_grey()

ggplot(clean_pdat,aes(event_date,rm1),colour='blue')+
  geom_hline(yintercept=0)+
  geom_point(aes(colour=as.character(is_over)),size=.3,alpha=.5)+
  facet_wrap(country~.,scales='fixed')+
  theme_grey()

ggplot(clean_pdat,aes(event_date,country,
                      fill=as.character(is_over)),colour='blue')+
  geom_tile()+
  theme_grey()


ggplot(clean_pdat,aes(event_date,rov,colour=country),colour='blue')+
  geom_hline(yintercept=0)+
  geom_smooth(se=F,linewidth=.5)+
  geom_smooth(se=F,colour='black',linewidth=.5)+
  # facet_wrap(country~.,scales="free_y")+
  theme_gray()



ggplot(clean_pdat,aes(event_date,country,
                      fill=over_val),colour='blue')+
  geom_tile()+
  scale_fill_viridis_c()


# mutate(nfrac=24/n) |>
# mutate(nfrac=if_else(is.infinite(nfrac),as.numeric(24),nfrac)) |>
# arrange(event_date) |>
# mutate(
#   rm1=rollmean(n,k=14,fill=NA),
#   rm2=rollmean(n,k=28,fill=NA),
#   rm3=rollmean(n,k=42,fill=NA),
#   rm4=rollmean(n,k=56,fill=NA)) |>
# mutate(rm1=if_else(is.finite(rm1),rm1,n),
#        rm2=if_else(is.finite(rm2),rm2,n),
#        rm3=if_else(is.finite(rm3),rm3,n),
#        rm4=if_else(is.finite(rm4),rm4,n)) |>


ggplot()+
  # datsum1 |>
  #        gather(grp,val,-c(n,event_date,data_period)),aes(event_date,val,colour=grp),colour='blue')+
  # geom_line(linewidth=.3)+
  geom_point(data=datsum1 |>
               # filter(grepl("U",country)) |>
               ungroup(),aes(event_date,y=n),size=.2,colour='black')+
  facet_wrap(country~.,scales='free')+
  theme_grey(base_size=6)

dslong=datsum1 |>
  group_by(country) |>
  arrange(event_date) |>
  mutate(cumn=cumsum(n)) |>
  mutate(cumn2=if_else(n==0,as.numeric(NA),cumn)) |>
  mutate(cumn2=na.approx(cumn2,na.rm=F)) |>
  mutate(n2=cumn2-lag(cumn2)) |>
  ungroup() |>view()
mutate(n2=if_else(!is.finite(n2),cumn2,n2)) |>
  # gather(grp,val,-c(event_date,data_period)) |>
  mutate(hrs_per_event=24*(1/n2)) |>
  filter(data_period=="Second") |>
  ungroup()

ggplot(dslong,aes(event_date,hrs_per_event),colour='blue')+
  # geom_smooth(span=1,linewidth=.2,alpha=.3,colour='black')+
  # geom_smooth(span=.5,linewidth=.2,alpha=.3,colour='black')+
  geom_smooth(span=.1,linewidth=.2,alpha=.3,colour='black')+
  geom_point(size=1,alpha=.5,colour='blue')+
  scale_y_log10()+
  facet_wrap(country~.,scales='free')+
  theme_grey(base_size=6)

# geom_point(data=datsum1,aes(y=n),size=.2,colour='black')+
# facet_grid(.~data_period,space='free',scales="free_x")



# ## count events in each group.
# dat_sum=dat_fin|>
#   mutate(event_date=as.character(event_date))|>
#   gather(met,val,-event_id)|>
#   group_by(met,val)|>
#   summarise(nev=as.numeric(n()),.groups='drop')|>
#   group_by(met)|>
#   mutate(metsum=sum(nev,na.rm=T))|>
#   ungroup()|>
#   mutate(prop=nev/metsum)|>
#   group_by(met)|>
#   mutate(relprop=prop/max(prop,na.rm=T))|>
#   ungroup()
# dat_sum
#
unique(dat_sum$met)

mets=c("carrier","eq_type","location",
       "manufacturer")

# mets=c("description")

pdat=dat_sum|>
  filter(met %in% mets)|>
  # view()
  mutate(val=if_else(relprop<.2,paste(met," - Others"),val)) |>
  # mutate(val=if_else(relprop>.05|relprop<.01,"Others",val)) |>
  filter(val!="Others") |>
  group_by(met,val)|>
  summarise(nev=sum(nev,na.rm=T),.groups='drop')|>
  group_by(met)|>
  mutate(metsum=sum(nev,na.rm=T))|>
  ungroup()|>
  mutate(prop=nev/metsum)|>
  group_by(met)|>
  mutate(relprop=prop/max(prop,na.rm=T))|>
  ungroup()|>
  mutate(val=fct_reorder(val,nev))

# ## colour tile. long render.
ggplot(pdat,aes(nev,val),colour='blue')+
  geom_col(position=position_stack(),aes(fill=val),colour="grey10",size=.1)+
  geom_text(position = position_stack(vjust = 0.5),aes(label=val),size=3,colour="grey10")+
  facet_wrap(met~.,scales="free")+
  # theme(legend.position="bottom")+
  theme(legend.position="")+
  theme()

ggplot(pdat,aes(log(relprop)),colour='blue')+
  geom_density(aes(colour=met))





# ## other urls
#
# "http://example.com/archive[1996-1999]/vol[1-4]/part{a,b,c}.html"
# "https://avherald.com/h?list=&opt=0&offset=20221223165359%2B4eff33e8"
# "https://avherald.com/h?list=&opt=0&offset=*"
#
# library(curl)
# av_curl_res_path="~/Desktop/av_curl_res.rda"
# curl_download("https://avherald.com/",av_curl_res_path)
# pig=readRDS(av_curl_res_path)
#
# wget_call="wget --spider --recursive https://avherald.com/"
# wget_call="wget -r https://avherald.com/h?list=&opt=0&offset="
# wget_call="curl --recursive https://avherald.com/h?list=&opt=0&offset="
# # paste0("Wget -r ",av_url_arch_root)
# # wget_call=paste0("Wget -r ",av_url)
#
# setwd("~/Desktop/")
# pig=system(paste0(wget_call))
# pig




# css_all_urls="urlset:nth-child(1)"
#
# sitemap_url="https://www.xml-sitemaps.com/download/avherald.com-a2e33ab38/sitemap.xml?view=1"
# rurls=sitemap_url|>
#   read_html()|>
#   html_element(css=css_all_urls)|>
#   html_children()
# #
# urldat=tibble(x1=unlist(strsplit(paste0(rurls,collapse=" --- "),"---")))|>
#   mutate(x1=gsub("amp[;]","",x1))|>
#   filter(grepl("https[:][/][/]avherald[.]com[/]h[?]list[=][&]opt[=]0[&]offset[=]",x1))|>
#   ungroup()
# urldat
