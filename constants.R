

## constants for string pattern matching when summarising events.
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




## levels of aircraft types.
AIRCRAFT_SEARCH_STRINGS=list(
  
  ## airbus #####
  airbus=list(
    "A300 family"=list("A300"=c("A300","A306","A30B","Airbus A306")),
    "A310 family"=list("A310"=c("A310","A312","A313")),
    "A320 family"=list("A318"=c("A318"),
                       "A319"=c("A319"),
                       "A320"=c("A320","Airbus,A320"),
                       "A321"=c("A321","Airbus A321")),
    "A322 family"=list("A322"=c("A322")),
    "A330 family"=list("A330"=c("A330","Airbus A330"),
                       "A332"=c("A332","Airbus A332"),
                       "A333"=c("A333"),
                       "A339"=c("A339")),
    "A340 family"=list("A340"=c("A340"),
                       "A342"=c("A342"),
                       "A343"=c("A343"),
                       "A345"=c("A345"),
                       "A346"=c("A346","Airbus A346")),
    "A350 family"=list("A359"=c("A359","Airbus A359"),
                       "A35K"=c("A35K","A35k")),
    "A380 family"=list("A380"=c("A380","A388","Airbus A380")),
    "Beluga family"=list("A3ST"=c("A3ST"),
                         "A337"=c("A337","Airbus A337")),
    "A220 family"=list(
      "A20"=c("A20n","A20N"),
      "A21"=c("A21n","A21N","BCS1"),
      "A22"=c("A22n","A22N","BCS2"),
      "A23"=c("A23n","A23N","BCS3"))),
  
  
  
  antonov=list(
    "A12"=list("A12"=c("A12","AN12","AN-12")),
    "A24"=list("A24"=c("A24","AN24","AN-24")),
    "A26"=list("A26"=c("A26","AN26","AN-26")),
    "A28"=list("A28"=c("A28","AN28","AN-28")),
    "A30"=list("A30"=c("A30","AN30","AN-30"),
               "A32"=c("AN32","A32"),
               "A38"=c("AN38","A38")),
    "A74"=list("A74"=c("A74","AN74","AN-74")),
    "A124"=list("A124"=c("A124","AN124","AN-124")),
    "A140"=list("A140"=c("A140","AN140","AN-140")),
    "A148"=list("A148"=c("A148","AN148","AN-148")),
    "A225"=list("A225"=c("A225","AN225","AN-225"))),
  
  
  
  ## ATR
  atr=list(
    "AT42 family"=list(
      "AT42"=c("AT42","ATR42"),
      "AT43"=c("AT43","ATR43"),
      "AT45"=c("AT45","ATR45")),
    "AT72 family"=list(
      "AT72"=c("AT72","ATR72"),
      "AT76"=c("AT76","ATR76"))),
  
  
  avro=list("Vulcan"=list("Vulcan"=c("Vulcan"))),
  
  
  ## boeing ####
  boeing=list(
    "Dreamlifter family"=list("BLCF"=list("BLCF"=c("BLCF"))),
    "707 family"=list("B707"=list("B707"=c("B703","B707"))),
    "717 family"=list("B717"=list("B717"=c("B712","B717"))),
    "727 family"=list("B727"=list("B727"=c("B721","B722","B723","B727","B72F","B727-200"))),
    "737 family"=list(
      "B737"=list(
        "B737"=c("737","B737","Boeing 737"),
        "B732"=c("B732","Boeing 737-200","B737-200","737-200"),
        "B733"=c("B733","Boeing 737-300"),
        "B734"=c("B734","Boeing 737-400"),
        "B735"=c("B735","Boeing 737-500"),
        "B736"=c("B736","Boeing 737-600"),
        "B73G"=c("B73G","B73W"),
        "B738"=c("B738","738","B38M","Boeing 737-800"),
        "B739"=c("B739","B39M","B73H","B737-900"))),
    
    "747 family"=list(
      "B747"=list(
        "B741"=c("B741","B747-132"),
        "B742"=c("B742"),
        "B743"=c("B743"),
        "B744"=c("B744","B747-400"),
        "B744F"=c("B744F"),
        "B747"=c("747","B747"),
        "B74S"=c("B74S"),
        "B748"=c("B748"))),
    
    
    "757 family"=list(
      "B757"=list(
        "B752"=c("B752","Boeing 752"),
        "B753"=c("B753","Boeing 753"),
        "B757"=c("B757"))),
    
    "767 family"=list(
      "B767"=list(
        "B762"=c("B762","Boeing 762"),
        "B763"=c("B763","763","Boeing 763","Boeing 767-300","B763s"),
        "B764"=c("B764","Boeing 764"),
        "B767"=c("B767","Boeing 767","Boeing 737-600"))),
    
    "777 family"=list(
      "B777"=list(
        "B777"=c("B777","Boeing 777"),
        "B77W"=c("B77W"),
        "B77F"=c("B77F"),
        "B772"=c("B772"),
        "B773"=c("B773"))),
    
    "787 family"=list(
      "B787"=list(
        "B787"=c("Boeing 787","B787","B78X","787"),
        "B788"=c("B788"),
        "B789"=c("B789")))),
  
  
  british_aerospace=list(
    "ATP family"=list("ATP"=c("ATP")),
    "146 family"=list("BAe146"=c("RJ70","RJ85","RJ1H","RJ100","B461","B462","B463","BAe 146","BAe 146-200","BAe146")),
    "Jetstream family"=list("Jetstream"=c("Jetstream 32","JS31","JS32","JS41","J31","J32"))),
  
  
  
  britten_norman=list(
    "BN2P"=list("BN2P"=c("BN2P")),
    "TRIS"=list("TRIS"=c("TRIS"))),
  
  ## beachcraft
  beechcraft=list(
    "BE99"=list("BE99"=c("BE99")),
    "B1900"=list("B1900"=c("B190","B1900","B1900D"))),
  
  
  
  bombardier=list(
    "CL600 family"=list("CL600"=c("CL600","CL-600")),
    "CRJ family"=list(
      "CRJ"=c("CRJ","CRJX","CRJs"),
      "CRJ1"=c("CRJ1","CRJ-100","CRJ100"),
      "CRJ2"=c("CRJ2","CRJ-200","CRJ200"),
      "CRJ4"=c("CRJ4","CRJ-400","CRJ400"),
      "CRJ7"=c("CRJ7","CRJ-700","CRJ700"),
      "CRJ9"=c("CRJ9","CRJ-900","CRJ900"))),
  
  
  buffalo=list("C46"=list("C46"=c("C46"))),
  
  ## casa ####
  casa=list("C212 family"=list("C212"=c("C212"))),
  
  cessna=list(
    "Caravan"=list("208 Caravan"=c("Caravan","C208GC")),
    "C402"=list("C402"=c("C402")),
    "C421"=list("C421"=c("C421")),
    "C525"=list("C525"=c("C525"))),
  
  citation=list("501"=list("501"=c("501"))),
  
  convair=list(
    "CVLT"=list("CVLT"=c("CVLT")),
    "CVLP"=list("CVLP"=c("CVLP"))),
  
  
  de_havilland_canada=list(
    "Twin Otter family"=list("Twin Otter"=c("Twin Otter")),
    "Dash6 family"=list("Dash6"=c("DHC6")),
    "Dash7 family"=list("Dash7"=c("DHC7")),
    "Dash8 family"=list("Dash8"=c("Dash8","DH82","DH8A","DH8B","DH8C","DH8D","Q400","DH8Ds","Dash8-300","Dash8-400"))),
  
  dornier=list(
    "Do228 family" = list("Do228" = c("D228","D228","Dornier 228")),
    "Do328 family" = list("Do328" = c("J328","Do328","D328","Dornier 328"))),
  
  
  embraer=list(
    "EMB110 family"=list("E110"="E110"),
    "EMB120 family"=list("E120"=c("E120","EMB120")),
    "EMB135 family"=list("E135"=c("E135","ERJ135"),
                         "E140"=c("E140"),
                         "E145"=c("E145","EMB145","Embraer 145","ERJ145")),
    "EMB170 family"=list("E170"=c("E170","EMB170"),
                         "E175"=c("E175")),
    "EMB190 family"=list("E190"=c("E190","EMB190","ERJ-190"),
                         "E195"=c("E195","EMB195")),
    "EMB290 family"=list("E290"=c("E290"),
                         "E295"=c("E295"))),
  
  
  
  ilyushin=list(
    "IL18 family"=list("IL18"="IL18"),
    "IL62 family"=list("IL62"="IL62"),
    "IL76 family"=list("IL76"="IL76"),
    "IL86 family"=list("IL86"="IL86"),
    "IL96 family"=list("IL96"="IL96"),
    "IL18 family"=list("IL18"="IL18")),
  
  
  
  # fairchild #### 
  fairchild=list(
    "C123 family"=list("C123"="C123"),
    "SW4 family"=list("SW4"="SW4")),
  
  fokker=list(
    "F27 family"=list("F27"=c("F27","F28","Fokker 27","Fokker 28")),
    "F50 family"=list("F50"=c("F50","Fokker 50")),
    "F70 family"=list("F70"=c("F70","Fokker 70")),
    "F100 family"=list("F100"=c("F100","Fokker 100"))),
  
  
  
  hawker_siddeley=list("A748"=list("A748"=c("A748"))),
  
  junkers=list("JU52"=list("JU52"=c("JU52"))),
  
  let=list("L410 family"=list("L410"="L410")),
  
  
  lockheed=list(
    "C130 family"=list("C130"="C130"),
    "L188 family"=list("L188"="L188"),
    "L101 family"=list("L101"="L101")),
  
  ## mcdonnell ####
  mcdonnell_douglas=list(
    "DC3 family"=list(
      "DC3"=c("DC3","DC3T")),
    "DC4 family"=list(
      "DC4"=c("DC4")),
    "DC6 family"=list(
      "DC6"=c("DC6")),
    "DC8 family"=list(
      "DC8"=c("DC8","DC85","DC86","DC87")),
    "DC9 family"=list(
      "DC9"=c("DC9","DC-9","DC91","DC92","DC93","DC94","DC95","DC9E","DC-9-83")),
    "DC10 family"=list("DC10"=c("DC10","DC-10")),
    "MD10 family"=list(
      "MD10"=c("MD10","MD-10"),
      "MD11"=c("MD11","MD-11")),
    "MD80 family"=list(
      "MD80"=c("MD80","MD-80"),
      "MD81"=c("MD81","MD-81"),
      "MD82"=c("MD82","MD-82"),
      "MD83"=c("MD83","MD-83"),
      "MD87"=c("MD87","MD-87"),
      "MD88"=c("MD88","MD-88"),
      "MD90"=c("MD90","MD-90"))),
  
  
  saab=list(
    "SF340 family"=list("SF34"=c("SF34","Saab 340")),
    "SB20 family"=list("SB20"=c("SB20","Saab 2000"))),
  
  shorts=list(
    "SH33 family"=list("SH33"="SH33"),
    "SH36 family"=list("SH36"=c("SH36","Shorts 360","SH360"))),
  
  
  sukhoi=list(
    "Superjet 100 family"=list("SU100"=c("SU95","Superjet-100","SU100"))),
  
  
  tupolev=list(
    "Tu-134 family"=list("T134"=c("T134","Tu-134")),
    "Tu-154 family"=list("T154"=c("T154","Tu-154","TU-154","TU-154M","TU154M")),
    "Tu-204 family"=list("T204"=c("T204","T-204","T214"))),
  
  
  yakovlev=list("YK40 family"=list("YK40"=c("YK40","YK42"))),
  
  
  xian=list(
    "MA60 family"=list("MA60"="MA60"))
)


AIRLINES=list(
  "1time","1Time","9 Air","ABS","ABX","ABX Air","ACT","ACT Airlines","ADA","Adam Air","ADC","Adria","Aegean","Aer Arann","Aer Caribe","Aer Lingus",
  "AerCaribe","Aero Charter","Aero Contractors","Aero Republica","Aero Services","Aerocaribbean","Aerocon","Aerocondor","AeroContractors","Aerodynamics",
  "Aeroflot","Aerogal","Aerolift","Aerolineas","Aerologic","Aeromar","Aeromexico","Aeromexico Connect","Aeropostal","Aeroregional","Aeroservice","Aerostan",
  "Aerosucre","Aerosur","AeroSur","Aerosvit","Aerotrans Cargo","AeroUnion","Africa Airlines","Afriqiyah","Agni","Aigle Azur","Air Algerie","Air Antilles",
  "Air Arabia","Air Asia","Air Astana","Air Atlanta Icelandic","Air Austral","Air Baltic","Air Berlin","Air Blue","Air Bridge Cargo","Air Burkina","Air Busan",
  "Air Canada","Air Central","Air China","Air Comet","Air Contractors","Air Corsica","Air Creebec","Air Do","Air Dolomiti","Air Europa","Air Europe","Air Finland",
  "Air France","Air Georgian","Air Greenland","Air Iceland","Air India","Air Inuit","Air Italy","Air KBZ","Air Macau","Air Malta","Air Mauritius","Air Mediterranee",
  "Air Moorea","Air Namibia","Air Nelson","Air New Zealand","Air Niamey","Air Nippon","Air Nippon Network","Air North","Air Nostrum","Air One","Air Pacific",
  "Air Seychelles","Air Southwest","Air Tanzania","Air Transat",
  
  "Air Transport International","Air Wisconsin","Air Zimbabwe","AirAsia","AirBaltic","Airblue","AirBlue","AirBridgeCargo","Airbus","Aircalin","Aires","Airest",
  "AirExplore","Airlin","Airlinair","Airlink","Airnorth","Airphilexpress","Airtran","AirTran","Airwork","Ajet","Ak Bars","Akasa","Alaska","Alaska Airlines",
  "Albastar","Alfa Airlines","Algerie","Aliansa","Alitalia","ALK","Alkan","All Nippon","Allegiant","Alliance","Allied Cargo","Alma de Mexico","Aloha Cargo",
  "Alrosa","Amakusa Airlines","Amapola","Amaszonas","AMC","Amelia","American","American Airlines","American Eagle","Ameriflight","Amerijet","ANA","Anadolu",
  "Anadolujet","Andes","Angara","Antonov","ANZ","Arabia","Arann","Argentinas","Ariana","Arik","Arik Air","Arkefly","Arkia","Armavia","Arrow Cargo","AS Avies",
  "Aseman","Aserca","Asia India","Asia Pacific Airlines","Asian Spirit","Asiana","ASL","ASL Belgium","ASL France","Astana","Astra","Astraeus","Astral","Ata","ATA",
  "ATI","Atlanta","Atlanta Icelandic","Atlantic","Atlantic Airways","Atlas","Atlasglobal","AtlasGlobal","Atlasjet","Atrak","Atran","Augsburg","Aurela","Aurigny",
  "Aurora","Austral","Australian Air Express",
  
  
  "Austrian","Avelo","Avia Traffic","Avianca","Aviastar","Avion Express","Avior","Avitrans","AZAL","Azerbaijan","Azerbaijan Airlines","Azimuth","Azman",
  "Azores","Azul","Azur","BA","Badr","Bagan","Bahamas","Bahamasair","Balkan Holidays","Baltic","Bangkok","Batavia","Batik","BAW","Bearskin","Bek","Belair",
  "Belavia","Belgium","Bellview Airlines","Berlin","Berry","BH Air","BH Airlines","Bhutan","Biman","BinAir","Binter","Binter Canarias","Blue","Blue1","Bluebird",
  "Bluebird Cargo","bmi","BMI","bmibaby","BoA","Boeing","Boliviana","Botswana","Braathens","Bradley","Bratsk","Bravo","Breeze","Bridge Cargo","Brit Air","Britair",
  "BritAir","British Airway","British Airways","British Midland","Brussels","Brussels Airlines","Budapest","Budapest Air","Buddha","Buffalo","Bukovyna","Bulgaria",
  "Bulgaria Air","Bulgarian Air Charter","Bulgarian Charter","Busan","Busy Bee","Buzz","CAA","Cairo","Caledonie","Calm","Calm Air","Camair","Canada","Canadian North",
  "Canjet","Capital Beijing","Caraibes","Cargo","Cargojet","Cargologic","Cargolux","Caribbean","Carpat","Carpatair","Caspian","Cathay","Cavok",
  
  "Cayman","CCM","Cebu","Cebu Pacific","Cem","Cemair","CemAir","Central Mountain","Centralwings","Centurion","Chair","Chalair","Cham Wings","Chanchangi","Chautauqua",
  "Chengdu","China Airlines","China Cargo","China Eastern","China Express","China Southern","China United","Chrono","Chukotavia","Cimber","Cirrus","Citilink",
  "City Airline","City Airlines","City Star Airlines","Cityjet","CityJet","Citywing","Click Mexicana","Cobham","Colgan","Comair","Commut","Commutair","CommutAir",
  "Commute","Compagnie Africaine Aviation","Compass","Condor","ConocoPhillips","Contact","Contact Air","Continental","Contour","Conviasa","Copa","COPA","Corendon",
  "Corendon Airlines","Corporate Airlines","Corsair","Corsica","Creebec","Croatia","Croatia Airlines","CSA","Cubana","Cyprus","Czech Airlines","Daallo","Daghestan",
  "Daily","Dana","DANA","Danish Air Transport","Danu","Darwin","DAT","Delays","Delta","Delta Airlines","Denim Air","Deraya","DHL","Discover","Dniproavia","Dolomiti",
  "Donavia","Donbassaero","Dragon","Dragonair","Druk","Druk Air","Dynamic","Dynamic Airlines","Eagle","Eagle Air","Eastar","Eastern","Easyfly","Easyjet","EasyJet",
  "EAT","Edelweiss","Egypt",
  
  "Egypt Air","Egypt Express","Egyptair","EgyptAir","El Al","Elal","Electra","Ellin","Emerald","Emirates","Empire Airlines","Endeavor","Enter","Envoy","Era","Estelar",
  "Estonian","Ethiopian","Ethiopian Airlines","Etihad","Euro Atlantic","Euroatlantic","Eurofly","Eurolot","Europa","Europe Airpost","European Air Charter",
  "European Air Transport","European Charter","Eurowings","Eva","EVA","Evergreen","Everts","Exin","Exploits Valley","Express","Expressjet","ExpressJet",
  "FAA fines American Airlines","Far Eastern","Farnair","Fastjet","FAT","Fedex","FedEx","Fiji","Finnair","Finncomm","Fire","Firefly","First","Flair","Fleet",
  "Flightline","Fly Africa","Fly540","Flybe","FlyBe","FlyBE","Flybondi","Flydubai","FlyDubai","Flyegypt","FlyEgypt","FlyGeorgia","Flyglobespan","Flynas","FlyNAS",
  "Flysafair","Four Star Cargo","France","Freedom","Frontier","Fuji Dream","Garuda","Gazprom","Gazpromavia","Georgian","German","German Airways","Germania",
  "Germanwings","Global Air","Global Aviation","Globus","GMG","Go","Go2Sky","GoAir","Gojet","GoJet","Gol","GOL","Golden","Golden Air","Goma","Gomair",
  "Great Lakes","Great Wall Airlines",
  
  "Greenland","Grozny","Grozny Avia","Guicango","Gulf","Gulf Air","Hainan","Hainan Airlines","Hawaiian","Hawkair","Helvetic","Hemus","Henan Airlines","Hermes",
  "Hevilift PNG","Hewa Bora","Hifly","HiFly","HK Express","Hokkaido","Hola Airlines","Hong Kong","Hongkong Airlines","Hop","HOP","Horizon","Horizon Air",
  "Horizont","iAero","IBC","Iberia","Iberworld","Ibex","Iceland","Icelandair","IFL","iFly","IFly","Ikar","India","India Express","India Regional","Indian Airlines",
  "Indigo","IndiGo","Indonesia AirAsia","Insel","Interjet","Intersky","Inuit","IrAero","Iran","Iran Airtours","Iraqi","IRS","Islena Airlines","Israir","ITA",
  "Itali Airlines","ItAli Airlines","Itapemirim","Itek Air","Izhavia","Izmir Airlines","JAC","Jade Cargo","JAL","Jambo","Japan","JAT","JAT Airways","Jayawijaya",
  "Jazeera","Jazz","Jeju","Jet Airways","Jet Time","Jet Time Finland","jet2","Jet2","Jet4You","Jetairfly","Jetblue","Jetconnect","Jetgo","JetKonnect","Jetlink",
  "Jetlite","Jetsmart","Jetstar","Jettime","Jetways","JetX","Jin","Jinnah","Jonika","Joon","Jordan","Jota","Joy",
  
  
  "JSX","Jubba","Juneyao","Kabo","Kalitta","Kalstar","Kam Air","Karun","Kasai","Katekavia","KayaAirlines","KD Avia","Keewatin","Kelowna","Kenn Borek","Kenya",
  "Key Lime","Khabarovsk","Khors","Kingfisher","Kish","Kish Air","Klasjet","KLM","Kolavia","Komiaviatrans","Korean","Korean Air","Korean Airlines",
  "Korean Airlines B777","Koryo","Krasavia","Kuban","Kulula","Kunming","Kuwait","Kyrgyzstan Airlines","LAM","LAN","Lanhsa","Lao","Laser","LATAM","Lauda Europe",
  "Laudamotion","LC Busre","LGW","LGW Walter","LH Cityline","LIAT","Libyan","Libyan Arab","Lift","Lingus","Link","Lion","Lionair","Livingston","Logan","Loganair",
  "Longtail","LOT","LTE","LTU","Lufthansa","Lux","Luxair","Lynx","Madagascar","Magnicharters","Mahan","Malaysia","Malaysian","Maldivian","Maldivian Air Taxi",
  "Maleth","Malev","Malindo","Malmo","Malmo Aviation","Malta","Malu","Mandala","Mandala Airlines","Mandarin","Mango","Manta","Manx2","MAP","Martin","Martinair",
  "MAS Cargo","MASWings","Mauritania","Mauritius","Mavi Gok","Max","Maximus","MEA","Medallion",
  
  
  "Mediterranee","MedView","Mel","Meraj","Meridiana","Merpati","Mesa","Mesaba","Mesaba Airlines","Mexicana","Miami","Miat","MIAT","Mid Airlines","Middle East",
  "Midwest","Miniliner","Mistral","MNG","MNG Airlines","Mocambique Expresso","Modern Logistics","Moldova","Monarch","Montenegro","Morningstar","Moskovia",
  "Motor Sich","Mount Cook","Murray","Mwant","My Freighter","Myanma","Myanmar","Myanmar National","MyCargo","Naft","Namibia","NAS Air","National","National Jet",
  "National Jet Systems","Nationwide","Nauru","Naysa","Nelson","Neos","Nepal","Nepal Airlines","Nesma","Network Australia","New Zealand","NewGen","Nextjet","Niki",
  "Nippon Cargo","Niugini","Nok","NOK","Nolinor","Nordavia","Nordic Regional","Nordica","Nordstar","Nordwind","Norra","NORRA","North","North American","North Cariboo",
  "Northern Air Cargo","Northern Cargo","Northwest","NorthWest","Northwestern","Norwegian","Norwegian Air Shuttle","Nostrum","Nouvel","Nouvelair","Nova","Novair",
  "Ocean Air","OceanAir","Okay","OLT","Olympic","Olympic Air","Olympic Airlines","Oman","Oman Air","Omni","Omni Air","One","Onur","Open Skies France","Openskies",
  "Orange2fly","Orbest","Orenair",
  
  "Orenburg","Orient Thai","Oriental Air Bridge","Oriental Bridge","Overland","Ozjet","Pacific Blue","Pacific Coastal","PAL","Panama","Pantanal","Paramount","Pascan",
  "Passaredo","Pawa Dominicana","Peace","Peach","Pegas","Pegasus","Pelitta","Pen","Penair","PenAir","Perimeter","Peruvian","PGA","Philippine","Philippine Airlines",
  "Philippines AirAsia","Phuket Airlines","PIA","Piedmont","Pinnacle","Pionair","Pluna","PNG","Pobeda","Polar","Polet","Porter","Portugalia","Precision","Primera",
  "Prince Edward Air","Privilege","Provincial","PSA","PSA Airlines","Qantas","Qantaslink","Qatar","Qatar Airways","Qazaq","Qeshm","Quatar Airways","RAF","RAK","RAM",
  "Red Sea","Red Wings","Regional","Regional 1","Regional CAE","Republic","Republic Airline","Republic Airlines","Rex","REX","Riau","Rimbun","Rossiya","Rouge",
  "Royal","Royal Air Maroc","Royal Brunei","Royal Jordanian","Rusline","Rutaca","Rwandair","Ryan Int","Ryanair","RyanAir","S7","SA Airlink","SA Express","SAA",
  "Safair","Safarilink","Saga","Saga Airlines","Saha","Saha Airlines","Salaam","Salam","San Marino","Santa Barbara","Santa SSLH","Saratov","Saratov Airlines",
  "Saravia",
  
  
  "SARPA","SAS","Sat","SAT","Sata","SATA","Satena","Saudi","Saudia","Saurya","SBA","Scandinavian","Scat","SCAT","Scoot","Seaborne","SEAir","Senegal","Serbia",
  "Serene","Serve","Services Air","Severstal","Shaheen","Shan Xi Airlines","Shandong","Shanghai","Shanghai Airlines","Shenzhen","Shree","Shuttle","Shuttle America",
  "Sial","Siberia Airlines","Sibir","Sichuan","Sichuan Airlines","Silk Way","Silkair","Silver","Silverstone","Sindbard","Singapore","Sita","Skippers",
  "Skippers Aviation","Sky","SkyBahamas","Skybus","Skyexpress","Skyjet","Skymark","Skynet Asia","Skyservice","Skytrans","SkyUp","Skyward","Skyway Enterprises",
  "Skyways","Skywest","SkyWest","Skywork","Small Planet","Smartlynx","Smartwings","Sol","Solaseed","Solomon","Somon","South African","South Airlines",
  "South Sudan Supreme","South Supreme","South West Aviation","Southern Air","Southwest","SouthWest","Spanair","Spicejet","SpiceJet","Spirit","Spirit Airlines",
  "Spring","Spring Airlines","Sprint","Sri Lankan","Srilankan","SriLankan","SriLankan Airlines","Sriwijaya","Sriwijaya Air","Star Air Freight","Star Flyer",
  "Star Freight","Star Peru","Starbow","Starlux","Stobart","Strategic","Sudan",
  
  
  "Sukhoi","Summit","Summit Air","Sun Country","Sun Express","Sunclass","Sundair","Sunday","Sunexpress","SunExpress","Sunstate","Sunwest","Sunwing",
  "Suramericanas","Surinam","Swift","Swiftair","Swiss","Swoop","Syrian Arab","TAAG","Taban","TACA","TACV","Taftan","TAG","Tahiti","Tahiti Nui",
  "Tailwind","Tajik","TAM","TAME","Tanzania","TAP","Tara","Tara Air","Tarco","Tarco Airlines","Tarom","TAROM","Tasman Cargo","Tassili","Tatarstan",
  "Thai","Thai Airways","Thai B773","Thomas Cook","Thomson","Thomson Airways","Thomsonfly","ThomsonFly","THY","Tianjin","Tibet","Tiger","Tiger Airways",
  "Tindi","Titan","TMA","TNT","Tomsk","Tomskavia","Total","Tracep","Trade","Trans Capital","Trans Maldivian","Trans States","Transaero","Transasia",
  "TransAsia","Transat","Transavia","Transcarga","Transport International","Transwest","Travel Service","Trigana","TriMG","Trip","TRIP","TSM","TUI",
  "TUI Belgium","TUI Nederland","Tuifly","TuiFly","TUIfly","TUIFly","Tulpar","Tunis","Tunis Air","Tunisair","Turkish","Turkmenistan","Tway","UIA",
  "Ukraine","ULS","ULS Cargo",
  
  
  "Ultimate","Uni","Uni Airways","United","unknown","Up","UPS","Ural","Ural Airlines","Urga","US Airways","USA Jet","USA Jet Airlines","UTair","UTAir",
  "UVT","Uzbekistan","V Australia","Valuejet","Van","Vanilla","Vanuatu","VARA","Varesh","Venezolana","Veteran","Via","Vietjet","VietJet","Vietnam",
  "Viking","Vim","VIM","Virgin","Virgin Atlantic","Vision","Vistara","Viva","VivaAerobus","VivaAeroBus","VivaColombia","Vladivostok","Vladivostok Air",
  "Vladivostok Avia","VLM","Voepass","VoePass","Volaris","Volga Dnepr","Volotea","Vostok","Voyager","Voyageur","Vueling","Vulkan","Walter","Wasaya","WDL",
  "Webjet","West Air","West Atlantic","West Sweden","West Wind","Western Air","Western Global","Westjet","WestJet","White","Wideroe","Wind Jet","Wind Rose",
  "Windjet","Windrose","Wings","Wisconsin","Wizz","Wizz Air","Wizz Ukraine","Wizzair","World Airways","World Atlantic","World2Fly","WOW","Xfly","Xiamen",
  "Xiamen Airlines","XL Airways","XL France","Xtra","Yakutia","Yamal","Yan","Yanair","Yas Air","Yemenia","Yeti","Zagros","Zest","Zest Air","Zhejiang Loong",
  
  
  "Zimbabwe","Zimex","Zoom Airlines","Zoom UK"
)
