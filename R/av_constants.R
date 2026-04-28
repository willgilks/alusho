
## dat scrape and analysis
end_of_first_period="2022-07-15"
start_of_second_period="2022-08-15"
end_of_second_period="2023-08-01"
earliest_overall_date="1994-03-01"
earliest_test_date=Sys.Date()-365


## constants for string pattern matching when summarising events.
{
  times1=list(
    approach=c("approach","descent","go around","initial climb","landed","on landing","landing","hard landing","localizer",
               "touch down","touchdown","touched down","roll out","rollout"),
    runway=c("runway"),
    taxiway=c("taxiway"),
    on_ground=c("apron","at stand","ground worker","push back","taxi","line up","turn off","at gate"),
    departure=c("departure","departed","climb out","takeoff","take off","could not retract landing gear","took off"),
    enroute=c("in flight","midair","enroute"))
  
  animals1=list(
    bird=c("birds","bird","goose","geese"),
    other=c("dog","coyote")
  )
  
  weather=list(
    rain=c("rain")
  )
  ac_parts1=list(
    antiice=c("anti-ice","antiice"), 
    battery=c("battery","Battery","batteries"),
    electric=c("electric","electronic"),
    navigation=c("nav","navigation"),
    toilet=c("lavatory","toilet"),
    FMS=c("FMS","FMSs"),
    GPS=c("GPS"),
    GPWS=c("EGPWS","GPWS"),
    engine=c("engine"),
    propeller=c("propeller"),
    oil=c("^oil"," oil"),
    oven=c("^oven"," oven"),
    seat=c(" seat "," seat$"),
    comms=c(" comms"," communcation"),
    pressure=c("pressurization","pressure"),
    vehicle=c("vehicle"),
    snow_plough="snow plough")
  
  ac_parts2=c(
    "avionics",
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
    "bleed",
    "brake",
    "cabin",
    "charger",
    "cockpit",
    "coffee",
    "cargo",
    "door",
    "display",
    "elevator",
    "computer",
    "flight deck",
    "fuselage",
    "flap",
    "fuel", 
    "galley",
    "gear",
    "loader",
    "luggage",
    "tyre","wheel",
    "on board",
    "oxygen","panel",
    "phone",
    "power bank",
    "radar altimeter",
    "RAT","radio","radome","reverser",
    "slat","spoiler","stairs",
    "tail",
    "weather radar",
    "radar",
    "rudder",
    "slat",
    "ventilation",
    "water system",
    "windshield","window","wing","wing tip")
  
  
  
  
  people1=list(
    pilot=c("captain","copilot","pilot","^pilot","first officer"),
    cabin_crew=c("flight attendant","attendant","cabin crew","crew"),
    atc=c("ATC","tower","controller"),
    ground_worker=c("ground worker","luggage handler","technicians"),
    passenger=c("passenger","people"))
  
  
  
  events1=list(
    `7700`=c("7700"),
    activation=c("activation","activates"),
    alert=c("alert","alarm"),
    cancel=c("cancels","cancelled"),
    collision=c("collide","collision","Collision"),
    near_collision=c("near collision","nearly collided","near miss"),
    crash=c("crashed","crashes","Crash","crash"),
    decsent=c("descent","descend"),
    died=c("died"," dies"),
    failure=c("failure","failed"),
    fire=c("flames","fire","Fire","burned"),
    injury=c("injury","injuries","injures","injured"),
    lightning=c("lightning","lightening"),
    noise=c("noisy","noise"),
    overrun=c("overran","overrun"),
    smell=c("odour","smell"),
    return=c("return"),
    divert=c("divert","diversion"),
    volcano=c("volcanic","volcano","Volcano"),
    decompression=c("rapid decompression","sudden decompression"),
    bird_strike=c("bird strike","bird","struck flock of ducks","bird"),
    windshear=c("wind shear","windshear"))
  
  events2=list(
    "asymmetry",
    "beeping",
    "bang",
    "below minimum safe altitude",
    "blew",
    "burst",
    "breaks",
    "burning",
    "clogged",
    "collapse",
    "contact",
    "could not retract",
    "cracked",
    "damage","detached","deployed",
    "disabled","disagree",
    "discrepancy",
    "dislodged",
    "dropped",
    "emergency","error","evacuation","excursion",
    "exposed",
    "fault","flamed out",
    "generator",
    "go around",
    "fell",
    "fumes",
    "hail strike",
    "heaviness",
    " hit",
    "hole",
    "impacted","incapacitated","incursion","indication",
    "inspection",
    "issue",
    " ill",
    "jammed",
    "killed",
    "leak",
    "locked",
    "load shift",
    "loss of power",
    "loss of separation",
    "lost power",
    "lost height",
    "maintenance",
    "malfunction",
    "mismatch",
    "opened",
    "overflew","overheat","overload","overspeed",
    "pressure","pressurize",
    "problem",
    "ran off",
    "rejected","returned",
    "runway incursion",
    "separated","shot","shut down","smoke","stall",
    "stick shaker",
    "tailstrike",
    "TCAS",
    "touched down short of runway",
    "trouble",
    "turbulence",
    "thermal runaway",
    "scrape",
    "skidded",
    "snow",
    "sparks",
    "stuck",
    "UAV",
    "veered off","vibrations",
    "wake turbulence",
    "warning",
    "windshear")
  
  adjectives1=list(instability=c("stabilisation","unstable"),
                   uncommanded=c("uncommanded"),
                   erroneous=c("erroneous","incorrect","wrong"))
  
  adjectives2=list(
    "hard","insufficient",
    "medical",
    "severe",
    "unidentified","unreliable","unsafe",
    "unusual",
    "unplanned",
    "without clearance")
  
  
  ## ac_condition
  ac_condition1=list(
    ice=" ice", altitude=c("altitude","height"),
    speed=c("airspeed","speed"),
    attitude="attitude",
    angle="angle",
    performance="performance",
    pitch="pitch",
    thrust="thurst")
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

## incomplete
AIRLINES=list(
  "1time",
  "4you",
  "9 Air",
  "Ababeel",
  "Abeer Services",
  "ABS",
  "Abu Dhabi",
  c("ABX","ABX Air"),
  c("Ace Air","Ace Air Cargo"),
  c("ACG"),
  c("ACSA"),
  c("ACT","ACT Airlines"),
  "ADA",
  c("Adam Air","Adam"),
  "ADC",
  "Adria",
  "Aegean",
  "Aer Arann",
  c("Aer Lingus","Lingus"),
  c("AerCaribe","Aer Caribe"),
  "Aero Charter",
  c("Aero Contractors","Aero Contractor","AeroContractors","Contractors","Contractor"),
  "Aero K",
  "Aero Mongolia",
  "Aero Republica",
  c("Aero Services","Aero Service"),
  c("Aero VIP","Aerovip"),
  "Aero-Fret",
  "Aero-Pioneer",
  "Aeriantur",
  "AeroBratsk",
  "Aerocaribbean",
  "Aerocon",
  "Aerocondor",
  "Aerodesierto",
  "Aerodienst",
  "Aerodynamics",
  
  c("Aeroflot","Aeroflot-Don","Aeroflot-Nord"),
  
  "Aerogal",
  "Aeroitalia","Aerojet","Aerolift",
  
  c("Aerolineas Argentinas","Aerolineas","Argentinas","Argentina"),
  "Aerolinea Sky",
  "Aerologic","Aeromar",
  
  c("Aeromexico","Aeromexcio","Aeromexiko","Aeromexico Connect"),
  "Aeronav","Aeronaves","Aeroperlas",
  "Aeropostal","Aeroregional",
  
  "Aeroservice",
  "Aeroservis",
  
  "Aerostan",
  "Aerostar","Aerosucre","Aerosur","AeroSur","Aerosvit",
  
  c("Aerotrans Cargo","Aerotranscargo"),
  
  "AeroUnion",
  
  
  "Africa Airlines",
  "Africa Charter",
  "Africa One",
  c("African Air Services","Africa Air Services"),
  c("African Express","Africa Express"),
  "Africa's Connection",
  "Afriqiyah",
  
  "Agefreco",
  "Agni","Aigle Azur",
  
  "Air Aland",
  
  c("Air Algerie","Algerie"),"Air Antilles",
  "Air Arabia",
  c("Air Asia","AirAsia","Asia X","Indonesia Air Asia","Indonesia AirAsia","Indonesia Asia",
    "Philippines AirAsia"),
  "Air Astana",
  
  c("Air Atlanta","Air Atlanta Icelandic"),
    
    "Air Austral",
  "Air Bagan",
  "Air Baltic","Air Berlin","Air Blue",
  
  "Air Botswana",
  
  "Air Bridge Cargo","Air Burkina","Air Busan",
  
  c("Air Canada","Canada"),
  
  
  c("Air Caraibes","Caraibes"),
  
  c("Air Cargo","Air Cargo Carrier"),
  
  
  "Air Central",
  "Air China",
  
  "Air Class",
  
  "Air Comet","Air Contractors","Air Corsica","Air Creebec",
  c("Air Deccan","Deccan"),
  c("Air Do","AirDo"),
  "Air Dominicana",
  "Air Dolomiti","Air Europa","Air Europe","Air Finland",
  c("Air France","France"),
  "Air Georgian",
  "Air Greenland",
  "Air Guyane",
  "Air Iceland",
  
  
  c("Air India","India","India Express","India Regional","Indian Airlines","Indian"),
  
  
  "Air Inuit",
  c("Air Italy","Italy","Italy Polska"),
  c("Air Jamaica","Jamaica"),
  "Air KBZ",
  
  "Air Libya",
  
  "Air Macau",
  
  c("Air Madagascar","Madagascar"),
  "Air Malawi",
  
  "Air Malta",
  
  "Air Mandalay",
  "Air Mauritius","Air Mediterranee",
  "Air Moorea","Air Namibia","Air Nelson",
  
  c("Air New Zealand","Air NZ","New Zealand"),
  
  
  "Air Niugini",
  "Air Niamey",
  
  "Air Nigeria",
  
  # "Air Nippon","Air Nippon Network",
  "Air Norterra","Air North","Air Nostrum","Air One","Air Pacific",
  
  "Air Service Berlin",
  
  "Air Seychelles","Air Southwest",
  
  c("Air Tahiti Nui","Air Tahiti"),
  "Air Tahoma",
  "Air Tanzania","Air Transat",
  "Air Tran",
  
  "Air Transport International","Air Wisconsin","Air Zimbabwe",
  "AirBaltic","Airblue","AirBlue",
  
  c("Air Bridge Cargo","AirBridgeCargo","AirBridge Cargo"),
  "Air Vanuatu",
  "Air West Georgia",
  
  "Airbus",
  "Aircalin",
  
  c("Air Cargo Carrier","Cargo Carrier","Cargo Carriers"),
  "Aires","Airest",
  "AirExplore",
  "Airhub",
  
  c("Airlin","Airlinair"),
  "Airlink","Airnorth",
  c("Airphil express","Airphilexpress"),
  
  c("Airswift"),
  
  c("Airtran","AirTran"),
  
  c("Air Volga","Airvolga"),
  "Airwork",
  
  
  c("AIS Airlines","AIS"),
  
  "AIT",
  
  "Ajet","Ak Bars","Akasa",
  
  "Alada",
  
  c("Alaska Airlines","Alaska","Alaskan"),
  
  "Albanian",
  "Albanwings",
  "Albastar",
  "ALCI",
  "Alexandria",
  c("Alitalia","Italia"),
  "Alfa Airlines","Aliansa","Alitalia","ALK","Alkan",
  c("All Nippon","ANA","ANN","Nippon Cargo","Air Nippon","Air Nippon Network"),
  "All Ways",
  "All West Freight",
  "Allas",
  c("Allegiant Air","Allegiant","Allegiance"),
  
  c("Alliance Airlines","Alliance"),
  
  c("Allied Services"),
  
  c("Allied Cargo"),
  
  "Alma de Mexico",
  
  "Almasria",
  
  c("Aloha","Aloha Airlines","Aloha Cargo"),
  
  "Alpine Air",
  "Alrosa","Alsair","Altyn",
  c("Amakusa Airlines","Amakusa"),
  "Amapola","Amaszonas","Amazon Sky",
  
  c("Amerijet","Ameri"),
  
  "AMC","Amelia",
  
  "America West",
  c("American Airlines","American","American Eagle","Executive Airlines"),
  
  "AmericanConnection", # Chautauqua
  
  "Ameriflight",
  
  "Ameristar",
  
  c("Amsterdam Airlines","Amsterdam"),
  
  "Anda",
  
  c("AJet","Anadolus","Andalus","Anadolu","Anadolujet"),
  
  "Andes","Angara","Angola","Antonov",
  c("ANZ","Air New Zealand"),
  
  c("Air Antilles","Antilles","Antilles Express"),
  
  c("Air Colombia","Colombia"),
  
  "Air Labrador",
  "Antrak",
  
  "Arabia","Arajet","Aramco","Arann","Arall",
  "Arctic Sunwest",
  
  
  c("Aria Air","Airia Air"),
  
  "Ariana",
  
  
  c("Arik","Arik Air"),
  "Arkefly","Arkia","Armavia",
  c("Armenian","Armenia"),
  "Arrow Cargo",
  "Aruba",
  "AS Avies",
  "Aseman","Aserca",
  "Asia Airways",
  c("Asia Pacific","Asia Pacific Airlines"),
  "Asia India","Asia Pacific Airlines","Asian Spirit",
  "Asiana",
  c("ASL","ASL Belgium","ASL France"),
  "AsiaLink Cargo",
  "Aspire",
  "Associated",
  c("Astar Air Cargo","Astar Air","Astair","Astar"),
  "Astana","Astra","Astraeus","Astral",
  c("Ata","ATA"),
  "ATI",
  "Atlanta","Atlanta Icelandic","Atlantic","Atlantic Airways",
  
  c("Atlas","Atlasglobal","AtlasGlobal","Atlasjet"),
  "ATMA","Atsa",
  "Atrak","Atran","Augsburg","Aurela",
  "Aurigny",
  "Aurora","Austral","Australian Air Express",
  "Austrian",
  c("Avanti Air","Avanti"),
  "Avelo",
  c("Avia Traffic","Avia Kyrgyzstan","Aviatraffic"),
  "Aviacon Zitotrans",
  "Aviacsa","Aviajet","Avianova",
  "Avianca",
  c("Aviastar","Aviastar-TU"),
  "Aviateca",
  "Avient Aviation",
  "Avies",
  "Aviogenex",
  c("Avion Express","Avion Malta"),
  "Avior",
  "Avis Amur","Avitrans",
  c("Ayk Avia","Aykavia"),
  "AZAL",
  c("Azerbaijan","Azerbaijan Airlines"),
  c("Azimuth","Azimut"),
  "Azman","Azores","Azul",
  c("Azurair","Azur"),
  c("Azza Transport","Azza"),
  
  # B
  "Baboo",
  "Badr","Bagan","Baghdad","Bahamas","Bahamasair","Baires","Balkan Holidays","Baltic",
  "Bako Air","Bamboo","Bangkok","Bar","Barkol","Batavia","Batik","Bearskin","Bek",
  "Belair",
  "Belavia","Belgium",
  c("Belle Air","Belle Air Europe","Belle"),
  c("Bellview Airlines","Bellview"),
  "Bemidji","Berjaya","Berkud","Berlin","Berniq","Berry",
  "BH Air",
  "BH Airlines",
  "Bhoja","Bhutan","Biega","Biman","BinAir","Bingo","Binter","Binter Canarias","Blue","Blue1","Bluebird",
  
  "Bluebird Cargo",
  c("British Midland","BMI","bmi","bmibaby"),
  
  "BoA","Boeing","Boliviana","Bombardier","Bondi","Bonza",
  "Botswana",
  
  c("BQB Lineas Aereas","BQB"),
  c("BoraJet","Bora Jet"),
  "Braathens","Bradley","Bratsk","Bravo","Breeze","Bridge Cargo",
  "Brindabella",
  "Bristow",
  c("Brit Air","Britair","BritAir"),
  c("British Airways","British Airway","BAW","BA"),

  "British European",
  "British Gulf",
  "British Mediterranean",
  
  "Brussels","Brussels Airlines","Buana",
  c("Budapest Air","Budapest"),
  "Buddha","Buff Services","Buffalo","Bugulma",
  c("Bukovyna","Bukovina"),
  
  c("Bulgaria Air","Bulgaria"),
  c("Bulgarian Air Charter","Bulgarian Charter"),
  "Bulog",
  "Busan",
  "Bush",
  "Business Aviation Centre",
  "Busy Bee","Buzz",
  
  
  # C
  "CAA","Cabo Verde Express","Cairo","Caledonie","CAL Cargo","Cally",
  c("Calm Air","Calm"),
  "Camair","Cambodia Angkor","CAMEX",
  "Canadian North",
  "Canarias",
  "CanaryFly",
  "Canjet",
  c("Capital Beijing","Capital"),
  "Cardig",
  "Cargo Air Lines",
  "Cargo Aircraft Management",
  "Cargo Global",
  "Cargo2Fly",
  "Cargojet","Cargologic","Cargolux",
  c("Caribbean","CAL"),
  "Carnival",
  "Carpat","Carpatair","Carpediem","Carson","Caspian","Cathay",
  "Caucasus","Caverton","Cavok",
  "Cayman","CCM","Cebgo",
  c("Cebu","Cebu Pacific"),
  c("CemAir","Cem","Cemair"),
  c("Ceiba","Ceiba Intercontinental"),
  "Cello",
  "Central American Airways",
  "Central Mountain","Centralwings",
  "Center South",
  "Centurion","Century",
  c("Cetraca Aviation","Cetraca Aviation Service Let","CAS","Cetraca"),
  "Chair","Chalair",
  "Challenge",
  "Chalk Ocean Airways","Cham Wings","Chanchangi","Chautauqua",
  "Chengdu",
  "China Airlines","China Cargo","China Eastern","China Express",
  c("China Southern","CSN"),
  "China United",
  "China Postal",
  "China West Air",
  "Chrono","Chukotavia",
  "Cielos","Cimber","Cirrus","Citilink",
  
  c("City Airline","City Airlines"),
  
  c("City Airways"),
  
  c("City Star","City Star Airlines"),
  "CityJet",
  "Citywing","Civil",
  
  c("Click","Click Mexicana","Clickair"),
  "Club",
  c("CM Airlines","CM"),
  "Coast Air","Cobalt","Cobham",
  "Coco South Sudan",
  "Colgan","Comair",
  c("CommutAir","Commut","Commute"),
  "Compagnie Aerienne du Mali",
  "Compagnie Africaine Aviation",
  "Compass","Condor",
  "Congo","ConocoPhillips",
  "Conquest Cargo",
  c("Contact Air","Contact"),
  "Continental","Continentavia","Contour",
  "Contract Air Cargo","Conviasa",
  c("Copa","COPA"),
  "Corendon",
  "Corendon Airlines","Corporate Airlines","Corpflite",
  "Corsair",
  "Cosmos Air Cargo",
  "Costa",
  c("Cote d'Ivoire","Cote Ivoire","Ivoire"),
  "Courtesy Air",
  c("Air Corsica","Corsica"),
  "Creebec",
  "Crossair",
  c("Croatia Airlines","Croatia"),
  "Cubana","Cyprus",
  c("Czech Airlines","CSA"),
  
  # D
  c("Aerovias DAP","DAP"),
  
  "Daallo",
  c("Daghestan","Dagestan"),
  "Daily","Dana","DANA",
  "Danish",
  "Danish Air Transport","Danu",
  "Danube Wings","Darwin","DAT",
  "Deer Air",
  "Delavia",
  c("Delta Airlines","Delta"),
  c("Denim Air","Denim"),
  "Deraya","Deta Air","DHL","Discover","Dniproavia","Dolomiti",
  "Donavia","Donbassaero","Dragon","Dragonair","Druk","Druk Air",
  c("Dutch Antilles Express","DAE"),
  "Divi Divi",
  "Djibouti",
  c("Dnieproavia","Dniprovia"),
  "Donghai",
  c("Doren Congo","Doren"),
  "Drukair",
  "Dynamic","Dynamic Airlines",
  
  # E
  c("Eagle","Eagle Air"),
  "East African Express",
  c("East Air","East"),
  "Eastar","Eastern","Easyfly",
  c("Eastok Avia","Eastok"),
  "EasyJet","EasySky","EastSky","EAT","Ecojet","Edelweiss",
  c("Egypt Air","Egypt","Egyptair","EgyptAir"),
  "Egypt Express",
  c("El Al","Elal"),
  "El Magal","Electra","Elite","Ellin","Elytra","Emerald",
  "Emirates",
  "Empire Airlines",
  "Endeavor","Enerjet","Enter","EOS","Evelop","Envoy","Era","Estelar","Estonian","Equa","Ernest","ETF",
  c("Ethiopian Airlines","Ethiopian","Ethopian"),
  "Etihad",
  c("Euro Airlines","Euro"),
  c("Euro Atlantic","Euroatlantic"),
  "Eurofly","Eurolot","Europa","Europe Airpost",
  "Eurocypria",
  c("European Air Charter","European Charter"),
  "European Air Transport",
  "Eurowings",
  c("Eva","EVA"),
  "Evergreen","Everjet","Everts","Exin","Exploits Valley",
  c("Express Airways","Express"),
  "Exploit Valley",
  c("Expressjet","ExpressJet"),
  "EZ",
  
  # F
  "Fair Aviation","Falcon Air Express","Falcon","Far Eastern","Farnair",
  c("Fastjet","Fast Congo"),
  "FAT","Favori",
  c("Federal Express","FedEx"),
  "Feeder","Felix Airways","Field","Fiji","Field","Filair",
  "Finnair","Finncomm","Firefly","First","Flair","Flamenco","Flash Air",
  "Fleet","Flightline","Flugfelag",
  "Fly Africa","Fly540","Fly 365","Fly Air41","Fly Jamaica","Fly Jordan",
  "Fly One","Fly2Sky","Flyant","FlyArystan","Flybaboo","Flydamas","Flyme","Flymna",
  c("Flybe","FlyBe","FlyBE"),
  "Flybondi","Flydubai","FlyDubai",
  c("Flyegypt","FlyEgypt"),
  "FlyGeorgia","Flyglobespan",
  c("Flynas","FlyNAS"),
  "Flysafair","Four Star Cargo",
  "Freebird","Freedom","Freeflight",
  "Freight Runners",
  "French Bee","Frontier","Fuji Dream",
  "Futura","Fuzhou",
  
  ## G
  c("Galapagos","Galapagous"),
  "Garuda",
  c("Gazprom","Gazpromavia"),
  "GB",
  "Georgian","German","German Airways","Germania","Germanwings",
  "Gestair","Getjet","Ghana International",
  c("Global Air","Global"),
"Global Aviation",
"Global Airlift",
"Global Supply",
"Globus","GMG",
  "Go2Sky",
  c("GoAir","Go","Go!"),
"Gofirst",
  c("Gojet","GoJet"),
  c("Gol","GOL"),
  c("Golden","Golden Air"),
  c("Goma","Gomair"),
"GR-Avia","Grand Cru",
c("Great Lakes","Great Business Lakes"),
c("Great Wall","Great Wall Airlines"),
"Green Africa","Greenland","Grodo",
  c("Grozny Avia","Grozny","Groznyavia"),
"Guanghui","Guicango",
  c("Gulf","Gulf Air"),

# H
c("Hapag-Lloyd Flug","Hapag Fly","Hapag Llyod","Hapag Lloyd"),
  c("Hainan","Hainan Airlines"),
"Halla",
c("Hamburg","Hamburg International"),
"Hawaiian",
c("Hawkair","Hawk"),
"Hebei","Helios Airways","Hello","Helvetic","Hemus","Henan Airlines","Hermes","Heston",
  c("Hevilift","Hevilift PNG"),
"Hewa Bora","Hi Air","Hifly","HiFly","HiSky","HK Express","Hokkaido","Hola Airlines",
  c("Hongkong Airlines","Hong Kong"),
  c("Hop","HOP","Hop!"),
  c("Horizon","Horizon Air","Horizont"),
"Hunnu","Hydro-Quebec",

# I
"iAero","IBC","Iberia","Iberworld","Ibex","Ibom","ICAR","Icaro",
  c("Icelandair","Iceland","ICE","Icelandic"),
  "ICON","IFL",
  c("iFly","IFly","I-Fly"),
  "Ikar","Imperial Air Cargo","Incheon",
c("Indigo","IndiGo"),
"Indus","Innu Mikun","Insel","Inter Iles",
c("InterCaribbean","Intercaribbean"),
"Interjet","Interlink","Intersky","Inuit","IrAero","Iran","Iran Airtours","Iraqi","IRS",
"ISD","Island Air Transport","Island","Islena Airlines","Israir","ITA",
  
  c("Itali Airlines","ItAli Airlines"),
  "Itapemirim","Itek Air","Izhavia","Izmir Airlines","JAC","Jade Cargo","JAL","Jambo",
c("Japan Airlines","Japan","J-Air"),
c("JAT Airways","JAT"),
"Jayawijaya","Jazeera","Jazz","Jeju","Jet Airways","Jet Time","Jet Time Finland",
  
  c("jet2","Jet2"),
  c("Jet4You","Jetairfly"),
  c("Jetblue","Jeblue"),
  c("Jet Connect","Jetconnect"),
"Jetgo","JetKonnect","Jetlink","Jetlite","Jet One Express","Jet2.com","Jetair",
"Jetsmart","Jetstar","Jett8","Jettime","Jetways","JetX","Jiangxi","Jin","Jinnah","Jonika","Joon","Jordan","Jota",
c("JoyAir","Joy"),
"JS Air","JSX","Ju-Air",
c("Juba Air Cargo","Juba"),
"Jubba","Jump","Juneyao",
  
  # K
  c("K-Mile Asia","K-MILE"),
  "Kabo","Kalitta","Kalstar","Kam Air","Kapo","Kargo Express","Karthago","Karun","Kasai","Katekavia","KayaAirlines",
"Kaz Trans","Kazan","KBZ","KD Avia","Keewatin","Kelowna","Kenn Borek","Kenya",
  "Key Lime","Kestrel","Khabarovsk","Khors","Kin Avia","Kingfisher","Kirov Air","Kish","Kish Air","Klasjet","KLM",
c("KMV Avia","KMV"),
"Kolavia","Komiaviatrans","Koral Blue",
  c("Korean","Korean Air","Korean Airlines"),
  "Koryo","Kosmos",
c("Krasavia","Krasair"),
"Kuban",
c("Kulula","Kulula.com"),
"Kunming","Kuwait",
c("Kyrgyzstan Airlines","Kyrgyzstan","Kyrgyz"),
  
  # L
  
  "LACSA","LAM",
c("LaMia Airlines","LAMINA","LAMIA Bolivia"),
"LAN",
c("Lanco","LANCO"),
"Lanhsa","Lao","Laser",
c("Lineas Aereas Suramericanas","LAS"),
  "LATAM",
c("Lauda Air","LaudaAir","Lauda","Lauda Europe","Laudamotion"),
"LC Busre",
c("Latin American Wings","LAW"),
  "LC Peru","LeAir","Lease Fly","Leisure Egypt","Level",
c("Luftfahrtgesellschaft Walter","LG Walter","LGW"),
c("LIAT","LIAT20"),
c("Libyan","Libya"),
"Libyan Arab","Lift","Link",
  c("Lion","Lionair"),
  "Linea Aerea Puertorriquena",
c("Links Air","Links","Linksair"),
"Livingston",
c("Lloyd Aereo","Llyod Aero","Llyod Aereo"),
c("Logan Air","Logain Air","Logan","Loganair"),
"London Executive","Longtail","LOT","Lotus Air","LTE","LTU","Lucky",
  c("Lufthansa","Lufthansa Cityline","Lufthana Cityline","LH Cityline"),
  "Lufttransport","Lulutai",
  c("Lux","Luxair"),
  "Luxwing","Lynden","Lynx",
  
  
  # M
  "MacAir","Macau","Maersk","Magnicharters","Mahan","MAIS","Malaysia","Malaysian",
  c("Maldivian","Maldivian Air Taxi"),
  "Maleth","Malev","Malindo",
  c("Malmo Aviation","Malmo","Malmoe","Malmo Aviation","Malmoe Aviation"),
  "Malta","Malu",
  c("Mandala","Mandala Airlines"),
  "Mandarin","Mango","Manta","Manaus Aerotaxi","Mandalay","Manx",c("Manx2.com","Manx2"),
c("MapJet","MAP"),
c("Marabu","Marabu/Nordica"),
"Marathon",
  c("Martinair","Martin"),
"Mars","MAS Cargo","MASWings","Mauritania","Mauritius","Mavi Gok","Max","Maximus","MEA",
c("MedView","Med-View"),
"Medallion", "Medavia",
c("Mediterranean","Mediterranee"),
"Mega","Mel","Meraj",
c("Meridian","Meridiana"),
  "Merpati","Mesa",
  c("Mesaba","Mesaba Airlines"),
"Metrojet","Mexicana","MHS","Miami",
c("Miat","MIAT"),
c("Mid Airlines","MidAirlines"),
c("Middle East","Mid East"),
"Midwest","Mihin Lanka","Military","Miniliner","Mistral",
  c("MNG","MNG Airlines"),
  "Mocambique Expresso","Modern Logistics",
c("Moldavian","Moldova"),
"Mombasa Air Safari","Monarch","Mongolia","Mont Gabaon","Montenegro","Montserrat","Mordovia",
c("Morning Star","Morningstar"),
"Moskovia",
c("Motor-Sich","Motor Sich"),
"Mount Cook","Murray","Mwant",
"My Indo","My Freighter","My Travel","MyAir","MyCargo","MyWay",
c("Myanma","Myanmar","Myanmar National"),
  
  # N
  
"NAC","Naft",
c("NAM Air","NAM"), "Namibia","NAS Air",
c("FlyNAS","NAS"),
c("National Test Pilot School","NTPS"),
c("National Jet Systems","National Jet"),
"Nationale","Nationwide","Nauru","Naysa","Nelson","Neos",
  c("Nepal","Nepal Airlines"),
  "Nesma",
c("Network Aviation","Network Australia","Network"),
"NewGen","Nextjet","Niki","Nile","Nimbus","Niugini","NOAR",
  c("Nok","NOK"),
  "Nolinor","Nomad","Nordavia",
c("Nordic","Nordic Regional"),
"Nordica","Nordstar","Nordwind","Norra","NORRA",
c("Norse Atlantic","Norse Atlantic Airways"),
"North American","North Cariboo","Northern Air Cargo","Northern Cargo",
  
c("Air North","North"),
"North Flying","North Star",
  c("Northwest","NorthWest"),
  "Northwestern",
  c("Norwegian","Norwegian Air Shuttle"),
"Nostrum",
c("Nouvelair","Nouvel"),
"Nouvelle Gabon","Nusantara",
c("NyxAir","Nyx"),
c("Nova","Novair"),
  
  # O
  c("Ocean Air","OceanAir"),
c("Off We Go","OWG"),
  "Okay","OLT",
  c("Olympic","Olympic Air","Olympic Airlines"),
"Olympus",
  c("Oman","Oman Air"),
  c("Omni","Omni Air"),
  "One","Onur",
  c("Open Skies","Openskies","Open Skies France"),
  "Orange2fly","Orbest",
c("Orenair","Oren"),
"Orenburzhye",
"Orient Thai",
  c("Oriental Air Bridge","Oriental Bridge"),
  "Overland","Ozjet",
"Pacific Aircorp",
"Pacific Blue","Pacific Coastal","Pacific Sun","PAK West",
c("Pakistan International","PIA","Pakistan International Airlines"),
"Panama","Pantanal","Paramount","Pascan","Passaredo","Pawa Dominicana","Peace","Peach",
"Pegas","Pegasus","Pelitta","Pen","Penair","PenAir","Perimeter","Peruvian","PGA",
c("Philippine Airlines","Philippine","PAL","Philippines"),
c("Phuket Airlines","Phuket","Phuket Air"),
"PalmAir",
c("Pamir Airways","Pamir"),
c("PAN","Pan"),
c("Pel Air","Pel"),
c("Pelita Air","Pelita"),
c("People's Viennaline","People's"),
"Petro","Petropavlovsk-Kamchatsky","Piedmont","Pinnacle","Pionair","Pioneer Regional",
"Play","Pluna","Plus Ultra","PNG","Pobeda","Polar",
"Polet","Polish Air Force","Polynesia Blue","Porter","Portugalia","Pouya","Pratt&Whitney","Precision","Presidential","Primera",
c("Prince Edward Air","Prince Edward"),
"PrivatAir","Private Wings","Privilege","Proflight Zambia","Progress TsSKB","Provincial","Protocole",
c("PSA","PSA Airlines"),
c("Pskovavia","Pskov"),
"Pullmantur","PVL",

# Q
"Qanot",
  c("Qantas","Qantaslink","Qantas'"),
  c("Qatar Airways","Qatar","Quatar Airways"),
  "Qazaq",
c("Qeshm","Queshm"),

# R
"R'Komor",
"Rada",
"RAF",
c("RAF-Avia","RafAvia"),
"RAK","RAM",
"Rano",
"Red Air",
  "Red Sea","Red Wings","Regent","Regional","Regional 1","Regional CAE",
  
"Renegade",
  c("Republic","Republic Airline","Republic Airlines"),
  
  c("Rex","REX"),
  
  "Riau","Rico Linhas","Rimbun","Rio Cargo","Rio Linhas","Romavia","Rossiya","Rotana","Rovos Rail",
"Rouge","Royal",
c("Royal Air Maroc","RAM"),
"Royal Brunei","Royal Jordanian","Rudufu","Ruili","Rusair","Rusline","Rutaca",
c("Rwandair","Rwanda"),
"Ryan Int",
"Ryan Services",
  
  c("Ryanair","RyanAir","Ryan"),
  
# S
  c("S7","Siberia Airlines","Siberia"),
"Sabang Merauke Raya",
  
  
"Saeta",
  "Safair","Safarilink",

c("Safari Express Cargo","Safari Express","Safari"),
  
"Safe","Safi",

  c("Saga","Saga Airlines"),
  c("Saha","Saha Airlines"),
  
"Sakhalinsk",
  c("Salaam","Salam"),
  
"Sales & Services",
"Sam","Samair","Sanga","Santa",

  "San Marino","Santa Barbara","Santa SSLH",
"SARAT",
  
  c("Saratov","Saratov Airlines"),
  
  "Saravia","SARPA",
  
  c("SAS","Scandinavian"),
"Sasca",
  
  c("Sat","SAT"),
  c("Sata","SATA"),
  
  "Satena",
  
  c("Saudi","Saudia"),
  
  "Saurya","SBA","Scat","SCAT","Scoot",
c("Scot Airways","Scot"),
"Seaborne","SEAir","Senegal","Seoul","Sepahan","Serbia",
  "Serene","Serve","Services Air","Severstal","Seychelles","Shaheen","Shan Xi Airlines","Shandong",
  
  c("Shanghai","Shanghai Airlines"),
"Shovkoviy",
  
  "Shenzhen","Shree",
  
  c("Shuttle","Shuttle America"),
  
  "Sial",
  
  
  "Sibir",
  
  c("Sichuan","Sichuan Airlines"),
"SILA",
c("Silk Way","Silkway West"),
c("Silkair","Silk"),
"Silver","Silverjet","Silverstone","Simrik","Sindbard",
  
  "Singapore",
  "Sirius",
  "Sita","SJL",
c("Skippers Aviation","Skippers"),
"Sky",
"Skyairlines",
"SkyAirWorld",
"SkyBahamas","Skybus","Skyexpress","Skygreece","Skyhigh","Skyjet","SkyKing",
c("SkyLease","SkyLease Cargo"),
"Skyline",
"Skymark","Skynet Asia","Skyservice","Skytraders","Skytrans","SkyUp","Skyward","Skyway Enterprises",
"Skyworks",
  "Skyways",
  
  c("Skywest","SkyWest"),
  
  "Skywork","Small Planet","Smartlynx","Smartwings","Sol","Solaseed","Solomon","Somon",
  
  c("South African","SAA","SA Airlink","SA Express","SAX"), 
  
  "South Airlines",
  "South Sudan Supreme","South Supreme","South West Aviation","Southern Air",
  
  c("Southwest","SouthWest"),
  
  "Spanair",
  
  c("Spicejet","SpiceJet"),
  c("Spirit","Spirit Airlines"),
  c("Spring","Spring Airlines"),

  "Sprint",
  
  c("Sri Lankan","Srilankan","SriLankan","SriLankan Airlines"),
  c("Sriwijaya","Sriwijaya Air"),
  c("Star Air Freight","Star Freight"),
  
  "Star Flyer","Star Peru","Starbow","Starlux","Stobart","Strategic","Sudan","Sukhoi","Summit","Summit Air","Sun Country","Sun Express","Sunclass","Sundair","Sunday",
  
  c("Sunexpress","SunExpress"),
  
  "Sunstate","Sunwest","Sunwing",
  "Suramericanas","Surinam",
  
  c("Swift","Swiftair"),
  
  "Swiss","Swoop","Syrian Arab","TAAG","Taban","TACA","TACV","Taftan","TAG",
  
  c("Tahiti","Tahiti Nui"),
  
  "Tailwind","Tajik","TAM","TAME","Tanzania","TAP",
  
  c("Tara","Tara Air"),
  c("Tarco","Tarco Airlines"),
  c("Tarom","TAROM"),
  
  "Tasman Cargo","Tassili","Tatarstan",
  
  c("Thai","Thai Airways"),
  
  "Thomas Cook",
  
  c("Thomson","Thomson Airways","Thomsonfly","ThomsonFly"),
  
  c("Turkish","THY"),
  
  "Tianjin","Tibet",
  
  c("Tiger","Tiger Airways"),
  
  "Tindi","Titan","TMA","TNT","Tomsk","Tomskavia","Total","Tracep","Trade","Trans Capital","Trans Maldivian","Trans States","Transaero",
  
  c("Transasia","TransAsia"),
  
  "Transat","Transavia","Transcarga","Transport International","Transwest","Travel Service","Trigana","TriMG",
  
  c("Trip","TRIP"),
  "TSM",
  
  c("TUI","TUI Belgium","TUI Nederland","Tuifly","TuiFly","TUIfly","TUIFly"),
  
  "Tulpar",
  c("Tunis","Tunis Air","Tunisair"),
  
  "Turkmenistan","Tway","UIA",
  "Ukraine",
  
  c("ULS","ULS Cargo"),
  
  
  "Ultimate",
  
  c("Uni","Uni Airways"),
  
  "United",
  
  "Up","UPS",
  
  c("Ural","Ural Airlines"),
  "Urga","US Airways","USAF",
  c("USA Jet","USA Jet Airlines"),
  
  c("UTair","UTAir"),
  "UVT","Uzbekistan",
  
  "V Australia","Valuejet","Van","Vanilla","Vanuatu","VARA","Varesh","Venezolana","Veteran","Via","Vietjet","VietJet","Vietnam",
  "Viking",
  
  c("Vim","VIM"),
  c("Virgin","Virgin Atlantic"),
  "Vision","Vistara","Viva",
  
  c("VivaAerobus","VivaAeroBus"),
  
  "VivaColombia",
  
  c("Vladivostok","Vladivostok Air"),
  
  "Vladivostok Avia",
  
  "VLM","Voepass","VoePass","Volaris","Volga Dnepr","Volotea","Vostok",
  
  "Voyager","Voyageur",
  
  "Vueling",
  
  "Vulkan","Walter","Wasaya","WDL",
  "Webjet","West Air","West Atlantic","West Sweden","West Wind","Western Air","Western Global",
  
  c("Westjet","WestJet"),
  
  "White","Wideroe",
  
  c("Wind Jet","Windjet"),
  
  c("Wind Rose","Windrose"),
  "Wings","Wisconsin",
  c("Wizz Air","Wizz","Wizz Ukraine","Wizzair"),
  
  "World Airways","World Atlantic","World2Fly","WOW","Xfly",
  
  c("Xiamen","Xiamen Airlines"),
  
  c("XL Airways","XL France"),
  "Xtra","Yakutia","Yamal","Yan","Yanair","Yas Air","Yemenia","Yeti",
  
  # Z
  "Zagros",
  c("Zest","Zest Air"),
  "Zhejiang Loong",
  
  
  "Zimbabwe","Zimex",
  c("Zoom Airlines","Zoom UK")
)
