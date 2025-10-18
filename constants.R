

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
