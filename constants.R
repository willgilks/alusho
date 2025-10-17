
# manuf_list=list(
#   "A"="Airbus","AN"="Antonov","ATR"="ATR","AT"="ATR",
#   "ATP"="British Aerospace",
#   "RJ"="British Aerospace",
#   "B"="Boeing",
#   "BLCF"="Boeing",
#   "BCS"="Airbus",
#   "D"="Dornier",
#   "DH"="De Havilland",
#   "DHC"="De Havilland",
#   "E"="Embraer",
#   "CR"="Bombardier",
#   "CRJ"="Bombardier",
#   "CRJX"="Bombardier",
#   "DC"="Basler",
#   "F"="Fokker",
#   "IL"="Ilyushin",
#   "J"="Fairchild-Dornier",
#   "JS"="British Aerospace",
#   "L"="Lockheed",
#   "MD"="McDonnell Douglas",
#   "SF"="Saab",
#   "SH"="Short",
#   "SSLH"="Santa",
#   "SW"="Swearingen")






## (your ac_types vector already defined)
manufacturers=list(
  Airbus=str_subset(ac_types,"^(A(3|2|30|31|32|33|34|35|36|37|38)|Airbus)"),
  Antonov=str_subset(ac_types,"^(AN|An)"),
  ATR=str_subset(ac_types,"^(AT|ATR)"),
  BAe_Avro=str_subset(ac_types,"^(BAe|Avro|RJ)"),
  Bombardier_DeHavilland=str_subset(ac_types,"^(CL|CRJ|DH8|DHC|Q400|Dash8)"),
  Boeing=str_subset(ac_types,"^(B7(0|1|2|3|4|5|6|7|8|9)|B74|B75|B76|B77|B78|B79|B70[7-9]|Boeing)"),
  Cessna_CASA=str_subset(ac_types,"^C(12|13|14|15|17|18|19|20|21|23|24|25|40|41|42|52|208|402|421|525)"),
  McDonnell_Douglas=str_subset(ac_types,"^(DC|MD)"),
  Dornier=str_subset(ac_types,"^(Dornier|DO|D3)"),
  Embraer=str_subset(ac_types,"^(E|ERJ|Embraer)"),
  Fokker=str_subset(ac_types,"^(F(1|2|5|7|okker))"),
  Ilyushin=str_subset(ac_types,"^IL"),
  Jetstream=str_subset(ac_types,"^(JS|Jetstream)"),
  Saab=str_subset(ac_types,"^(Saab|SF|SB)"),
  Shorts=str_subset(ac_types,"^(SH|Shorts)"),
  Sukhoi=str_subset(ac_types,"^SU"),
  Swearingen=str_subset(ac_types,"^SW"),
  Yakovlev=str_subset(ac_types,"^YK")
)

## deduplicate and sort
manufacturers=purrr::map(manufacturers,\(x) sort(unique(x)))



standardised_map = list(
  
  ## --- Airbus family ---
  A20N = c("A320neo", "Airbus A320neo", "A20n"),
  A21N = c("A321neo", "Airbus A321neo"),
  A306 = c("A300-600", "Airbus A300-600", "Airbus A306"),
  A310 = c("A310-300", "Airbus A310-300"),
  A319 = c("A319-100", "Airbus A319-100"),
  A320 = c("A320-200", "Airbus A320-200", "Airbus,A320"),
  A321 = c("A321-200", "Airbus A321-200", "Airbus A321"),
  A330 = c("A330-300", "Airbus A330", "Airbus A330-300"),
  A332 = c("A330-200", "Airbus A330-200"),
  A333 = c("A330-300", "Airbus A330-300"),
  A359 = c("A350-900", "Airbus A350-900"),
  A35K = c("A350-1000", "Airbus A350-1000"),
  A388 = c("A380-800", "Airbus A380-800"),
  
  ## --- Antonov ---
  AN12 = c("AN-12", "Antonov AN-12"),
  AN24 = c("AN-24", "Antonov AN-24"),
  AN26 = c("AN-26", "Antonov AN-26"),
  AN32 = c("AN-32", "Antonov AN-32"),
  AN74 = c("AN-74", "Antonov AN-74"),
  AN140 = c("AN-140", "Antonov AN-140"),
  
  ## --- BAe / Avro ---
  `BAe146` = c("BAe 146", "BAe 146-200", "Avro RJ85", "Avro RJ100"),
  
  ## --- Boeing family ---
  B737 = c("Boeing 737", "Boeing 737-200", "Boeing 737-600", "B737-200", "Boeing 737NG"),
  B738 = c("Boeing 737-800", "Boeing 737NG"),
  B739 = c("Boeing 737-900", "Boeing 737NG"),
  B744 = c("Boeing 747-400", "Boeing 747"),
  B752 = c("Boeing 757-200", "Boeing 757"),
  B763 = c("B767-300", "Boeing 767-300", "Boeing 767"),
  B764 = c("B767-400", "Boeing 767-400"),
  B772 = c("B777-200", "Boeing 777-200", "Boeing 777"),
  B773 = c("B777-300", "Boeing 777-300"),
  B788 = c("B787-8", "Boeing 787-8", "Boeing 787"),
  B789 = c("B787-9", "Boeing 787-9"),
  B78X = c("B787-10", "Boeing 787-10"),
  
  ## --- Bombardier CRJ family ---
  CRJ1 = c("CRJ-100", "Bombardier CRJ-100", "CRJ100"),
  CRJ2 = c("CRJ-200", "Bombardier CRJ-200", "CRJ200"),
  CRJ7 = c("CRJ-700", "Bombardier CRJ-700", "CRJ700"),
  CRJ9 = c("CRJ-900", "Bombardier CRJ-900", "CRJ900"),
  
  ## --- McDonnell Douglas (Douglas DC and MD) ---
  DC10 = c("DC-10", "McDonnell Douglas DC-10"),
  DC9  = c("DC-9", "McDonnell Douglas DC-9"),
  MD11 = c("McDonnell Douglas MD-11"),
  MD81 = c("McDonnell Douglas MD-81"),
  MD82 = c("McDonnell Douglas MD-82"),
  MD83 = c("McDonnell Douglas MD-83"),
  MD88 = c("McDonnell Douglas MD-88"),
  
  ## --- De Havilland / Bombardier Dash 8 ---
  DH8A = c("DHC-8-100", "De Havilland Dash 8-100"),
  DH8B = c("DHC-8-200", "De Havilland Dash 8-200"),
  DH8C = c("DHC-8-300", "De Havilland Dash 8-300"),
  DH8D = c("DHC-8-400", "De Havilland Dash 8-400", "Q400"),
  
  ## --- Fokker ---
  F27  = c("Fokker 27", "F27-500"),
  F50  = c("Fokker 50"),
  F70  = c("Fokker 70"),
  F100 = c("Fokker 100"),
  
  ## --- Shorts ---
  SH36 = c("Shorts 360", "Shorts SD360", "SH360")
)


## 3. Build nested structure: each manufacturer → sublist of aircraft → sub-aliases

manufacturers_nested = purrr::map(manufacturers, \(models) {
  
  ## for each aircraft, always include itself, then any aliases
  sublist = purrr::map(models, \(m) {
    aliases = standardised_map[[m]]
    unique(c(m, aliases))
  })
  
  names(sublist) = models
  sublist
})


manufacturers_nested$Airbus

manufacturer_table =
  purrr::imap_dfr(manufacturers_nested, \(sublist, manufacturer_name) {
    
    purrr::imap_dfr(sublist, \(aliases, aircraft_code) {
      tibble(
        manufacturer = manufacturer_name,
        aircraft_code = aircraft_code,
        alias = aliases
      )
    })
  })

