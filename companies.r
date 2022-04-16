library(hash)


  
companyColNames <- c("Blue Ribbon Taxi Association Inc."
  ,"Taxi Affiliation Services"
  ,"Taxicab Insurance Agency, LLC"
  ,"Choice Taxi Association"
  ,"Star North Management LLC"
  ,"Top Cab Affiliation"
  ,"Chicago Independents"
  ,"KOAM Taxi Association"
  ,"1085 - 72312 N and W Cab Co"
  ,"Chicago Medallion Management"
  ,"Chicago Carriage Cab Corp"
  ,"Flash Cab"
  ,"Globe Taxi"
  ,"Patriot Taxi Dba Peace Taxi Associat"
  ,"City Service"
  ,"24 Seven Taxi"
  ,"Sun Taxi"
  ,"Medallion Leasin"
  ,"Taxi Affiliation Service Yellow"
  ,"Nova Taxi Affiliation Llc"
  ,"Gold Coast Taxi"
  ,"Chicago Taxicab"
  ,"Blue Diamond"
  ,"Yellow Cab"
  ,"312 Medallion Management Corp"
  ,"Checker Taxi Affiliation"
  ,"5 Star Taxi"
  ,"Metro Jet Taxi A"
  ,"Checker Taxi"
  ,"6742 - 83735 Tasha ride inc"
  ,"Setare Inc"
  ,"American United Taxi Affiliation"
  ,"1469 - 64126 Omar Jada"
  ,"American United"
  ,"6743 - 78771 Luhak Corp"
  ,"Leonard Cab Co"
  ,"4053 - 40193 Adwar H. Nikola"
  ,"3011 - 66308 JBL Cab Inc."
  ,"4623 - 27290 Jay Kim"
  ,"3094 - 24059 G.L.B. Cab Co"
  ,"2092 - 61288 Sbeih company"
  ,"2733 - 74600 Benny Jona"
  ,"6574 - Babylon Express Inc."
  ,"3623 - 72222 Arrington Enterprises"
  ,"Chicago Star Taxicab"
  ,"3721 - Santamaria Express, Alvaro Santamaria"
  ,"5006 - 39261 Salifu Bawa"
  ,"5062 - 34841 Sam Mestas"
  ,"5074 - 54002 Ahzmi Inc"
  ,"3620 - 52292 David K. Cab Corp."
  ,"5874 - 73628 Sergey Cab Corp."
  ,"3591 - 63480 Chuks Cab")


compAbrv <- c("BRTA",
              "TAS",
              "TIA",
              "ChTA",
              "SNM",
              "TCA",
              "CI",
              "KOAM",
              "NWC",
              "CMM",
              "CCCC",
              "FC",
              "GT",
              "PTDPTA",
              "CS",
              "7T",
              "ST",
              "ML",
              "TASY",
              "NTAL",
              "GCT",
              "CT",
              "BD",
              "YC",
              "MMC",
              "CkTA",
              "5ST",
              "MJT",
              "CkT",
              "TsRI",
              "StrI",
              "AUTA",
              "OJ",
              "AmU",
              "LuC",
              "LnCC",
              "AHN",
              "JBL",
              "JyKm",
              "GLB",
              "SbhC",
              "BJ",
              "Exp",
              "AE",
              "CStrT",
              "SantaE",
              "SalB",
              "SMs",
              "AzI",
              "DKCC",
              "SCC",
              "CkCb")
  
  
companies <- c(
  "Blue Ribbon Taxi Association",
  "Taxi Affiliation Services",
  "Taxicab Insurance Agency",
  "Choice Taxi Association ",
  "Star North Management LLC",
  "Top Cab Affiliation",
  "Chicago Independents",
  "KOAM Taxi Association",
  "N and W Cab Co",
  "Chicago Medallion Management",
  "Chicago Carriage Cab Corp",
  "Flash Cab",
  "Globe Taxi",
  "Patriot Taxi Dba Peace Taxi Associat",
  "City Service",
  "Seven Taxi",
  "Sun Taxi",
  "Medallion Leasin",
  "Taxi Affiliation Service Yellow",
  "Nova Taxi Affiliation Llc",
  "Gold Coast Taxi",
  "Chicago Taxicab",
  "Blue Diamond",
  "Yellow Cab",
  "Medallion Management Corp",
  "Checker Taxi Affiliation",
  "Star Taxi",
  "Metro Jet Taxi A",
  "Checker Taxi",
  "Tasha ride inc",
  "Setare Inc",
  "American United Taxi Affiliation",
  "Omar Jada",
  "American United",
  "Luhak Corp",
  "Leonard Cab Co",
  "Adwar H. Nikola",
  "JBL Cab Inc.",
  "Jay Kim",
  "G.L.B. Cab Co",
  "Sbeih company",
  "Benny Jona",
  "Express Inc.",
  "Arrington Enterprises",
  "Chicago Star Taxicab",
  "Santamaria Express, Alvaro Santamaria",
  "Salifu Bawa",
  "Sam Mestas",
  "Ahzmi Inc",
  "David K. Cab Corp.",
  "Sergey Cab Corp.",
  "Chuks Cab"
)

compHash <- hash(companyColNames, seq(length(companies)))

compUnhash <- hash(seq(length(companies)), companyColNames)

companyTable <- data.frame(companies, companyColNames, compAbrv)
