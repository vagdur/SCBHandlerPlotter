SCBdata <- new.env()

SCBdata$boendeform <- read.csv("SCB/SCB_boendeform.csv",stringsAsFactors = TRUE, fileEncoding="UTF8")
SCBdata$resultatrakning <- read.csv("SCB/SCB_resultatrakning.csv",stringsAsFactors = TRUE, fileEncoding="UTF8")
SCBdata$markanvandning <- read.csv("SCB/SCB_markanvandning.csv",stringsAsFactors = TRUE, fileEncoding="UTF8")
SCBdata$civilstand <- read.csv("SCB/SCB_civilstand.csv",stringsAsFactors = TRUE, fileEncoding="UTF8")
SCBdata$utbildningsniva <- read.csv("SCB/SCB_utbildningsniva.csv",stringsAsFactors = TRUE, fileEncoding="UTF8")
SCBdata$inkomster <- read.csv("SCB/SCB_inkomster.csv",stringsAsFactors = TRUE, fileEncoding="UTF8")
SCBdata$kommunal_och_regionalskatt <- read.csv("SCB/SCB_kommunal_och_regionalskatt.csv",stringsAsFactors = TRUE, fileEncoding="UTF8")
SCBdata$inrikesvsutrikesfodda <- read.csv("SCB/SCB_inrikesvsutrikesfodda.csv",stringsAsFactors = TRUE, fileEncoding="UTF8")
SCBdata$hushallsstorlek <- read.csv("SCB/SCB_hushallsstorlek.csv",stringsAsFactors = TRUE, fileEncoding="UTF8")
SCBdata$alderkon <- read.csv("SCB/SCB_alderkon.csv",stringsAsFactor=TRUE, fileEncoding="UTF8")
SCBdata$gymnasiebehorighet <- read.csv("SCB/Skolverket_gymnasiebehorighet.csv", fileEncoding="UTF8")


#' Fetch data from SCB tables
#'
#' This function reads data from the collected SCB tables. It infers which table to look at based on which
#' parameters are set and which are not. So there are two types of parameter -- general ones which occur
#' in several tables (Municipality, Gender, Age) and specific ones which occur only in one table. There are
#' also specific parameters to fetch data from a table
#' The function looks for a specific parameter set, and then looks at that table. If any parameter contained
#' in the table -- general or specific -- is not set, it will sum over all possible values of that parameter.
#' So, for example, \code{SCB(Municipality="Stockholm", Age=19, Gender="män", TotalPopulation=TRUE)} will return the
#' number of 19-year-old men in Stockholm, while \code{SCB(Municipality="Stockholm", Gender="män", TotalPopulation=TRUE)}
#' will return the total number of men, since it sums over all possible values of "Age". Similarly, passing a list of
#' parameters will give the sum over all elements of the list, so \code{SCB(Municipality="Stockholm", Age=c(13:19), Gender="män", TotalPopulation=TRUE)}
#' would count the number of teenagers.
#'
#' @param Municipality Municipality - general parameter, occurs in every table.
#' @param Age Age - general parameter, occurs in all tables except land use, tax rate, and financial result
#' @param Gender Gender - general parameter, occurs in all tables except land use, tax rate, and financial result
#' @param BornInCountry Count people born in Sweden. Set to TRUE to count people born in Sweden and FALSE to count foreign-born people. Table also contains Age and Gender.
#' @param HouseholdType Family structure of a household. Specific parameter. Table also contains HousingForm.
#' @param HousingForm Housing type of a household, e.g. rented apartment or house. Table also contains HouseholdType.
#' @param TotalHouseholds
#' @param MaritalStatus
#' @param HouseholdSize
#' @param MunicipalTax
#' @param RegionTax
#' @param LandUseClass
#' @param TotalArea
#' @param MeanIncome
#' @param MedianIncome
#' @param TaxIncome
#' @param SubsidiesAndEqualisation
#' @param Education
#' @param TotalPopulation
#' @param TotalStudents
#' @param HighSchoolEligible
#' @param SchoolOrganiser
#'
#' @examples
#' SCB(Municipality = "Värmdö", Age=24, Gender="kvinnor", MaritalStatus = "skilda")
#' SCB(Municipality = "Värmdö", LandUseClass = "total skogsmark")
#'
#' @return A number fetched from the requested table.
#' @export
SCB <- function(Municipality = NA, # Finns i alla tabeller, i samma format
                Age = NA, Gender = NA, # Dyker upp i flera tabeller, ibland
                                       # med olika nivåer.
                BornInCountry = NA, # För data om inrikes/utrikes födda. TRUE
                                    # ger antal inrikes födda, FALSE ger antal
                                    # utrikes födda.
                HouseholdType = NA, HousingForm = NA, # För boende- och hushållstyps-datan
                TotalHouseholds = NA, # Sätt till TRUE för att få totalt antal hushåll
                MaritalStatus = NA, # För datan om civilstånd - kan samköras med ålder
                                    # och kön
                HouseholdSize = NA, # För data om hushållsstorlekar
                MunicipalTax = NA, RegionTax = NA, # För data om skattesatser
                LandUseClass = NA, # För data om markanvändning
                TotalArea = NA, # Sätt till TRUE för att få total area av givna kommuner
                MeanIncome = NA, MedianIncome = NA, # För data om invånarnas inkomster
                                                    # Sätt till TRUE för denna typ av data.
                TaxIncome = NA, SubsidiesAndEqualisation = NA, # Skatteintäkter och
                                                               # "generella statsbidrag och
                                                               # utjämning". Sätt till TRUE.
                Education = NA, # För data om utbildningsnivå. Kan samköras med ålder
                                # och kön, dock bara uppdelat i femårsklasser.
                TotalPopulation = NA, # Sätt till TRUE för att räkna total befolkning -
                                     # kan delas upp på ålder och på kön. Notera att "100"
                                     # i själva verket är alla åldrar över 99.
                TotalStudents = NA, # Sätt till TRUE för att räkna antal elever som
                                    # avslutade grundskolan
                HighSchoolEligible = NA, # Sätt till TRUE för att få antal elever som
                                        # gick ut grundskolan och blev behöriga till
                                        # gymnasiet. (Ej exakt -- räknas som procent
                                        # gånger antal totalt.)
                SchoolOrganiser = NA # "Kommunal" eller "Enskild" för att filtrera
                                     # på huvudman för skolor.
                ) {
  ## En funktion för att hämta statistik ur SCBs tabeller. (Och en från Skolverket.)
  ## Lämnas en variabel som NA klumpas alla nivåer på den ihop (på relevant sätt
  ## beroende på mode), annars filtreras på den. Det är möjligt att ange flera
  ## nivåer, i vilket fall just de nivåerna klimpas ihop. Vilken tabell vi skall
  ## söka i avgörs beroende på vilka parametrar som satts till NA och inte.

  # Municipality är en gemensam kolumn för alla tabellerna, så den kan vi ordna
  # redan nu:
  if (identical(NA,Municipality)) {
    Municipality = levels(SCBdata$boendeform$region)
  }


  # Hämtar ur alla SCB-tabellerna, så först behöver vi avgöra vilken
  # tabell vi skall hämta datan ur
  if (!identical(NA,BornInCountry) && is.logical(BornInCountry)) {
    # Statistik om inrikes vs. utrikes födda önskas. Notera att
    # parametern måste vara TRUE eller FALSE, för siffror om inrikes
    # respektive utrikes födda. Att klumpa ihop de två hade ju bara gett
    # total befolkning.
    if (identical(NA,Age)) {
      Age <- unique(SCBdata$inrikesvsutrikesfodda$ålder)
    }
    if (identical(NA,Gender)) {
      Gender <- levels(SCBdata$inrikesvsutrikesfodda$kön)
    }
    if (BornInCountry) {
      dat <- subset(SCBdata$inrikesvsutrikesfodda, (födelseregion=="Född i Sverige")&
                     (ålder %in% Age)&(kön %in% Gender)&(region %in% Municipality))
    } else {
      dat <- subset(SCBdata$inrikesvsutrikesfodda, (födelseregion=="Utrikes född")&
                       (ålder %in% Age)&(kön %in% Gender)&(region %in% Municipality))
    }
    return(sum(dat$X2019))
  } else if (!identical(NA, HouseholdType)|!identical(NA,HousingForm)|(!is.na(TotalHouseholds)&TotalHouseholds)) {
    # Statistik om hushållstyp eller boendeform eller bägge samtidigt sökes. Ifall
    # TotalHouseholds är satt till TRUE returneras totalt antal hushåll i kommunerna.
    # Notera att output här är antal hushåll, inte antal individer.
    if (identical(NA,HouseholdType)) {
      HouseholdType <- levels(SCBdata$boendeform$hushållstyp)
    }
    if (identical(NA,HousingForm)) {
      HousingForm <- levels(SCBdata$boendeform$boendeform)
    }
    dat <- subset(SCBdata$boendeform, (region %in% Municipality)&
                               (hushållstyp %in% HouseholdType)&
                               (boendeform %in% HousingForm))
    return(sum(dat$X2019))
  } else if (!identical(NA,MaritalStatus)) {
    if (identical(NA,Age)) {
      Age <- unique(SCBdata$civilstand$ålder)
    }
    if (identical(NA,Gender)) {
      Gender <- levels(SCBdata$civilstand$kön)
    }
    dat <- subset(SCBdata$civilstand, (region %in% Municipality)&
                              (ålder %in% Age)&
                              (kön %in% Gender)&
                              (civilstånd %in% MaritalStatus))
    return(sum(dat$X2019))
  } else if (!identical(NA, HouseholdSize)) {
    # För data om hushållsstorlekar. Notera att denna funktionen
    # ger antal personer som bor i varje hushållsstorlek, inte
    # antal hushåll av varje given storlek.
    dat <- subset(SCBdata$hushallsstorlek, (region %in% Municipality)&
                                   (hushållsstorlek %in% HouseholdSize))
    return(sum(dat$X2019))
  } else if (!is.na(MunicipalTax)&MunicipalTax) {
    # Kommunskatt - vektoriserad, men kan vara förrädisk
    # om man försöker mata in mer än en kommun -- svaren
    # kommer ut i den ordning som kommunerna ligger i tabellen,
    # inte i den ordning man matade in kommunerna.
    SCBdata$kommunal_och_regionalskatt$Skattesats.till.kommun.2021[SCBdata$kommunal_och_regionalskatt$region %in% Municipality]
  } else if (!is.na(RegionTax)&RegionTax) {
    # Regionskatt - förrädisk på samma vis som kommunskatt.
    SCBdata$kommunal_och_regionalskatt$Skattesats.till.region.2021[SCBdata$kommunal_och_regionalskatt$region %in% Municipality]
  } else if (!identical(NA,LandUseClass)) {
    # Data om landanvändning i kommunen. Notera att datan är från 2015.
    dat <- subset(SCBdata$markanvandning, (region %in% Municipality)&
                                  (markanvändningsklass %in% LandUseClass))
    return(sum(dat$X2015))
  } else if (!is.na(TotalArea)&TotalArea) {
    # Plocka ut kommunens totala area.
    LandUseClass <- "total landareal"
    dat <- subset(SCBdata$markanvandning, (region %in% Municipality)&
                                  (markanvändningsklass %in% LandUseClass))
    return(sum(dat$X2015))
  } else if(!is.na(MeanIncome)&MeanIncome) {
    # Genomsnittsinkomst.
    # Om Age är NA returneras medelinkomst för 16+, andra alternativ
    # är "16-19 år", "20-24 år", samt "totalt 20+ år". Mer än en ålderskategori kan
    # inte anges.
    ## TODO: Implementera det nedan:
    # Om man anger mer än en kommun returneras medelinkomsten över de kommunerna
    # - korrekt korrigerad för befolkningsstorlekar.
    if (is.na(Age)) {
      Age <- "totalt 16+ år"
    }
    if (is.na(Gender)||setequal(Gender,c("män","kvinnor"))) {
      Gender <- "totalt"
    }
    dat <- subset(SCBdata$inkomster, (region %in% Municipality)&
                             (ålder %in% Age)&
                             (kön %in% Gender))
    if ((length(Municipality) == 1)&&(length(Age)==1)&&(length(Gender)==1)) {
      return(dat$Medelinkomst..tkr.2018[1])
    } else {
      stop("Ännu inte implementerat att räkna ut medelinkomster över flera kommuner.")
    }
  } else if(!is.na(MedianIncome)&MedianIncome) {
    # Medianinkomst.
    # Om Age är NA returneras medelinkomst för 16+, andra alternativ
    # är "16-19 år", "20-24 år", samt "totalt 20+ år". Mer än en ålderskategori kan
    # inte anges.
    # Går inte att ange mer än en kommun, eftersom medianerna inte kan sammanräknas
    # mellan dem.
    if (is.na(Age)) {
      Age <- "totalt 16+ år"
    }
    if (is.na(Gender)||setequal(Gender,c("män","kvinnor"))) {
      Gender <- "totalt"
    }
    dat <- subset(SCBdata$inkomster, (region %in% Municipality)&
                             (ålder %in% Age)&
                             (kön %in% Gender))
    if ((length(Municipality) == 1)&&(length(Age)==1)&&(length(Gender)==1)) {
      return(dat$Medianinkomst..tkr.2018[1])
    } else {
      stop("Går inte att räkna ut medianinkomst för mer än en kommun.")
    }
  } else if (!is.na(TaxIncome)&TaxIncome) {
    # Skatteinkomster i tusen kronor.
    dat <- subset(SCBdata$resultatrakning, region %in% Municipality,
                  resultaträkningsposter == "skatteintäkter")
    return(sum(dat$X2019))
  } else if (!is.na(SubsidiesAndEqualisation)&SubsidiesAndEqualisation) {
    # Generella statsbidrag och utjämning.
    dat <- subset(SCBdata$resultatrakning, region %in% Municipality,
                  resultaträkningsposter == "generella statsbidrag och utjämning")
    return(sum(dat$X2019))
  } else if (!identical(NA, Education)) {
    # Utbildningsnivå.
    # Kan samköras med ålder och kön, dock är ålder bara uppdelat i femårsklasser.
    if (identical(NA, Age)) {
      Age <- levels(SCBdata$utbildningsniva$ålder)
    }
    if (identical(NA, Gender)) {
      Gender <- levels(SCBdata$utbildningsniva$kön)
    }
    dat <- subset(SCBdata$utbildningsniva, (region %in% Municipality)&
                                   (ålder %in% Age)&
                                   (kön %in% Gender)&
                                   (utbildningsnivå %in% Education))
    return(sum(dat$X2019))
  } else if (!identical(NA,TotalPopulation)&TotalPopulation) {
    # Total befolkning i kommunerna. Kan uppdelas efter ålder
    # och kön.
    if (identical(NA, Age)) {
      Age <- unique(SCBdata$alderkon$ålder)
    }
    if (identical(NA, Gender)) {
      Gender <- levels(SCBdata$alderkon$kön)
    }
    dat <- subset(SCBdata$alderkon, (region %in% Municipality)&
                            (ålder %in% Age)&
                            (kön %in% Gender))
    return(sum(dat$X2019))
  } else if (!identical(NA, TotalStudents)&TotalStudents) {
    if (is.na(SchoolOrganiser)) {
      SchoolOrganiser <- "Samtliga"
    }
    if (is.na(Gender)) {
      Gender <- "Samtliga"
    }
    # Skolverket hanterar så klart inte "män" och "kvinnor" utan "Pojkar"
    # och "Flickor". För enkelhetens skull kan vi fixa detta.
    if (Gender == "män") {
      Gender <- "Pojkar"
    }
    if (Gender == "kvinnor") {
      Gender <- "Flickor"
    }

    dat <- subset(SCBdata$gymnasiebehorighet, (Kön %in% Gender) &
                                      (Huvudman %in% SchoolOrganiser) &
                                      (Kommun %in% Municipality))
    return(sum(dat$AntalElever))
  } else if (!identical(NA, HighSchoolEligible)&HighSchoolEligible) {
    if (is.na(SchoolOrganiser)) {
      SchoolOrganiser <- "Samtliga"
    }
    if (is.na(Gender)) {
      Gender <- "Samtliga"
    }
    # Skolverket hanterar så klart inte "män" och "kvinnor" utan "Pojkar"
    # och "Flickor". För enkelhetens skull kan vi fixa detta.
    if (Gender == "män") {
      Gender <- "Pojkar"
    }
    if (Gender == "kvinnor") {
      Gender <- "Flickor"
    }

    dat <- subset(SCBdata$gymnasiebehorighet, (Kön %in% Gender) &
                    (Huvudman %in% SchoolOrganiser) &
                    (Kommun %in% Municipality))

    return(sum(dat$AntalElever*(1-dat$AndelObehöriga/100)))
  } else {
    stop("Kan inte avgöra vad du letar efter för data.")
  }
}

VectorSCB <- function(VectorVar = "Municipality",
                      Municipality = NA,
                      Age = NA, Gender = NA,
                      BornInCountry = NA,
                      HouseholdType = NA, HousingForm = NA,
                      TotalHouseholds = NA,
                      MaritalStatus = NA,
                      HouseholdSize = NA,
                      MunicipalTax = NA, RegionTax = NA,
                      LandUseClass = NA,
                      TotalArea = NA,
                      MeanIncome = NA, MedianIncome = NA,
                      TaxIncome = NA, SubsidiesAndEqualisation = NA,
                      Education = NA,
                      TotalPopulation = NA,
                      TotalStudents = NA,
                      HighSchoolEligible = NA,
                      SchoolOrganiser = NA) {
  if (VectorVar == "Municipality") {
    if (identical(NA,Municipality)) {
      Municipality <- levels(SCBdata$alderkon$region)
    }
    retval <- sapply(Municipality, function(mun) SCB(Municipality = mun, Age, Gender,
                                                     BornInCountry, HouseholdType, HousingForm,
                                                     TotalHouseholds, MaritalStatus, HouseholdSize,
                                                     MunicipalTax, RegionTax, LandUseClass,
                                                     TotalArea, MeanIncome, MedianIncome,
                                                     TaxIncome, SubsidiesAndEqualisation,
                                                     Education, TotalPopulation, TotalStudents,
                                                     HighSchoolEligible, SchoolOrganiser))
    return(retval)
  }
}
