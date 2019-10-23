#' Routine d'interrogation de la banque hydro pour obtenir une chronique de données pour une station de mesure.
#' Cette routine interroge la banque hydro autant de fois que nécessaire pour compléter la chronique demandée.
#'
#' @param station Code de la station
#' @param DateHeureDeb Date de début d'événement au format string "JJ/MM/AAAA HH:MM:SS"
#' @param DateHeureFin Date de fin d'événement au format string "JJ/MM/AAAA HH:MM:SS"
#' @param procedure Variable à importer : "QTFIX", QTVAR", "H-TEMPS" (QTVAR par défaut)
#' @param url URL de la banque hydro (http://www.hydro.eaufrance.fr par défaut)
#'
#' @return dataframe contenant le tableau produit sur la page de visualisation de la chronique de la banque hydro
#'
#' @example df<-rbanqhydro.get("Y3204010","16/03/2017 00:00", "05/04/2017 23:59")
#'
#' @author David Dorchies david.dorchies@irstea.fr
#' @date 13/04/2017 - 15/03/2019
rbanqhydro.get <- function (station, DateHeureDeb, DateHeureFin, procedure = "QTVAR", url="http://www.hydro.eaufrance.fr") {

  PackageRequire("httr")

  # Formulaire de sélection des stations
  form0<- list(
    cmd = "filtrer",
    consulte = "rechercher",
    code_station = "",
    cours_d_eau = "Lez",
    commune = "",
    departement = "",
    bassin_hydrographique = "",
    station_en_service = "1",
    station_hydrologique = "1",
    btnValider = "Visualiser"
  )
  form0[["station[]"]] = station

  url.selection = paste(url,"selection.php", sep = "/")

  res <- POST(
    url.selection,
    body = form0, encode = "form", verbose()
  )

  # Formulaire de sélection de la variable
  form1 <- list(
    categorie = "rechercher",
    procedure = procedure
  )
  form1[["station[]"]] =  station

  url.procedure = paste(url,"presentation/procedure.php", sep = "/")
  res <- POST(
    url.procedure,
    body = form1, encode = "form", verbose()
  )

  # Extraction des chroniques (répétition des interrogations)
  DateHeureDeb = as.POSIXct(DateHeureDeb, format = "%d/%m/%Y %H:%M", tz = "UTC")
  DateHeureFin = as.POSIXct(DateHeureFin, format = "%d/%m/%Y %H:%M", tz = "UTC")
  DateHeureDebOld = 0
  df = data.frame(NULL)

  while(DateHeureDeb < DateHeureFin & DateHeureDeb > DateHeureDebOld) {
    DateHeureDebOld = DateHeureDeb
    dfi = rbanquehydro.get.timeserie(url.procedure, procedure, DateHeureDeb, DateHeureFin)
    if(nrow(dfi) > 0) {
      df = rbind(df, dfi)
      DateHeureDeb = tail(dfi,1)$Date + 60 # Last end time + 60 seconds
    }
  }

  return (df)

}

#' Sous routine d'interrogation de la banque hydro pour obtenir une chronique de données pour une station de mesure et une période donnée.
#' Cette routine est appelée à partir de rbanqhydro.get
#'
#' @param url.procedure URL du formulaire d'interrogation de la chronique temporelle
#' @param procedure Variable à importer : "QTFIX", QTVAR", "H-TEMPS" (QTVAR par défaut)
#' @param DateHeureDeb Date de début d'événement au format POSIX
#' @param DateHeureFin Date de fin d'événement au format POSIX
#'
#' @return dataframe contenant le tableau produit sur la page de visualisation de la chronique de la banque hydro
#'
#' @example df<-rbanqhydro.get("Y3204010","16/03/2017 00:00", "05/04/2017 23:59")
#'
#' @author David Dorchies david.dorchies@irstea.fr
#' @date 13/04/2017 - 15/03/2019
rbanquehydro.get.timeserie <- function (url.procedure, procedure, DateHeureDeb, DateHeureFin)  {
  # Formulaire de sélection de la date
  form2 <- list(
    procedure = procedure,
    affichage = 2,
    echelle = 1,
    date1 = format(DateHeureDeb, "%d/%m/%Y"),
    heure1 = format(DateHeureDeb, "%H:%M"),
    date2 = format(DateHeureFin, "%d/%m/%Y"),
    heure2 = format(DateHeureFin, "%H:%M"),
    precision = "00",
    btnValider = "Valider"
  )


  res <- POST(
    url.procedure,
    body = form2, encode = "form", verbose()
  )
  
 
  PackageRequire("XML")
  
  #Check if no data
  pageToRead=content(res, "text", encoding = "iso-8859-1")
  #Ici on regarde sur la page si un message d'erreur est affiché :
  urlLines=0
  urlLines = c(urlLines,grep("Pas\\sde\\sdonnées\\sdisponibles", pageToRead))
  urlLines  = c(urlLines,grep("Aucune\\sdonnée\\sdisponible", pageToRead)) 
  #message("urlLines=",urlLines)
  
  dfi = data.frame(NULL)
  if (sum(urlLines)==0){
      # On récupère le dataframe du 3ème tableau de la page
      dfi = readHTMLTable(
        content(res, type="text/plain", encoding="cp1252"),
        stringsAsFactors = FALSE,
        which = 3
      )
  
      dfi[,"Date"] = as.POSIXct(dfi[,"Date"], format = "%d/%m/%Y %H:%M", tz = "UTC")
   }
  

  return(dfi)
}

#' Création d'un fichier de chronique de débit au format GRP 2018
#'
#' @param station Code de la station
#' @param DateHeureDeb Date de début d'événement au format string "JJ/MM/AAAA HH:MM"
#' @param DateHeureFin Date de fin d'événement au format string "JJ/MM/AAAA HH:MM"
#'
#' @example rbanquehydro.createGRP_Q_file("U2345030", "01/01/2002 00:00", "23/03/2009 23:00", "BDD/BDD_Q/U2345030_Q.txt")
#'
#' @author David Dorchies david.dorchies@irstea.fr
#' @date 15/03/2019
rbanquehydro.createGRP_Q_file <- function(station, DateHeureDeb, DateHeureFin, sFilePath) {

  df = rbanqhydro.get(station, DateHeureDeb, DateHeureFin, procedure, url)
  dfGRP = data.frame(
    AAAAMMJJHHMM = as.numeric(format(df[,"Date"], "%Y%m%d%H%M")),
    Q = as.numeric(df[,2])
  )
  write.table(dfGRP, sFilePath, sep = ";", row.names = FALSE)
}


################################################################################
#' Test la présence d'un package, le télécharge au besoin et le charge.
#' Le programme est stoppé en cas d'échec.
#' @param x Chaîne de caractère avec le nom du package à charger
#' @url http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
#' @date 31/07/2014
################################################################################
PackageRequire <- function(x)
{
  if (!require(x,character.only = TRUE)) {
    install.packages(x,dep=TRUE,repos="http://cran.r-project.org")
  }
  if(!require(x,character.only = TRUE)) {
    stop("Package not found")
  }
}
