

#' Download and parse the gassco umm.
#'
#' Function returns NULL if there are no messages to parse.
#'
#' @param path Path.
#'
#' @export
#'
#' @examples
#'
#' library(gasscor)
#' gassco_outages_parse()
#'
gassco_outages_parse <- function(path = "https://umm.gassco.no/atom.xml"){


  outages_list <- xml2::as_list(xml2::read_xml(x = path, encoding = "UTF-8"))

  outages_list <- outages_list$feed

  if("entry" %in% names(outages_list)){
    outages_list <- outages_list[names(outages_list) == "entry"]
  } else {
    return(NULL)
  }
  outages_list <- lapply(outages_list, gassco_outages_parse_entry)
  outages_df <- dplyr::bind_rows(outages_list)

  outages_df$updated <- lubridate::ymd_hms(outages_df$updated)
  outages_df$event_eventstart <- lubridate::ymd_hms(outages_df$event_eventstart)
  outages_df$event_eventstop <- lubridate::ymd_hms(outages_df$event_eventstop)
  outages_df$publicationdatetime <- lubridate::ymd_hms(outages_df$publicationdatetime)

  outages_df$capacity_unavailablecapacity <- as.numeric(outages_df$capacity_unavailablecapacity)
  outages_df$capacity_availablecapacity <- as.numeric(outages_df$capacity_availablecapacity)
  outages_df$capacity_technicalcapacity <- as.numeric(outages_df$capacity_technicalcapacity)

  outages_df
}


gassco_outages_parse_entry <- function(entry){

  outage_summary <- xml2::read_xml(entry$summary[[1]])
  outage_summary <- xml2::as_list(outage_summary)
  outage_summary <- outage_summary$REMITUrgentMarketMessages$UMM

  outage_summary_bz <- outage_summary[names(outage_summary) == "balancingZone"]
  outage_summary_bz <- paste(unlist(outage_summary_bz), collapse = "|")

  outage_summary <- outage_summary[names(outage_summary) != "balancingZone"]
  outage_summary <- unlist(outage_summary)
  outage_summary_names <- names(outage_summary)
  outage_summary <- as.data.frame(matrix(outage_summary, nrow = 1), stringsAsFactors = FALSE)
  names(outage_summary) <- outage_summary_names

  names(outage_summary) <- tolower(names(outage_summary))
  names(outage_summary) <- stringr::str_replace_all(names(outage_summary), "[.]", "_")

  outage_df <- tibble::tibble(title = entry$title[[1]],
                              entry_link = attr(entry$link, "href"),
                              id = entry$id[[1]],
                              updated = entry$updated[[1]],
                              balancing_zone = outage_summary_bz)
  outage_df <- dplyr::bind_cols(outage_df, outage_summary)

  outage_df
}
