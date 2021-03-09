
#' Get the Aggregated daily nominations for a gasday.
#'
#' @export
#'
gassco_noms <- function(){

  # read feed, convert it to a list and get the feed data.
  gs_nom <- xml2::read_xml("https://umm.gassco.no/realTimeAtom.xml")
  gs_nom <- xml2::as_list(gs_nom)
  gs_nom <- gs_nom$feed

  # subset to entries
  gs_nom <- gs_nom[names(gs_nom) == "entry"]

  # convert list to a data.frame.
  gs_nom <- lapply(gs_nom,
                   function(x){
                     data.frame(
                       title = x$title[[1]],
                       author_name = x$author$name[[1]],
                       id = x$id[[1]],
                       updated = x$updated[[1]],
                       content = x$content[[1]],
                       stringsAsFactors = FALSE
                       )
                   })
  gs_nom <- dplyr::bind_rows(gs_nom)

  # fix types.
  gs_nom$updated <- lubridate::ymd_hms(gs_nom$updated)
  gs_nom$content <- as.numeric(gs_nom$content)

  # convert to tibble for pretty print
  gs_nom <- tibble::as_tibble(gs_nom)

  gs_nom
}
