#' fars_read
#'
#' A function for reading in FARS data, it checks if a given file exists.
#' If it does then it reads it in using the read_csv function.
#' If it does not then it returns 'does not exist'
#'
#' @param filename A character string giving the location of the file
#'
#' @return a data frame as read by read_csv with default options.
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' @export
fars_read <- function(filename) {

  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' make_filename
#'
#' This function takes a year as a character string, coerces it to an integer, then uses sprintf to generate a filename for one of the FARS datasets.
#' This function has no input sanitation and will allow any year string and any character.
#'
#' @param year a character string defining a year
#'
#' @return returns a string in the format accident_YYYY.csv.bz2
#' @export
#'
#' @examples
#' make_filename("2013")
make_filename <- function(year) {

  year <-
    as.integer(year)

  #data is downloaded to the data subdirectory by 'get_fars_data', so insert this.
  fname <-
    file.path(
      "data",
      sprintf("accident_%d.csv.bz2", year))

  return(fname)
}

#' fars_read_years
#'
#' A wrapper function for applying the `fars_read` & `make_filename` to read multiple years of data using lapply.
#' If an invalid year string is provided it will return an error
#'
#' @param years A list of character vectors
#'
#' @return returns a list containing data (or an error) for each of the years passed in.
#' @export
#'
#' @examples
#' years <- c("2013","2014")
#' fars_read_years(years)
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' fars_summarize_years
#'
#' A summary function to display the number of rows of data for each month of each year.
#'
#' @param years a list of years (strings) to summarise data for.
#'
#' @return a tibble of the number of datapoints with columns for each year and rows for each month
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' years <- c("2013","2014")
#' fars_summarize_years(years)
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' fars_map_state
#'
#' A function which uses the maps package to plot the locations accidents in the FARS dataset for a given year and state.
#' This function will return an error if the provided state number is invalid,
#' or if there are no accidents in the dataset for the given state and year combination.
#'
#' Accidents with a longitude >900 or a latitude >90 have their lat/long set to NA
#'
#' @param state.num the number of the state in the FARS data - coerced to an integer
#' @param year the year for which data is read
#'
#' @return a map of the state, with accident locations plottec
#' @export
#'
#' @examples
#' fars_map_state('51','2014')
#'
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
