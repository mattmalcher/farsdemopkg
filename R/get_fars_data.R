#' get_fars_data
#'
#' This function downloads the fars data to the local apps directory of the current user (if given permission to do so)
#' Function created to allow vignette to be built
#'
#' @param force
#'
#'
#' @export get_fars_data
#'
#' @examples
#' get_fars_data(force = TRUE)
get_fars_data <- function(force = FALSE,
                          durl = "https://d3c33hcgiwev3.cloudfront.net/_e1adac2a5f05192dc8780f3944feec13_fars_data.zip?Expires=1546300800&Signature=byljyRPskWKpmmyUfzF1hMKln-IdQOoa4vCnVh82WqFBP4HgnTT~3kjr~XysTzFdVuS-6bTvLZ6V3b1jp3Cg4Gq797jTAGqK3NPfi1oRyDGm9MjBerNNIS8MiMcSJqFgInBXDxFXr-SOqFLE9GLUDNKoCPmb3qynYfZRV06FtQU_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"){

  if(!force && interactive()){

    result <-
      select.list(c("Yes", "No"),
                  title = "May this program download data to your working directory?")

  } else {

    result <- "No"

  }

  if(result == "Yes" || force){

    zippath <- file.path(getwd(),"farsdata.zip")

    download.file(
      url = durl,
      destfile = zippath,
      mode = "wb")

    unzip(zipfile = zippath,
          files = "data/",
          exdir = getwd())

  } else {
    warning("fars data not downloaded")
  }

}
