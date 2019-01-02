#' get_fars_data
#'
#' This function downloads the fars data to the local apps directory of the current user (if given permission to do so)
#' Function created to allow vignette to be built
#'
#' @param force Flag for use when programming - skips y/n dialogue before downloading file
#' @param durl URL for Coursera FARS data
#'
#' @export get_fars_data
#'
#' @importFrom utils download.file select.list unzip
#'
#' @examples
#' get_fars_data(force = TRUE)
get_fars_data <- function(force = FALSE,
                          durl = paste0("https://d3c33hcgiwev3.cloudfront.net/",
                                        "_e1adac2a5f05192dc8780f3944feec13_fars_data.zip",
                                        "?Expires=1546560000",
                                        "&Signature=gabwk0MIC8yBFZFr75x5P-SbDIu7suFq3gOm1Aef1U4SwQtPUav",
                                        "Em8o3YB107kJcPRZXf2ZPADrcISbSJ4TXnEN725ewt8YPZBajP0LtZCtBTqNA",
                                        "~P5UYbVd620rw4ew~ZnXqoBO1Ofr6hY0LP4Kv5ZEV2TtN75hgRX75JAyCcE_",
                                        "&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A")){

  if(!force && interactive()){

    result <-
      select.list(c("Yes", "No"),
                  title = "May this program download data to your working directory?")

  } else {

    result <- "No"

  }

  if(result == "Yes" || force){

    #generate a temporary directory
    tpath <- tempfile()    # a file
    print(tpath)
    tdir <- dirname(tpath) # enclosing folder


    download.file(
      url = durl,
      destfile = tpath,
      mode = "wb")

    #unzip data file
    unzip(zipfile = tpath,
          exdir = tdir)

    #move the contents from the temporary directory to the working directory
    todir <- getwd()
    print(todir)

    file.copy(from = file.path(tdir, "data") ,
              to = todir,
              recursive = TRUE)

    return()

  } else {
    warning("fars data not downloaded")
  }

}
