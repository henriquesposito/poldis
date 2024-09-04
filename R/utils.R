# Helper function for checking and downloading packages
thisRequires <- function(pkgname){
  if (!requireNamespace(pkgname, quietly = TRUE)) {
    if(utils::askYesNo(msg = paste("The", pkgname,
                                   "package is required to run this function. Would you like to install", pkgname, "from CRAN?"))) {
      utils::install.packages(pkgname)
    } else {
      stop(paste("Please install", pkgname, "from CRAN to run this function."))
    }
  }
}

# Helper function to get urgency or topics codebook.
open_codebook <- function(codebook = "urgency") {
  thisRequires("cli")
  url <- "https://github.com/henriquesposito/poldis/tree/develop/inst"
  if (codebook == "urgency") {
    tryCatch({
      utils::browseURL(paste0(url, "/urgency_codebook.pdf"),
                       browser = getOption("browser"), encodeIfNeeded = FALSE)
    }, error = function(e) {
      message(paste0("Unable to open codebook, please visit: ",
                     cli::style_hyperlink(paste0(url, "/urgency_codebook.pdf"),
                                          paste0(url, "/urgency_codebook.pdf"))))
    })
  } else if (codebook == "topic") {
    tryCatch({
      utils::browseURL(paste0(url, "/topic.pdf"), browser = getOption("browser"), encodeIfNeeded = FALSE)
    }, error = function(e) {
      message(paste0("Unable to open codebook, please visit: ",
                     cli::style_hyperlink(paste0(url, "/topic.pdf"),
                                          paste0(url, "/topic.pdf"))))
    })
  } else {
    message(paste0("Codebook not found, to see the available codebooks please visit: ",
                   cli::style_hyperlink(url, url)))
  }
}
