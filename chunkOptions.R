library(knitr)
# The hooks below add html tags to the code chunks and their output so that they
# are properly formatted when the site is built.

hook_in <- function(x, options) {
  lg <- tolower(options$engine)
  style <- paste0(".language-", lg)
  
  stringr::str_c("\n\n~~~\n",
                 paste0(x, collapse="\n"),
                 "\n~~~\n{: ", style, "}\n\n")
}

hook_out <- function(x, options) {
  x <- gsub("\n$", "", x)
  stringr::str_c("\n\n~~~\n",
                 paste0(x, collapse="\n"),
                 "\n~~~\n{: .output}\n\n")
}

hook_error <- function(x, options) {
  x <- gsub("\n$", "", x)
  stringr::str_c("\n\n~~~\n",
                 paste0(x, collapse="\n"),
                 "\n~~~\n{: .error}\n\n")
}

hook_warning <- function(x, options) {
  x <- gsub("\n$", "", x)
  stringr::str_c("\n\n~~~\n",
                 paste0(x, collapse = "\n"),
                 "\n~~~\n{: .warning}\n\n")
}

knit_hooks$set(source = hook_in, output = hook_out, warning = hook_warning,
               error = hook_error, message = hook_out)