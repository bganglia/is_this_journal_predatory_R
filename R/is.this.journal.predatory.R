#' is.this.journal.predatory: A package for checking whether a journal is legitimate or predatory
#'
#' The is_predatory function, which accepts either the name or URL of a journal, will output 
#' a sentence stating whether the journal is listed in Beall's List of predatory journals
#' (beallslist.net).
#'
#' @docType package
#' @name is.this.journal.predatory
NULL

get_domain <- function(url) {
    url <- gsub("http://", "", url)
    url <- gsub("https://", "", url)
    slash_idx <- gregexpr(pattern="/", url)[[1]]
    if (length(slash_idx) == 0) {
        domain = url
    }
    else {
        domain = substr(url, 0, slash_idx)
    }
    domain
}

matches_domain <- function(predatory_name, text_descriptor)  {
    any(sapply(predatory_name, get_domain) == get_domain(text_descriptor))
}

matches_name <- function(predatory_name, text_descriptor) {
    any(tolower(predatory_name) == tolower(text_descriptor))
}

matches <- function(predatory_info, text_descriptor) {
    matches_domain(predatory_info$url, text_descriptor) | matches_name(predatory_info$name, text_descriptor)
}

format_predatory_string <- function (predatory_info) {
    sprintf(
        "The journal %s at %s is listed as a predatory journal in Beall's List",
        predatory_info$name[1],
        predatory_info$url[1]
    )
}

format_not_predatory <- function(text_descriptor) {
    sprintf(
        "%s is not listed as a predatory journal. To judge for yourself, read more at https://thinkchecksubmit.org/",
         text_descriptor
    )
}

json_path <- "predatory_journals.json"

#predatory_info_sets <- rjson::fromJSON(file=json_path)[[1]]
#usethis::use_data(predatory_info_sets, internal=TRUE, overwrite=TRUE)
load("R/sysdata.rda")

#' Check whether a journal is predatory.
#' 
#' @param text_descriptor A name or URL to search.
#' @return A text description stating whether the journal is known to be predatory.
#' @examples
#' is_predatory("Academic Journals")
#' is_predatory("http://academiapublishing.org/index.htm")
#' @export
is_predatory <- function(text_descriptor) {
    text_response = ""
    for (predatory_info in predatory_info_sets) {
        if (matches(predatory_info, text_descriptor)) {
            text_response <- format_predatory_string(predatory_info)
        }
    }
    if (text_response == "") {
        text_response <- format_not_predatory(text_descriptor)
    }
    text_response
}
