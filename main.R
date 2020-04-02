library(rjson)

setwd("~/Documents/proj/open_source/original/is_this_journal_predatory/is_this_journal_predatory_R")

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
    #print(tolower(predatory_name))
    #print(tolower(text_descriptor))
    #print(any(tolower(predatory_name) == tolower(text_descriptor)))
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

predatory_info_sets <- fromJSON(file=json_path)[[1]]

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

is_predatory("Academic Journals")