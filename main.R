get_domain <- function(url) {
    url <- gsub("http://", "", url)
    url <- gsub("https://", "", url)
    slash_idx <- gregexp(pattern="/", url)[[1]]
    if (length(slash_idx) == 0) {
        domain = url
    }
    else {
        domain = substr(url, 0, slash_idx)
    }
    domain
}

matches_domain <- function(predatory_name, text_descriptor)  {
    any(get_domain(text_descriptor) == domains)
}

matches_name <- function(predatory_name, text_descriptor) {
    any(tolower(predatory_name) == tolower(text_descriptor))
}

matches <- function(predatory_info, text_descriptor) {
    matches_domain(predatory_info$domain, text_descriptor) | matches_name(predatory_info$name, text_descriptor)
}

format_predatory_string <- function(predatory_info) {

}

format_not_predatory <- function(text_descriptor) {

}

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
