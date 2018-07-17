#' Get template file name based on current configuration
#'
#' @param part The report part
#' @param customer_id The customer identifier
#' @param report_type The report type, and default to \code{\link{NULL}}
#' @param ext The file extension of the template, the default value is
#'   \code{Rmd}, namely R markdown file
#' @export
get_tmpl_name <- function(part, customer_id, report_type = NULL, ext = "Rmd") {
  if (is.null(report_type)) {
    tmpl_name <- paste(part, customer_id, ext, sep = ".")
  } else {
    tmpl_name <- paste(part, customer_id, report_type, ext, sep = ".")
  }
  return(tmpl_name)
}

#' Render markdown for normal part
#'
#' @param text The raw R markdown file loaded from template
#' @export
render_part_normal <- function(text) {
  md <- stringr::str_glue(text, .open = "<<", .close = ">>")
  return(md)
}

#' Render markdown for body (special) part
#'
#' @param heading The heading text of the body part
#' @param content The raw R markdown file loaded from template
#' @param ab_ids Full character vector containing all the ability identifiers to
#'   be reported
#' @export
render_part_body <- function(heading, content, ab_ids) {
  content_vector <- character(length(ab_ids))
  names(content_vector) <- ab_ids
  for (ab_id in ab_ids) {
    content_vector[ab_id] <- stringr::str_glue(content, .open = "<<", .close = ">>")
  }
  contents <- paste(content_vector, collapse = "\n\n")
  md <- render_heading_content(heading, contents)
  return(md)
}

#' Render heading and content section
#'
#' @param heading The section title
#' @param content The section content
#' @param hlevel The heading level, default to 1
#' @param style The name of the custom style
#' @param glue Will the content be evaluated by \code{\link[stringr]{str_glue}}?
#' @param ... Additional arguments passed to \code{\link[stringr]{str_glue}}
#' @return The markdown string to render as a section
#' @export
render_heading_content <- function(heading, content, hlevel = 1, style = "", glue = FALSE, ...) {
  heading_md <- customize_style(render_heading(heading, hlevel), style = style)
  md <- paste(heading_md, content, sep = "\n\n")
  if (glue) {
    md <- purrr::map_chr(md, stringr::str_glue, ...)
  }
  return(md)
}

#' Render heading markdown according to level
#'
#' @param heading Heading text
#' @param hlevel Heading level, default to 1
#' @return The corresponding heading level markdown content
render_heading <- function(heading, hlevel = 1) {
  prefix <- strrep("#", hlevel)
  md <- paste(prefix, heading)
  return(md)
}

#' Add custom style to markdown text
#'
#' @param text The text to be adjusted
#' @param style The name of the custom style
#' @return The pandoc-flavored markdown string
customize_style <- function(text, style = "") {
  prefix <- ifelse(
    style == "", "",
    unclass(stringr::str_glue("::: {{custom-style=\"{style}\"}}"))
  )
  suffix <- ifelse(style == "", "", ":::")
  md <- paste(prefix, text, suffix, sep = "\n")
  return(md)
}
