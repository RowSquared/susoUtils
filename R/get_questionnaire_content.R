#' Parse Questionnaire JSON Files
#'
#' This function reads all JSON files in a specified directory that describe the content
#' of a questionnaire built in the Survey Solutions software system. It extracts
#' information about sections, questions, logical expressions, and other relevant details.
#' The function returns a structured `data.table` object containing this information.
#'
#' @param location A string specifying the directory where the JSON files are located. The function
#'   recursively searches this directory for all files with a `.json` extension.
#' @param remove_html A logical value indicating whether HTML tags should be removed from the text
#'   fields within the questionnaire content. Default is `FALSE`.
#' @param remove_revisions A logical value indicating whether to remove older revisions of questions
#'   when multiple versions are available. If `TRUE`, only the latest revision of each question is kept.
#'   Default is `TRUE`.
#' @return A `data.table` object containing structured information extracted from the questionnaire JSON files.
#'   The table includes details such as question keys, variable names, question texts, section titles, and more.
#' @import data.table
#' @importFrom jsonlite fromJSON
#' @importFrom susometa parse_questionnaire
#' @examples
#' # Assuming JSON files are in the "questionnaires" directory
#' qx_structure <- get_questionnaire_content(location = "path/to/questionnaires")
#' @export
get_questionnaire_content <- function(location = NULL, remove_html = FALSE, remove_revisions = TRUE) {
  # List all JSON files in the specified directory
  js_files <- list.files(file.path(location), pattern = "*.json", recursive = TRUE)

  # Read and process each JSON file
  js_list <- lapply(js_files, function(x) {
    dt <- as.data.table(susometa::parse_questionnaire(file.path(location, x)))
    dt.help <- jsonlite::fromJSON(txt = (file.path(location, x)))

    dt[, c("instrt", "qx_id", "revision") := .(
      unique(dt.help$VariableName),
      unique(dt.help$PublicKey),
      unique(dt.help$Revision)
    )]
  })

  document <- rbindlist(js_list, fill = TRUE)

  # Identify important columns
  level_names <- names(document)[grep("^l_", names(document))]
  val_cols <- names(document)[grep("^validation|^l_|^severity", names(document))]
  answer_cols <- names(document)[grep("^answer", names(document))]

  # Static texts where varname is empty, replace with public_key
  document[type == "StaticText", varname := public_key]

  # Replace text with question text where necessary
  dt <- copy(document)
  dt[is.na(text) & !is.na(question_text), text := question_text]
  dt[type == "Variable", text := label_variable]
  dt[is.na(text) & !is.na(title), text := title]
  assertthat::assert_that(assertthat::noNA(dt$text))
  setnames(dt, "public_key", "question_key")

  # Remove older revisions if requested
  if (remove_revisions) {
    dt <- unique(dt, by = c("qx_id", "question_key"))
    document <- unique(document, by = c("qx_id", "public_key"))

    if (nrow(document[is.na(l_1) & type == "Group", .(l_0, instrt, title)]) != nrow(unique(document[is.na(l_1) & type == "Group", .(l_0, instrt, title)], by = c("instrt", "l_0")))) {
      warning("Attention, (sub-) Sections appear to have changed between versions")
    }
  }

  # Identify section titles (level 0)
  dt[, "title" := NULL]
  dt <- merge(dt, unique(document[is.na(l_1) & type == "Group", .(l_0, instrt, section_title = title, title_key = public_key)], by = c("instrt", "l_0")),
              by = c("l_0", "instrt"), all.x = TRUE)

  # Identify subsections if they exist
  subsec_dt <- document[is.na(question_text) & !is.na(l_1) & type == "Group", !..answer_cols]

  subsec_dt <- Filter(function(x) !all(is.na(x)), subsec_dt)
  if (nrow(subsec_dt) > 0) {
    lev_subsec_names <- names(subsec_dt)[grep("^l_", names(subsec_dt))]
    setcolorder(subsec_dt, c("instrt", "title", lev_subsec_names))

    for (value in 0:length(lev_subsec_names)) {
      current_var <- paste0("l_", value)
      leading_var <- paste0("l_", value + 1)
      dt[is.na(get(leading_var)), c(current_var) := NA]
    }

    count <- 1
    for (value in seq(from = length(lev_subsec_names) - 1, to = 0)) {
      trailval <- value + 1
      reduced_subsec_names <- lev_subsec_names[1:trailval]

      if (count == 1) {
        dt <- merge(dt,
                    unique(subsec_dt[!is.na(get(paste0("l_", value))), c(..reduced_subsec_names, "instrt", "title", "public_key")], by = c("instrt", reduced_subsec_names)),
                    by = c(reduced_subsec_names, "instrt"), all.x = TRUE)
      } else if (count > 1) {
        dt <- merge(dt,
                    unique(subsec_dt[!is.na(get(paste0("l_", value))) &
                                       is.na(get(paste0("l_", trailval))), c(..reduced_subsec_names, "instrt", "title", "public_key")], by = c(reduced_subsec_names, "instrt")),
                    by = c(reduced_subsec_names, "instrt"), all.x = TRUE)
      }

      setnames(dt, c("title", "public_key"), c(paste0("title_", value), paste0("key_", value)))
      setcolorder(dt, c(paste0("title_", value), paste0("key_", value)))

      count <- count + 1
    }
    dt[, c("title_0", "key_0", level_names) := NULL]
  }

  sub.sec.keys <- names(dt)[grepl("key_\\d", names(dt))]
  setcolorder(dt, c(rev(sub.sec.keys), "title_key"))
  dt[, section_key := fcoalesce(.SD), .SDcols = patterns("title_key|key_\\d")][, c("title_key", sub.sec.keys) := NULL]
  dt[, section_key := gsub("\\-", "", section_key)]
  dt[, question_key := gsub("\\-", "", question_key)]

  setcolorder(dt, c("instrt", "qx_id", "section_title", titles, "section_key", "question_key", "text", "varname"))

  if (remove_html) {
    char_cols <- names(dt)[sapply(dt, is.character)]
    dt[, (char_cols) := lapply(.SD, remove_html), .SDcols = char_cols]
  }

  dt[is_roster == TRUE, type := "Roster"]
  dt[type == "Group" & section_title == text, type := "Section"]
  dt[type == "Group" & section_title != text, type := "SubSection"]

  dt[, c("name_variable", "document.id", "label_variable") := NULL]

  setorder(dt, "qx_id", "varname", -"revision")
  dt <- rbindlist(list(unique(dt[!is.na(varname) & varname != ""], by = c("qx_id", "varname", "revision")),
                       dt[is.na(varname) | varname == ""]))

  setcolorder(dt, c("instrt", "qx_id", "varname", "type"))

  return(dt)
}
