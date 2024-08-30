#' Column Classes for SurSol System Generated Files
#'
#' Returns a list of columns to be parsed as integer, date or character in `data.table::fread`.
#'
#' @return A list with three elements: `integer`, `Date` and `character`, each containing column names.
#' @export
#' @examples
#' col_classes <- suso.sys.colClasses()
suso.sys.colClasses <- function() {
  list(
    integer = c(
      "assignment__id", "action", "role", "responsible_role", "id1", "id2", "id3", "id4",
      "order", "interview__status", "interviewers", "rejections__sup", "rejections__hq",
      "entities__errors", "type", "message__number"
    ),
    Date=c("date"),
    character = c(
      "time", "originator", "responsible_name", "old__value", "new__value",
      "comment", "interview__key", "interview__id", "roster", "variable", "responsible",
      "interview__duration", "message"
    )
  )
}





#' Convert and Label Interview Status Column
#'
#' This function converts the `interview__status` column in a `data.table` to a factor with descriptive labels.
#' It also checks that the column contains only valid status codes.
#'
#' @param dt A `data.table` containing the data with an `interview__status` column.
#' @return The input `data.table` with the `interview__status` column converted to a factor with descriptive labels.
#' @export
#' @import data.table
#' @import assertthat
label_interview_status <- function(dt) {
  # Check if input is a data.table
  assertthat::assert_that(is.data.table(dt),
                          msg = "Input must be a data.table.")

  # Check if the interview__status column exists
  assertthat::assert_that("interview__status" %in% colnames(dt),
                          msg = "Column 'interview__status' not found in the data.table.")

  # Define the valid statuses and their corresponding labels
  valid_statuses <- c(-1, 0, 20, 40, 60, 65, 80, 85, 95, 100, 120, 125, 130)
  status_labels <- c("Deleted", "Restored", "Created", "SupervisorAssigned",
                     "InterviewerAssigned", "RejectedBySupervisor",
                     "ReadyForInterview", "SentToCapi", "Restarted",
                     "Completed", "ApprovedBySupervisor",
                     "RejectedByHeadquarters", "ApprovedByHeadquarters")

  # Check if the interview__status column contains only the valid statuses
  invalid_statuses <- unique(dt[!interview__status %in% valid_statuses]$interview__status)
  assertthat::assert_that(length(invalid_statuses) == 0,
                          msg = paste("Invalid interview__status values found:", paste(invalid_statuses, collapse = ", ")))

  # Convert the interview__status column to a factor with the defined labels
  dt[, interview__status := factor(interview__status, levels = valid_statuses, labels = status_labels)]
}



#' Convert Role Column to Factor and Validate
#'
#' This function converts the `role` column in a `data.table` to a factor with descriptive labels.
#' It also checks that the column contains only valid role codes. Usefol for Interview Comments
#'
#' @param dt A `data.table` containing the data with a `role` column.
#' @return The input `data.table` with the `role` column converted to a factor with descriptive labels.
#' @export
#' @import data.table
#' @import assertthat
label_role <- function(dt) {
  # Define the valid roles and their corresponding labels
  valid_roles <- c(0, 1, 2, 3, 4, 5)
  role_labels <- c("<UNKNOWN ROLE>", "Interviewer", "Supervisor", "Headquarter", "Administrator", "Api User")

  # Check if the role column contains only the valid roles
  invalid_roles <- unique(dt[!role %in% valid_roles]$role)
  assertthat::assert_that(length(invalid_roles) == 0,
                          msg = paste("Invalid role values found:", paste(invalid_roles, collapse = ", ")))

  # Convert the role column to a factor with the defined labels
  dt[, role := factor(role, levels = valid_roles, labels = role_labels)]
}

# Example usage:
# label_role(int_comments)



#' Get Action Codes Mapping
#'
#' This utility function returns a list that maps action codes to their corresponding descriptions in the `interview__actions` file from Survey Solutions.
#'
#' @return A named list where each integer code is mapped to its action description.
#'
#' @export

action_codes <- function() {

  action_mapping <- list(
    "0" = "SupervisorAssigned",
    "1" = "InterviewerAssigned",
    "2" = "FirstAnswerSet",
    "3" = "Completed",
    "4" = "Restarted",
    "5" = "ApprovedBySupervisor",
    "6" = "ApprovedByHeadquarter",
    "7" = "RejectedBySupervisor",
    "8" = "RejectedByHeadquarter",
    "9" = "Deleted",
    "10" = "Restored",
    "11" = "UnapprovedByHeadquarter",
    "12" = "Created",
    "13" = "InterviewReceivedByTablet",
    "14" = "Resumed",
    "15" = "Paused",
    "16" = "TranslationSwitched",
    "17" = "OpenedBySupervisor",
    "18" = "ClosedBySupervisor",
    "19" = "InterviewSwitchedToCawiMode",
    "20" = "InterviewSwitchedToCapiMode",
    "21" = "InterviewReceivedBySupervisor"
  )
  return(action_mapping)
}

