#' Get Interviewer Report from Survey Solutions Server
#'
#' This function retrieves a report of all interviewers from the specified
#' Survey Solutions server. The report can be returned in either tab-separated
#' values or CSV format.
#'
#' @param workspace A character string specifying the workspace name. Default is "primary".
#' @param server A character string specifying the server URL. This parameter is required.
#' @param hquser A character string specifying the Headquarters user name. This parameter is required.
#' @param password A character string specifying the password for the Headquarters user. This parameter is required.
#' @param archived A logical value indicating whether to include archived interviewers in the report. Default is `FALSE`.
#' @param type A character string specifying the format of the report. Must be one of "tab" or "csv". Default is "tab".
#'
#' @return A `data.table` containing the interviewer report.
#'
#' @details
#' This function constructs an API request to retrieve a list of all interviewers
#' from the specified Survey Solutions server and workspace. The report can be
#' returned in either tab-separated or CSV format, depending on the `type` parameter.
#'
#' The function checks for the required arguments (`server`, `hquser`, and `password`),
#' and will stop with an error message if any of these are missing.
#'
#' @examples
#' \dontrun{
#' report <- get_int_report(
#'   workspace = "primary",
#'   server = "https://your-server-url/",
#'   hquser = "your-username",
#'   password = "your-password",
#'   archived = FALSE,
#'   type = "csv"
#' )
#' print(report)
#' }
#'
#' @import httr
#' @import data.table
#' @export
get_int_report <- function(workspace = "primary",
                           server = NULL,
                           hquser = NULL,
                           password = NULL,
                           archived = FALSE,
                           type = "tab") {
  # Check that required arguments are provided
  if (is.null(server)) stop("Server URL must be provided.")
  if (is.null(hquser)) stop("Headquarters user must be provided.")
  if (is.null(password)) stop("Password must be provided.")

  # Remove any trailing slash from the server URL
  server <- sub("/+$", "", server)

  # Validate `type` input to avoid errors in the API request
  valid_types <- c("tab", "csv")
  if (!type %in% valid_types) {
    stop(sprintf("Invalid type. Please use one of the following: %s", paste(valid_types, collapse = ", ")))
  }

  # Construct the API URL
  api_URL <- sprintf(
    "%s/%s/UsersApi/AllInterviewers?draw=2&order[0][column]=0&order[0][dir]=asc&order[0][name]=UserName&start=0&length=20&search[value]=&search[regex]=false&supervisorName=&archived=%s&facet=None&exportType=%s",
    server, workspace, tolower(as.character(archived)), type
  )

  # Attempt to get the report from the API
  response <- try(httr::GET(api_URL, httr::authenticate(hquser, password)), silent = TRUE)

  # Check if the GET request was successful
  if (inherits(response, "try-error")) {
    stop("Failed to connect to the server or authenticate. Please check your credentials and server URL.")
  }

  # Check the status code of the response
  if (httr::status_code(response) != 200) {
    stop(sprintf("Request failed with status code: %s", httr::status_code(response)))
  }

  # Parse the response content based on the requested type
  content_type <- httr::headers(response)[["content-type"]]
  if (type == "tab" && grepl("text/tab-separated-values", content_type)) {
    report_dt <- data.table::as.data.table(httr::content(response, as = "parsed", type = "text/tab-separated-values"))
  } else if (type == "csv" && grepl("text/csv", content_type)) {
    report_dt <- data.table::as.data.table(httr::content(response, as = "parsed", type = "text/csv"))
  } else {
    stop("Unexpected content type received. Please check the 'type' parameter and the server response.")
  }

  # Return the report as a data.table
  return(report_dt)
}
