example = function() {
  json = 'Here is your json output: [{"x": 5, "y": [2,4]},{"a"="b" That is all.'
  json = '{"x":5'
  undebug(robust_from_json)
  robust_from_json(json)
}

base_from_json = function(txt,issues=NULL,...) {
  x = try(fromJSON(txt,...), silent=TRUE)
  list(x=x, json=txt, issues=issues, is_err = is(x, "try-error") )
}

#' Robustly parse a JSON string
#'
#' This function attempts to parse a JSON string using \code{base_from_json()}.
#' If the initial parse fails, it tries to remove potential prefixes or postfixes
#' and correct incomplete arrays or objects before parsing again.
#'
#' @param json A character string containing the JSON to parse.
#' @param ... Additional arguments passed on to \code{base_from_json()}.
#'
#' @details
#' \code{robust_parse_json()} makes multiple attempts to parse \code{json}:
#' \enumerate{
#'   \item It first calls \code{base_from_json()}.
#'   \item If that fails, it removes any prefix before the first \code{"["} or
#'   \code{"{"} and tries again.
#'   \item If that still fails, it removes any trailing characters after the
#'   final \code{"]"} or \code{"}"}.
#'   \item Finally, if the first bracket is \code{"["} but the last bracket isn't,
#'   it attempts to close an array. If it's an object, it removes trailing
#'   commas and attempts to close it.
#' }
#'
#' @return The result of calling \code{base_from_json()} on the (potentially modified)
#' JSON string. If all attempts fail, the returned object is expected to have
#' the element \code{is_err} set to \code{TRUE}.
#'
#' @export
robust_parse_json = function(json,...) {
  x = base_from_json(json,...)
  if (!x$is_err) return(x)
  restore.point("jhsdkhskd")
  issues = NULL
  # Remove start stuff
  start_pos = stri_locate_first_regex(json, "[\\[\\{]")
  left = start_pos[1,1]
  if (isTRUE(left>1)) {
    json = stri_sub(json, start_pos[1,1])
    issues = c(issues,"has_prefix")
    x = base_from_json(json,issues,...)
    if (!x$is_err) return(x)
  }

  end_pos = stri_locate_last_regex(json, "[\\]\\}]")
  right = end_pos[1,1]
  if (isTRUE(right<nchar(json))) {
    json = stri_sub(json,1, to=right)
    issues = c(issues,"has_postfix")
    x = base_from_json(json,issues,...)
    if (!x$is_err) return(x)
  }
  restore.point("kshkjdhkshd")
  # If too short stop
  first_char = stri_sub(json,1,length=1)
  last_char = stri_sub(json, nchar(json), length=1)
  if (first_char == "[" & last_char != first_char) {
    last_curley = stri_locate_last_fixed(json ,"}")
    json = stri_sub(json,1, last_curley[,1])
    json = paste0(json, "]")
    issues = c(issues,"incomplete_arr")
    x = base_from_json(json,issues,...)
    if (!x$is_err) return(x)
  } else if (first_char == "{" & last_char != first_char) {
    last_comma = stri_locate_last_fixed(json ,",")[,1]
    if (!is.na(last_comma)) {
      json = stri_sub(json,1, last_comma[,1]-1)
    }
    json = paste0(json, "}")
    issues = c(issues,"incomplete_obj")
    x = base_from_json(json,issues,...)
    if (!x$is_err) return(x)
  }
  return(x)
}
