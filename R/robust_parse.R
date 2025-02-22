example = function() {
  json = 'Here is your json output: [{"x": 5, "y": [2,4]},{"a"="b" That is all.'
  json = '{"x":5'
  undebug(robust_from_json)
  robust_parse_json(json)
}

base_from_json = function(json,issues=attr(json,"issues"),...) {
  x = try(fromJSON(json,...), silent=TRUE)
  list(x=x, json=json, issues=issues, is_err = is(x, "try-error") )
}

#' Robustly parse a JSON string
#'
#' This function attempts to parse a JSON string using \code{base_from_json()}.
#' If the initial parse fails, it tries to remove potential prefixes or postfixes
#' and correct incomplete arrays or objects before parsing again.
#'
#' @param json A character string containing the JSON to parse.
#' @param repairs which repairs shall be attempted.
#'
#' @return The result of calling \code{base_from_json()} on the (potentially modified)
#' JSON string. If all attempts fail, the returned object is expected to have
#' the element \code{is_err} set to \code{TRUE}.
#'
#' @export
robust_parse_json = function(json, repairs = c("prefix", "postfix", "quotes","premature_end")[1:3], ...) {
  restore.point("jhsdkhskd")

  if ("prefix" %in% repairs) {
    json = repair_json_prefix(json)
  }
  if ("postfix" %in% repairs) {
    json = repair_json_postfix(json)
  }
  if ("quotes" %in% repairs) {
    json = repair_json_quotes(json)
  }
  if ("premature_end" %in% repairs) {
    json = repair_json_premature_end(json)
  }

  x = base_from_json(json)
  return(x)
}

json_with_issues = function(json, issues) {
  attr(json, "issues") = issues
  json
}

repair_json_prefix = function(json, issues=attr(json, "issues")) {
  start_pos = stri_locate_first_regex(json, "[\\[\\{]")
  left = start_pos[1,1]
  if (isTRUE(left>1)) {
    json = stri_sub(json, start_pos[1,1])
    issues = union(issues, "prefix")
  }
  json_with_issues(json,issues)
}

repair_json_postfix = function(json, issues=attr(json, "issues")) {
  end_pos = stri_locate_last_regex(json, "[\\]\\}]")
  right = end_pos[1,1]
  if (isTRUE(right<nchar(json))) {
    json = stri_sub(json,1, to=right)
    issues = c(issues,"postfix")
  }
  json_with_issues(json,issues)
}


repair_json_premature_end = function(json, issues=attr(json, "issues")) {
  restore.point("kshkjdhkshd")
  # If too short stop
  first_char = stri_sub(json,1,length=1)
  last_char = stri_sub(json, nchar(json), length=1)
  if (first_char == "[" & last_char != first_char) {
    last_curley = stri_locate_last_fixed(json ,"}")
    json = stri_sub(json,1, last_curley[,1])
    json = paste0(json, "]")
    issues = c(issues,"incomplete_arr")
  } else if (first_char == "{" & last_char != first_char) {
    last_comma = stri_locate_last_fixed(json ,",")[,1]
    if (!is.na(last_comma)) {
      json = stri_sub(json,1, last_comma[,1]-1)
    }
    json = paste0(json, "}")
    issues = c(issues,"incomplete_obj")
  }
  return(json_with_issues(json,issues))
}

example = function() {
  json = '{ "a" : "my" text B "E is \\" here." }'
  cat(repair_json_quotes(json))

}

repair_json_quotes <- function(json, issues=attr(json, "issues")) {
  # Find all unquoted " and replace with \"
  not_open_pattern = "[^,\\:{\\[ \\\\][ \t\n]*\""
  not_close_pattern = "(?<!\\\\)\"[ \t\n]*[^,}\\]\\: ]"
  #stri_match_all_regex(json,not_open_pattern)
  #stri_match_all_regex(json,not_close_pattern)

  loc_nop = stri_locate_all_regex(json,not_open_pattern)[[1]][,2]
  loc_ncl = stri_locate_all_regex(json,not_close_pattern)[[1]][,1]

  wrong_quote_pos = intersect(loc_nop, loc_ncl)
  if (length(wrong_quote_pos)==0)   return(json_with_issues(json,issues))


  issues = c(issues,"quotes")

  # Process positions in reverse order so earlier insertions don't affect later ones.
  for (pos in sort(wrong_quote_pos, decreasing = TRUE)) {
    json = stri_sub_replace(json, pos, pos, value = "\\\"")
  }

  json_with_issues(json, issues)
}
