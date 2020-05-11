#' Convert selected currencies via API
#'
#' @param value numeric vector
#' @param input character string
#' @param output character string
#'
#' @return result
#'
#' @importFrom httr GET content
#' @importFrom logger log_info
#'
usd_rate <- function(value, input = 'EUR', output='USD') {
  base <- GET(paste0('https://api.exchangeratesapi.io/latest?base=', input, '&symbols=', output))
  base <- as.numeric(unlist(content(base)[[1]]))
  result <- value * base
  log_info('{value} {input} is equal to {result} {output}')
  return(as.numeric(result))
}

#' EUR/USD converter
#'
#' @param val numeric vector
#'
#' @return value_eur numeric value
#'
#' @importFrom httr GET content
#'
convert_to_eur <- function(val){
  value_eur <- usd_rate(val)
  return(value_eur)
}


#' EUR pretty printer
#'
#' @param value numeric vector
#'
#' @return EUR character string
#'
eur <- function(value){
  return(paste0('€', format(round(value, 2), nsmall=1, big.mark=",")))
}

#' EUR string to numeric converter
#'
#' @param str character string
#'
#' @return EUR in numeric format
#'
uneur <- function(str){
  str <- gsub('€', '', str)
  str <- gsub(',', '', str)
  return(as.numeric(str))
}


