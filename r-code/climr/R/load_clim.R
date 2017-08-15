#' Load in climate data from NASA
#'
#' @param type Either GLB, NH or SH for global, northern or southern hemisphere temperature anomalies respectively
#'
#' @return A list of \code{\link[tibble]{tibble}}s which contain the yearly, quarterly, and monthly values for each time series respectively, as well as which series was obtained (i.e. GLB, NH or SH)
#' @export
#' @importFrom dplyr "mutate" "select" "arrange" "%>%"
#' @importFrom tidyr "gather"
#' @importFrom readr "parse_factor" "read_csv"
#'
#' @seealso \code{\link{fit}}, \code{\link{plot.climr_fit}}
#' @examples
#' data = load_clim(type = 'SH')
load_clim = function(type = c('GLB', 'NH', 'SH')) {

  # Create global variables to avoid annoying CRAN notes
  DJF = Dec = `J-D` = Jan = SON = Year = month = pred = quarter = temp = x = year = NULL

  # Find out which type
  arg = match.arg(type)

  # Get the URL of the data set
  url = paste0('http://data.giss.nasa.gov/gistemp/tabledata_v3/',arg,'.Ts+dSST.csv')

  # Read in the data
  out = read_csv(url,
                 skip = 1,
                 na = '***',
                 col_types = paste(c('i', rep('d', 18)), collapse = ''),
                 progress = FALSE)

  # Sort out yearly data
  out_year = out %>% na.omit() %>%
    mutate(year = Year,
           temp = `J-D`,
           x = year) %>%
    select(year, temp, x) %>%
    arrange(x)

  # Sort out monthly data
  months = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  out_month = out %>%
    gather(key = month,
           value = temp,
           Jan:Dec,
           na.rm = TRUE) %>%
    mutate(month = parse_factor(month, levels = months, ordered = TRUE),
           year = Year,
           x = year + as.numeric(month)/12) %>%
    select(year, month, temp, x) %>%
    arrange(x)

  # Sort out quarterly data
  quarters = c('DJF', 'MAM', 'JJA', 'SON')
  out_quarter = out %>% gather(key = quarter,
                               value = temp,
                               DJF:SON,
                               na.rm = TRUE) %>%
    mutate(quarter = parse_factor(quarter, levels = quarters, ordered = TRUE),
           year = Year,
           x = year + as.numeric(quarter)/4 - 0.25) %>%
    select(year, quarter, temp, x) %>%
    arrange(x)

  # Put it all in a list and return
  out_list = list(clim_year = out_year,
                  clim_quarter = out_quarter,
                  clim_month = out_month,
                  type = arg)
  class(out_list) = 'climr'

  return(out_list)

}
