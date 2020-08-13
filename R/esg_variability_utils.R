# Add helper functions to prepare data for interactive tool ---------------

#' create_mrv
#' Create a table of most recent values
#'
#' @param df data.frame: data.frame (x) created by load_data.R
#'
#' @return data.frame
#' @importFrom magrittr %>%
#' @export
#'

create_mrv <- function(df) {

  out <- df %>%
    dplyr::select(indicatorID, indicator, iso3 = iso3c, year = date) %>%
    dplyr::group_by(indicatorID, indicator, iso3) %>%
    dplyr::summarise(
      year = max(year, na.rm = TRUE)
    )

  return(out)

}

compute_availability <- function(mrv, year_select, var_name, n_countries) {
  year_select <- rlang::enquo(year_select)
  var_name <- rlang::quo_name(var_name)

  out <- mrv %>%
    dplyr::filter(year >= !!year_select) %>%
    dplyr::group_by(indicatorID, indicator) %>%
    dplyr::summarise(
      !!var_name := length(iso3) / n_countries
    ) %>%
    dplyr::ungroup()

  return(out)
}

impute_years <- function(cvs, mrv, cv_max = 0.5, years_to_impute = 1) {

  cv_max <- rlang::enquo(cv_max)
  years_to_impute <- enquo(years_to_impute)


  out <- cvs %>%
    dplyr::mutate(
      imputed_years = dplyr::if_else(cv <= !!cv_max, !!years_to_impute, 0),
      imputed_years = dplyr::if_else(is.na(imputed_years), 0, imputed_years) # handle cases imputed years cannot be assigned
    ) %>%
    dplyr::select(indicatorID = indicator, iso3 = iso3c, imputed_years) %>%
    dplyr::right_join(mrv) %>%
    dplyr::mutate(
      imputed_years = dplyr::if_else(is.na(imputed_years), 0, imputed_years), # handle cases were merge is not successful
      year = year + imputed_years
    ) %>%
    dplyr::select(indicatorID, indicator, iso3, year)

  return(out)
}

create_baseline_imputed_df <- function(mrv, cvs, cv_max, years_to_impute, year_select, n_countries) {

  # Compute baseline df
  baseline <- compute_availability(mrv = mrv,
                                   year_select = year_select,
                                   var_name = "baseline",
                                   n_countries = n_countries)
  # Compute imputed df
  imputed <- impute_years(cvs = cvs,
                          mrv = mrv,
                          cv_max = cv_max,
                          years_to_impute = years_to_impute)
  imputed <- compute_availability(mrv = imputed,
                                  year_select = year_select,
                                  var_name = "imputed",
                                  n_countries = n_countries)

  # Combine dfs
  out <- dplyr::full_join(baseline, imputed)
  out <- dplyr::mutate(out,
                       baseline = tidyr::replace_na(baseline, 0),
                       imputed = dplyr::if_else(
                         (!is.na(baseline)) & is.na(imputed),
                         0,
                         imputed),
                       gain = imputed - baseline,
                       indicator = fct_reorder(indicator, -baseline)
  )
  out$indicator <- factor(out$indicator, levels = unique(out$indicator)[order(out$imputed, decreasing = TRUE)])

  return(out)
}

