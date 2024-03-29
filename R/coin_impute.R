#' Impute missing data
#'
#' Imputation of missing data data sets using a variety of methods (see `imtype`). This also includes the possibility of imputing
#' by grouping variables, i.e. columns of `IndData` that are prefaced by `"Group_"`.
#'
#' See [online documentation](https://bluefoxr.github.io/COINrDoc/missing-data-and-imputation.html#imputation-in-coinr) for further details and examples.
#'
#' @param COIN A COIN or a data frame
#' @param imtype The type of imputation method. Either:
#' * `"agg_mean"` (the mean of normalised indicators inside the aggregation group),
#' * `"agg_median"` (the median of normalised indicators inside the aggregation group),
#' * `"ind_mean"` (the mean of all the other units in the indicator),
#' * `"ind_median"` (the median of all the other units in the indicator),
#' * `"indgroup_mean"` (the mean of all the other units in the indicator, in the same group),
#' * `"indgroup_median"` (the median of all the other units in the indicator, in the same group),
#' * `"EM"` (expectation maximisation algorithm via AMELIA package, currently without bootstrapping)
#' * `"none"` (no imputation, returns original data set)
#' @param dset The data set in `.$Data` to impute
#' @param groupvar The name of the column to use for by-group imputation. Only applies when `imtype` is set to a group option.
#' @param EMaglev The aggregation level to use if `imtype = "EM"`.
#' @param out2 Where to output the imputed data frame. If `"COIN"` (default for COIN input), creates a new data set `.$Data$Imputed`.
#' Otherwise if `"df"` outputs directly to a data frame.
#'
#' @importFrom tidyr replace_na
#' @importFrom stringr str_subset
#' @importFrom dplyr filter mutate across bind_rows group_by ungroup
#' @importFrom rlang .data
#' @importFrom Amelia amelia
#'
#' @examples
#' # assemble the COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # Check how many missing data points are in raw data set
#' sum(is.na(ASEM$Data$Raw))
#' # impute data using Asia/Europe group mean
#' DataImputed <- impute(ASEM, dset = "Raw", imtype = "indgroup_mean", groupvar = "Group_EurAsia",
#' out2 = "df")
#' # See how many missing data points we have in the imputed data
#' sum(is.na(DataImputed))
#' # check no missing data
#' stopifnot(sum(is.na(DataImputed))==0)
#'
#' @return If `out2 = "COIN"` (default for COIN input), creates a new data set `.$Data$Imputed`.
#' Otherwise if `out2 = "df"` outputs directly to a data frame.
#'
#' @export

impute <- function(COIN, imtype = NULL, dset = NULL,
                  groupvar = NULL, EMaglev = NULL, out2 = "COIN"){

  # Check for dset. If not specified, exit.
  if (is.null(dset) & !("data.frame" %in% class(COIN))){
    stop("dset is NULL. Please specify which data set to operate on.")
  }

  ##----- SET DEFAULTS -------##
  # Done here because otherwise if we use regen, this input could be input as NULL
  if(is.null(imtype)){
    stop("Imputation type (imtype) not specified.")
  }

  # this is a switch for some old code where you can impute by year. It was an argument to the function, but since at the moment
  # COINs don't support years, it has no sense. To reactivate, this should be moved back to a function argument.
  byyear <- FALSE

  # First. check to see what kind of input we have.
  out <- getIn(COIN, dset = dset)
  ind_data <- out$ind_data
  IndCodes <- out$IndCodes

  if( out$otype=="COINobj" ) {
    # Write to Method
    COIN$Method$impute$imtype <- imtype
    COIN$Method$impute$dset <- dset
    COIN$Method$impute$groupvar <- groupvar
    #COIN$Method$impute$byyear <- byyear
    COIN$Method$impute$EMaglev <- EMaglev
  }

  # get number of NAs before imputation
  nasumz <- colSums(is.na(ind_data))
  nNA_start <- sum(nasumz[IndCodes])

  message(paste0("Missing data points detected = ", nNA_start))

  ###### IMPUTATION ######

  # first, get some info about years, if needed (either when imputing by year, or when using latest year)
  if (byyear==T | imtype == "latest_year"){
    nyears <- ind_data %>% select(starts_with("Year")) %>% unique() %>% nrow() # number of years present
    yrcol <- ind_data %>% colnames() %>% stringr::str_subset("Year") # the column name which has the years in it
    yrs <- ind_data %>% select(starts_with("Year")) %>% unique()
  }

  ## Now actually do the imputation, depending on the type...

  if ((imtype == "agg_mean") | (imtype == "agg_median") ){ # use the mean of the other indicators in the aggregation group. Only works if data is normalised first.

    # this only works with normalised data, so do that first
    # use minmax in [0, 1]
    ind_dataN <- normalise(COIN, dset = dset, ntype = "minmax", npara = c(0,1),
                                out2 = "df")
    # just get the indicator cols, not the other ones
    ind_dataN <- ind_dataN[IndCodes]
    # we will need the original min and max to reconstruct
    indmins <- apply(out$ind_data_only, 2, function(x) min(x, na.rm = T))
    indmaxs <- apply(out$ind_data_only, 2, function(x) max(x, na.rm = T))
    # indicator metadata
    metad <- COIN$Input$IndMeta
    # aggregation columns
    agg_cols <- metad %>% dplyr::select(dplyr::starts_with("Agg")) # the columns with aggregation info in them

    agg_colname <- colnames(agg_cols[1]) # the name of the aggregation level
    agg_names <- unique(agg_cols[[1]]) # the names of the aggregation groups
    sub_codes <- COIN$Input$IndMeta$IndCode # the ingredients to aggregate are the base indicators
    ind_data_impN <- ind_dataN

    if(is.null(COIN$Parameters$Weights$Original)){
      weights_lev <- rep(1, length(sub_codes))
      message("No weights found in COIN. Using equal weights.")
    } else {
      weights_lev <- COIN$Parameters$Weights$Original$Weight[
        COIN$Parameters$Weights$Original$AgLevel == 1
      ] # Indicator weights
    }

    for (agroup in 1:length(agg_names)){ # loop over aggregation groups, inside the given agg level

      iselect <- sub_codes[metad[,agg_colname]==agg_names[agroup]] %>% unique() # get indicator names belonging to group
      indselect <- ind_dataN[iselect]
      # get weights belonging to group, using codes
      weights_group <- weights_lev[unique(sub_codes) %in% iselect]

      if (imtype == "agg_mean"){
        # Now get the mean. Had to do in a roundabout way to avoid rowmeans type functions... probably an easier way exists though
        newcol <- indselect %>% dplyr::rowwise() %>%
          dplyr::transmute(!!agg_names[agroup] := matrixStats::weightedMean(dplyr::c_across(cols = dplyr::everything()),
                                                                            w = weights_group, na.rm = TRUE))
      } else if (imtype == "agg_median"){
        newcol <- indselect %>% dplyr::rowwise() %>%
          dplyr::transmute(!!agg_names[agroup] := matrixStats::weightedMedian(dplyr::c_across(cols = dplyr::everything()),
                                                                            w = weights_group, na.rm = TRUE))
      }

      # get any rows with NAs
      missrows <- which(rowSums(is.na(indselect)) > 0)
      # loop over NA rows
      for (jj in missrows){
        missind <- is.na(indselect[jj,]) %>% as.logical()
        indselect[jj,missind] <- newcol[jj,]
      }

      ind_data_impN[iselect] <- indselect
    }# for

    # now scale back to original scale, reverse minmax transformation
    # for the record, this is done by imodify, where .y is the index.
    ind_data_imp <- purrr::imodify(ind_data_impN,
                         ~{.x*(indmaxs[.y]-indmins[.y]) + indmins[.y]} )
    ind_data[IndCodes] <- ind_data_imp
    ind_data_imp <- ind_data

  } else if (imtype == "ind_mean"){ # impute using column MEAN, i.e. the mean of the indicator over all units

    if (byyear==T){ # If we have to impute by year

      ind_data_imp_list <- vector("list",nyears) # make an empty list for populating, one for each year
      for (yr in 1:nyears){

        ind_data_yr <- dplyr::filter(ind_data,.data[[yrcol]] == yrs[[yr,1]]) # get only rows from year
        #now impute...
        ind_data_imp_list[[yr]] <- ind_data_yr %>%
          dplyr::mutate(dplyr::across(all_of(IndCodes), ~{tidyr::replace_na(.x, mean(.x, na.rm = TRUE))}))
      }
      ind_data_imp <- dplyr::bind_rows(ind_data_imp_list) # join everything back together

    } else { # if not imputing by year
      # the following applies the replace_na function to all columns specified by IndCodes
      ind_data_imp <- ind_data %>% dplyr::mutate(dplyr::across(all_of(IndCodes), ~{tidyr::replace_na(.x, mean(.x, na.rm = TRUE))}))
    }

  } else if (imtype == "ind_median"){ # impute using column MEDIAN, i.e. the median of the indicator over all units

    if (byyear==T){ # If we have to impute by year
      ind_data_imp_list <- vector("list",nyears) # make an empty list for populating, one for each year
      for (yr in 1:nyears){

        ind_data_yr <- dplyr::filter(ind_data,.data[[yrcol]] == yrs[[yr,1]]) # get only rows from year
        #now impute...
        ind_data_imp_list[[yr]] <- ind_data_yr %>%
          dplyr::mutate(dplyr::across(all_of(IndCodes), ~{tidyr::replace_na(.x, median(.x, na.rm = TRUE))}))
      }
      ind_data_imp <- dplyr::bind_rows(ind_data_imp_list) # join everything back together

    } else { # if not imputing by year
      # the following applies the replace_na function to all columns specified by IndCodes
      ind_data_imp <- ind_data %>% dplyr::mutate(dplyr::across(all_of(IndCodes), ~{tidyr::replace_na(.x, median(.x, na.rm = TRUE))}))
    }

  } else if (imtype == "indgroup_mean"){ # use column MEAN, restricted to a particular group

    if(is.null(groupvar)){stop("Group mean imputation requires that you specify which grouping to use (column name).")} # throw error if no group

    if (byyear==T){ # If we have to impute by year

      ind_data_imp_list <- vector("list",nyears) # make an empty list for populating, one for each year
      for (yr in 1:nyears){

        ind_data_yr <- dplyr::filter(ind_data,.data[[yrcol]] == yrs[[yr,1]]) # get only rows from year
        #now impute...
        ind_data_imp_list[[yr]] <- ind_data_yr %>% dplyr::group_by(dplyr::across(dplyr::all_of(groupvar))) %>% # OLD dplyr::group_by(.dots=groupvar)
          dplyr::mutate(dplyr::across(all_of(IndCodes), ~tidyr::replace_na(.x, mean(.x, na.rm = TRUE))))
      }
      ind_data_imp <- dplyr::bind_rows(ind_data_imp_list) # join everything back together

    } else { # if not imputing by year
      # This works by grouping the data by the grouping variable first. Operations then performed by group.
      ind_data_imp <- ind_data %>% dplyr::group_by(dplyr::across(dplyr::all_of(groupvar))) %>%
        dplyr::mutate(dplyr::across(all_of(IndCodes), ~tidyr::replace_na(.x, mean(.x, na.rm = TRUE))))
    }

  } else if (imtype == "indgroup_median"){ # use column MEDIAN, restricted to a particular group

    if(is.null(groupvar)){stop("Group median imputation requires that you specify which grouping to use (column name).")} # throw error if no group

    if (byyear==T){ # If we have to impute by year
      ind_data_imp_list <- vector("list",nyears) # make an empty list for populating, one for each year
      for (yr in 1:nyears){

        ind_data_yr <- dplyr::filter(ind_data,.data[[yrcol]] == yrs[[yr,1]]) # get only rows from year
        #now impute...
        ind_data_imp_list[[yr]] <- ind_data_yr %>% dplyr::group_by(dplyr::across(dplyr::all_of(groupvar))) %>%
          dplyr::mutate(dplyr::across(all_of(IndCodes), ~tidyr::replace_na(.x, median(.x, na.rm = TRUE))))
      }
      ind_data_imp <- dplyr::bind_rows(ind_data_imp_list) # join everything back together

    } else { # if not imputing by year
      # This works by grouping the data by the grouping variable first. Operations then performed by group.
      ind_data_imp <- ind_data %>% dplyr::group_by(dplyr::across(dplyr::all_of(groupvar))) %>%
        dplyr::mutate(dplyr::across(all_of(IndCodes), ~tidyr::replace_na(.x, median(.x, na.rm = TRUE))))
    }

  } else if (imtype == "latest_year"){ # substitute NAs with any available points from previous years

    ind_data_imp_list <- vector("list",nyears) # make an empty list for populating, one for each year
    ind_data_imp_list[[1]] <- dplyr::filter(ind_data,.data[[yrcol]] == yrs[[1,1]]) # only imputing backwards in time, so first year available remains the same.

    if(nyears>1){
      for (yr in 2:nyears){

        # get indicator from year and year-1 as separate dfs
        ind_data_yr_all <- dplyr::filter(ind_data,.data[[yrcol]] == yrs[[yr,1]]) %>% as.data.frame() # get only rows from year and ind. cols. Have to change to df because otherwise next step doesn't work
        ind_data_yr <- ind_data_yr_all %>% select(IndCodes) # done in 2 steps so can access the other cols in a min.
        ind_data_prev_yr <- dplyr::filter(ind_data,.data[[yrcol]] == yrs[[yr-1,1]]) %>% as.data.frame() %>% select(IndCodes) # get only rows from year-1

        #now substitute any NAs from yr with those from prev_yr
        ind_data_yr[is.na(ind_data_yr)] <- ind_data_prev_yr[is.na(ind_data_yr)]

        ind_data_yr <- cbind(select(ind_data_yr_all,-IndCodes),ind_data_yr)

        ind_data_imp_list[[yr]] <- ind_data_yr # add to the list
      }
      ind_data_imp <- dplyr::bind_rows(ind_data_imp_list) # join everything back together
    } else {stop("You can't impute by latest year with only one year of data.")}

  } else if (imtype == "EM"){

    # Use the expectation maximisation algorithm via AMELIA package.
    # The main issue here is that the EM procedure needs a certain ratio of obs to variables.
    # This may often be exceeded, so we have to do by aggregation levels

    if(is.null(EMaglev)){
      EMaglev <- 2
    }

    # aggregation columns
    agg_cols <- COIN$Input$IndMeta %>% dplyr::select(dplyr::starts_with("Agg"))
    # columns of interest: the indicators, plus the aggregation level
    groupspecs <- cbind(COIN$Input$IndMeta$IndCode, agg_cols[[EMaglev-1]])
    # the names of the aggregation groups
    agg_names <- unique(groupspecs[,2])

    # loop over aggregation groups
    for (ii in 1:length(agg_names)){

      # get indicator data only including unit codes and indicators from group
      df <- ind_data[c("UnitCode", groupspecs[groupspecs[,2]==agg_names[ii],1])] %>% as.data.frame()

      if (sum(is.na(df))>0){
        # NAs are present
        amOut <- try(Amelia::amelia(df, m = 1, p2s = 0, cs = "UnitCode", boot.type = "none"))
        # may not have enough data points per variable depending on aglev
        if(("try-error" %in% class(amOut))|(amOut$code!=1)){
          stop("EM maximisation has not worked. This might be solved by imputing over a lower aggregation level (lower EMaglev)")
        }
        # collect imputed data
        imps <- amOut$imputations
      } else {
        # no NAs. Have to make a list anyway to agree with the rest
        imps <- rep(list(df),1)
      }

      if (ii == 1){
        # for the first iteration we need to create a list
        implist <- imps
      } else {
        # for successive iterations just append
        implist <- Map(cbind, implist, imps)
      }
    }

    # there will be duplicate IndCode columns, so remove all
    ind_data_imp <- lapply(implist, function(x) x[colnames(x)!="UnitCode"])[[1]]
    # add back the original non-numeric columns
    ind_data[IndCodes] <- ind_data_imp
    ind_data_imp <- ind_data

  } else if (imtype == "none"){
    # no imputation, return original data set
    ind_data_imp <- ind_data
  }

  nasumz <- colSums(is.na(ind_data_imp))
  nNA_end <- sum(nasumz[IndCodes]) # counts total number of NAs in indicator columns, after imputation
  message(paste0("Missing data points imputed = ", nNA_start-nNA_end, ", using method = ", imtype))

  # output to object if requested (only if input is COIN and out2 not df)
  if( (out$otype=="COINobj") & (out2 !=  "df") ) {
    COIN$Data$Imputed <- dplyr::ungroup(ind_data_imp)
    COIN$Analysis$Imputed$NImputed <- nNA_start-nNA_end
    return(COIN)
  } else {
    return(ind_data_imp)
  }

}
