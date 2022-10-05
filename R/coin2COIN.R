#' Convert coin class to COIN
#'
#' Converts a newer format "coin" s3 class object from COINr > v1.0 to the older format "COIN" class compatible with COINr6. This is essentially used for importing
#' coins from COINr to use some of the visualisation tools in COINr6 which were not carried over to the new format, e.g. interactive plots etc.
#'
#' A coin cannot be perfectly converted to a COIN because both COINs and coins store the functions used in their creation in the `.$Method` and `.$Log` sub-lists respectively.
#' Since the syntax and functionality of COINr6 and COINr is quite different, it is not possible to recreate the Method from Log or vice versa. This means that a COIN created
#' by converting from a coin will not be able to be regenerated which means [regen()], [sensitivity()] and any other functions involving regeneration will not work.
#'
#' The intended use of this conversion function is therefore either (a) to convert without recovering data sets, then use COINr6 syntax to build datasets, with full functionality
#' of COINr6, or (b) to convert and recover all data sets, but use COINr6 more in a read-only mode, e.g. just for plotting, using the apps like [resultsDash()] and [indDash()] etc.
#'
#' @param coin A "coin" class object from the COINr package (> v1.0)
#' @param recover_dsets If `FALSE` (default) will only extract the raw data and assemble a COIN. If `TRUE`, will also recover all datasets. See details.
#' @param out2 Either `"COIN"` (default) to output an assembled COIN, or else `"list"` to output a list with the `IndData`, `IndMeta` and `AggMeta` data frames in COINr6 format.
#'
#' @return A "COIN" class object or a list of dataframes, depending on `out2`.
#' @export
#'
#' @examples
#' # note ASEM_newcoin is a built in new-format coin object
#' COIN <- coin_to_COIN(ASEM_newcoin, recover_dsets = TRUE)
coin_to_COIN <- function(coin, recover_dsets = FALSE, out2 = "COIN"){

  if(!inherits(coin, "coin")){
    stop("This function requires a coin class object as an input.")
  }

  # IndData
  idata <- extract_IndData(coin, "Raw", withdenoms = TRUE)

  # IndMeta
  imeta <- coin$Meta$Lineage
  # names
  names(imeta) <- c("IndCode", paste0("Agg", 1:(ncol(imeta)-1)))
  # merge on rest of info
  imeta <- merge(imeta, coin$Meta$Ind, by.x = names(imeta)[1], by.y = "iCode")
  # rename cols
  names(imeta)[names(imeta) == "iCode"] <- "IndCode"
  names(imeta)[names(imeta) == "iName"] <- "IndName"
  names(imeta)[names(imeta) == "Weight"] <- "IndWeight"
  names(imeta)[names(imeta) == "Unit"] <- "IndUnit"
  # remove unused cols
  imeta <- imeta[!(names(imeta) %in% c("Level", "Parent", "Type"))]

  # AggMeta
  agmeta <- coin$Meta$Ind[coin$Meta$Ind$Type == "Aggregate", ]
  # rename cols
  names(agmeta)[names(agmeta) == "Level"] <- "AgLevel"
  names(agmeta)[names(agmeta) == "iCode"] <- "Code"
  names(agmeta)[names(agmeta) == "iName"] <- "Name"
  names(agmeta)[names(agmeta) == "Level"] <- "AgLevel"
  # remove unused cols
  agmeta <- agmeta[!(names(agmeta) %in% c("Direction", "Unit", "Target", "Denominator", "Parent", "Type"))]

  # build COIN
  COIN <- assemble(idata, imeta, agmeta)

  if(recover_dsets){
    # cycle through data sets and add to COIN
    dsets <- setdiff(names(coin$Data), "Raw")
    for(dset in dsets){
      COIN$Data[[dset]] <- extract_IndData(coin, dset)
    }

  }

  if(out2 == "COIN"){
    COIN
  } else if (out2 == "list"){
    list(IndData = idata,
         IndMeta = imeta,
         AggMeta = agmeta)
  } else {
    stop("out2 should be either 'list' or 'COIN'")
  }

}

# extract a named data set from a coin in COIN-format
extract_IndData <- function(coin, dset, withdenoms = FALSE){


  idata <- coin$Data[[dset]]

  # merge on metadata
  idata <- merge(coin$Meta$Unit, idata, by = "uCode")

  # names
  names(idata)[names(idata) == "uCode"] <- "UnitCode"
  names(idata)[names(idata) == "uName"] <- "UnitName"
  names(idata)[names(idata) == "Time"] <- "Year"

  # get imeta
  imeta <- coin$Meta$Ind

  # rename groups
  grps <- imeta$iCode[imeta$Type == "Group"]
  if(length(grps) > 0){
    for(grp in grps){
      names(idata)[names(idata) == grp] <- paste0("Group_", grp)
    }
  }
  # rename dens
  dens <- imeta$iCode[imeta$Type == "Denominator"]
  if(length(dens) > 0){
    for(den in dens){
      names(idata)[names(idata) == den] <- paste0("Den_", den)
    }
  }
  # rename others
  oths <- imeta$iCode[imeta$Type == "Others"]
  if(length(oths) > 0){
    for(oth in oths){
      names(idata)[names(idata) == oth] <- paste0("x_", oth)
    }
  }

  if(!withdenoms){
    # remove denominators
    idata <- idata[!startsWith(names(idata), prefix = "Den_")]
  }

  idata

}
