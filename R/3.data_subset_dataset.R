# functions to add new entries to master datasets for U5MR, IMR, NMR



#' Create IGME_Key column
#'
#' Extra strings like "Preliminary" or "MM/NN adjusted" are removed in the
#' created `IGME_Key` column
#'
#' @param dt0 dataset
#'
#' @return dt0 dataset with added column `IGME_Key`
#' @export create.IGME.key
create.IGME.key <- function(dt0){
  strings_to_remove <- " \\(Adjusted\\)| \\(MM adjusted\\)| \\(NN adjusted\\)| \\(Preliminary\\)| \\(preliminary\\)"

  # the process to create IGME_Key
  if ("Country.Code"%in%colnames(dt0)&is.character(dt0$Country.Code)) {
    dt0[, Code:= Country.Code]
  } else if ("Country.ISO"%in%colnames(dt0)&is.character(dt0$Country.ISO)) {
    dt0[, Code:= Country.ISO]
  } else {stop("Check Country.Code and Country.ISO")}
  # Some SVR like South Africa has year associated with it
  dt0[Series.Category %in% c("VR", "SVR"), IGME_Key := paste0(Code, "-", Series.Category)]
  dt0[Series.Type %in% c("Life Table"), IGME_Key := paste0(Code, "-", Series.Type)]
  # dt0[Series.Type %in% c("Life Table"), ]
  dt0[!Series.Category %in% c("VR", "SVR", "Life Table"), IGME_Key := paste0(Code, "-", Series.Year, "-", Series.Name)]
  dt0[Series.Category %in% c("SVR") & Country.Name == "South Africa", IGME_Key := paste0(Code, "-", Series.Year, "-", Series.Category)]
  dt0[, IGME_Key := gsub(strings_to_remove, "", IGME_Key)]
  dt0[, Code:=NULL]
  return(dt0)
}


# Add new series ----------------------------------------------------------

#' Row-bind two datasets, check duplicated keys and set order
#'
#' @param dt_master master dataset
#' @param dt_new new entries
#' @return `dt1` as `rbind(dt_master, dt_new)`
#' @export rbinddataset
rbinddataset <- function(dt_master, dt_new){
  message("old nrow:", nrow(dt_master))
  dt1 <- rbind(dt_master, dt_new)
  dup_key <- dt1[duplicated(dt1), unique(IGME_Key)]
  if(length(dup_key)>0) message("Notice duplicated series: ", paste(dup_key, collapse = ", "))
  setorder(dt1, Country.Name, -Indicator, -Sex, -End.date.of.Survey, Series.Name, Series.Type, -Date.Of.Data.Added, -Reference.Date, - Inclusion)
  message("new nrow:", nrow(dt1))
  if(nrow(dt_master) + nrow(dt_new) != nrow(dt1)) warning("check row numbers ")
  return(dt1)
}

#' Row-bind two datasets for NMR with slight changes, check duplicated keys, set
#' order
#'
#' @param dt_master master dataset
#' @param dt_new new entries
#' @return `dt1` as `rbind(dt_master, dt_new)`
#' @export rbinddatasetNMR
rbinddatasetNMR <- function(dt_master, dt_new){
  message("old nrow:", nrow(dt_master))
  dt1 <- rbind(dt_master, dt_new)
  dup_key <- dt1[duplicated(dt1), unique(IGME_Key)]
  if(length(dup_key)>0) message("Notice duplicated series: ", paste(dup_key, collapse = ", "))
  setorder(dt1, Country.Name, -End.date.of.Survey, Series.Name, Series.Type, -Date.Of.Data.Added, -Reference.Date, -Inclusion)
  message("new nrow:", nrow(dt1))
  if(nrow(dt_master) + nrow(dt_new) != nrow(dt1)) warning("check row numbers ")
  return(dt1)
}

#' Remove 25-34 age group in `dt_new_entries`
#'
#' Internal function used by `add.new.series` functions
#' @param dt_new_entries dt of new entries to be added
revise.age.group <- function(dt_new_entries){
  if(nrow(dt_new_entries[Age.Group.of.Women=="25-34"])>0){
    message("Remove AOW group '25-34' for ", paste(dt_new_entries[Age.Group.of.Women=="25-34", unique(IGME_Key)], collapse = ", "))
    dt_new_entries <- dt_new_entries[Age.Group.of.Women!="25-34", ]
  }
  return(dt_new_entries)
}

#' Add an original series name
#'
#' Add `Original.Series.Name` column by the supplied `ori_name` argument
#'
#' @param dt_new_entries  dt of new entries to be added
#' @param ori_name value for Original.Series.Name
#' @return dt_new_entries
#' @export add.Original.Series.Name
add.Original.Series.Name <- function(dt_new_entries, ori_name){
  dt_new_entries[, Original.Series.Name:= as.character(Original.Series.Name)]
  dt_new_entries[, Original.Series.Name:= ori_name]
  return(dt_new_entries)
}

#' Add new U5MR or IMR entries (dt_new_entries)
#'
#' @param dt_master the U5MR master dataset
#' @param dt_new_entries  dt of new entries to be added
#' @param remove_old if TRUE, remove old entries, if FALSE, set old entries to invisible and excluded
#' @return the new dt_master
#' @export add.new.series.u5mr
add.new.series.u5mr <- function(
  dt_master,
  dt_new_entries,
  remove_old = TRUE
  ){
  message("original nrow:", nrow(dt_master))
  dt_new_entries <- revise.age.group(dt_new_entries)
  nrow_old <- nrow(dt_master[IGME_Key %in% unique(dt_new_entries$IGME_Key),])
  if(nrow_old > 0){
    if(remove_old){
      message("Remove ", nrow_old, " existing (perhaps old) entries: ",
              paste(unique(dt_new_entries$IGME_Key), collapse = ", "))
      dt_master <- dt_master[!IGME_Key %in% unique(dt_new_entries$IGME_Key)]
      message("nrow after removing old entries:", nrow(dt_master))

    } else {
      message("Change inclusion and visible for old entry to 0: ", paste(unique(dt_new_entries$IGME_Key), collapse = ", "))
      dt_master[IGME_Key %in% unique(dt_new_entries$IGME_Key), Inclusion:=0]
      dt_master[IGME_Key %in% unique(dt_new_entries$IGME_Key), Visible := 0]
    }

  }
  # recreate IGME Key
  dt_new_entries <- create.IGME.key(dt_new_entries)
  dt1 <- rbind(dt_master, dt_new_entries)
  dup_key <- dt1[duplicated(dt1), unique(IGME_Key)]
  if(length(dup_key)>0) message("Notice duplicated series: ", paste(dup_key, collapse = ", "))
  setorder(dt1, Country.Name, -Indicator, -Sex, -End.date.of.Survey, Series.Name, Series.Type, -Date.Of.Data.Added, -Reference.Date, - Inclusion)
  message("new nrow:", nrow(dt1), " -adding- ", nrow(dt1) - nrow(dt_master))
  message("Newly added:", paste(unique(dt_new_entries$IGME_Key), collapse = ", "))
  if(nrow(dt_master) + nrow(dt_new_entries) != nrow(dt1)) warning("check row numbers ")
  return(dt1)
}

#' Add new IMR entries (dt_new_entries), now the same function as
#' `add.new.series.u5mr`
#'
#' Still kept in case needed in the future in case need to differentiate IMR
#' from U5MR process
#'
#' @param dt_IMR the IMR master dataset
#' @param dt_new_entries  dt of new entries to be added
#' @param remove_old if TRUE, remove old entries, if FALSE, set old entries to
#'   invisible and excluded
#' @return the new dt_IMR
#' @export add.new.series.imr
add.new.series.imr <- function(
  dt_IMR,
  dt_new_entries,
  remove_old = TRUE
  ){
  message("original nrow:", nrow(dt_IMR))
  dt_new_entries <- revise.age.group(dt_new_entries)
  # dt_new_entries$To.be.adjusted
  # dt_IMR[, table(To.be.adjusted, useNA = "ifany")]
  # dt_IMR$To.be.adjusted <- NA
  if(nrow(dt_IMR[IGME_Key %in% unique(dt_new_entries$IGME_Key),]) > 0){
    if(remove_old){
      message("Remove existing (possibly old) entries: ", paste(unique(dt_new_entries$IGME_Key), collapse = ", "))
      dt_IMR <- dt_IMR[!IGME_Key %in% unique(dt_new_entries$IGME_Key)]
    } else {
      message("Change inclusion and visible for old entry to 0: ", paste(unique(dt_new_entries$IGME_Key), collapse = ", "))
      dt_IMR[IGME_Key %in% unique(dt_new_entries$IGME_Key), Inclusion:=0]
      dt_IMR[IGME_Key %in% unique(dt_new_entries$IGME_Key), Visible := 0]
    }

  }
  message("nrow after removing old entries:", nrow(dt_IMR))
  # recreate IGME Key
  dt_new_entries <- create.IGME.key(dt_new_entries)
  dt1 <- rbind(dt_IMR, dt_new_entries)
  dt1[duplicated(dt1), unique(IGME_Key)]
  setorder(dt1, Country.Name, -Indicator, -Sex, -End.date.of.Survey, Series.Name, Series.Type, -Date.Of.Data.Added, -Reference.Date, - Inclusion)
  message("new nrow:", nrow(dt1), " -adding- ", nrow(dt1) - nrow(dt_IMR))
  message("Newly added:", paste(unique(dt_new_entries$IGME_Key), collapse = ", "))
  if(nrow(dt_IMR) + nrow(dt_new_entries) != nrow(dt1)) warning("check row numbers ")
  return(dt1)
}

#' Add new NMR entries (`dt_new_entries`)
#'
#' @param dt_nmr the NMR master dataset
#' @param dt_new_entries dt of new entries to be added
#' @return the new dt_nmr_new
#' @export add.new.series.nmr
add.new.series.nmr <- function(
  dt_nmr,
  dt_new_entries
  ){
  message("old nrow:", nrow(dt_nmr))

  key0s <- dt_new_entries[, unique(IGME_Key)]
  dt_existing <- dt_nmr[IGME_Key%in%key0s & Visible==1]
  if(nrow(dt_existing)>0){
    message("Remove ", dt_existing[,.N], " rows of ", dt_existing$IGME_Key[1] ," as old data exist")
    dt_nmr <- dt_nmr[!(IGME_Key%in%key0s & Visible==1)]
  }

  for(key0 in key0s){
    dt_nmr[IGME_Key==key0]
    if(nrow(dt_nmr[IGME_Key==key0])>0) {
      message("Check existing entries for ", key0)
      message("Inclusion for", key0, dt_nmr[IGME_Key==key0, Inclusion], "\n")
      dt_nmr[IGME_Key==key0, Inclusion:=0]
      message("New Inclusion for", key0, dt_nmr[IGME_Key==key0, Inclusion], "\n")
      message("Visible for", key0, dt_nmr[IGME_Key==key0, Visible], "\n")
      dt_nmr[IGME_Key==key0, Visible:=0]
      message("New Visible for", key0, dt_nmr[IGME_Key==key0, Visible], "\n")
    }
  }
  # recreate IGME Key
  dt_new_entries <- create.IGME.key(dt_new_entries)
  dt_nmr_new <- rbind(dt_nmr, dt_new_entries, fill = TRUE)
  if(nrow(dt_nmr) + nrow(dt_new_entries) != nrow(dt_nmr_new)) warning("check row numbers ")
  if(ncol(dt_nmr) != ncol(dt_new_entries)) warning("check col numbers ")
  setorder(dt_nmr_new, Country.Name, -End.date.of.Survey, Series.Name, Series.Type, -Date.Of.Data.Added, -Reference.Date, -Inclusion)

  message("new nrow:", nrow(dt_nmr_new), " -adding- ", nrow(dt_nmr_new) - nrow(dt_nmr))

  message("old ncol:", ncol(dt_nmr))
  message("new ncol:", ncol(dt_nmr_new))
  message("Newly added:", paste(unique(dt_new_entries$IGME_Key), collapse = ", "))

  return(dt_nmr_new)
}


