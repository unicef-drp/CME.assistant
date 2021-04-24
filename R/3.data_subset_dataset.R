# functions to add new entries to master datasets for U5MR, IMR, NMR



#' Create IGME_Key column
#'
#' Create column `IGME_Key` in the format of
#' ISO3Code-Series.Year-Series.Name-Series.Category,
#' e.g."TZA-1988-Census-Indirect", "RWA-2017-Malaria Indicator Survey-Indirect".
#' Extra strings like "Preliminary" or "MM/NN adjusted" are removed
#'
#'
#' @param dt0 dataset
#' @param add_Indicator default to `FALSE`, if `TRUE` will add indicator in the
#'   end of `IGME_Key`, keep another `IGME_Key_s` column which doesn't have
#'   indicator in the end
#'
#' @return dt0 dataset with added column `IGME_Key`
#' @export create.IGME.key
create.IGME.key <- function(dt0, add_Indicator = FALSE){
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
  # countries with SVR by year
  dt0[Series.Category %in% c("SVR") & Country.Name == "South Africa", IGME_Key := paste0(Code, "-", Series.Year, "-", Series.Category)]

  dt0[, IGME_Key := gsub(strings_to_remove, "", IGME_Key)]
  # remove blank
  dt0[, IGME_Key := trimws(IGME_Key)]
  # add direct/indirect?
  dt0[grepl("Direct", Series.Type), IGME_Key := paste0(IGME_Key, "-Direct")]
  dt0[grepl("Indirect", Series.Type), IGME_Key := paste0(IGME_Key, "-Indirect")]

  if(add_Indicator){
    dt0[, IGME_Key_s := IGME_Key]
    dt0[, IGME_Key := paste0(IGME_Key_s, "-", Indicator)]
  }
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
#' function used by `add.new.series` functions, remove unwanted age groups
#' @param dt_new_entries dt of new entries to be added
#' @return dt_new_entries with 25-34 and 10-19 removed
#' @export revise.age.group
revise.age.group <- function(dt_new_entries){
  if(nrow(dt_new_entries[Age.Group.of.Women=="25-34"])>0){
    message("Remove AOW group '25-34' for ", paste(dt_new_entries[Age.Group.of.Women=="25-34", unique(IGME_Key)], collapse = ", "))
    dt_new_entries <- dt_new_entries[!Age.Group.of.Women%in%c("25-34"), ]

  }
  if(nrow(dt_new_entries[Age.Group.of.Women=="19-Oct"])>0){
    message("Remove AOW group '10-19' for ", paste(dt_new_entries[Age.Group.of.Women=="25-34", unique(IGME_Key)], collapse = ", "))
    dt_new_entries <- dt_new_entries[!Age.Group.of.Women%in%c("10-19", "19-Oct"), ]
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
  remove_old = FALSE
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
      # print those existing ones that are changed to 0
      message("Change inclusion and visible for old entry to 0: \n",
              paste(dt_master[IGME_Key %in% unique(dt_new_entries$IGME_Key), unique(IGME_Key)],
              collapse = "\n"))
      dt_master[IGME_Key %in% unique(dt_new_entries$IGME_Key), Inclusion:=0]
      dt_master[IGME_Key %in% unique(dt_new_entries$IGME_Key), Visible := 0]
    }

  }
  # recreate IGME Key
  dt_new_entries <- create.IGME.key(dt_new_entries)
  dt1 <- rbindlist(list(dt_master, dt_new_entries))
  dup_key <- dt1[duplicated(dt1), unique(IGME_Key)]
  if(length(dup_key)>0) message("Notice duplicated series: ", paste(dup_key, collapse = ", "))
  setorder(dt1, Country.Name, -Indicator, -Sex, -End.date.of.Survey, Series.Name, Series.Type, -Date.Of.Data.Added, -Reference.Date, - Inclusion)
  message("Newly added:", paste(unique(dt_new_entries$IGME_Key), collapse = ", "))
  message("new nrow:", nrow(dt1), " -adding- ", nrow(dt1) - nrow(dt_master))
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
  remove_old = FALSE
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
      # print those existing ones that are changed to 0
      message("Change inclusion and visible for old entry to 0: \n",
              paste(dt_IMR[IGME_Key %in% unique(dt_new_entries$IGME_Key), unique(IGME_Key)],
                    collapse = "\n"))
      dt_IMR[IGME_Key %in% unique(dt_new_entries$IGME_Key), Inclusion:=0]
      dt_IMR[IGME_Key %in% unique(dt_new_entries$IGME_Key), Visible := 0]
    }

  }
  message("nrow after removing old entries:", nrow(dt_IMR))
  # recreate IGME Key
  dt_new_entries <- create.IGME.key(dt_new_entries)
  dt1 <- rbindlist(list(dt_IMR, dt_new_entries))
  dt1[duplicated(dt1), unique(IGME_Key)]
  setorder(dt1, Country.Name, -Indicator, -Sex, -End.date.of.Survey, Series.Name, Series.Type, -Date.Of.Data.Added, -Reference.Date, - Inclusion)
  message("Newly added:", paste(unique(dt_new_entries$IGME_Key), collapse = ", "))
  message("new nrow:", nrow(dt1), " -adding- ", nrow(dt1) - nrow(dt_IMR))
  if(nrow(dt_IMR) + nrow(dt_new_entries) != nrow(dt1)) warning("check row numbers ")
  return(dt1)
}

#' Add new NMR entries (`dt_new_entries`)
#'
#' @param dt_nmr the NMR master dataset
#' @param dt_new_entries dt of new entries to be added
#' @param remove_old if TRUE, remove old entries, if FALSE, set old entries to
#'   invisible and excluded
#' @return the new dt_nmr_new
#' @export add.new.series.nmr
add.new.series.nmr <- function(
  dt_nmr,
  dt_new_entries,
  remove_old = FALSE
  ){
  message("old nrow:", nrow(dt_nmr))
  dt_new_entries <- revise.age.group(dt_new_entries)

  key0s <- dt_new_entries[, unique(IGME_Key)]
  if(nrow(dt_nmr[IGME_Key %in% key0s,]) > 0){
    if(remove_old){
      message("Remove existing (possibly old) entries: ", paste(key0s, collapse = ", "))
      dt_nmr <- dt_nmr[!IGME_Key %in% key0s]
    } else {
      # print those existing ones that are changed to 0
      message("Change inclusion and visible for old entry to 0: \n",
              paste(dt_nmr[IGME_Key %in% key0s, unique(IGME_Key)],
                    collapse = "\n"))
      dt_nmr[IGME_Key %in% key0s, Inclusion:=0]
      dt_nmr[IGME_Key %in% key0s, Visible := 0]
    }

  }

  # recreate IGME Key
  dt_new_entries <- create.IGME.key(dt_new_entries)
  dt_nmr_new <- rbind(dt_nmr, dt_new_entries, fill = TRUE)
  if(nrow(dt_nmr) + nrow(dt_new_entries) != nrow(dt_nmr_new)) warning("check row numbers ")
  if(ncol(dt_nmr) != ncol(dt_new_entries)) warning("check col numbers ")
  setorder(dt_nmr_new, Country.Name, -End.date.of.Survey, Series.Name, Series.Type, -Date.Of.Data.Added, -Reference.Date, -Inclusion)


  message("old ncol:", ncol(dt_nmr))
  message("new ncol:", ncol(dt_nmr_new))
  message("Newly added:", paste(key0s, collapse = ", "))
  message("new nrow:", nrow(dt_nmr_new), " -adding- ", nrow(dt_nmr_new) - nrow(dt_nmr))

  return(dt_nmr_new)
}




#' Add new VR series
#'
#' @param dt_master master dataset
#' @param dt_new_entries new entries
#' @param NMR is it NMR or not, only controls how the columns are sorted
#' @param hide_old default to TRUE, set old ones to invisible = 0 and inclusion
#'   = 0, otherwise leave as it is
#'
#' @export
add.new.VR <- function(
  dt_master,
  dt_new_entries,
  NMR = FALSE,
  hide_old = TRUE
){
  dt_master <- copy(dt_master)
  message("original nrow:", nrow(dt_master))
  dt_new_entries <- revise.age.group(dt_new_entries)

  nrow_old <- nrow(dt_master[IGME_Key %in% unique(dt_new_entries$IGME_Key)
                             & Series.Name%in%unique(dt_new_entries$Series.Name),])
  if(nrow_old > 0){
    if(hide_old){
      # print those existing ones that are changed to 0
      message("Change inclusion and visible for old entry to 0: /n",
              paste(dt_master[IGME_Key %in% unique(dt_new_entries$IGME_Key)
                              & Series.Name%in%unique(dt_new_entries$Series.Name), unique(IGME_Key)],
                    collapse = "/n"))
      message("Change inclusion and visible for old entry to 0: /n",
              paste(dt_master[IGME_Key %in% unique(dt_new_entries$IGME_Key)
                              & Series.Name%in%unique(dt_new_entries$Series.Name), unique(Series.Name)],
                    collapse = "/n"))
      dt_master[IGME_Key %in% unique(dt_new_entries$IGME_Key) &
                  Series.Name%in%unique(dt_new_entries$Series.Name),
                `:=`(Inclusion = 0, Visible = 0)]
    } else {
      message("Old VR series unchanged.")
    }
  }

  # recreate IGME Key
  dt_new_entries <- create.IGME.key(dt_new_entries)
  dt1 <- rbind(dt_master, dt_new_entries)
  dup_key <- dt1[duplicated(dt1), unique(IGME_Key)]
  if(length(dup_key)>0) message("Notice duplicated series: ", paste(dup_key, collapse = ", "))
  if(NMR){
    setorder(dt1, Country.Name, -End.date.of.Survey, Series.Name, Series.Type, -Date.Of.Data.Added, -Reference.Date, -Inclusion)

  } else {
    setorder(dt1, Country.Name, -Indicator, -Sex, -End.date.of.Survey, Series.Name, Series.Type, -Date.Of.Data.Added, -Reference.Date, - Inclusion)
  }
  message("Newly added:", paste(unique(dt_new_entries$IGME_Key), collapse = ", "))
  message("new nrow:", nrow(dt1), " -adding- ", nrow(dt1) - nrow(dt_master))
  if(nrow(dt_master) + nrow(dt_new_entries) != nrow(dt1)) warning("check row numbers ")
  return(dt1)
}


