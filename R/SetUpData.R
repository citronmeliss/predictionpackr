#' Set up study data function
#' @param raw_study_data imported data with no manipulations
#' @export

SetUpData <- function(raw_study_data){

  # Apply age exclusion
  study_data <- raw_study_data %>%
    filter(age >= 18)

  # Set up mechanism of injury
  study_data <- CollapseMoi(study_data)

  # Change 999 to NA
  study_data <- study_data %>%
    mutate(nsi = na_if(nsi, "999"),
           age = na_if(age, "999"),
           sbp = na_if(sbp, "999"),
           hr = na_if(hr, "999"),
           rr = na_if(rr, "999"),
           egcs = na_if(egcs, "999"),
           vgcs = na_if(vgcs, "999"),
           mgcs = na_if(mgcs, "999")
    )

  # Create new column for GCS
  # Add together eye, verbal and motor component of GCS and create combined GCS column
  study_data <- study_data %>%
    mutate(cgcs = egcs + vgcs + mgcs)

  # Create new column for ICU admission < 48h
  # Create new columns, ed_admit and icu_admit for combined time and date of arrivial
  study_data <- study_data %>%
    mutate(ed_admit = paste0(doar, toar)) %>%
    mutate(icu_admit = paste0(daicu, taicu))

  # Transform columns into POSIXct object to handle as dates
  study_data$ed_admit <- as.POSIXct(study_data$ed_admit, format = "%Y-%m-%d %H:%M")

  study_data$icu_admit <- as.POSIXct(study_data$icu_admit, format = "%Y-%m-%d %H:%M")

  # Create final column with ICU adission within 48 h
  study_data <- study_data %>%
    mutate(icu48h = difftime(icu_admit, ed_admit, units = "hours"))

  # Change all who were admitted within 48h to 1, and the rest to 0
  study_data$icu48h <- ifelse(study_data$icu48h <= 48, 1, 0)

  # Convert NA values in icu48h to those of daicu/taicu
  study_data$icu48h <- with(study_data, ifelse(daicu == "0" | taicu == "0", 0, icu48h ))

  study_data$icu48h <- with(study_data, ifelse(daicu == "999" | taicu == "999", 999, icu48h))

  # Change to factor
  # Change sex from integer to factor
  study_data$sex <- as.factor(study_data$sex)

  # Change avpu
  study_data$avpu <- as.factor(study_data$avpu)

  # Change s24h
  study_data$s24h <- as.factor(study_data$s24h)

  # Change icu48h
  study_data$icu48h <- as.factor(study_data$icu48h)

  # Modify 24h mortality column
  # Combine the two categories for alive in the s24h column
  study_data <- study_data %>%
    mutate(s24h = fct_collapse(s24h,
                               alive = c("0", "2"),
                               dead = "1",
                               unknown = "999"))

  # Relevel sex
  study_data$sex <- factor(study_data$sex, levels = c("1", "0", "999"))

  # Relevel s24h
  study_data$s24h <- factor(study_data$s24h, levels = c("dead", "alive", "unknown"))

  # Relevel moi
  study_data$moi <- factor(study_data$moi, levels = c("Transport accident",
                                                      "Fall",
                                                      "Other external cause of accidental injury",
                                                      "Assault",
                                                      "Burn",
                                                      "Intentional self harm",
                                                      "Event of undetermined intent",
                                                      "999"))

  # Relevel avpu
  study_data$avpu <- factor(study_data$avpu, levels = c("3", "2", "1", "0", "999"))

  # Relevel icu48h
  study_data$icu48h <- factor(study_data$icu48h, levels = c("1", "0", "999"))

  # Create composite outcome
  study_data <- study_data %>%
    mutate(composite = ifelse(icu48h == 1 | s24h == "dead", 1, 0))

  # Add columns with prediction model scores
  study_data <- study_data %>%
    mutate(gap = ModelGAP(study_data))

  study_data <- study_data %>%
    mutate(gerdin = ModelGerdin(study_data))

  study_data <- study_data %>%
    mutate(kts = ModelKTS(study_data))

  study_data <- study_data %>%
    mutate(rts = ModelRTS(study_data))

  return(study_data)
}
