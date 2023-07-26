pinfo <- function(AWS_ATHENA_CONN_NOCTUA, PIN, YEAR) {
  PIN <- format_pin(PIN)

  # Check that PIN is correct length
  if (nchar(PIN) == 14) {
    output <- dbGetQuery(
      AWS_ATHENA_CONN_NOCTUA,
      glue("
        SELECT * FROM rpie.vw_code_retrieval
        WHERE pin = '{PIN}' AND year = '{YEAR}'
        ")
    )

    # Check that PIN returns any information
    if (nrow(output > 0)) {
      return(output)
    } else {
      new_pin <- dbGetQuery(
        AWS_ATHENA_CONN_NOCTUA,
        glue("
        SELECT MAX(taxyr) FROM iasworld.pardat
        WHERE parid = '{PIN}'
        ")
      )

      if (new_pin == substr(Sys.Date(), 1, 4)) {
        return("new pin")
      } else {
        return("bad pin")
      }
    }
  } else {
    return("no pin")
  }
}

message <- function(y, year) {
  # Formats the main output message for the end-user providing
  # the RPIE code and address for a given PIN

  if (!is.data.frame(y)) {
    if (y == "bad pin") {
      # If PIN field isn't empty and PIN can't be found
      msg <- paste0(tags$h3(paste0(
        "Sorry, we don't have an RPIE record for this PIN ",
        emoji("pensive")
      )))
      return(msg)
    } else if (y == "new pin") {
      # Deliver pdf URL if PIN is new
      msg <- sprintf(paste0(
        "<h3>This is a new PIN for ",
        year,
        " and has not yet been assigned an RPIE code - you will need to submit an ",
        "<a href='https://github.com/ccao-data/wiki/blob/master/RPIE/RPIE2023.pdf'>RPIE pdf form</a>",
        " with your appeal rather than using the RPIE website.</h3>"
      ))
      return(msg)
    } else if (y == "no pin") {
      # Output nothing if PIN field is empty
      msg <- paste0(tags$h3(paste0(
        "Please enter a valid PIN ",
        emoji("sweat_smile")
      )))
      return(msg)
    }
  } else {
    # Create HTML formatted elements of output message
    PIN <- tags$span(
      style = "color:coral; font-family:consolas; font-weight:bold",
      pin_format_pretty(y$pin, full_length = TRUE)
    )
    RPIE_CODE <- tags$span(
      style = "color:#337ABD; font-family:consolas; font-weight:bold",
      y$rpie_code
    )
    address <- tags$p(
      style = "margin-left: 40px; font-style: italic",
      HTML(paste(y %>% select(starts_with("mail")), collapse = "<br>"))
    )

    if (substr(y$class, 1, 1) != 2) {
      return(paste0(
        "<br><br><br><h3>The ",
        year,
        " RPIE code for PIN ",
        PIN,
        " is ",
        RPIE_CODE,
        "<br></h3><i>
          Please note the difference between
          <span style='font-family:consolas'>0</span> (zero)
           and <span style='font-family:consolas'>O</span> (letter O)
           when communicating with taxpayers.
          </i><h3><hr>
          It was mailed to:
          <br><br>",
        address,
        "</h3>"
      ))
    } else {
      # Let the user know if a taxpayer has requested a code for a
      # residential property (which won't need to file an RPIE)
      return(paste0(
        "<h3>The ",
        year,
        " RPIE code for PIN ",
        PIN,
        " is ",
        RPIE_CODE,
        "</h3><hr><h4>This is a class ",
        y$class,
        " (",
        tolower(ccao::class_dict %>%
          dplyr::filter(class_code == y$class) %>%
          dplyr::pull(class_desc)),
        ") residential parcel ",
        "and did not receive a mailer.</h4>"
      ))
    }
  }
}

format_pin <- function(PIN) {
  # format the input PIN
  PIN <- gsub("\\D+", "", PIN)

  # make sure PINs are 14 digits
  if (nchar(PIN) == 10) {
    PIN <- str_pad(PIN, width = 14, side = "right", pad = "0")

    return(PIN)
  } else {
    return(PIN)
  }
}

clean_list <- function(data, year) {
  # Cleans the PIN column in data that users upload so that
  # it can be joined to RPIE codes

  data <- data %>%
    mutate(
      # Remove non-numeric characters from list of PINs
      PIN = gsub("\\D+", "", PIN),
      across(where(is.character), ~ na_if(.x, "")),
      # Make sure PINs are 14 digits
      PIN = str_pad(PIN, width = 14, side = "right", pad = "0")
    )

  # Structure PINs for SQL pull
  pins <- paste0("'", paste(data$PIN, collapse = "', '"), "'")

  output <- data %>%
    left_join(

      # Gather RPIE codes
      dbGetQuery(AWS_ATHENA_CONN_NOCTUA, glue('
      SELECT pin AS PIN, rpie_code AS "RPIE Code"
      FROM rpie.pin_codes
      WHERE year = CAST({year} AS varchar)
      AND pin IN ({pins})
      '))
    ) %>%
    # Format PIN for output
    mutate(PIN = ccao::pin_format_pretty(PIN, full_length = TRUE)) %>%
    relocate(c("PIN", "RPIE Code")) %>%
    discard(~ all(is.na(.) | . == ""))

  # If no PINs have RPIE codes, RPIE Code column will need to be added back
  if (ncol(output) < 2) output[, 2] <- NA

  names(output)[1:2] <- c("PIN", paste0(year, " RPIE Code"))

  return(output)
}
