
# Scraping event data from Dept website

# Libraries
library(tidyverse)
library(rvest)
library(calendar)

##### Identifying Series and Pulling Splash Pages #####
pull_splash <- function(){
  # First, the Deparmental Seminar Series page is pulled
  series_page = read_html("https://www.arch.cam.ac.uk/events/departmental-seminar-series")
  # Then, the titles and corresponding urls are scraped and stored in a list
  dept_talks <- series_page %>%
    html_nodes(".campl-teaser-title a") %>% 
    html_text()
  
  talk_splashes <- series_page %>%
    html_nodes(".campl-primary-cta") %>%
    html_attr("href")
  
  info <- list(series_title = c(dept_talks, 
                                "Other Cambridge Events", 
                                "Outreach Events"),
               splash_href = c(talk_splashes, 
                               "/events/other-cambridge-talks",
                               "/events/outreach-and-public-engagement-events"))
  
  # Finally, the splash pages for the seminar series (which contains the event info) are
  #   pulled and stored as a third element in the list
  for (i in seq_along(info$series_title)) {
    info$splash[i] <- list(splash_pages = read_html(paste("https://www.arch.cam.ac.uk",
                                                     info$splash_href[i],
                                                     sep = "")))
  }
  # The class is added to control inputs into follow on functions
  class(info) <- "cdal_splash"
  return(info)
}




##### Cleaning and parsing date/time data #####
# This function takes the date and time information as it is formatted on the 
#   Departmental website and formats it to the iCal standard.
dt_cleanup <- function(raw_dt) {
  
  talk_year = vector()
  talk_month = vector()
  talk_day = vector()
  talk_start = vector()
  start_dt = vector()
  end_dt = vector()
  output = vector()
  
  for (i in seq_along(raw_dt)) {
    
    talk_year[i] = raw_dt[i] %>%
      str_remove(pattern = "^.*,\\s") %>%
      str_remove(pattern = "\\s-\\s.*") 
    
    talk_month[i] = raw_dt[i] %>%
      str_extract(pattern = "\\s[A-z]*,") %>%
      str_extract(pattern = "[A-Z][a-z]{2}") %>%
      match(table = month.abb) %>%
      as.character()  %>%
      lapply(FUN = function(x){
        if (nchar(x) < 2) {x = paste0("0", x, sep = "")}
        else {x=x}
        }
      )
          
    talk_day[i] = raw_dt[i] %>%
      str_extract(pattern = "\\s[0-9]{1,2}") %>%
      str_extract(pattern = "[0-9]{1,2}") %>%
      lapply(FUN = function(x){
        if (nchar(x) < 2) {x = paste0("0", x, sep = "")}
        else {x=x}
        }
      )
    
    talk_start[i] = raw_dt[i] %>%
      str_extract(pattern = "-\\s[0-9]{2}:[0-9]{2}") %>%
      str_extract(pattern = "[0-9]{2}:[0-9]{2}") %>%
      str_remove(pattern = ":")
    
# Formats dt info into iCal compliant dt string
    start_dt = paste(talk_year,
                     talk_month,
                     talk_day,
                     "T",
                     talk_start,
                     "00Z",
                     sep = "")

    
    output = start_dt
  }
  return(output)
}




##### Scraping event info from the pulled splash pages #####
scrape_events <- function(x) {
  
# Allows only the output of the pull_splash function as input
  if (class(x) != "cdal_splash"){
    stop("Input must be of class cdal_splash")
  }
  else {
# Scrape talk name, speaker name, POC email, talk location, talk date and time,
#   and the name of the associated seminar series
    output <- data.frame()
    for (i in seq_along(x$splash)){
      event_count <- length(x$splash[[i]] %>%
                              html_nodes("#block-views-child-events-block a"))
      temp_mat <- matrix(NA, 
                         nrow = event_count,
                         ncol = 6)
      
      talk_name <- x$splash[[i]] %>%
        html_nodes("#block-views-child-events-block a") %>%
        html_text() 
      
      talk_speaker <- x$splash[[i]] %>%
        html_nodes("td.views-field-field-event-speaker") %>%
        html_text() %>%
        str_remove(pattern = "\\n\\s*") %>%
        str_remove(pattern = "\\s*$") 
      
      talk_email <- x$splash[[i]] %>% 
        html_node(".field-label-above a") %>%
        html_text() %>%
        rep(length(talk_name))
      
      talk_start <- x$splash[[i]] %>%
        html_nodes(".date-display-single") %>%
        html_text() %>%
        dt_cleanup()

      talk_location <- rep(x$splash[[i]] %>%
                             html_nodes(".field-name-field-event-location .even") %>%
                             html_text(),
                           times = event_count
      )
      
      talk_series <- rep(x$series_title[i],
                         times = event_count
      )

# Combine scraped event info into temporary matrix.
      temp_mat[,1] <- talk_series
      temp_mat[,2] <- talk_name
      temp_mat[,3] <- talk_speaker
      temp_mat[,4] <- talk_start
      if (identical(talk_location, character(0)) == FALSE){
        temp_mat[,5] <- talk_location
      }
      temp_mat[,6] <- talk_email
      
# Bind matrix together into dataframe
      output <- data.frame(rbind(output, data.frame(temp_mat, 
                                         stringsAsFactors = FALSE)
                                 )
      )
      
    }
    return(output)
  }
}  

##### Convert scraped events into iCal format
format_vevents <- function(events) {
  temp <- tibble()
  for (i in seq_along(events[,1])) {
    temp <- rbind(temp,
                  ic_event(start_time = as.POSIXct(ic_datetime(events[i,4])),
                           summary = paste(events[i,3],
                                     "@",
                                     events[i,1]),
                           more_properties = TRUE,
                           event_properties = c(LOCATION = events[i,5],
                                                DESCRIPTION = paste("Title:",
                                                              events[i,2] %>%
                                                                str_replace(pattern = ",",
                                                                            replacement = "\\\\,"),
                                                              "\n",
                                                              "Contact Email:",
                                                              events[i,6],
                                                              sep = "")
                                                )
                           )
                  )
  }
  temp = ical(temp,
              ic_attributes = c(BEGIN = "VCALENDAR",
                                PRODID = "-//ARCH.CAM.AC.UK//CDAL-CALENDAR//EN",
                                VERSION = "2.0")
  )
  return(temp)
}

##### Save iCal file

save_ical <- function(ical_events) {
  ic_write(ic = ical_events,
           file = choose.files(default = "department_calendar.ics",
                               multi = FALSE,
                               caption = "Save Archaeology Department Calendar"))
}
