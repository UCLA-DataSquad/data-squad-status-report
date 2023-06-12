library(tidyverse)
library(lubridate)
library(ggthemes)

lib <- read_csv("libinsights_dataframe.csv")

### PREPARING THE DATA

# Getting rid of columns containing only 1 unique value
helpful_ix <- sapply(lib, function(x) length(unique(x))) != 1
li <- lib[, helpful_ix]

# Changing Event Date column to lubridate datetime objects
li$`Event Date and Time (System required field)` <- 
  mdy_hm(li$`Event Date and Time (System required field)`)

# Subsetting meetings by a certain date
ds_2023 <- li$`Event Date and Time (System required field)` > "2021-08-01"
li <- li[ds_2023, ]

# Removing meeting ID column
li <- li[, -1]

### FINDING THE NUMBER OF CONSULTATIONS

ds_members <- c("Gong, Emily", "Mirpuri, Shail", "Dewing, Tristan", 
                "Front, Vincenty", "Wood, Julia", "Pablo, Mae, Keona",
                "Foote, William")

contained_ds_member <- logical(nrow(li))

for (meeting in seq_along(contained_ds_member)) {
  
  if (li[meeting, "Entered By (System optional field)"] %in% ds_members) {
    
    contained_ds_member[meeting] <- TRUE
    
  } else {
    
    names <- li[meeting, "Instructor/Co-instructor", drop = TRUE] %>% 
      str_split_1(";") %>% 
      str_to_title()
    
    contained_ds_member[meeting] <- any(names %in% ds_members)
  }
  
  if (li[meeting, "Event or Class Title"] == "testing") {

    contained_ds_member[meeting] <- FALSE
  }
}

# consultation_count - number of meetings that contained a DataSquad member
consultation_count <- sum(contained_ds_member)

### FINDING THE TYPES OF MEETINGS

ds_meetings <- li[contained_ds_member, ]

# Get a table of emails so we can pick out the ones of DSC members
ds_meetings %>%
  select(`Faculty/TA/Staff Email`) %>%
  group_by(`Faculty/TA/Staff Email`) %>%
  count() %>%
  arrange(desc(n)) #%>% 
  #View()

dsc_emails <- c("tdennis@library.ucla.edu", "edvinmolla@ucla.edu", 
                "mirpurishail@gmail.com", "leighphan@ucla.edu", 
                "ethanallavarpu@g.ucla.edu", "juliawood1@gmail.com",
                "williamfoote@g.ucla.edu", "ibraheemali@library.ucla.edu",
                "keonamaepablo@g.ucla.edu", "ashleypeterson@library.ucla.edu",
                "dougdaniels@library.ucla.edu", "egong2000@gmail.com",
                "jamison@library.ucla.edu", "keonamaepablo@gmail.com")

# Grouping meeting titles into smaller groups
meeting_types <- ds_meetings[, "Event or Class Title", drop = TRUE]
new_types <- character(length(meeting_types))

for (meeting in seq_along(meeting_types)) {
  
  type <- meeting_types[meeting] 
  
  if (type %>% str_detect("Minute Meeting") | 
      type %>% str_detect("Hour Meeting") | 
      type %>% str_detect("One Meeting")) 
    {
    
    if (ds_meetings[meeting, "Faculty/TA/Staff Email"] %in% dsc_emails) {
      
      new_types[meeting] <- "Inner DSC Meeting/Consultation"
      
    } else {
      
      new_types[meeting] <- "Consultation with Patron"
    }
    
  } else if (type %>% str_detect("Consult") | 
             (type %>% str_detect("Data") & 
              type != "Data Collection Project Meeting")
             ) {
    
    new_types[meeting] <- "Consultation with Patron"
    
  } else {
    
    new_types[meeting] <- "Inner DSC Meeting/Consultation"
    
  }
}
  
# Looking at just the "Consultation with Patron" meetings
formal_consults <- meeting_types[new_types == "Consultation with Patron"]

formal_consults[formal_consults == "Data & Coding Consultation"] <- "Coding Consultation"

# Filtering down to only DataSquad Calendly consultation events
formal_consults_ix <- 
  formal_consults %in% c("Coding Consultation", "Data Cleaning and Manipulation",
                         "Data Visualization", "Statistical Consulting")
formal_consults <- formal_consults[formal_consults_ix]

patron_counts <- data.frame(sort(table(formal_consults), decreasing = TRUE))

### FINDING UCLA AFFILIATION OF PATRONS

# This information wasn't gathered effectively for this report. Future reports
# should have code written to include this information.

# FINDING THE DEPARTMENTS DSC WORKED WITH
# In the future, this should just be DataSquad but there wasn't enough information

depts <- li[, "School/Department/Center", drop = TRUE]

dept_counts <- data.frame(sort(table(depts)))

# Finding the top n most helped departments - arbitrarily chose a top 12 by 
# looking at the dept_counts object
top12_ix <- nrow(dept_counts):(nrow(dept_counts) - 11)
top12_depts <- dept_counts[top12_ix, ]

# Getting rid of "Other" and "Multiple"
top10_depts <- top12_depts[-c(10, 12), ]

# For Status Report Markdown
top3depts <- top10_depts[1:3, "depts", drop = TRUE]
