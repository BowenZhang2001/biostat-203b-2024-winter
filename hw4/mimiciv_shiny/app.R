# LOAD PACKAGES
library(tidyverse)
library(shiny)
library(ggplot2)
library(bigrquery)
library(dbplyr)

# LOAD DATA
mimic_icu_cohort <- readRDS("mimic_icu_cohort.rds") |>
  mutate(insurance = as.factor(insurance),
         marital_status = as.factor(marital_status),
         gender = as.factor(gender))

satoken <- "../biostat-203b-2024-winter-313290ce47a6.json"
bq_auth(path = satoken)
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)

# GET DATA FROM BIGQUERY
patients_info <- tbl(con_bq, "patients") |>
  select(subject_id, gender, anchor_age) |>
  collect()

diagnoses <- tbl(con_bq, "d_icd_diagnoses")

patients_diagnose <- tbl(con_bq, "diagnoses_icd") |>
  filter(seq_num <= 3) |>
  left_join(diagnoses, by = c("icd_code", "icd_version"))

rm(diagnoses)

procedure <- tbl(con_bq, "d_icd_procedures")

patients_procedures <- tbl(con_bq, "procedures_icd") |>
  mutate(chartdate = as.POSIXct(chartdate)) |>
  left_join(procedure, by = c("icd_code", "icd_version")) |>
  mutate(Procedure = str_extract(long_title, "[^,]+"))

rm(procedure)

patients_adt <- tbl(con_bq, "transfers") |>
  filter(eventtype != "discharge") |>
  select(-transfer_id) |>
  mutate(ICU_CCU = str_detect(careunit, 'CU'))

patients_lab <- tbl(con_bq, "labevents")

icu_stay <- tbl(con_bq, "icustays")

icu_stay_id <- icu_stay |>
  select(subject_id) |>
  collect() |>
  distinct()

items <- tbl(con_bq, "d_items") |>
  filter(abbreviation %in% c("HR", "NBPd", "NBPs", "RR", "Temperature F")) |>
  select(itemid, label, abbreviation)

vitals_tble <- tbl(con_bq, "chartevents") |>
  filter(itemid %in% c(220045, 220180, 220179,
                       223761, 220210)) |>
  left_join(items, by = "itemid") |>
  select(subject_id, stay_id, charttime, label, abbreviation, valuenum) |>
  filter(!is.na(valuenum)) |>
  mutate(charttime = as.POSIXct(charttime))


generate_title <- function(info){
  subject_id <- str_c("Patient ", as.character(info[1, "subject_id"]))
  gender <- as.character(info[1, "gender"])
  age <- str_c(as.character(info[1, "anchor_age"]), " years old")
  return(str_c(subject_id, gender, age, str_to_lower(info[1, "race"]),
               sep = ", "))
}


# CHOOSE VARIABLES
var_gp <- c("demo", "lab_measure", "vitals")
demo <- c("insurance", "marital_status", "race", "gender", "age_intime")
lab_measure <- c("Hematocrit", "Bicarbonate", "Glucose", "Chloride",
                 "WBC", "Potassium", "Creatinine", "Sodium")
vitals <- c("RR", "HR", "NBPs", "NBPd", "TempF")

# USER INTERFACE
ui <- fluidPage(
  titlePanel("ICU Cohort Data"),
  
  tabsetPanel(
    # Tab 1 -- Graphical and numerical summaries
    tabPanel("Summary",
             sidebarPanel(
               # First choose variable group demo/lab_measure/vitals
               selectInput("var_group", "Variable Group", var_gp),
               # Then choose variable within that group
               uiOutput("var_selector"),
               uiOutput("slider")
               ),
             mainPanel(
               plotOutput("plot1"),
               verbatimTextOutput("table1")
               )
             ),
    # Tab 2 -- Specific patient's ADT & ICU info
    tabPanel("Patient Info",
             sidebarPanel(
               numericInput("subject_id", "Subject ID", ""),
               actionButton("submit", "Submit"),
               selectInput("plot_selector", "Select a plot:",
                           choices = c("ADT", "ICU"))
             ),
             mainPanel(
               verbatimTextOutput("NotExist"),
               plotOutput("icu_info")
             )
             )
             
             
  )
)
  


# SERVER LOGIC
server <- function(input, output) {
  # Make the 2nd input box
  output$var_selector <- renderUI({
    var_group <- input$var_group
    choices <- switch(var_group,
                      "demo" = demo,
                      "lab_measure" = lab_measure,
                      "vitals" = vitals)
    selectInput("var_name", "Variable", choices = choices)
  })
  
  output$slider <- renderUI({
    var_group <- input$var_group
    var_name <- input$var_name
    if (var_group != "demo" || var_name == "age_intime") {
      xlim_min <- min(mimic_icu_cohort[[var_name]], na.rm = T)
      xlim_max <- max(mimic_icu_cohort[[var_name]], na.rm = T)
      sliderInput("range", "xlim",
                  min = xlim_min, max = xlim_max,
                  value = c(xlim_min, xlim_max))
    }
  })
  
  # Get the selected variable
  selected_vars <- reactive({
    var_group <- input$var_group
    var_name <- input$var_name
    list(var_group = var_group, var_name = var_name)
  })
  
  # Make the plot according to the variable
  output$plot1 <- renderPlot({
    # Get the selected variable via selected_vars
    selected <- selected_vars()
    var_group <- selected$var_group
    var_name <- selected$var_name
    
    if (var_group == "demo" && var_name != "age_intime") {
      ggplot(data = mimic_icu_cohort) +
        geom_bar(mapping = aes_string(x = var_name, y = "after_stat(count)",
                                      fill = var_name),
                 na.rm = T) +
        theme_bw()
    } else {
      ggplot(data = mimic_icu_cohort) +
        geom_histogram(mapping = aes_string(x = var_name)) +
        theme_bw() +
        xlim(input$range)
    }
  })
  
  output$table1 <- renderPrint({
    var_name <- selected_vars()$var_name
    summary(mimic_icu_cohort[[var_name]])
  })
  
  # If press the action, show the plot selector
  observeEvent(input$submit, {
  select_patient <- reactive({
    patient_id <- input$subject_id
    list(patient_id = patient_id)
  })
  
  selected <- select_patient()$patient_id
  if(selected %in% icu_stay_id$subject_id){
    output$NotExist <- NULL
    
    # Create a new tible for the selected patient
    patient <- patients_info |>
      filter(subject_id == selected)
    
    patient$race <- tbl(con_bq, "admissions") |>
      filter(subject_id == selected) |>
      select(race) |>
      collect() |>
      distinct() %>% .$race
    
    plot_title <- generate_title(patient)

    
    # Then we get the subtitle of the plot
    selected_subtitle <- patients_diagnose |>
      filter(subject_id == selected) |>
      arrange(hadm_id) |>
      select(long_title) |>
      collect()
    selected_subtitle <- str_c(selected_subtitle[1, "long_title"],
                               selected_subtitle[2, "long_title"],
                               selected_subtitle[3, "long_title"], sep = "\n")
    
    # Then we get the adt info
    selected_adt <- patients_adt |>
      filter(subject_id == selected) |>
      arrange(intime) |>
      collect()
    
    # Then we get the lab info
    selected_lab <- patients_lab |>
      filter(subject_id == selected) |>
      select(charttime) |>
      collect() |>
      distinct()
    
    # Last we get the procedure info
    selected_procedure <- patients_procedures |>
      filter(subject_id == selected) |>
      collect()
    
    # Some preparation for the plot
    number_of_procedure <- selected_procedure |>
      select(Procedure) |>
      distinct() |>
      nrow()
    
    time_range <- c(selected_adt[[1, "intime"]],
                    selected_adt[[nrow(selected_adt), "outtime"]])
    
    empty_data <- tibble(CalenderTime = c(time_range, NA),
                         Information = c("ADT", "Lab", "Procedure"))
    
    empty_data$Information <- factor(empty_data$Information,
                                     levels = c("Procedure", "Lab", "ADT"))

    # Draw the plot2
    plot2 <- ggplot(empty_data,
                    mapping = aes(x = CalenderTime, y = Information)) +
      geom_blank() +
      geom_point(data = selected_lab, mapping = aes(x = charttime, y = "Lab"),
                 shape = 3, size = 2) +
      geom_point(data = selected_procedure,
                 mapping = aes(x = chartdate,
                               y = "Procedure",
                               shape = Procedure),
                 size = 3) +
      geom_segment(data = selected_adt,
                   mapping = aes(x = intime, xend = outtime,
                                 y = "ADT", yend = "ADT",
                                 color = careunit,
                                 linewidth = ICU_CCU)) +
      theme_bw() +
      # legend.position = "bottom" is to put the legend at the bottom
      # legend.box = "vertical" is to arrange legend shape & color vertically
      theme(legend.position="bottom", legend.box = "vertical",
            text = element_text(size = 8)) +
      # shape is on the top and color below, no linewidth legend
      guides(shape = guide_legend(ncol = 2, order = 1),
             color = guide_legend(ncol = 2, order = 2),
             linewidth = FALSE) +
      # the name of the color.legend should be Care Unit
      labs(title = plot_title,
           subtitle = selected_subtitle,
           color = "Care Unit",
           x = "Calendar Time", y = "") +
      scale_shape_manual(values = c(1:number_of_procedure))
    
    # Grab the data for icu_plot (plot3)

    selected_icu <- icu_stay |>
      filter(subject_id == selected) |>
      arrange(intime) |>
      collect()
    
    selected_vital <- vitals_tble |>
      filter(subject_id == selected) |>
      collect() |>
      group_by(stay_id)
    
    # Draw the plot3
    plot3_title <- str_c("Patient", as.character(selected),
                         "ICU stays - Vitals", sep = " ")
    plot3 <- ggplot(data = selected_vital) +
      geom_line(mapping = aes(x = charttime, y = valuenum,
                              color = abbreviation), show.legend = FALSE) +
      geom_point(mapping = aes(x = charttime, y = valuenum,
                               color = abbreviation),
                 size = 1, show.legend = FALSE) +
      facet_grid(abbreviation ~ stay_id, scales = "free") +
      theme_light() +
      labs(title = plot3_title, x = "", y = "") +
      scale_x_datetime(date_labels = "%b %d %H:%M") +
      # This function make the x_axis more readable
      guides(x = guide_axis(n.dodge = 2))
    
    
    # render the plot2
    output$icu_info <- renderPlot({
      if(input$plot_selector == "ADT"){plot2}
      else{plot3}
    })
    

    
    
  }
  else{
    output$NotExist <- renderPrint({
      "This patient_id does not exist!"
    })
    output$adt_info <- NULL
    output$icu_info <- NULL
  }
  })
  
}

shinyApp(ui = ui, server = server)