# ==============================================================================
#
library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(stringr)
library(bslib)


file_path <- "job_data.csv"
if (!file.exists(file_path)) {
  stop("job_data.csv not found. Please ensure the file is in the correct directory.")
}


job_data_raw <- read.csv(file_path, stringsAsFactors = FALSE)


job_data_clean <- job_data_raw %>%
  rename_all(~tolower(gsub("[^A-Za-z0-9_]", "", .))) %>%
  
  rename(city = indian_city, skills = required_skills) %>%
  select(-indian_state)


skills_long <- job_data_clean %>%
 
  select(company, position, city, skills) %>%
  mutate(skills = str_split(skills, ", ")) %>%
 
  unnest(skills) %>%
 
  mutate(skills = str_trim(skills)) %>%
  
  filter(skills != "" & !is.na(skills) & skills != "na") %>%
  
  mutate(skills = str_to_title(skills))

ui <- navbarPage(
  title = "Indian Job Market Analysis",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#4E79A7", secondary = "#F28E2B"),
  
  
  tabPanel("Analysis Dashboard",
           sidebarLayout(
             sidebarPanel(
               h3("Filter and Controls"),
               
              
               selectInput(
                 inputId = "city_filter",
                 label = "Filter by City:",
                 choices = c("All Cities" = "", unique(sort(job_data_clean$city))),
                 selected = ""
               ),
               
              
               sliderInput(
                 inputId = "top_n",
                 label = "Select Top N Items to Display:",
                 min = 5,
                 max = 20,
                 value = 10,
                 step = 1
               ),
               
               width = 3 # Sidebar width
             ),
             
             mainPanel(
               h2("Exploratory Data Visualization"),
               fluidRow(
                 column(6,
                        h4(HTML("Top Required Skills")),
                        # Plot 1: Top N Skills
                        plotlyOutput("skill_plot")
                 ),
                 column(6,
                        # UI TEXT MODIFIED HERE
                        h4(HTML("Job Opportunities by City")), 
                        # Plot 2: Top N Cities (Fixed/Unfiltered)
                        plotlyOutput("city_plot")
                 )
               ),
               hr(),
               # New Row for Top Job Profiles and their Required Skills
               fluidRow(
                 column(6,
                        h4(HTML("Top 7 Job Profiles and Top 3 Skills")),
                        # Table 2: Top Positions and Top 3 Skills
                        DTOutput("job_profile_skill_table")
                 ),
                 column(6,
                        h4(HTML("Top Hiring Companies")),
                        # Table 1 (DT): Top Companies
                        DTOutput("company_table")
                 )
               )
             )
           )
  ),
 
  tabPanel("Raw Data",
           h2("Original Job Posting Data"),
           DTOutput("raw_data_table")
  ),
 
  tabPanel("About",
           fluidRow(
             column(8, offset = 2,
                    h2("About this Analysis", class = "text-center"),
                    hr(),
                    
                    h3("Data Source and Scope"),
                    p("The dataset, provided as ", code("job_data.csv"), ", contains scraped job posting data (as outlined in the project proposal). Key columns include job position, company, location (Indian City), and a comma-separated list of required skills. The original state column has been excluded from this analysis."),
                    p("The analysis focuses on exploring the demand for specific skills and the geographic distribution of job opportunities across India."),
                    
                    h3("Exploratory Data Analysis (EDA) & Methods"),
                    p("The analysis dashboard utilizes several key data transformation and visualization techniques:"),
                    tags$ul(
                      tags$li(strong("Data Cleaning:"), "The 'Required_Skills' column was cleaned by splitting the comma-separated strings, trimming whitespace, and converting entries to Title Case to ensure consistent grouping of identical skills (e.g., 'python' and ' Python' are treated as the same)."),
                      tags$li(strong("Aggregation:"), "Job postings were aggregated to calculate the frequency of unique skills, the number of postings per city, and the most frequent hiring companies."),
                      tags$li(strong("Role-Specific Skills:"), "A dedicated analysis identifies the top 7 most frequent job titles and determines the **top 3** most in-demand skills for each of those roles, providing role-specific insights."),
                      tags$li(strong("Interactivity:"), "The dashboard allows users to filter the analysis based on a specific ", strong("City"), " and adjust the 'Top N' items. **The Job Opportunities by City chart is intentionally fixed to display overall market trends, regardless of the City filter.**"),
                      tags$li(strong("Visualization:"), "We use interactive bar charts (via the ", code("plotly"), " package) and data tables (via the ", code("DT"), " package).")
                    )
             )
           )
  )
)

server <- function(input, output, session) {
 
  filtered_jobs <- reactive({
    data <- job_data_clean
    
    
    if (input$city_filter != "") {
      data <- data %>% filter(city == input$city_filter)
    }
    
    data
  })
  
  
  filtered_skills <- reactive({
    req(nrow(filtered_jobs()) > 0)
    
   
    filtered_jobs() %>%
      select(company, position, city, skills) %>%
      mutate(skills = str_split(skills, ", ")) %>%
      unnest(skills) %>%
      mutate(skills = str_trim(skills)) %>%
      filter(skills != "" & !is.na(skills) & skills != "na") %>%
      mutate(skills = str_to_title(skills))
  })
 
  top_profiles_and_skills <- reactive({
    req(nrow(filtered_skills()) > 0)
    
    top_positions <- filtered_jobs() %>%
      count(position, sort = TRUE) %>%
      head(7) %>%
      pull(position)
    
   
    skill_data_for_top_jobs <- filtered_skills() %>%
      filter(position %in% top_positions)
   
    top_skills_by_position <- skill_data_for_top_jobs %>%
      group_by(position, skills) %>%
      count(name = "skill_count") %>%
      ungroup() %>%
     
      group_by(position) %>%
      arrange(desc(skill_count), .by_group = TRUE) %>%
      slice(1:3) %>% 
      summarise(
        top_required_skills = paste(skills, collapse = ", "),
        .groups = 'drop'
      ) %>%
      
     
      left_join(filtered_jobs() %>% count(position, name = "job_count"), by = "position") %>%
      arrange(desc(job_count)) %>%
      select(position, job_count, top_required_skills) %>%
      rename("Job Profile" = position, "Total Postings" = job_count, "Top 3 Required Skills" = top_required_skills)
    
    return(top_skills_by_position)
  })
  
  output$job_profile_skill_table <- renderDT({
    req(nrow(top_profiles_and_skills()) > 0)
    
    datatable(top_profiles_and_skills(),
              options = list(
                paging = FALSE,
                searching = FALSE,
                info = FALSE
              ),
              caption = "Top 7 Job Profiles and the top 3 most common skills required for each."
    )
  })
 
  output$skill_plot <- renderPlotly({
    req(nrow(filtered_skills()) > 0) 
    skill_counts <- filtered_skills() %>%
      count(skills, sort = TRUE) %>%
      head(input$top_n) %>%
      mutate(skills = factor(skills, levels = skills)) # Preserve order
    
    p <- ggplot(skill_counts, aes(x = skills, y = n, fill = n)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_text(aes(label = n), hjust = -0.1, size = 3) +
      coord_flip() +
      scale_fill_gradient(low = "#4E79A7", high = "#A0CBE8") +
      labs(
        x = "Skill",
        y = "Number of Job Postings",
        title = paste("Top", input$top_n, "Required Skills (Filtered by City)")
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)
      )
    
    # Convert ggplot to interactive plotly object
    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE) # Hide plot controls
  })
  
 
  output$city_plot <- renderPlotly({
    req(nrow(job_data_clean) > 0)
    
    city_counts <- job_data_clean %>%
      count(city, sort = TRUE) %>%
      head(input$top_n) %>% # Always use top_n on the full list
      mutate(city = factor(city, levels = city)) # Preserve order
    
    p <- ggplot(city_counts, aes(x = city, y = n, fill = n)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_text(aes(label = n), vjust = -0.5, size = 3) +
      scale_fill_gradient(low = "#F28E2B", high = "#FFBE7D") +
      labs(
        x = "City",
        y = "Number of Job Postings"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE)
  })
 
  output$company_table <- renderDT({
    req(nrow(filtered_jobs()) > 0)
    
    
    company_counts <- filtered_jobs() %>%
      count(company, sort = TRUE) %>%
      head(input$top_n) %>%
      rename("Company Name" = company, "Total Postings" = n)
    
    datatable(company_counts,
              options = list(
                paging = FALSE,
                searching = FALSE,
                info = FALSE
              ),
              caption = paste("Showing Top", input$top_n, "Companies in the current filter context.")
    )
  })
 
  output$raw_data_table <- renderDT({
    datatable(job_data_clean,
              options = list(
                pageLength = 10,
                scrollX = TRUE
              ))
  })
}

shinyApp(ui = ui, server = server)