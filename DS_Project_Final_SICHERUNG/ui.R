# Set working directory
#setwd('/home/ubuntu/shiny_apps/DS_Project_RobertNeulen/')

# Load data
source("global_variables.R")


# Set UI
navbarPage(
  
  # App title
  title="Corona Dashboard",
  

  # App theme
  theme = shinytheme("flatly"),
  
  
  
  # Additional CSS settings
  header=tags$head(tags$style(HTML("
      .navbar .navbar-nav {font-size: 20px; } 
      .navbar.navbar-default.navbar-static-top{font-size: 20px;}
      .navbar .navbar-header {float: left; } 
      .navbar-default .navbar-brand { font-size: 20px;} 
      .nav-tabs {font-size: 20px} "
                  )
             )
            ),
  
  
  
  
  ###########################################################################################################################################
  # Page 1
  tabPanel("Introduction",
           
          sidebarPanel(h3("Description"),
                       p(style="text-align: justify; font-size:18px;",
                         "This project has been developed as a Data Science project in the M.Sc. program 'Data Science in Business and Economics' at the University of Tuebingen",
                         br(),
                         br(),
                         "If you have feedback, questions or further remarks  with respect to the project, please do not hesitate to contact me."),
                       br(),
                       p(style="text-align: justify; font-size:18px;",
                         strong("Development")),
                       p(style="text-align: justify; font-size:18px;",
                         "This project was develped by Robert Neulen"),
                       actionButton(inputId = "robert_neulen_marketing", 
                                    icon = icon("linkedin"),
                                    label = NULL,
                                    onclick = "window.open('https://www.linkedin.com/in/robert-neulen-24999711b', '_blank')"),
                       br(),
                       br(),
                       p(style="text-align: justify; font-size:18px;",
                         strong("Supervision")),
                       p(style="text-align: justify; font-size:18px;",
                         "This project was supervised by Prof. Dr. Dominik Papies, Professor of Marketing"),
                       actionButton(
                         inputId = "marketing_university", 
                         icon = icon("university"),
                         label = NULL,
                         onclick = "window.open('https://uni-tuebingen.de/fakultaeten/wirtschafts-und-sozialwissenschaftliche-fakultaet/faecher/fachbereich-wirtschaftswissenschaft/wirtschaftswissenschaft/lehrstuehle/betriebswirtschaftslehre/marketing/marketing/', '_blank')"),
                       width=4),
          
          
          mainPanel(fluidRow(column(5,
                                    h3("Introduction"),
                                    p(style="text-align: justify; font-size:18px;",
                                      "In December 2019, the first cases of the initial strain of the Corona virus were reported in Wuhan, China (",
                                      a('cnn.com',
                                        href = 'https://edition.cnn.com/2021/08/09/health/covid-19-pandemic-timeline-fast-facts/index.html',
                                        .noWS = c('outside')),
                                      "). In a matter of months later, the virus spreaded over the whole world. Since then, government worldwide try to deal with the pandemic in different ways. In Germany, daily discussions about the right course,",
                                      "especially with respect to non-pharmaceutical interventions (NPI), are part of the agenda.")),
                             column(5, style="margin-left:30px",
                                    h3("Project Goal"),
                                    p(style="text-align: justify; font-size:18px;",
                                      "The app should open up the opportunity to explore the Corona situation in Germany, regional demographic and economical characteristics as well as how different characteristics and interventions influence the course of the pandemic."))
                             ),


                    br(),
                   
                    fluidRow(column(5, 
                                    h3("App Structure"),
                                    p(style="text-align: justify; font-size:18px;",
                                      strong("Corona Situation in Germany")),
                                    p(style="text-align: justify; font-size:18px;",
                                      "Provides an overview over critical Corona key figures such as infection and death cases, ICU capacities and the progress of the vaccination campaign."),
                                    
                                    p(style="text-align: justify; font-size:18px;",
                                      strong("Non-Pharmaceutical Interventions (NPI)")),
                                    p(style="text-align: justify; font-size:18px;",
                                      "Different NPIs got introduced at different points in time in different states. To provide an overview, this section allows to select a specific intervention (on aggregated or non-aggregated level)",
                                      "and visualizes at which time it got introduced or dropped again. It is also possible to select multiple regions to control for local differences in the use of NPIs to fight the pandemic."),
                                    
                                    p(style="text-align: justify; font-size:18px;",
                                      strong("Socio-Demographic and Economic Analysis")),
                                    p(style="text-align: justify; font-size:18px;",
                                      "The regions within Germany differ quite substantially with respect to their socio-demographic and economic characteristics. This part provides the opportunity to check the characteristics of a specific region",
                                      "and compare it to other regions."),
                                    
                                    p(style="text-align: justify; font-size:18px;",
                                      strong("Cluster Analysis")),
                                    p(style="text-align: justify; font-size:18px;",
                                      "Characteristics as described in the previous section can have a crucial effect on the course of the pandemic. To get an intuition, this section applies a k-Mean algorithm to cluster regions by selected characteristics",
                                      "and shows the distribution of infection and death cases for the regions within these clusters.",
                                      br(),
                                      "Do be aware, this is",
                                      strong("NOT"),
                                      "a casual analysis and the results should be evaluated with caution."),
                                    
                                    p(style="text-align: justify; font-size:18px;",
                                      strong("NPI Evaluation")),
                                    p(style="text-align: justify; font-size:18px;",
                                      "Politicians have to decide which interventions should be implemented or dropped again, depending on the course of the pandemic. For this, they need to know which interventions are useful to slow down the pandemic.",
                                      "This section allows to apply a fixed-effects regression to evaluate the effect of different NPIs, which can be chosen individually.")),
                             column(5, style="margin-left:30px",
                                    h3("Data"),
                                    p(style="text-align: justify; font-size:18px;",
                                      "The data is provided by ",
                                      a("Corona data platform infas",
                                        href = "https://www.corona-datenplattform.de/",
                                        .noWS = c('outside')),
                                      ", which is part of a project comissioned by the Federal Ministiry for Economic Affairs and Energy.",
                                      br(),
                                      br(),
                                      "The data not only gathers information with respect to up-to-date Corona indicators (incidence rate, death rate, hospitality rate, etc.)",
                                      "but also socio-economic and demographic variables (e.g. regional GDP). These variables are gathered from other official data sources,",
                                      "like the Federal Statistical Office and the Federal Employment Agency. A full overview on the data sources of the database can be seen in the",
                                      a("dataplatform report (phase 1)",
                                        href = "https://www.bmwi.de/Redaktion/DE/Downloads/I/Infas%20360%20Bericht%20Corona-Datenplattform%20BMWI%20Phase%201.html"),
                                      "on the website of the Federal Ministiry for Economic Affairs and Energy",
                                      br(),
                                      br(),
                                      "The data plattform is supervised by a diverse team of researchers from different institutions, ranging from universities and research institutions to public authorities.",
                                      "The full list of the supervisory board, as well as an overview of research which uses the dataplatform can also be seen on the ",
                                      a("website",
                                        href = "https://www.corona-datenplattform.de/pages/projekt",
                                        .noWS = c('outside')),
                                      "."))
                             )
          
          )
  ),

  


  ###########################################################################################################################################
  # Page 2
  tabPanel("Corona in Germany",

           tabsetPanel(

             tabPanel("Corona Situation in Germany",
                      sidebarPanel(h3("Description"),
                                   p(style="text-align: justify; font-size:18px;",
                                     "This page provides an overview over the Corona pandemic in Germany since its start in March 2020.",
                                     "In the main panel, the cases in Germany over time are shown, as well as the segmentation for age group and gender.",
                                     "In the selection field below it is possible to switch between the graphs for the infection and the fatal cases.",
                                     "A gap in the graphs for the segmentation indicates a day with 0 cases, either infection or fatal cases, depending on the selection.",
                                     "It is also possible to select a specific region and whether the graph of the cases should be scaled so that it shows the cases per 100,000."
                                     ),

                                   br(),

                                   h3("Select Topic of Interest"),
                                   selectInput("selection_germany_plots_var",
                                               label=NULL,
                                               choices = c("Infection Cases"="kr_inf_md",
                                                           "Fatal Cases"="kr_tod_md"),
                                               selected=c("kr_inf_md")),


                                   h3("Select Region"),
                                   selectInput("selection_germany_plots_region",
                                               label=NULL,
                                               choices=c("All", l_kreis),
                                               multiple=FALSE,
                                               selected=c("All")),

                                   h3("Cases per 100,000"),
                                   switchInput("selection_germany_plots_scaled",
                                               label=NULL,
                                               value=TRUE,
                                               onLabel="Yes",
                                               offLabel="No"),

                                   br(),


                                   h3("7-Day Incidence Rate"),

                                   p(style="text-align: justify; font-size:18px;",
                                     "The graph shows the 7-day incidence rate of Germany per region on a daily basis.",
                                     "The date slider below the graph allows to select the incidence rate for a specific date."
                                     ),

                                   plotOutput("map_germany", height=500) %>% withSpinner(color="#0dc5c1"),


                                   sliderInput("dates_maps_germany",
                                               label=NULL,
                                               min = min(df_incidence$date), 
                                               max = max(df_incidence$date),
                                               timeFormat="%Y-%m-%d",
                                               value=min(df_incidence$date)),



                                   width=4),

                      br(),
                      br(),

                      mainPanel(h3("Cases per Day"),
                                plotOutput("graph_germany") %>% withSpinner(color="#0dc5c1"),
                                br(),
                                br(),
                                h3("Distribution of Cases by Age Group and Gender"),
                                plotOutput("graph_germany_age_gender") %>% withSpinner(color="#0dc5c1")

                      )
             ),


             tabPanel("Intensive Care Units",

                      sidebarPanel(h3("Description"),
                                   p(style="text-align: justify; font-size:18px;",
                                     "Intensive care unit (ICU) beds are one of the crucial resources in the fight against",
                                     "the Corona pandemic. This tab provides on overview over the number of ICU beds in Germany or per selected region,",
                                     "as well as over the amount of free ICU capacities."),
                                   br(),
                                   p(style="text-align: justify; font-size:18px;",
                                     "In addition the share of COVID-19 patients among all ICU patients as well as the share of COVID-19 ICU patients",
                                     "who needs a ventilator treatment can be seen."),

                                   h3("Select Region"),
                                   selectInput("select_region_icu",
                                               label=NULL,
                                               choices=c("All", l_kreis),
                                               multiple=FALSE,
                                               selected=c("All")),


                                   width=4),

                      mainPanel(h3("ICU Capacities"),
                                plotOutput("icu_beds", height=800) %>% withSpinner(color="#0dc5c1"),

                                br(),
                                br(),

                                h3("Distribution of used ICU Capacities"),
                                plotOutput("icu_beds_ventilator", height=800) %>% withSpinner(color="#0dc5c1"))
             ),


             tabPanel("Vaccination Campaign",

                      sidebarPanel(h3("Description"),
                                   p(style="text-align: justify; font-size:18px;",
                                     "The start of the vaccination campaign marks a crucial turning point in the fight against the pandemic."),
                                   br(),
                                   p(style="text-align: justify; font-size:18px;",
                                     "This tab provides an overview over the ongoing vaccination campaign. Since data is not available",
                                     "on a regional level, the overview is provided on state level. It is also possible to see the vaccination",
                                     "progress for the entirety of Germany."),
                                   br(),

                                   h3("Select State"),
                                   selectInput("select_region_vaccination",
                                               label=NULL,
                                               choices=l_bundesland,
                                               multiple=FALSE,
                                               selected=c("All")),
                                   width=4),


                      mainPanel(h3("Vaccinations per Day"),
                                plotOutput("graph_vaccination") %>% withSpinner(color="#0dc5c1"),

                                br(),
                                br(),

                                h3("Cumulated Vaccinations"),
                                plotOutput("graph_vaccination_cumulated") %>% withSpinner(color="#0dc5c1"))



             )
           )
  ),



  ###########################################################################################################################################
  # Page 3
  tabPanel("Non-Pharmaceutical Interventions (NPI)",
           sidebarLayout(

             # Side panel: Selection options
             sidebarPanel(

               # Description box
               h3("Description"),
               p(style="text-align: justify; font-size:18px;",
                 "This panel provides on overview over selected (non-pharmaceutical) interventions for a selected region over time.",
                 br(),
                 br(),
                 "It is possible to select between the aggregated intervention level and the non-aggregated level.",
                 "For a first intuition it is recommended to stay on the aggregated level since the non-aggregated level is broken down into very detailed sub-interventions.",
                 "You can download the entire overview of all interventions using the button",
                 strong("Download Intervention Description"),
                 " below.",
                 br(),
                 br(),
                 "When choosing the region of interest, it is possible to choose one, multiple, or all regions at once. If you choose",
                 strong("'All'"),
                 " then all regions in Germany will be shown, ignoring the other selections.",
                 br(),
                 br(),
                 "In the graph, the fields marked in blue indicate that there was an intervention in place at that point in time. Between the 2021-05-14 and the 2021-6-15,",
                 "no data with respect to the NPI is available."
                 ),

               br(),

               # Aggregation choice
               h3("Aggregated Level"),
               switchInput("select_agg_interventions",
                           label=NULL,
                           value=TRUE,
                           onLabel="Yes",
                           offLabel="No"),

               br(),

               # Select Intervention
               h3("Intervention"),
               fluidRow(column(12,
                               DT::dataTableOutput("intervention_description")
                               )
                        ),
               h4("Select Intervention"),
               uiOutput("intervention_choice"),


               # Download button
               downloadButton("download_description_intervention", "Download Intervention Description", class = "butt"),
               tags$head(tags$style(".butt{background-color:white;} .butt{color: black;}")),

               br(),
               br(),

               # Location selection
               h3("Region"),
               selectizeInput(inputId="select_intervention_location",
                              label=NULL,
                              choices=c("All", l_kreis),
                              multiple=TRUE,
                              options = list(plugins= list('remove_button')),
                              selected=c("Tuebingen")),

               width=4),

             # Main panel
             mainPanel(

               # Plot of interventions
               plotOutput("intervention") %>% withSpinner(color="#0dc5c1")
             )
           )
  ),





  ###########################################################################################################################################
  # Page 4
  tabPanel("Socio-Demographic and Economic Analysis",

           sidebarPanel(h3("Description"),

                        p(style="text-align: justify; font-size:18px;",
                          "This tab provides the opportunity to get more information on socio-demographic and economic characteristics of a specific region.",
                          "It is also possible to compare regions with each other and to include the entirety of German as a benchmark to get even more insights.",
                          br(),
                          br(),
                          "In the selection field below it is possible to select one ore more regions. By choosing the selection",
                          strong("'Germany'"),
                          "it is possible to inlude all of Germany as an additional benchmark."),

                        br(),

                        h3("Select Region"),
                        selectizeInput(inputId="region_SEA",
                                       label=NULL,
                                       choices=c("Germany", l_kreis),
                                       multiple=TRUE,
                                       options = list(plugins= list('remove_button')),
                                       selected=c("Tuebingen")),
                        width=4),

           mainPanel(
             tabsetPanel(

               tabPanel("Housing Situation",

                        br(),
                        h3("Share of Households by Living Space in Square Meter (sqm)"),
                        plotOutput("hh_livingspace_sqm")  %>% withSpinner(color="#0dc5c1"),

                        br(),
                        h3("Share of Households by Building Class"),
                        plotOutput("hh_livingclass")  %>% withSpinner(color="#0dc5c1")),



               tabPanel("Education",

                        br(),
                        h3("School Students"),
                        plotOutput("education_cs", height=400)  %>% withSpinner(color="#0dc5c1"),

                        br(),
                        h3("General Educational Level"),
                        plotOutput("education_gel", height=400)  %>% withSpinner(color="#0dc5c1"),

                        br(),
                        h3("Daycare Coverage"),
                        plotOutput("education_dcc", height=400)  %>% withSpinner(color="#0dc5c1")),


               tabPanel("Economy",

                        br(),

                        fluidRow(column(width=6,
                                        h3("GDP per Capita in 1,000 EUR"),
                                        plotOutput("map_gdp", height=600)  %>% withSpinner(color="#0dc5c1")
                                        ),
                                 column(width=6,
                                        h3("Share of each Industry"),
                                        plotOutput("economy_sector", height=600)  %>% withSpinner(color="#0dc5c1")
                                        )
                                 ),

                        br(),
                        h3("Unemployment Quota"),
                        plotOutput("unemployment", height=400)  %>% withSpinner(color="#0dc5c1"),

                        ),


               tabPanel("Social Indicator",

                        br(),
                        h3("Social Indicators - Poverty Quota"),
                        plotOutput("social_indicator", height=400)  %>% withSpinner(color="#0dc5c1")
                        ),

               tabPanel("Medical Sector",

                        br(),
                        h3("Medical Infrastructure"),
                        plotOutput("medical", height=400)  %>% withSpinner(color="#0dc5c1")
                        )

               )
             )





  ),






  ###########################################################################################################################################
  # Page 5

  tabPanel("Cluster Analysis",

           sidebarPanel(h3("Description"),
                        p(style="text-align: justify; font-size:18px;",
                          "To get a first intuition whether regional characteristics could influence COVID-19 case and death numbers,",
                          "this section provides a k-Means clustering algorithm to analyze these factors of interest depending on regional characteristics.",
                          "This is not a causal analysis and doesn't take differences in political interventions into account, hence the results should be",
                          "evaluated with caution."),
                        br(),

                        h3("Cluster Determination"),
                        p(style="text-align: justify; font-size:18px;",
                          "The first tab,", 
                          strong("Number of Clusters for Determination", .noWS = c("after")), 
                          ", deals with the question of how many clusters should be used in the k-Means analysis. Based on the variable categories in",
                          strong("Variable Selection", .noWS = c("after")), 
                          ", the graph shows the total within-cluster sum of squares (TWCSS) between each data point and the respective cluster centre (max. number of clusters can be chosen with",
                          strong("Number of Clusters for Determination", .noWS = c("after")),
                          "), depending on the number of clusters chosen."),
                          
                        br(),

                        h5(strong("Variable Selection")),
                        pickerInput("select_cluster_vars",
                                    label=NULL,
                                    choices=c("Age structure",
                                              "Economic sector structure",
                                              "Economic characteristics",
                                              "School and university students",
                                              "Population density",
                                              "Nursing home inhabitants",
                                              "Settlement types",
                                              "Childcare",
                                              "Clubs and Societies",
                                              "Foreign habitants",
                                              "Commuting"),
                                    options = list(`actions-box` = TRUE, style = "color:black"),
                                    multiple = T,
                                    selected = "Age structure",
                                    choicesOpt = list(style = rep("color: black;background: white;", length(unique(df_cluster_feat$category))))),

                        br(),

                        h5(strong("Max. Number of Tested Clusters")),
                        sliderInput("select_cluster_deter",
                                    label=NULL,
                                    min=5,
                                    max=25,
                                    value=10,
                                    step=1),
                        actionButton("action_cluster_determination","Analyse Optimal Number of Clusters", width="100%"),

                        br(),
                        br(),
                        

                        h3("Cluster Analysis"),
                        p(style="text-align: justify; font-size:18px;",
                          "After choosing the number of clusters using the analysis, the second tab provides the results of the analysis using the chosen variables",
                          "and the right number of clusters, specified below. The result tab",
                          strong("Cluster Evaluation"), 
                          "shows the distribution of the regions, as well as the distribution cumulated case and death counts",
                          "per cluster. Below these clusters, it is possible to select one or multiple variable groups which were used and shows the average value for each variable per cluster.",
                          "Absolute values are scaled per 10,000 habitants but rates are kept without any changes applied."),

                        numericInput("select_number_clusters",
                                     label=NULL,
                                     value = 2,
                                     min = 2,
                                     max = 25),
                        actionButton("action_cluster_analysis","Run Clustering", width="100%"),
                        width=4),


           mainPanel(
             tabsetPanel(

               tabPanel("Cluster Determination",
                        br(),
                        h3("Analysis of optimal number of clusters"),
                        p(style="text-align: justify; font-size:18px;",
                          "A good intuition is to choose the number of clusters where the change between TWCSS becomes less significant. Since there is no perfect method",
                          "for choosing the right number of clusters, varying specifications can be tested."),
                        plotOutput("cluster_determination", height=400)  %>% withSpinner(color="#0dc5c1")
                        ),
               

               tabPanel("Cluster Evaluation",

                        br(),
                        br(),

                        fluidRow(column(6,
                                        plotOutput("cluster_distribution", height=600)  %>% withSpinner(color="#0dc5c1")
                                        ),
                                 column(6,
                                        plotOutput("cluster_analysis", height=600)  %>% withSpinner(color="#0dc5c1")
                                        )
                                 ),

                        br(),
                        br(),

                        uiOutput("selection_table_cluster"),
                        br(),
                        DT::dataTableOutput("table_clustering_features"))

             )

           )

  ),


  ###########################################################################################################################################
  # Page 6
  tabPanel("NPI Evaluation",

           sidebarPanel(h3("Description"),
                        p(style="text-align: justify; font-size:18px;",
                          "To analyze the effect of different non-pharmaceutical interventions (NPI), this section provides the opportunity to analyze",
                          "different NPIs using a fixed-effect regression model. Region and month fixed-effects are used in this regression model.",
                          "These fixed-effects take regional characteristics (e.g. demographic characteristics) into account. It is also possible to include",
                          "other, time dependent, fixed-effects."),
                        p(style="text-align: justify; font-size:18px;",
                          strong("Time fixed-effects")),
                        checkboxGroupInput('fe_time',
                                           label=NULL,
                                           choices = list("Year"="year",
                                                          "Month"="month",
                                                          "Week"="week"),
                                           selected = NULL),
                        br(),
                        p(style="text-align: justify; font-size:18px;",
                          "The model is divided into three tabs:"),
                        p(style="text-align: justify; font-size:18px;",
                          strong("Target Variable")),
                        p(style="text-align: justify; font-size:18px;",
                          "It is possible to select from multiple target variables for the analysis. This tab also allows the user to restrict the time horizon",
                          "of the analysis by clicking into the plot and selecting the time horizon of interest."),
                        p(style="text-align: justify; font-size:18px;",
                          strong("NPI Selection")),
                        p(style="text-align: justify; font-size:18px;",
                          "This tab allwos to explore and select the NPIs of interest."),
                        p(style="text-align: justify; font-size:18px;",
                          strong("Model Result")),
                        p(style="text-align: justify; font-size:18px;",
                          "Provides the final overview over the model results after running the model using ", 
                          strong("Run Model", .noWS = c("after")), "."),

                        br(),

                        h3("Exclude Sunday and Monday Data"),
                        p(style="text-align: justify; font-size:18px;",
                          "Since the reporting data can be biased on Sundays and Mondays (due to reporting issues in the German authority system),",
                          "it is possible to exlcude those days from the analysis."),
                        switchInput("selection_days_drop",
                                    label=NULL,
                                    value=FALSE,
                                    onLabel="Yes",
                                    offLabel="No"),

                        br(),

                        h3("Number of Time Lags for Interventions"),
                        p(style="text-align: justify; font-size:18px;",
                          "Since there is a time-lag between the implementation of an NPI and its first measurable effect, this selection allows to push the effect of the NPIs",
                          "into the future."),
                        sliderInput("select_lag_int",
                                    label=NULL,
                                    min=0,
                                    max=14,
                                    value=0,
                                    step=1),


                        br(),
                        actionButton("action_model","Run Model", width="100%"),

                        width=4),


           mainPanel(

             tabsetPanel(

               tabPanel("Target Variable",

                        h3("Select Target Variable"),
                        pickerInput("select_target_pred",
                                    label=NULL,
                                    choices=c("7-Day Incidence Rate"="kr_inz_rate",
                                              "7-Day Death Rate"="kr_death_rate",
                                              "New Death Rate (Share of Cumulated Cases)"="kr_sterbe_rate",
                                              "New Infection Rate (Share of Cumulated Cases)"="kr_neuinf_rate"),
                                    options = list(`actions-box` = TRUE, style = "color:black"),
                                    multiple = F,
                                    choicesOpt = list(style = rep("color: black;background: white;", 4))),

                        br(),

                        h3("Average over Germany over Time"),
                        plotOutput("model_target", height=600, brush = brushOpts(id="brush", direction="x"))  %>% withSpinner(color="#0dc5c1")

               ),

               tabPanel("NPI Selection",

                        br(),

                        fluidRow(column(width=3,
                                        h3("Aggregated Interventions"),
                                        switchInput("select_agg_analysis",
                                                    label=NULL,
                                                    value=TRUE,
                                                    onLabel="Yes",
                                                    offLabel="No"),
                                        ),
                                 column(width=3,
                                        h3("Select Intervention"),
                                        uiOutput("int_choice_analysis")
                                        )

                                 ),

                        h3("Intervention"),
                        DT::dataTableOutput("int_desc_analysis"),

               ),


               tabPanel("Results",

                        br(),

                        h3("Model Coefficients (ordered) with 90% Confidence Interval"),
                        plotOutput("model_coeff", height=400)  %>% withSpinner(color="#0dc5c1"),

                        br(),

                        h3("Model Summary Table"),
                        verbatimTextOutput("modelSummary") %>% withSpinner(color="#0dc5c1")
               )
             ),



           )


  )
  
  
  
  
)

           