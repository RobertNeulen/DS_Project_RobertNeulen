
# Server
function(input, output) {
  
  
  ###########################################################################################################################################
  # Page 1
  


  ###########################################################################################################################################
  # Corona Situation

  # Incidence plot Germany
  output$map_germany <- renderPlot({

    plot_germany_incidence(df_incidence, input$dates_maps_germany, df_geo)

  }, height=500, bg="transparent")


  # Cases plot Germany
  output$graph_germany <- renderPlot({

    plot_germany_cases(df_infection_death, input$selection_germany_plots_var, input$selection_germany_plots_region, input$selection_germany_plots_scaled)

  })


  # Share of cases by group of Germany
  output$graph_germany_age_gender <- renderPlot({

    plot_cases_death_age_germany(df_infection_death_group, input$selection_germany_plots_var, input$selection_germany_plots_region)

  }, height=800)


  # ICU Total
  output$icu_beds <- renderPlot({

    plot_icu_spots(df_icu, input$select_region_icu)

  }, height=800)


  # ICU by group
  output$icu_beds_ventilator <- renderPlot({

    plot_ventilator(df_ventilator, input$select_region_icu)

  }, height=800)


  # Vaccination total
  output$graph_vaccination <- renderPlot({

    plot_vaccination(df_impfung, input$select_region_vaccination)

  })

  # Vaccination by producer
  output$graph_vaccination_cumulated <- renderPlot({

    plot_vaccination_cumulated(df_impfung_cumulated, input$select_region_vaccination)

  })




  ###########################################################################################################################################
  # NPI

  # Choice of intervention variables (aggregated or not)
  output$intervention_choice <- renderUI({
    switch(as.character(input$select_agg_interventions),
           "TRUE" = selectInput('select_intervention_variable',
                               label = NULL,
                               choices = names_agg),
           "FALSE" = selectInput('select_intervention_variable',
                              label = NULL,
                              choices = names_notagg)
    )})

  # Description of intervention
  output$intervention_description <-
    DT::renderDataTable({

      df_table <- datatable(table_interventions(input$select_agg_interventions),
                            options=list(paging=TRUE, pageLength=5),
                            style="bootstrap")


    })



  # Overview: location - intervention
  observe(output$intervention <-
            renderPlot({

              plot_interventions(df_npi, df_date_coverage, input$select_intervention_variable, input$select_intervention_location)

            },
            # Customize height of plot in App
            height=function(){if("All" %in% input$select_intervention_location){
              8000
              }else if(!isTruthy(input$select_intervention_location)){
                200
                }else{
                  length(input$select_intervention_location)*200
                  }}#,
            #width=1200
            )
  )



  # Download of xlsx file
  output$download_description_intervention <- downloadHandler(
    filename = function() {
      paste0("Corona_Intervention_Description.xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(list("Code" = df_download, "Sub-Code" = df_download_sub), file)
    }
  )






  ###########################################################################################################################################
  # Socio-economic Analysis


  # Households living space
  observe(output$hh_livingspace_sqm <- renderPlot({

    plot_housing_sqm(df_housing_sqm, input$region_SEA)

  }, height=400))

  # Housing class
  observe(output$hh_livingclass <- renderPlot({

    plot_housing_class(df_housing_class, input$region_SEA)

  },height=600))


  # Education
  output$education_cs <- renderPlot({

    plot_education_cs(df_education_profession, input$region_SEA)

  }, height=400)


  output$education_gel <- renderPlot({

    plot_education_gel(df_education_profession, input$region_SEA)

  }, height=400)


  output$education_dcc <- renderPlot({

    plot_education_dcc(df_daycare, input$region_SEA)

  }, height=400)


  # Social Indicator
  output$social_indicator <- renderPlot({

    plot_social_indicator(df_social_indicator, input$region_SEA)

  }, height=400)



  # Economy
  output$economy_sector <- renderPlot({

    plot_economy_sector(df_econ_sector, input$region_SEA)

  }, height=600)


  output$map_gdp <- renderPlot({

    plot_germany_gdp(df_gdp_capita, input$region_SEA, df_geo)

  }, height=600)


  output$unemployment <- renderPlot({

    plot_unemplyoment(df_unemployment, input$region_SEA)

  }, height=400)



  # Medical
  output$medical <- renderPlot({

    plot_medical(df_medical, input$region_SEA)

  }, height=600)




  ###########################################################################################################################################
  # Cluster Analysis

  # Cluster Determination
  cluster_deter <- eventReactive(input$action_cluster_determination,{
    cluster_determination(df_cluster_analysis, input$select_cluster_deter, input$select_cluster_vars, df_cluster_feat)
  })

  # Elbow plot
  output$cluster_determination <- renderPlot({
    cluster_deter()
  })


  # Cluster Analysis
  cluster_final <- eventReactive(input$action_cluster_analysis,{
    results <- cluster_analysis(df_cluster_analysis, input$select_number_clusters, input$select_cluster_vars, df_cluster_feat, df_geo)

    list(plots_cases=results$plots_cases, plot_cluster=results$plot_cluster, table=results$table)
  })

  # Cluster map
  output$cluster_distribution <- renderPlot({
    result_list <- cluster_final()
    result_list$plot_cluster
  }, height=600)

  # Cluster distributions
  output$cluster_analysis <- renderPlot({
    result_list <- cluster_final()
    result_list$plots_cases
  }, height=600)

  # Cluster table
  output$selection_table_cluster <- renderUI({
    pickerInput("select_variable_category",
                label=NULL,
                multiple=T,
                choices = as.character(input$select_cluster_vars),
                options = list(`actions-box` = TRUE, style = "color:black"),
                choicesOpt = list(style = rep("color: black;background: white;", length(input$select_cluster_vars))))
  })

  output$table_clustering_features <-
    DT::renderDataTable({

      result_list=cluster_final()

      # Just choose selected categories
      result_list <- result_list$table

      df_table <- datatable(cluster_feat_table(result_list, input$select_variable_category),
                            options=list(paging=TRUE, pageLength=25),
                            style="bootstrap")


    })



  ###########################################################################################################################################
  # Prediction Model

  # Target variable
  output$model_target <- renderPlot({

    pred_target_plot(df_npi, input$select_target_pred)

  }, height=600)

  # Choice of intervention variables (aggregated or not)
  output$int_choice_analysis <- renderUI({
    switch(as.character(input$select_agg_analysis),
           "TRUE" = pickerInput("select_intervention_analysis",
                                label=NULL,
                                choices=names_agg,
                                options = list(`actions-box` = TRUE, style = "color:black"),
                                multiple = T,
                                selected = "M01a",
                                choicesOpt = list(style = rep("color: black;background: white;", length(names_agg)))),
           "FALSE" = pickerInput("select_intervention_analysis",
                                 label=NULL,
                                 choices=names_notagg,
                                 options = list(`actions-box` = TRUE, style = "color:black"),
                                 multiple = T,
                                 selected = "M01a_010",
                                 choicesOpt = list(style = rep("color: black;background: white;", length(names_notagg))))



    )})

  # Description of intervention
  output$int_desc_analysis <-
    DT::renderDataTable({

      df_table <- datatable(table_interventions(input$select_agg_analysis),
                            options=list(paging=TRUE, pageLength=5),
                            style="bootstrap")


    })


  # Regression
  npi_evaluation <- eventReactive(input$action_model,{

    # Depends on whether time window is selected on graph
    if(!is.null(input$brush)){

      results <- fix_effect_reg(df_npi, input$select_intervention_analysis, input$select_lag_int, input$selection_days_drop,
                                input$select_target_pred, input$brush$xmin, input$brush$xmax, input$fe_time)

    } else {

      results <- fix_effect_reg(df_npi, input$select_intervention_analysis, input$select_lag_int, input$selection_days_drop, input$select_target_pred,
                                as.Date(unlist(df_date_coverage[df_date_coverage$df =="df_interventions", "min"])),
                                as.Date(unlist(df_date_coverage[df_date_coverage$df =="df_interventions", "max"])),
                                input$fe_time)

    }


    list(plots=results$plot, summary=results$summary)
    })

  # Coefficient graph
  output$model_coeff <- renderPlot({
    result_list <- npi_evaluation()
    result_list$plot
  }, height=400)


  # Summary overview
  output$modelSummary <- renderPrint({
    result_list <- npi_evaluation()
    result_list$summary
  })




  
}


