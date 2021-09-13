#############################################################################################################################################
# Prediction target plot
pred_target_plot <- function(df, target){
  
  # Select column
  df_agg <- df[,c("date", target)]
  
  # Aggregate
  df_agg <- df_agg %>% dplyr::group_by(date) %>% summarize_all(list(mean))
  
  # Set column names
  colnames(df_agg) <- c("date", "mean")
  
  # Plot target variable
  ggplot(df_agg) + 
    geom_area(aes(x=date, y=mean), size=0.8, color="#2D708EFF", fill="#2D708EFF") +
    scale_x_date(date_breaks = "1 months",
                 date_minor_breaks = "1 months",
                 expand=c(0,0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    theme(legend.position=NULL,
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size=20))
  
}

#pred_target_plot(df, "kr_neuinf_rate")


#############################################################################################################################################
# Fixed effect regression

fix_effect_reg <- function(df, vars, n_lags, delete_days, target, date_start, date_end, additional_fe){

  # Select columns
  df_adj <- df[,c("kreis","date", "year", "month", "week", vars, target)]
  
  # Select date
  df_adj <- df_adj[df_adj$date >= date_start,]
  df_adj <- df_adj[df_adj$date <= date_end,]
  
  # Order data frame
  df_adj <- dplyr::arrange(df_adj, kreis, date)
  
  # Set target variable
  df_adj$target <- df_adj[,target]
  
  # Delete Sundays and Mondays if selected
  if(delete_days == TRUE){
    df_adj$day <- weekdays(as.Date(df_adj$date))
    df_adj <- df_adj[!(df_adj$day=="Montag") & !(df_adj$day=="Sonntag") ,]
    df_adj <- df_adj[, !colnames(df_adj) %in% "day"]
  }
  
  # Adjust interventions
  list_interventions <- colnames(df_adj)[grepl("M", colnames(df_adj))]
  for(i in list_interventions){
  
    # Select variable
    var_select <- i
    var_lag <- paste0("lag_", var_select)
  
    # Create lagged columns
    df_adj <- df_adj %>% dplyr::group_by(kreis) %>% mutate(!!var_lag := lag(!!as.name(var_select), n=n_lags))
  
    }
  
  # Drop all "old" intervention columns
  df_adj <- df_adj[, !names(df_adj) %in% list_interventions]
  
  # Adjust previous column names
  df_adj <- df_adj %>% rename_with(~str_remove(., 'lag_'))
  
  # Just complete cases
  df_adj <- df_adj[complete.cases(df_adj),]
  
  # Delete columns
  df_adj <- df_adj[, !(names(df_adj) %in% c("date", target))]
  
  # Regressors
  ind_vars <- paste(vars, collapse=" + " )
  
  # Fixed effects
  fe_vars <- c("kreis", additional_fe)
  fe_vars <- do.call(paste, c(as.list(fe_vars), sep = " + "))
  
  # Combined formula
  reg_formular <- as.formula(paste0("target ~ ", ind_vars, " | ", fe_vars))
  
  # Regression
  model <- feols(reg_formular, df_adj)
  model_values <- summary(model)
  
  # Get results
  model_results <- model_values$coeftable
  
  # Add confidence interval (90%)
  ci <- confint(model, level=0.9)
  model_results <- cbind(model_results, ci)
  
  # Adjust dataframe
  colnames(model_results) <- c("coefficient", "se", "t_value", "p_value", "CI_LowerBound", "CI_UpperBound")
  model_results$var <- rownames(model_results)
  model_results$sig <- ifelse(model_results$p_value > 0.1, "Not significant (p-value > 0.1)", "Significant (p-value <= 0.1)")

  # Plot coefficients
  cols <- c("Not significant (p-value > 0.1)" = "brown4", "Significant (p-value <= 0.1)" = "#2D708EFF")
  
  plot_coef <- ggplot(model_results) + 
    geom_bar(aes(x=reorder(var, coefficient), y=coefficient, fill=sig),
             stat="identity") + 
    geom_errorbar(aes(x=reorder(var, coefficient), ymin=CI_LowerBound, ymax=CI_UpperBound),
                  width=0.4, size=1.3, colour="#440154FF") +
    geom_abline(slope=0, intercept=0,  col = "black", size=1.2) +
    scale_fill_manual(values = cols) +
    theme(legend.position="bottom",
          legend.title=element_blank(),
          legend.text=element_text(size=20),
          legend.spacing.x = unit(1.0, 'cm'),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size=20))

  # Result table
  model_summary <- summary(model)
  
  # Return all elements
  return(list("plot" = plot_coef, "summary" = model_summary))

}


#fix_effect_reg(df, c("M01a","M01b","M10","M17"),7,TRUE)
