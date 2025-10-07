## Code by Benjamin Wildermuth
## Part of the analysis for the paper:
## Every species counts: Arthropod species loss, but not their identity, underpins biomass declines
## Nature Ecology & Evolution

######## Jena Experiment ##################

############## calculation of Price components using the priceTools package ###############

## calculation of all possible combinations takes extensive computing power

## example structure of the Price calculations, full data provided

############################ load packages & data ##########

# This developing R package & installation details can be found at:
# https://github.com/ctkremer/priceTools
library(priceTools)
library(dplyr)

# Load data:
data<-read.csv2("raw_data_cleaned_Jena.csv", dec = ("."), stringsAsFactors = F)

# define plant species mixtures (plant species richness)
{
  # monocultures
  spec1 <- c("B1A08",	"B1A15",	"B1A18",	"B2A04",	"B2A05",	"B2A13",
             "B2A15",	"B3A01",	"B3A06",	"B3A12",	"B3A17",	"B4A09",
             "B4A12",	"B4A13")
  
  # 2 species plots
  spec2 <- c("B1A05", "B1A07", "B1A16", "B1A17", "B2A02", "B2A08",
             "B2A19", "B2A20", "B3A02", "B3A08", "B3A19", "B3A21",
             "B4A14", "B4A15", "B4A17", "B4A21")
  
  # 4 species plots
  spec4 <- c("B1A04",	"B1A13",	"B1A19",	"B1A21",	"B2A01",	"B2A06",
             "B2A09",	"B2A16",	"B3A03",	"B3A11",	"B3A13",	"B3A23",
             "B4A04",	"B4A07",	"B4A11",	"B4A22")
  
  # 8 species plots
  spec8 <- c("B1A02",	"B1A03",	"B1A12",	"B1A14",	"B2A12",	"B2A14",
             "B2A17", "B2A21",	"B3A04",	"B3A05",	"B3A07",	"B3A20",
             "B4A06",	"B4A08",	"B4A10",	"B4A16")
  
  # 16 species plots
  spec16 <- c("B1A01",	"B1A06",	"B1A11",	"B1A20",	"B2A10",	"B2A18",
              "B2A22", "B3A09",	"B3A16",	"B3A22",	"B3A24",	"B4A02",
              "B4A18",	"B4A20")
  
  # 60 species plots
  spec60 <- c("B1A22", "B2A03", "B3A14", "B4A01")
}

############### Loop Price #####################

# select comparison and filter
# years available: 10, 12, 14, 16, 17, 18, 19, 20 # 18 sampled incomplete

h <- 0
res_out <- list()
tmp <- vector("list", 1) # increase list depending on comparisons made
for (i in 1:1) {
  year_baseline <- c("2010")
  year_comparison <- c("2017") # all relevant comparisons can be added in the baseline & comparison
  data1 <- data[data$year %in% c(year_baseline[i], year_comparison[i]), ]
  
  tmp2 <- vector("list", 6)
  for (j in 1:6) {
    species_mixture <- list(spec1, spec2, spec4, spec8, spec16, spec60)
    species_mixture_output <- c("spec1", "spec2", "spec4", "spec8", "spec16", "spec60")
    data2 <- data1[data1$plot %in% unlist(species_mixture[j]), ]
    
    # delete replicate loop for the control!!
    
    tmp3 <- vector("list", 2)
    for (k in 1:2) {
      sampling_replicate <- c("A", "B")
      data3 <- data2[data2$replicate %in% sampling_replicate[k], ]
      
      # add another loop in case of guild-specific analyses
      
    #  tmp4 <- vector("list", 2)
    #  for (l in 1:2) {
    #    trophic_guild <- c("herbivore", "predator")
    #    data4 <- data3[data3$trophic_guild %in% trophic_guild[l], ]
      
      # Grouping data by key treatment/experimental columns:
      
      grouped.data<-data3 %>% group_by(year,plot)
      # grouped.data <- data2 %>% group_by(year, replicate, plot) # for the control
      
      # Run pairwise comparisons generating Price components:
      
      res1<- pairwise.price(grouped.data,species="morphospecies",func="biomass_sum")
      
      # save iterations
      
      h <- h + 1
      
      res_out[[h]] <- cbind(res1, species_mixture_output[j], 
                            sampling_replicate[k])
      
    }}}

(output_res <- do.call(rbind, res_out))
(filtered_output_res <- na.exclude(output_res))

## filtering relevant comparisons

filtered_output_res1 <- filtered_output_res[filtered_output_res$plot.x == filtered_output_res$plot.y,]
Price_data_jena_temp <- filtered_output_res1[filtered_output_res1$year.y > filtered_output_res1$year.x, ]

# filtering for control

#filtered_output_res1 <- filtered_output_res[filtered_output_res$plot.x == filtered_output_res$plot.y,]
#Price_data_jena_ctrl <- filtered_output_res1[filtered_output_res1$year.y == filtered_output_res1$year.x, ]

############## Load complete provided data: ################

# comparisons over time
raw_data<-read.csv2("Price_data_jena_clean.csv", dec = ("."), stringsAsFactors = F)

############### Main analysis #####################

# define the baseline

year_baseline <- c("2010", "2012", "2014") # first 5 years

data <- raw_data[raw_data$year.x %in% c(year_baseline), ]

    # Set up data for analysis
{
    data$year.x <- as.factor(data$year.x)
    data$year.y <- as.factor(data$year.y)
    data$SL.rich<-data$x.rich-data$c.rich # lost species
    data$SG.rich<-data$y.rich-data$c.rich # gained species
    data$SR.rich<-data$y.rich-data$x.rich # species richness change
    data2 <- na.exclude(data)

    data2$total <- data2$y.func-data2$x.func # total biomass loss
  
  library(priceTools)
  
  group.vars<-c('plot')
  treat.vars<-c('year')
  data3<-group.columns(data2,gps=c(group.vars,treat.vars),drop=F)
  year <- as.data.frame(paste(data3$year.x, data3$year.y))  # years compared
  names(year)<-"year"
  cbind(data3, year)
  data3$year <- as.factor(data3$year)

  data3$SL.rich <- (-(data3$SL.rich)) # define lost species as negative
  data3$plot <- as.factor(data3$plot)
  
  # exclude unreplicated time spans (temp_diff) & incomplete sampling years
  data3 <- data3[data3$temp_diff != 10, ]
  data3 <- data3[data3$temp_diff != 9, ]
  data3 <- data3[data3$temp_diff != 8, ]
  data3 <- data3[data3$temp_diff != 3, ]
  data3 <- data3[data3$year.x != 2018, ]
  data3 <- data3[data3$year.y != 2018, ]

  }
  
  # Columns of interest
  columns_of_interest <- c("SRE.L", "SRE.G", "SIE.L", "SIE.G", "CDE", "total", "SL.rich", "SG.rich", "SR.rich")
  
    # Function to add square root transformed columns
  add_sqrt_columns <- function(df, cols) {
    df %>%
      mutate(across(all_of(cols), 
                    ~ sign(.) * sqrt(abs(.)), 
                    .names = "sqrt_{col}"))}
  
  # Apply square root transformation to the original columns of interest
  data3 <- add_sqrt_columns(data3, columns_of_interest)
  
  ##########  Subsampling workflow    #################
  
  library(lme4)
  library(lmerTest)
  library(MuMIn)
  library(parameters)
  library(emmeans)
  library(ggeffects)
  library(dplyr)
  library(purrr)
  library(tibble)
  
  set.seed(10)
  n_iter <- 1000
  
  results <- vector("list", n_iter)
  levels_k <- unique(data3$species_mixture_output) # Plant species richness (PSR) levels
  
  var_names <- c("sqrt_SRE.L", "sqrt_SRE.G", "sqrt_SIE.L", "sqrt_SIE.G",
                 "sqrt_CDE", "sqrt_total", "sqrt_SL.rich", "sqrt_SG.rich", "sqrt_SR.rich")
  
  tmp <- vector("list", length(var_names))
  names(tmp) <- var_names
  
  ######### function for picking as many sampling years as possible ################
  
  sample_plot_pairs <- function(df_plot, base_yrs = c(2010, 2012, 2014)) { # restricted moving average
    df_plot <- df_plot %>%
      mutate(
        year.x = as.integer(as.character(year.x)),
        year.y = as.integer(as.character(year.y))
      )
    
    used_years <- integer(0)  # sampling years already used (in any position)
    picked     <- list()
    
    candidates <- sample(intersect(base_yrs, df_plot$year.x))  # shuffled base years
    
    for (b in candidates) { # only unused sampling years can be picked
      rows_b <- df_plot %>%
        filter(year.x == b, year.y >= b) %>%
        filter(
          (!(year.x %in% used_years) & !(year.y %in% used_years)) |
            (year.x == year.y & !(year.x %in% used_years)) # same baseline & comparison year possible (control)
        )
      
      if (nrow(rows_b) == 0) next
      
      row_sel <- slice_sample(rows_b, n = 1)
      picked[[ length(picked) + 1 ]] <- row_sel
      
      used_years <- union(used_years, c(row_sel$year.x, row_sel$year.y))
      
      if (length(picked) == length(base_yrs)) break  # maximum picks
    }
    
    bind_rows(picked)
  }
  
  ############## loop model & store result lists ################
  
  library(furrr)
  library(progressr)
  
  for (l in seq_along(var_names)) {
    data3$response <- data3[[var_names[l]]]
    
    # Setup parallel computing
    plan(multisession, workers = parallel::detectCores() - 1)
    
    # progress tracking
    handlers(global = TRUE)
    handlers("txtprogressbar")
    
    # Wrap in progressr context
    results <- with_progress({
      p <- progressor(steps = n_iter)
      
      #apply subsampling function
      future_map(1:n_iter, function(i) {
        data_sampled <- data3 %>%
          split(.$plot) %>% 
          map_dfr(sample_plot_pairs)
        
        #model
        mod <- lmer(
          response ~ scale(temp_diff) * scale(species_mixture_output) +
            (1 | plot) + (1 | year.x) + (1 | year.y),
          data    = data_sampled,
          control = lmerControl(optimizer = "bobyqa"),
          na.action = na.omit
        )
        
        #trends per PSR level
        trends_obj <- suppressWarnings(
          emtrends(mod,
                   ~ species_mixture_output,
                   var  = "temp_diff",
                   at   = list(species_mixture_output = levels_k),
                   pbkrtest.limit = 30000)
        )
        
        trend_sum <- suppressWarnings(
          as.data.frame(summary(trends_obj, infer = c(TRUE, TRUE)))
        )
        
        # Update progress bar
        p()
        
        #list of results
        list(
          r2     = suppressWarnings(r.squaredGLMM(mod)),
          params = suppressWarnings(parameters(mod, ci_method = "satterthwaite")),
          trends = trend_sum,
          pred1  = suppressWarnings(
            ggemmeans(mod, c("temp_diff", "species_mixture_output"))),
          pred2  = suppressWarnings(ggemmeans(mod, "temp_diff"))
        )
      }, .options = furrr_options(seed = 10))
    })
    
    # store and summarize R²
    r2_df <- bind_rows(lapply(results, function(x) as.data.frame(x$r2)))
    r2_summary <- r2_df %>%
      summarise(
        R2m_median = median(R2m),  R2m_CI_low = quantile(R2m, 0.025),
        R2m_CI_high = quantile(R2m, 0.975),
        R2c_median = median(R2c),  R2c_CI_low = quantile(R2c, 0.025),
        R2c_CI_high = quantile(R2c, 0.975)
      )
    
    #store and summarize coefficients
    param_all <- bind_rows(lapply(seq_along(results), function(i) {
      out <- results[[i]]$params
      out$iter <- i
      out
    }))
    
    param_summary <- param_all %>%
      group_by(Parameter) %>%
      summarise(
        Estimate_median = median(Coefficient, na.rm = TRUE),
        CI_low = quantile(Coefficient, 0.025, na.rm = TRUE),
        CI_high = quantile(Coefficient, 0.975, na.rm = TRUE),
        p_value = {
          med <- median(Coefficient, na.rm = TRUE)
          2 * mean(
            if (med > 0) Coefficient < 0 else Coefficient > 0,
            na.rm = TRUE
          )
        },
        .groups = "drop"
      )
    
    #store and summarize trends per PSR level
    trend_all <- bind_rows(lapply(results, `[[`, "trends"))
    
    trend_col <- intersect(names(trend_all), c("trend", "temp_diff.trend"))[1]
    
    trend_summary <- trend_all %>%
      group_by(species_mixture_output) %>%
      summarise(
        trend_median = median(.data[[trend_col]], na.rm = TRUE),
        trend_CI_low = quantile(.data[[trend_col]], 0.025, na.rm = TRUE),
        trend_CI_high = quantile(.data[[trend_col]], 0.975, na.rm = TRUE),
        p_value = {
          med <- median(.data[[trend_col]], na.rm = TRUE)
          2 * mean(
            if (med > 0) .data[[trend_col]] < 0 else .data[[trend_col]] > 0,
            na.rm = TRUE
          )
        },
        .groups = "drop"
      )
    
    #store and summarize predictions per PSR level
    pred1_all <- bind_rows(lapply(results, function(x) as.data.frame(x$pred1)))
    pred1_summary <- pred1_all %>%
      group_by(x, group) %>%
      summarise(
        predicted_median = median(predicted),
        CI_low = quantile(predicted, 0.025),
        CI_high = quantile(predicted, 0.975),
        .groups = "drop"
      )
    
    #store and summarize overall predictions
    pred2_all <- bind_rows(lapply(results, function(x) as.data.frame(x$pred2)))
    pred2_summary <- pred2_all %>%
      group_by(x) %>%
      summarise(
        predicted_median = median(predicted),
        CI_low = quantile(predicted, 0.025),
        CI_high = quantile(predicted, 0.975),
        .groups = "drop"
      )
    
    # store all results per iteration together
    tmp[[l]] <- list(
      r2_summary     = r2_summary,
      param_summary  = param_summary,
      trend_summary  = trend_summary,
      pred1_summary  = pred1_summary,
      pred2_summary  = pred2_summary,
      results        = results # raw iteration results
    )
  }
  
  ################ clean predictions (rounding errors) ############
  
  for (v in var_names) {
    pred1_all <- bind_rows(lapply(tmp[[v]]$results, function(x) as.data.frame(x$pred1)))
    
    tmp[[v]]$pred1_summary <- pred1_all %>%
      mutate(
        x = ifelse(abs(x) < 1e-8, 0, round(x, 6)), # treat near-zero as 0; round others
        group = as.numeric(as.character(group)),              # convert factor to numeric
        group = ifelse(abs(group - round(group)) < 1e-8, round(group), group),
        group = factor(group)  # convert back to factor if needed
      ) %>%
      group_by(x, group) %>%
      summarise(
        predicted_median = median(predicted),
        CI_low = quantile(predicted, 0.025),
        CI_high = quantile(predicted, 0.975),
        .groups = "drop"
      )
  }
  
  for (v in var_names) {
    pred2_all <- bind_rows(lapply(tmp[[v]]$results, function(x) as.data.frame(x$pred2)))
    
    tmp[[v]]$pred2_summary <- pred2_all %>%
      mutate(
        x = ifelse(abs(x) < 1e-8, 0, round(x, 6))  # treat near-zero as 0; round others
      ) %>%
      group_by(x) %>%
      summarise(
        predicted_median = median(predicted),
        CI_low = quantile(predicted, 0.025),
        CI_high = quantile(predicted, 0.975),
        .groups = "drop"
      )
  }
  
  ############## combine components ############
  
  combined_components <- list( # step 1: combine random L and G, and identity L and G
    richness = c("sqrt_SRE.L", "sqrt_SRE.G"),
    identity = c("sqrt_SIE.L", "sqrt_SIE.G")
  )
  
  # Combine predictions per replicate, keeping matching between L and G
  for (comp in names(combined_components)) {
    vars <- combined_components[[comp]]
    
    #  check
    if (length(vars) != 2) stop("Each component must have exactly two subcomponents")
    
    # Combine subsampled predictions
    combined_bootstrap <- map_dfr(seq_along(tmp[[vars[1]]]$results), function(i) {
      df1 <- as.data.frame(tmp[[vars[1]]]$results[[i]]$pred2)
      df2 <- as.data.frame(tmp[[vars[2]]]$results[[i]]$pred2)
      stopifnot(all(df1$x == df2$x))  # ensure matching timepoints
      
      df_combined <- df1
      df_combined$predicted <- sign(df1$predicted) * (df1$predicted)^2 +
        sign(df2$predicted) * (df2$predicted)^2
      df_combined$replicate <- i
      return(df_combined)
    })
    
    # Save full combined subsample if needed
    tmp[[comp]]$pred2_all <- combined_bootstrap
    
    # Summarise CIs
    tmp[[comp]]$pred2_summary <- combined_bootstrap %>%
      mutate(x = ifelse(abs(x) < 1e-8, 0, round(x, 6))) %>%
      group_by(x) %>%
      summarise(
        predicted_median = median(predicted),
        CI_low = quantile(predicted, 0.025),
        CI_high = quantile(predicted, 0.975),
        .groups = "drop"
      )
  }
  
  combined_components2 <- list( # step 2: combine all species turnover 
    turnover = c("sqrt_SRE.L", "sqrt_SRE.G", "sqrt_SIE.L", "sqrt_SIE.G")
  )
  
  # Combine predictions per subsample, keeping matching between all 4 components
  for (comp in names(combined_components2)) {
    vars <- combined_components2[[comp]]
    
    #  check
    if (length(vars) != 4) stop("Each component must have exactly four subcomponents")
    
    # Combine subsampled predictions
    combined_bootstrap <- map_dfr(seq_along(tmp[[vars[1]]]$results), function(i) {
      dfs <- lapply(vars, function(v) as.data.frame(tmp[[v]]$results[[i]]$pred2))
      
      # Ensure all timepoints match
      stopifnot(all(sapply(dfs[-1], function(df) all(df$x == dfs[[1]]$x))))
      
      df_combined <- dfs[[1]][, "x", drop = FALSE] 
      
      # Combine with the same logic as in step 1
      df_combined$predicted <-
        sign(dfs[[1]]$predicted) * (dfs[[1]]$predicted)^2 +
        sign(dfs[[2]]$predicted) * (dfs[[2]]$predicted)^2 +
        sign(dfs[[3]]$predicted) * (dfs[[3]]$predicted)^2 +
        sign(dfs[[4]]$predicted) * (dfs[[4]]$predicted)^2
      
      df_combined$replicate <- i
      return(df_combined)
    })
    
    # Save full combined subsample
    tmp[[comp]]$pred2_all <- combined_bootstrap
    
    # Summarise CIs
    tmp[[comp]]$pred2_summary <- combined_bootstrap %>%
      mutate(x = ifelse(abs(x) < 1e-8, 0, round(x, 6))) %>%
      group_by(x) %>%
      summarise(
        predicted_median = median(predicted),
        CI_low = quantile(predicted, 0.025),
        CI_high = quantile(predicted, 0.975),
        .groups = "drop"
      )
  }
  
  ############ rbind all results ##############
  
  # function to backtransform predictions
  backtransform <- function(df) {
    df[] <- lapply(names(df), function(col) {
      if (col == "x") {
        return(df[[col]])  # Keep "x" unchanged
      } else {
        x <- df[[col]]
        if (is.numeric(x)) {
          x <- sign(x) * (x^2)  # Back-transform
        }
        return(x)
      }
    })
    return(as.data.frame(df))
  }
  
  # model coefficients
  param_summary_all <- bind_rows(
    lapply(var_names, function(v) {
      tmp[[v]]$param_summary %>%
        mutate(response = v)
    })
  )
  
  # R² summary
  r2_summary_all <- bind_rows(
    lapply(var_names, function(v) {
      tmp[[v]]$r2_summary %>%
        mutate(response = v)
    })
  )
  
  # pred1 summary (PSR levels)
  pred1_summary_all <- bind_rows(
    lapply(var_names, function(v) {
      df <- tmp[[v]]$pred1_summary
      df <- backtransform(df)
      df$response <- v
      df
    })
  )
  
  # pred2 summary (overall effect)
  pred2_summary_all <- bind_rows(
    lapply(var_names, function(v) {
      df <- tmp[[v]]$pred2_summary
      df <- backtransform(df)
      df$response <- v
      df
    })
  )
  
  combined_var_names <- names(combined_components)
  combined_var_names2 <- names(combined_components2)  # "turnover"
  
  # Combined pred2 summary (for combined responses only — already backtransformed earlier)
  combined_pred2_summary_all <- bind_rows(
    lapply(c(combined_var_names, combined_var_names2), function(v) {
      df <- tmp[[v]]$pred2_summary
      df$response <- v
      df
    })
  )