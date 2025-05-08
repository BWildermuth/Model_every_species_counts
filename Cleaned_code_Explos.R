## Code by Benjamin Wildermuth
## Part of the analysis for the manuscript:
## Every species counts: Arthropod species loss, but not their identity, underpins biomass declines
## Under review, do not share

########## Biodiversity Exploratories ###################

############## calculation of Price components using the priceTools package ###############

## calculation of all possible combinations takes extensive computing power

## example structure of the Price calculations based on a subset of data

############################ load packages & data ##########

# This developing R package & installation details can be found at:
# https://github.com/ctkremer/priceTools
library(priceTools)

# Load data:
data<-read.csv2("example_data_before_price_Explos.csv", dec = ("."), stringsAsFactors = F)


# define LUI categories
{
  # extensive
  extensive <- seq(0, 1.5, by = 0.01)
  
  # intensive
  intensive <- seq(1.51, 3.9, by = 0.01)
}

############### Loop Price #####################

# select comparison and filter
# years available: 08-18

h <- 0
res_out <- list()
tmp <- vector("list", 1) # increase list depending on comparisons made
for (i in 1:1) {
  year_baseline <- c("2008") 
  year_comparison <- c("2017") # all relevant comparisons can be added in the baseline & comparison
  data1 <- data[data$year %in% c(year_baseline[i], year_comparison[i]), ]
  
  
  tmp2 <- vector("list", 2)
  for (j in 1:2) {
    LUI <- list(intensive, extensive)
    LUI_output <- c("intensive", "extensive")
    data2 <- data1[data1$LUI %in% unlist(LUI[j]), ]
    
    # delete replicate loop for the control!!
    
    tmp3 <- vector("list", 2)
    for (k in 1:2) {
      sampling_replicate <- c("A", "B")
      data3 <- data2[data2$replicate %in% sampling_replicate[k], ]
      
      # add another loop in case of guild-specific analyses
      
    #  tmp4 <- vector("list", 2)
    #  for (l in 1:2) {
    #    trophic_guild <- c("h", "c") # herbivores & predators (c)
    #    data4 <- data3[data3$feeding_guild %in% trophic_guild[l], ]
      
      # Grouping data by key treatment/experimental columns:
      
      grouped.data<-data3 %>% group_by(year,plot)
      # grouped.data<-data2 %>% group_by(year, replicate, plot) # for the control
      
      # Run pairwise comparisons generating Price components:
      
      res1<- pairwise.price(grouped.data,species="species",func="biomass")
      
      # save iterations
      
      h <- h + 1
      
      res_out[[h]] <- cbind(res1, LUI_output[j], 
                            sampling_replicate[k])
      
    }}}

(output_res <- do.call(rbind, res_out))
(filtered_output_res <- na.exclude(output_res))

## filtering relevant comparisons

filtered_output_res1 <- filtered_output_res[filtered_output_res$plot.x == filtered_output_res$plot.y,]
Price_data_explos_temp <- filtered_output_res1[filtered_output_res1$year.y > filtered_output_res1$year.x, ]

# filtering for control

#filtered_output_res1 <- filtered_output_res[filtered_output_res$plot.x == filtered_output_res$plot.y,]
#Price_data_explos_ctrl <- filtered_output_res1[filtered_output_res1$year.y == filtered_output_res1$year.x, ]

############## Load complete provided data: ################

# comparisons over time
raw_data<-read.csv2("Price_data_explos_temp.csv", dec = ("."), stringsAsFactors = F)
  
# control comparisons
raw_dataC<-read.csv2("Price_data_explos_ctrl.csv", dec = ("."), stringsAsFactors = F)

############### Main analysis #####################

# define the baseline

year_baseline <- c("2008", "2009", "2010", "2011", "2012")# first 5 years

data <- raw_data[raw_data$year.x %in% c(year_baseline), ]
data1 <- raw_dataC[raw_dataC$year.x %in% c(year_baseline), ]

  # Set up data for analysis
{
  data$year.x <- as.factor(data$year.x)
  data$year.y <- as.factor(data$year.y)
  data$SL.rich<-data$x.rich-data$c.rich # lost species
  data$SG.rich<-data$y.rich-data$c.rich # gained species
  data$SR.rich<-data$y.rich-data$x.rich # species richness change
  data <- na.exclude(data)
  datax <- data[ -c(1,2, 22) ]

  # Set up ctrl for analysis
  data1$year.x <- as.factor(data1$year.x)
  data1$year.y <- as.factor(data1$year.y)
  data1$SL.rich<-data1$x.rich-data1$c.rich # lost species
  data1$SG.rich<-data1$y.rich-data1$c.rich # gained species
  data1$SR.rich<-data1$y.rich-data1$x.rich # species richness change
  data1 <- na.exclude(data1)
  datay <- data1[ -c(1,2,4,7) ]

  data3 <- rbind(datax, datay)
  data3$total <- data3$y.func-data3$x.func # total biomass loss
  
  library(priceTools)
  
  group.vars<-c('plot')
  treat.vars<-c('year')
  data4<-group.columns(data3,gps=c(group.vars,treat.vars),drop=F)
  year <- as.data.frame(paste(data4$year.x, data4$year.y)) # years compared
  names(year)<-"year"
  cbind(data4, year)
  data4$year <- as.factor(data4$year)

  data4$SL.rich <- (-(data4$SL.rich)) # define lost species as negative
  data4$plot <- as.factor(data4$plot)
  
  data4 <- data4[data4$diff != 10, ] # remove time spans (diff) with only one replicate
  
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
  data4 <- add_sqrt_columns(data4, columns_of_interest)
  
  ################ Loop responses ###############
  
  tmp <- vector("list", 9)

  for (l in 1:9) { # choose response of interest
    data4$response <- data4[, c("sqrt_SRE.L", "sqrt_SRE.G", "sqrt_SIE.L", "sqrt_SIE.G", "sqrt_CDE", "sqrt_total", 
                                "sqrt_SL.rich", "sqrt_SG.rich", "sqrt_SR.rich")[l]]
    
   library(lme4)
   
   mod_numeric <- lmer(response ~ scale(diff) * scale(LUI_numeric) + 
                      (1 | plot) + (1 | year.x:plot) + (1 | year.y:plot),
                    data = data4, na.action = na.omit,
                    control = lmerControl(optimizer = "bobyqa"))
   
   # Generate diagnostic plots
   try(plot(resid(mod_numeric)))
   try(hist(resid(mod_numeric)))

   summary(mod_numeric)
   
   library(MuMIn)
   
   r2 <- r.squaredGLMM(mod_numeric)
   
   library(lmerTest)
   
   anova_mod_numeric <- anova(mod_numeric)
 
   library(parameters)
   
   param <- model_parameters(mod_numeric, ci_method = "satterthwaite")
   param_clean <- as.data.frame(param)
   param_clean <- param_clean[1:4, 1:9]
   param_clean$Parameter <- c("(Intercept)", "scale(Years passed)", "scale(LUI)", "scale(Years):scale(LUI)")
   colnames(param_clean) <- c("Parameter", "Estimate", "Std. Error", "CI", "CI_low", "CI_high", "t-value", "den. DF", "Pr(>|t|)")
   
   # trends for levels of land-use intensity
   library(emmeans)

   levels_k <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5)
   trends <- emtrends(mod_numeric, ~ LUI_numeric, var = "diff", at = list(LUI_numeric = levels_k),
                      pbkrtest.limit = 30000)
   trend_summary <- summary(trends, infer = c(TRUE, TRUE))
   
   # predictions
   library(ggeffects)
   
   (pred1 <- ggemmeans(mod_numeric, c("diff", "LUI_numeric [levels_k]")))
   
   (pred2 <- ggemmeans(mod_numeric, "diff"))
   
  }