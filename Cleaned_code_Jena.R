## Code by Benjamin Wildermuth
## Part of the analysis for the manuscript:
## Every species counts: Arthropod species loss, but not their identity, underpins biomass declines
## Under review, do not share

######## Jena Experiment ##################

############## calculation of Price components using the priceTools package ###############

## calculation of all possible combinations takes extensive computing power

## example structure of the Price calculations

############################ load packages & data ##########

# This developing R package & installation details can be found at:
# https://github.com/ctkremer/priceTools
library(priceTools)

# Load data:
data<-read.csv2("example_data_before_price_Jena.csv", dec = ("."), stringsAsFactors = F)

# define plant species mixture plots
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
    #    data4 <- data3[data3$feeding_guild %in% trophic_guild[l], ]
      
      # Grouping data by key treatment/experimental columns:
      
      grouped.data<-data3 %>% group_by(year,plot)
      # grouped.data <- data2 %>% group_by(year, replicate, plot) # for the control
      
      # Run pairwise comparisons generating Price components:
      
      res1<- pairwise.price(grouped.data,species="species",func="biomass")
      
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
raw_data<-read.csv2("Price_data_jena_temp.csv", dec = ("."), stringsAsFactors = F)

# control comparisons
raw_dataC<-read.csv2("Price_data_jena_ctrl.csv", dec = ("."), stringsAsFactors = F)

############### Main analysis #####################

# define the baseline

year_baseline <- c("2010", "2012", "2014") # first 5 years

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
    datay <- data1[ -c(1,2, 4,7) ]

    data3 <- rbind(datax, datay)
    data3$total <- data3$y.func-data3$x.func # total biomass loss
  
  library(priceTools)
  
  group.vars<-c('plot')
  treat.vars<-c('year')
  data4<-group.columns(data3,gps=c(group.vars,treat.vars),drop=F)
  year <- as.data.frame(paste(data4$year.x, data4$year.y))  # years compared
  names(year)<-"year"
  cbind(data4, year)
  data4$year <- as.factor(data4$year)

  data4$SL.rich <- (-(data4$SL.rich)) # define lost species as negative
  data4$plot <- as.factor(data4$plot)
  
  # exclude unreplicated time spans (diff) & incomplete sampling years
  data4 <- data4[data4$diff != 10, ]
  data4 <- data4[data4$diff != 9, ]
  data4 <- data4[data4$diff != 8, ]
  data4 <- data4[data4$diff != 3, ]
  data4 <- data4[data4$year.x != 2018, ]
  data4 <- data4[data4$year.y != 2018, ]

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

   mod_numeric <- lmer(response ~ scale(diff) * scale(species_mixture_output.k.) + 
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
   param_clean$Parameter <- c("(Intercept)", "scale(Years passed)", "scale(PSR)", "scale(Years):scale(PSR)")
   colnames(param_clean) <- c("Parameter", "Estimate", "Std. Error", "CI", "CI_low", "CI_high", "t-value", "den. DF", "Pr(>|t|)")
   
   # trends for levels of plant species richness
   library(emmeans)
   
   levels_k <- unique(data4$species_mixture_output.k.)
   trends <- emtrends(mod_numeric, ~ species_mixture_output.k., var = "diff", at = list(species_mixture_output.k. = levels_k),
                      pbkrtest.limit = 30000)
   trend_summary <- summary(trends, infer = c(TRUE, TRUE))
   
   # predictions
   library(ggeffects)
   
   (pred1 <- ggemmeans(mod_numeric, c("diff","species_mixture_output.k.")))

   (pred2 <- ggemmeans(mod_numeric, "diff"))
   
  }