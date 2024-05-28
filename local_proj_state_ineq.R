### Author : Chinmaya Singh
### Title : Local Projections on the UK data
### For EC424 Extended Essay


### Loading libraries
library(tidyverse)
library(lpirfs)
library(readxl)
library(tseries)
library(gridExtra)
library(ggpubr)
library(reshape2)
library(zoo)

### set the working directory and place the data file in the same folder
setwd("...")

### Loading data
macro_data  <- read_csv("regression_data.csv")
# ggplot( data = macro_data, mapping = aes(x = Time, y = `inf_cpi_ons_qtr` ) ) + geom_point()


macro_data <- macro_data %>% rename(cpi_ons = cpi_ons_qtr, 
                                    cpi_cloyne = cpi_cloyne_qtr,
                                    cpi_log_cloyne = cpi_log_cloyne_qtr,
                                    inf_cpi_ons = inf_cpi_ons_qtr, 
                                    inf_cpi_cloyne = inf_cpi_cloyne_qtr,
                                    gdp_index_ons = `GDP_index(ONS)`,
                                    gdp_index_cloyne = `GDP_index(cloyne)`,
                                    gdp_loglevel_cloyne = `GDP_log_level(cloyne)`,
                                    gdp_growth_qnq = gdp_qnq)
names(macro_data)[names(macro_data) == 'dummy'] <- 'binary'

print(colnames(macro_data))

##########################
### Descriptive statistics
##########################

ggplot(macro_data, aes(Time,Shock)) + 
  geom_line()
ggsave(
  filename = paste0('shock_series','.png'),
  plot = last_plot(),
  device = "png",
  path = "C:\\Users\\Neha\\Desktop\\Thesis\\Coding\\figures\\publish\\"
)

state_plot <- macro_data %>% select('Time', 'binary', 'smooth')
state_plot <- melt(state_plot ,  id.vars = 'Time', variable.name = 'type')
ggplot(state_plot, aes(Time,value)) + 
  geom_line(aes(colour = type)) +
  ylab("State value") +
  theme(legend.position = "top")
ggsave(
  filename = paste0('state_vars','.png'),
  plot = last_plot(),
  device = "png",
  path = "C:\\Users\\Neha\\Desktop\\Thesis\\Coding\\figures\\publish\\"
)


#####################
### Impulse responses of GDP and inflation
####################

# Endogenous data 
gdp_series   = 'gdp_loglevel_cloyne'
price_series = 'inf_cpi_cloyne' 

endog_data <- macro_data %>%
  select(gdp_series, price_series, 'bank_rate') #price_series, extra_var
endog_data <- endog_data[2:nrow(endog_data),]

romer_shock <- macro_data[2:nrow(macro_data),] %>% select('Shock')

num_lags = 4

impulse_response_GDP <- tibble(horizon = 1:16,
                               linear = numeric(16),
                               expansion = numeric(16),
                               recession = numeric(16))

impulse_response_inf <- tibble(horizon = 1:16,
                               linear = numeric(16),
                               expansion = numeric(16),
                               recession = numeric(16))

### Running basic(linear) local projections when the shocks are identified 
### externally (IV) (as per lpirfs documentation)
### Using romer_romer shocks

results_lin_iv <- lp_lin_iv(endog_data = endog_data, lags_endog_lin = num_lags,
                            shock = romer_shock,     trend = 0,
                            confint = 1.65,          hor = 16)
iv_lin_plots <- plot_lin(results_lin_iv)


impulse_response_GDP$linear <- results_lin_iv$irf_lin_mean[1,]
impulse_response_inf$linear <- results_lin_iv$irf_lin_mean[2,]


### Running state dependent local projections 
### Using romer_romer shocks
### state -> 'dummy' or 'smooth'

state_var_sm <- macro_data[2:nrow(macro_data),]$gdp_statevar

# Smooth state variable 
results_nl_iv <- lp_nl_iv(endog_data = endog_data, lags_endog_nl = num_lags,
                          shock = romer_shock,     trend = 0,
                          confint = 1.65,          hor = 16,
                          switching = state_var_sm,use_logistic = TRUE,
                          use_hp = FALSE,          gamma = 5)

# Make and save nonlinear plots
plots_nl_iv <- plot_nl(results_nl_iv)

impulse_response_GDP$expansion <- results_nl_iv$irf_s1_mean[1,]
impulse_response_inf$expansion <- results_nl_iv$irf_s1_mean[2,]

impulse_response_GDP$recession <- results_nl_iv$irf_s2_mean[1,]
impulse_response_inf$recession <- results_nl_iv$irf_s2_mean[2,]

# Make list to save all plots
combine_plots <- list()

# Save linear plots in list
combine_plots[[1]] <- iv_lin_plots[[1]]
combine_plots[[2]] <- iv_lin_plots[[2]]

# Save nonlinear plots for expansion period
combine_plots[[3]] <- plots_nl_iv$gg_s1[[1]]
combine_plots[[4]] <- plots_nl_iv$gg_s1[[2]]

# Save nonlinear plots for recession period
combine_plots[[5]] <- plots_nl_iv$gg_s2[[1]]
combine_plots[[6]] <- plots_nl_iv$gg_s2[[2]]

# Show all plots
lin_plots_all <- sapply(combine_plots, ggplotGrob)
marrangeGrob(lin_plots_all, nrow = 2, ncol = 3, top = NULL)


impulse_response_GDP <- melt(impulse_response_GDP ,  id.vars = 'horizon', variable.name = 'scenario')
ggplot(impulse_response_GDP, aes(horizon,value)) + geom_line(aes(colour = scenario)) + 
  xlab("Horizon") +
  ylab("Percentage change") +
  theme(legend.position = 'top')
ggsave(
  filename = paste0('impulse_response_GDP','.png'),
  plot = last_plot(),
  device = "png",
  path = "C:\\Users\\Neha\\Desktop\\Thesis\\Coding\\figures\\publish\\"
)


impulse_response_inf <- melt(impulse_response_inf ,  id.vars = 'horizon', variable.name = 'scenario')
ggplot(impulse_response_inf, aes(horizon,value)) + geom_line(aes(colour = scenario)) +
  xlab("Horizon") +
  ylab("Percentage point change")+
  theme(legend.position = 'top')
ggsave(
  filename = paste0('impulse_response_inf','.png'),
  plot = last_plot(),
  device = "png",
  path = "C:\\Users\\Neha\\Desktop\\Thesis\\Coding\\figures\\publish\\"
)





#####################
### Impulse responses of consumption/average house prices
####################

var_interest <- 'house_avg_price_log'

# Endogenous data 
gdp_series   = 'gdp_loglevel_cloyne'
price_series = 'inf_cpi_cloyne' 

endog_data <- macro_data %>%
  select(var_interest, gdp_series, price_series, 'bank_rate') #price_series, extra_var
endog_data <- endog_data[2:nrow(endog_data),]

romer_shock <- macro_data[2:nrow(macro_data),] %>% select('Shock')

num_lags = 4

impulse_response_con <- tibble(horizon = 1:16,
                               linear = numeric(16),
                               expansion = numeric(16),
                               recession = numeric(16))

### Running basic(linear) local projections when the shocks are identified 
### externally (IV) (as per lpirfs documentation)
### Using romer_romer shocks

results_lin_iv <- lp_lin_iv(endog_data = endog_data, lags_endog_lin = num_lags,
                            shock = romer_shock,     trend = 0,
                            confint = 1.65,          hor = 16)
iv_lin_plots <- plot_lin(results_lin_iv)

impulse_response_con$linear <- results_lin_iv$irf_lin_mean[1,]

### Running state dependent local projections 
### Using romer_romer shocks
### state -> 'dummy' or 'smooth'

state_var_sm <- macro_data[2:nrow(macro_data),]$gdp_statevar

# Smooth state variable 
results_nl_iv <- lp_nl_iv(endog_data = endog_data, lags_endog_nl = num_lags,
                          shock = romer_shock,     trend = 0,
                          confint = 1.65,          hor = 16,
                          switching = state_var_sm,use_logistic = TRUE,
                          use_hp = FALSE,          gamma = 5)

# Make and save nonlinear plots
plots_nl_iv <- plot_nl(results_nl_iv)

impulse_response_con$expansion <- results_nl_iv$irf_s1_mean[1,]
impulse_response_con$recession <- results_nl_iv$irf_s2_mean[1,]

# Make list to save all plots
combine_plots <- list()

# Save linear plots in list
combine_plots[[1]] <- iv_lin_plots[[1]]

# Save nonlinear plots for expansion period
combine_plots[[2]] <- plots_nl_iv$gg_s1[[1]]

# Save nonlinear plots for recession period
combine_plots[[3]] <- plots_nl_iv$gg_s2[[1]]

# Show all plots
lin_plots_all <- sapply(combine_plots, ggplotGrob)
marrangeGrob(lin_plots_all, nrow = 1, ncol = 3, top = NULL)

impulse_response_con <- melt(impulse_response_con ,  id.vars = 'horizon', variable.name = 'scenario')
ggplot(impulse_response_con, aes(horizon,value)) + 
  geom_line(aes(colour = scenario)) +
  xlab("Horizon") +
  ylab("Percentage change") +
  theme(legend.position = 'top')

ggsave(
  filename = paste0('impulse_response_',var_interest,'.png'),
  plot = last_plot(),
  device = "png",
  path = "C:\\Users\\Neha\\Desktop\\Thesis\\Coding\\figures\\publish\\"
)

#############################




############################
### Effect on inequality ###
############################

gini_vars <- c('gini_inc', 'gini_wage', "gini_tcon")

impulse_response_gini <- tibble(horizon = 1:16,
                                gini_inc   = numeric(16),
                                gini_wage  = numeric(16),
                                gini_tcon  = numeric(16))

impulse_response_gini_exp <- tibble(horizon = 1:16,
                                gini_inc   = numeric(16),
                                gini_wage  = numeric(16),
                                gini_tcon  = numeric(16))

impulse_response_gini_rec <- tibble(horizon = 1:16,
                                gini_inc   = numeric(16),
                                gini_wage  = numeric(16),
                                gini_tcon  = numeric(16))

for (var in gini_vars) {
  
endog_data <- macro_data %>%
  select(var, gdp_series, price_series, 'bank_rate')

endog_data <- endog_data[2:nrow(endog_data),]
romer_shock <- macro_data[2:nrow(macro_data),] %>% select('Shock')

num_lags = 4

### Running basic(linear) local projections when the shocks are identified 
### externally (IV) (as per lpirfs documentation)
### Using romer_romer shocks

results_lin_iv <- lp_lin_iv(endog_data = endog_data, lags_endog_lin = num_lags,
                            shock = romer_shock,     trend = 0,
                            confint = 1.65,          hor = 16, use_nw = TRUE)
iv_lin_plots <- plot_lin(results_lin_iv)

impulse_response_gini[[var]] <- results_lin_iv$irf_lin_mean[1,]

### Running state dependent local projections 
### Using romer_romer shocks
### state -> 'dummy' or 'smooth'

state_var_sm <- macro_data[2:nrow(macro_data),]$gdp_statevar

# Smooth state variable 
results_nl_iv <- lp_nl_iv(endog_data = endog_data, lags_endog_nl = num_lags,
                          shock = romer_shock,     trend = 0,
                          confint = 1.65,          hor = 16,
                          switching = state_var_sm,use_logistic = TRUE,
                          use_hp = FALSE,          gamma = 5)

impulse_response_gini_exp[[var]] <- results_nl_iv$irf_s1_mean[1,]
impulse_response_gini_rec[[var]] <- results_nl_iv$irf_s2_mean[1,]

# Make and save nonlinear plots
plots_nl_iv <- plot_nl(results_nl_iv)

# Make list to save all plots
combine_plots <- list()

# Save linear plots in list
combine_plots[[1]] <- iv_lin_plots[[1]]
combine_plots[[2]] <- iv_lin_plots[[2]]
combine_plots[[3]] <- iv_lin_plots[[3]]

# Save nonlinear plots for expansion period
combine_plots[[4]] <- plots_nl_iv$gg_s1[[1]]
combine_plots[[5]] <- plots_nl_iv$gg_s1[[2]]
combine_plots[[6]] <- plots_nl_iv$gg_s1[[3]]

# Save nonlinear plots for recession period
combine_plots[[7]] <- plots_nl_iv$gg_s2[[1]]
combine_plots[[8]] <- plots_nl_iv$gg_s2[[2]]
combine_plots[[9]] <- plots_nl_iv$gg_s2[[3]]

# Show all plots
lin_plots_all <- sapply(combine_plots, ggplotGrob)
# marrangeGrob(lin_plots_all, nrow = 3, ncol = 3, top = NULL)
}

impulse_response_gini <- melt(impulse_response_gini ,  id.vars = 'horizon', variable.name = 'measure on')
ggplot(impulse_response_gini, aes(horizon,value)) + 
  geom_line(aes(colour = `measure on`)) +
  xlab("Horizon") +
  ylab("Change in coefficient") +
  theme(legend.position = "top")
ggsave(
  filename = paste0('gini_vars','.png'),
  plot = last_plot(),
  device = "png",
  path = "C:\\Users\\Neha\\Desktop\\Thesis\\Coding\\figures\\publish\\"
)

impulse_response_gini_exp <- melt(impulse_response_gini_exp ,  id.vars = 'horizon', variable.name = 'measure on')
ggplot(impulse_response_gini_exp, aes(horizon,value)) + 
  geom_line(aes(colour = `measure on`)) +
  xlab("Horizon") +
  ylab("Change in coefficient") +
  theme(legend.position = "top")
ggsave(
  filename = paste0('gini_vars_exp','.png'),
  plot = last_plot(),
  device = "png",
  path = "C:\\Users\\Neha\\Desktop\\Thesis\\Coding\\figures\\publish\\"
)

impulse_response_gini_rec <- melt(impulse_response_gini_rec ,  id.vars = 'horizon', variable.name = 'measure on')
ggplot(impulse_response_gini_rec, aes(horizon,value)) + 
  geom_line(aes(colour = `measure on`)) +
  xlab("Horizon") +
  ylab("Change in coefficient") +
  theme(legend.position = "top")
ggsave(
  filename = paste0('gini_vars_rec','.png'),
  plot = last_plot(),
  device = "png",
  path = "C:\\Users\\Neha\\Desktop\\Thesis\\Coding\\figures\\publish\\"
)

#############################






#############################
### For analyzing percentiles for (wage and income)
#############################

impulse_response <- tibble(horizon = 1:16,
  p1 = numeric(16),
  p2 = numeric(16),
  p3 = numeric(16),
  p4 = numeric(16),
  p5 = numeric(16)
)

for (i in 1: 5){
  # Endogenous data 
  extra_var    = paste0('wage_p',i)
  gdp_series   = 'gdp_loglevel_cloyne'
  price_series = 'inf_cpi_cloyne' 

  endog_data <- macro_data %>%
    select(gdp_series, price_series, extra_var, 'bank_rate') #price_series, extra_var
  endog_data <- endog_data[2:nrow(endog_data),]
  
  romer_shock <- macro_data[2:nrow(macro_data),] %>% select('Shock')
  
  num_lags = 4
  
  ### Running basic(linear) local projections when the shocks are identified 
  ### externally (IV) (as per lpirfs documentation)
  ### Using romer_romer shocks
  results_lin_iv <- lp_lin_iv(endog_data = endog_data, lags_endog_lin = num_lags,
                              shock = romer_shock,     trend = 0,
                              confint = 1.65,          hor = 16)
  iv_lin_plots <- plot_lin(results_lin_iv)
  colname <- paste0("p",i)
  # print(results_lin_iv$irf_lin_mean)
  impulse_response[[colname]] <-  results_lin_iv$irf_lin_mean[3,]
  
  ### Running state dependent local projections 
  ### Using romer_romer shocks
  ### state -> 'dummy' or 'smooth'
  state_var_sm <- macro_data[2:nrow(macro_data),]$gdp_statevar

  # Smooth state variable 
  results_nl_iv <- lp_nl_iv(endog_data = endog_data, lags_endog_nl = num_lags,
                            shock = romer_shock,     trend = 0,
                            confint = 1.65,          hor = 16,
                            switching = state_var_sm,use_logistic = TRUE,
                            use_hp = FALSE,          gamma = 5)
  
  # Make and save nonlinear plots
  plots_nl_iv <- plot_nl(results_nl_iv)
  
  # Make list to save all plots
  combine_plots <- list()
  
  # Save linear plots in list
  combine_plots[[1]] <- iv_lin_plots[[1]]
  combine_plots[[2]] <- iv_lin_plots[[2]]
  combine_plots[[3]] <- iv_lin_plots[[3]]
  
  # Save nonlinear plots for expansion period
  combine_plots[[4]] <- plots_nl_iv$gg_s1[[1]]
  combine_plots[[5]] <- plots_nl_iv$gg_s1[[2]]
  combine_plots[[6]] <- plots_nl_iv$gg_s1[[3]]
  
  # Save nonlinear plots for recession period
  combine_plots[[7]] <- plots_nl_iv$gg_s2[[1]]
  combine_plots[[8]] <- plots_nl_iv$gg_s2[[2]]
  combine_plots[[9]] <- plots_nl_iv$gg_s2[[3]]
  
  # Show all plots
  lin_plots_all <- sapply(combine_plots, ggplotGrob)
  marrangeGrob(lin_plots_all, nrow = 3, ncol = 3, top = NULL)
}

for (i in 1:5){
  var <- paste0('p',i,'_avg')
  impulse_response[[var]] <- rollmean(impulse_response[[paste0('p',i)]], k=3, fill=NA, align='center')
}

impulse_response_raw <- subset(impulse_response, select = c('horizon', 'p1','p2','p3','p4','p5'))
impulse_response_avg <- subset(impulse_response, select = c('horizon', 'p1_avg','p2_avg','p3_avg','p4_avg','p5_avg'))
names(impulse_response_avg)[names(impulse_response_avg) == 'p1_avg'] <- 'p1'
names(impulse_response_avg)[names(impulse_response_avg) == 'p2_avg'] <- 'p2'
names(impulse_response_avg)[names(impulse_response_avg) == 'p3_avg'] <- 'p3'
names(impulse_response_avg)[names(impulse_response_avg) == 'p4_avg'] <- 'p4'
names(impulse_response_avg)[names(impulse_response_avg) == 'p5_avg'] <- 'p5'

impulse_response_avg[1,] <- impulse_response_raw[1,]
impulse_response_avg[16,] <- impulse_response_raw[16,]

impulse_response_raw <- melt(impulse_response_raw ,  id.vars = 'horizon', variable.name = 'percentiles')
impulse_response_avg <- melt(impulse_response_avg ,  id.vars = 'horizon', variable.name = 'percentiles')

ggplot(impulse_response_raw, aes(horizon,value)) + geom_line(aes(colour = percentiles))
ggplot(impulse_response_avg, aes(horizon,value)) + 
  geom_line(aes(colour = percentiles)) +
  xlab('Horizon') +
  ylab('Percentage change') +
  theme(legend.position = 'top')

ggsave(
  filename = paste0('wage_percentile_avg','.png'),
  plot = last_plot(),
  device = "png",
  path = "C:\\Users\\Neha\\Desktop\\Thesis\\Coding\\figures\\publish\\"
)










###################
### Using only lags of dependent variables as the control
###################



dep_var <- 'gini_inc'

endog_data <- macro_data %>%
  select(dep_var) #price_series, extra_var
endog_data <- endog_data[2:nrow(endog_data),]
romer_shock <- macro_data[2:nrow(macro_data),] %>% select('Shock')

num_lags = 4

### Running basic(linear) local projections when the shocks are identified 
### externally (IV) (as per lpirfs documentation)
### Using romer_romer shocks

results_lin_iv <- lp_lin_iv(endog_data = endog_data, lags_endog_lin = num_lags,
                            shock = romer_shock,     trend = 0,
                            confint = 1.65,          hor = 16)
iv_lin_plots <- plot_lin(results_lin_iv)

### Running state dependent local projections 
### Using romer_romer shocks
### state -> 'dummy' or 'smooth'

state_var_dm      <- macro_data[2:nrow(macro_data),]$dummy
state_var_debtdummy  <- macro_data[2:nrow(macro_data),]$debtgdp_ratio_dummy
# state_var_smooth <- macro_data[2:nrow(macro_data),]$smooth
state_var_sm <- macro_data[2:nrow(macro_data),]$gdp_statevar

# Using dummy state variable

# Smooth state variable 
results_nl_iv <- lp_nl_iv(endog_data = endog_data, lags_endog_nl = num_lags,
                          shock = romer_shock,     trend = 0,
                          confint = 1.65,          hor = 16,
                          switching = state_var_sm,use_logistic = TRUE,
                          use_hp = FALSE,          gamma = 5)

# Binary state variable
# results_nl_iv <- lp_nl_iv(endog_data = endog_data, lags_endog_nl = num_lags,
#                           shock = romer_shock,     trend = 0, 
#                           confint = 1.65,          hor = 16,
#                           switching = state_var_dm,use_logistic = FALSE,
#                           use_hp = FALSE)

# Make and save nonlinear plots
plots_nl_iv <- plot_nl(results_nl_iv)

# Make list to save all plots
combine_plots <- list()

# Save linear plots in list
combine_plots[[1]] <- iv_lin_plots[[1]]

# Save nonlinear plots for expansion period
combine_plots[[2]] <- plots_nl_iv$gg_s1[[1]]

# Save nonlinear plots for recession period
combine_plots[[3]] <- plots_nl_iv$gg_s2[[1]]

# Show all plots
lin_plots_all <- sapply(combine_plots, ggplotGrob)
marrangeGrob(lin_plots_all, nrow = 1, ncol = 3, top = NULL)