
wd = getwd()

library(readxl)
library(dplyr)
library(ggplot2)
library(broom)
library(reshape2)
data_file <- '../results/exported-data-FI.xlsx'

sheets <- excel_sheets(data_file)
dfs <- lapply(sheets, function(X) read_excel(data_file, sheet = X))

names(dfs) <- sheets

data.set <- 'donation0'

df_monthly <-dfs[['montly.statistics']] %>% filter(data.set == data.set)

create_cos_var_name <- function(column){
  return(paste0("cos_", column))
}

create_sin_var_name <- function(column){
  return(paste0("sin_", column))
}

create_cos_sin <- function(data, column, period){
  cos_var_name <- create_cos_var_name(column)
  sin_var_name <- create_sin_var_name(column)
  
  data <- data %>% mutate(!!cos_var_name := cos(2*pi*as.numeric(get({{column}}))/period),
                          !!sin_var_name := sin(2*pi*as.numeric(get({{column}}))/period))
  return(data)
}

fit_cos <- function(data, column, period, y_col='mean'){
  #convert column to cos(x)+sin(x) which corresponds to amplitude*cos(x-phase)
  data <- create_cos_sin(data, column, period)
  cos_var_name <- create_cos_var_name(column)
  sin_var_name <- create_sin_var_name(column)
  
  formula <- as.formula(paste0(y_col, '~', cos_var_name, '+', sin_var_name))
  fit <- lm(formula, data)
  return(fit)
}

predict_cos <- function(lm_object, data, column, period){
  data <- create_cos_sin(data, column, period)
  data$pred <- predict.lm(lm_object, data)
  data$correction <- data$pred - lm_object$coefficients[1]
  return(data)
}

fit_params_month <- df_monthly %>% split(df_monthly$Sex) %>% map(\(df) fit_cos(df, 'month', 12)) 

df_hourly <-dfs[['hourly.statistics']] %>% filter(data.set == data.set)

fit_params_hour <- df_hourly %>% split(df_hourly$Sex) %>% map(\(df) fit_cos(df, 'hour', 24)) 

df_age <-dfs[['annual.age']] %>% filter(data.set == data.set)

df_hb <- dfs[['annual.hb']] %>% filter(data.set == data.set)

summary <- df_hb %>% filter(Hb > 0, Hb < 20) %>%
  group_by(year, Sex) %>%
  summarise(mean_Hb = sum(Hb*n)/sum(n), hour = sum(mean.hour*n)/sum(n), age = sum(mean.age*n)/sum(n))


melt(summary, id.vars=c("Sex", "year")) %>%
  ggplot(aes(x=year, y=value, col=Sex)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(variable), scales='free')

summary <- summary %>% split(summary$Sex) %>%
  map(\(df) predict_cos(fit_params_hour[[df$Sex[1]]], df, 'hour', 24)) %>%
  bind_rows()

summary <- summary %>% mutate(mean_Hb_corrected_for_hour=mean_Hb-correction) 


ggplot(melt(summary, id.vars=c("Sex", "year"), measure.vars=c('mean_Hb_corrected_for_hour', 'mean_Hb')), aes(x=year, y=value, col=variable)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(vars(Sex), scale='free') 
  
