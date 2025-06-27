
wd = getwd()

library(readxl)
library(dplyr)
library(ggplot2)
library(broom)
library(reshape2)
library(tidyverse)

country <- 'NL'
dir.create(file.path('../plots'), showWarnings = FALSE)
plot_dir = file.path('../plots', country)
dir.create(plot_dir, showWarnings = FALSE)
data_file <- paste0('../results/exported-data-', country, '.xlsx')

sheets <- excel_sheets(data_file)
dfs <- lapply(sheets, function(X) read_excel(data_file, sheet = X))

names(dfs) <- sheets

data.set <- 'donation0'

#For FI there is only a few months for 2024 so ignore that year, because it is biased for month
latest_full_year <- 2024
if (country == "FI"){
  latest_full_year <- 2023
}

df_monthly <-dfs[['montly.statistics']] %>% filter(data.set == data.set, year <= latest_full_year)

df_hourly <-dfs[['hourly.statistics']] %>% filter(data.set == data.set, year <= latest_full_year)
df_age <-dfs[['annual.age']] %>% filter(data.set == data.set, year <= latest_full_year)

df_hb <- dfs[['annual.hb']] %>% filter(data.set == data.set, year <= latest_full_year)
df_hb <- df_hb %>% filter(Hb > 0, Hb < 200)


df_age <- df_age %>% na.omit()

age_summary <- df_age %>% mutate(age=as.numeric(age)) %>% group_by(Sex, age) %>%
  summarise(Hb.mean=sum(mean.hb*sd.hb^2*n)/sum(n*sd.hb^2), n_=sum(n), Hb.sd=sqrt(sum(sd.hb^2*n)/sum(n)))

spl_age <- age_summary %>% split(age_summary$Sex) %>% map(\(df) smooth.spline(df$age, df$Hb.mean, w=df$n_/df$Hb.sd^2, cv=T))

x <- seq(min(age_summary$age), max(age_summary$age), 1)
pred_m <- data.frame(age=x, Hb.mean=predict(spl_age$Male, x)$y, Sex='Male')
pred_f <- data.frame(age=x, Hb.mean=predict(spl_age$Female, x)$y, Sex='Female')
pred_age <- rbind(pred_m, pred_f)
ggplot(age_summary, aes(age, Hb.mean, col=Sex)) +
  geom_point() + 
  geom_linerange(aes(ymin=Hb.mean-Hb.sd/sqrt(n_), ymax=Hb.mean+Hb.sd/sqrt(n_))) +
  geom_line(data=pred_age, aes(age, Hb.mean))

ggsave(file.path(plot_dir, 'mean_Hb_vs_age.png'))

df_monthly <- df_monthly %>% mutate(mean.hb=mean, sd.hb=sd) %>% na.omit()

month_summary <- df_monthly %>% mutate(month=as.numeric(month)) %>% group_by(Sex, month) %>%
  summarise(Hb.mean=sum(mean.hb*sd.hb^2*n)/sum(n*sd.hb^2), n_=sum(n), Hb.sd=sqrt(sum(sd.hb^2*n)/sum(n)))

spl_month <- month_summary %>% split(month_summary$Sex) %>% map(\(df) smooth.spline(df$month, df$Hb.mean, w=df$n_/df$Hb.sd^2, cv=T))

x <- seq(min(month_summary$month), max(month_summary$month), 1)
pred_m <- data.frame(month=x, Hb.mean=predict(spl_month$Male, x)$y, Sex='Male')
pred_f <- data.frame(month=x, Hb.mean=predict(spl_month$Female, x)$y, Sex='Female')
pred_month <- rbind(pred_m, pred_f)
ggplot(month_summary, aes(month, Hb.mean, col=Sex)) +
  geom_point() + 
  geom_linerange(aes(ymin=Hb.mean-Hb.sd/sqrt(n_), ymax=Hb.mean+Hb.sd/sqrt(n_))) +
  geom_line(data=pred_month, aes(month, Hb.mean))

ggsave(file.path(plot_dir, 'mean_Hb_vs_month.png'))


df_hourly <- df_hourly %>% mutate(mean.hb=mean, sd.hb=sd) %>% na.omit()

hour_summary <- df_hourly %>% mutate(hour=as.numeric(hour)) %>% group_by(Sex, hour) %>%
  summarise(Hb.mean=sum(mean.hb*sd.hb^2*n)/sum(n*sd.hb^2), n_=sum(n), Hb.sd=sqrt(sum(sd.hb^2*n)/sum(n)))

spl_hour <- hour_summary %>% split(hour_summary$Sex) %>% map(\(df) smooth.spline(df$hour, df$Hb.mean, w=df$n_/df$Hb.sd^2, cv=T))

x <- seq(min(hour_summary$hour), max(hour_summary$hour), 1)
pred_m <- data.frame(hour=x, Hb.mean=predict(spl_hour$Male, x)$y, Sex='Male')
pred_f <- data.frame(hour=x, Hb.mean=predict(spl_hour$Female, x)$y, Sex='Female')
pred_hour <- rbind(pred_m, pred_f)
ggplot(hour_summary, aes(hour, Hb.mean, col=Sex)) +
  geom_point() + 
  geom_linerange(aes(ymin=Hb.mean-Hb.sd/sqrt(n_), ymax=Hb.mean+Hb.sd/sqrt(n_))) +
  geom_line(data=pred_hour, aes(hour, Hb.mean))

ggsave(file.path(plot_dir, 'mean_Hb_vs_hour.png'))

hb_summary <- df_hb %>% na.omit() %>%
  group_by(Sex, year) %>%
  summarise(Hb = sum(Hb*n)/sum(n), hour = sum(mean.hour*n*sd.hour^2)/sum(n*sd.hour^2),
            age = sum(mean.age*n*sd.age^2)/sum(n*sd.age^2), month = sum(mean.month*n*sd.month^2)/sum(n*sd.month^2))

hb_summary <- hb_summary %>%
  group_by(Sex) %>%
  mutate(pred_age = predict(spl_age[[first(Sex)]], age)$y, pred_age_mean=mean(pred_age),
         pred_hour = predict(spl_hour[[first(Sex)]], hour)$y, , pred_hour_mean=mean(pred_hour),
         pred_month = predict(spl_month[[first(Sex)]], month)$y, , pred_month_mean=mean(pred_month)) %>%
  ungroup()



hb_summary <- hb_summary %>% mutate(Hb_corr_age=Hb-pred_age+pred_age_mean,
                                    Hb_corr_hour=Hb-pred_hour+pred_hour_mean,
                                    Hb_corr_month=Hb-pred_month+pred_month_mean,
                                    Hb_corr_all=Hb-pred_age-pred_hour-pred_month+pred_age_mean+pred_hour_mean+pred_month_mean)

melt(hb_summary, id.vars=c("Sex", "year"),
     measure.vars=c('Hb', 'age', 'month', 'hour')) %>%
  ggplot(aes(x=year, y=value, col=Sex)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(variable), scales='free')

ggsave(file.path(plot_dir, 'mean_Hb_age_month_hour_vs_year.png'))


ggplot(melt(hb_summary, id.vars=c("Sex", "year"),
            measure.vars=c('Hb', 'Hb_corr_age', 'Hb_corr_month', 'Hb_corr_hour', 'Hb_corr_all')),
       aes(x=year, y=value, col=variable)) +
  geom_point() +
  geom_smooth(method='lm', aes(fill=variable), alpha=0.2) +
  facet_wrap(vars(Sex), scale='free') 

ggsave(file.path(plot_dir, 'mean_Hb_vs_year_corrected.png'))


#Idea:
#First get correction by age
#Then correct month and hour average Hb for age
#Then get correction for month and hour
#then correct yearly Hb with above...?

