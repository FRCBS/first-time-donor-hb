wd = getwd()

library(readxl)
library(broom)
library(reshape2)
library(tidyverse)
library(purrr)
library(openxlsx)

#set to NL or FI
country <- 'FI'

dir.create(file.path('../plots'), showWarnings = FALSE)
plot_dir = file.path('../plots', country)
dir.create(plot_dir, showWarnings = FALSE)
data_file <- paste0('../results/exported-data-', country, '.xlsx')

sheets <- excel_sheets(data_file)
dfs <- lapply(sheets, function(X)
  read_excel(data_file, sheet = X))

names(dfs) <- sheets

#only first time donors
data.set <- 'donation0'

#For FI there is only a few months for 2024 so ignore that year, because it is biased for month
latest_full_year <- 2024

#for FI, in 2000 there are many more donors, probably not all first time?
first_full_year <- 2000
if (country == "FI") {
  latest_full_year <- 2023
  
}

if (country == "NL") {
  convert_Hb <- 1 / 0.0621
} else if (country == "NA") {
  convert_Hb <- 10.
} else {
  convert_Hb <- 1.
}

#select data
df_monthly <- dfs[['montly.statistics']] %>% filter(data.set == !!data.set,
                                                    year <= latest_full_year,
                                                    year >= first_full_year)

df_hourly <- dfs[['hourly.statistics']] %>% filter(data.set == !!data.set,
                                                   year <= latest_full_year,
                                                   year >= first_full_year)
df_age <- dfs[['annual.age']] %>% filter(data.set == !!data.set,
                                         year <= latest_full_year,
                                         year >= first_full_year)

df_hb <- dfs[['annual.hb']] %>% filter(data.set == !!data.set,
                                       year <= latest_full_year,
                                       year >= first_full_year)
df_hb <- df_hb %>% filter(Hb > 0, Hb < 200)

#Function for weighted mean/sd
weighted_mean <- function(x, w) {
  #w = 1/sd^2
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

weighted_sd_average <- function(sds, Ns) {
  weights <- Ns / sds^2
  weighted_var <- sum(weights * sds^2) / sum(weights)
  sqrt(weighted_var)
}



# Take (weighted!) mean of Hb, age, month, hour for each year
hb_summary <- df_hb %>% na.omit() %>%
  group_by(Sex, year) %>%
  summarise(
    Hb.mean = weighted_mean(Hb, n),
    Hb.sd = sqrt(sum(n * (Hb - Hb.mean)^2) / sum(n)),
    hour.mean = weighted_mean(mean.hour, n / sd.hour^2),
    hour.sd = weighted_sd_average(sd.hour, n),
    age.mean = weighted_mean(mean.age, n / sd.age^2),
    age.sd = weighted_sd_average(sd.age, n),
    month.mean = weighted_mean(mean.month, n / sd.month),
    month.sd = weighted_sd_average(sd.month, n),
    n_ = sum(n)
  )

#Just for inspection of year vs age/month/hour
age_summary <- df_age %>% group_by(Sex, age) %>% summarise(mean_Hb = weighted_mean(mean.hb, n /
                                                                                     sd.hb^2))
month_summary <- df_monthly %>% group_by(Sex, month) %>% summarise(mean_Hb = weighted_mean(mean, n /
                                                                                             sd^2))
hour_summary <- df_hourly %>% group_by(Sex, hour) %>% summarise(mean_Hb = weighted_mean(mean, n /
                                                                                          sd^2))

#Overall means of Hb
means <- as.list(deframe(df_hb %>% group_by(Sex) %>% summarise(mean_Hb =
                                                                 sum(Hb * n) / sum(n))))

#Logic is as follows:
# Calculate (weighted) mean of Hb for each age/hour/month. Then using this "model" for Hb vs age/hour/month
#Calculate the correction per (groupby) that is again the weighted mean of all "model_hb", weighted by n the number
#of donors with that age. The correction for each year is then Hb_corr - mean(all sex specific Hbs).

age_corr <- df_age %>% group_by(Sex, age) %>%
  mutate(model_hb = weighted_mean(mean.hb, n / sd.hb^2)) %>%
  group_by(Sex, year) %>%
  summarise(Hb_corr = weighted_mean(model_hb, n)) %>%
  group_by(Sex) %>% mutate(corr_age = Hb_corr - means[[first(Sex)]])  %>% select(!Hb_corr)

hour_corr <- df_hourly %>% group_by(Sex, hour) %>%
  mutate(mean_hb = weighted_mean(mean, n / sd^2)) %>%
  group_by(Sex, year) %>%
  summarise(Hb_corr = weighted_mean(mean_hb, n)) %>%
  group_by(Sex) %>% mutate(corr_hour = Hb_corr - means[[first(Sex)]])   %>% select(!Hb_corr)

month_corr <- df_monthly %>% group_by(Sex, month) %>%
  mutate(mean_hb = weighted_mean(mean, n / sd^2)) %>%
  group_by(Sex, year) %>%
  summarise(Hb_corr = weighted_mean(mean_hb, n)) %>%
  group_by(Sex) %>% mutate(corr_month = Hb_corr - means[[first(Sex)]])  %>% select(!Hb_corr)

#need to substract correction
#e.g. there are more younger people in 2016, so the Hb in males is higher because of that, so should subtract
#the correction because it is positive
corrected_hb_year <- hb_summary %>% left_join(age_corr) %>% left_join(hour_corr) %>% left_join(month_corr) %>%
  mutate(corr_total = corr_age + corr_hour + corr_month,
         Hb_total_corr = Hb.mean - corr_total)


#Save corrected Hb vs year
write.xlsx(corrected_hb_year,
           paste0('../results/hb_year_corrected_', country, '.xlsx'))

ggplot(age_summary, aes(age, mean_Hb)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(vars(Sex), scale = 'free')

#Some plotting
test <- corrected_hb_year
ggplot(
  melt(
    test,
    id.vars = c("Sex", "year"),
    measure.vars = c('corr_age', 'corr_hour', 'corr_month', 'corr_total')
  ),
  aes(
    x = year,
    y = value * convert_Hb,
    col = variable
  )
) +
  geom_smooth() +
  facet_wrap(vars(Sex), scale = 'free')

melt(
  hb_summary,
  id.vars = c("Sex", "year"),
  measure.vars = c('age.mean', 'month.mean', 'hour.mean')
) %>%
  ggplot(aes(x = year, y = value, col = Sex)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(variable), scales = 'free')

ggplot(test) +
  aes(x = year, y = Hb_total_corr * convert_Hb) +
  geom_point(color='blue') +
  geom_linerange(aes(
    ymin = (Hb_total_corr - Hb.sd / sqrt(n_)) * convert_Hb,
    ymax = (Hb_total_corr + Hb.sd / sqrt(n_)) * convert_Hb
  )) +
  geom_smooth(method = 'lm', color='blue') +
  facet_wrap(vars(Sex), scale = 'free') +
  geom_point(aes(year, Hb.mean * convert_Hb), color = 'red') +
  geom_smooth(aes(year, Hb.mean * convert_Hb),
              method = 'lm',
              color = 'red')



coefficients_df <- test %>%
  mutate(
    x = (year - 2020),
    y = Hb_total_corr * convert_Hb,
    w = Hb.sd / sqrt(n_) * convert_Hb
  ) %>%
  group_by(Sex) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(y ~ x, weights = w, data = .)), coef = map(model, tidy)) %>%
  select(Sex, coef) %>%
  unnest(coef)
