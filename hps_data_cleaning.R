library(naniar)

variables_ph10 <- c(
  "RHISPANIC",
  "RRACE",
  "EEDUC",  
  "MS",
  "THHLD_NUMPER",
  "THHLD_NUMKID",
  "WRKLOSSRV",
  "EST_ST",
  "TENURE",
  "INCOME",
  "HLTH_MHCHLD1",
  "HLTH_MHCHLD2",
  "HLTH_MHCHLD9",
  "HWEIGHT"
)
variables_ph5to8 <- c(
  "RHISPANIC",
  "RRACE",
  "EEDUC",  
  "MS",
  "THHLD_NUMPER",
  "THHLD_NUMKID",
  "WRKLOSSRV",
  "EST_ST",
  "TENURE",
  "INCOME",
  "KIDBHVR1",
  "KIDBHVR2",
  "KIDBHVR9",
  "HWEIGHT"
)

# loop through weeks for phase 3.10
start_week_ph10 <- 61
end_week_ph10 <- 63
data_ph10 <- data.frame(matrix(ncol = length(variables_ph10), nrow = 0))
for (i in start_week_ph10:end_week_ph10) {
  data_file <- paste0('HPS_Week', i, '_PUF_CSV/pulse2023_puf_', i, '.csv')
  data_curr <- read.csv(file = data_file, header = TRUE)
  data_curr <- data_curr[, variables_ph10]
  data_ph10 <- rbind(data_ph10, data_curr)
}

# loop through weeks for phase 3.5 to 3.8
start_week_ph5to8 <- 46
end_week_ph5to8 <- 57
data_ph5to8 <- data.frame(matrix(ncol = length(variables_ph5to8), nrow = 0))

for (i in start_week_ph5to8:end_week_ph5to8) {
  if (i <= 52) {
    data_file <- paste0('HPS_Week', i, '_PUF_CSV/pulse2022_puf_', i, '.csv')
  }
  else {
    data_file <- paste0('HPS_Week', i, '_PUF_CSV/pulse2023_puf_', i, '.csv')
  }
  data_curr <- read.csv(file = data_file, header = TRUE)
  data_curr <- data_curr[, variables_ph5to8]
  data_ph5to8 <- rbind(data_ph5to8, data_curr)
}

names(data_ph5to8) <- names(data_ph10)
df_raw <- rbind(data_ph5to8, data_ph10)
df <- df_raw[df_raw$THHLD_NUMKID > 0,]
dim(df)


# define response
df$KidMentalIssue <- 1 * ((df$HLTH_MHCHLD1 > 0 | df$HLTH_MHCHLD2 > 0) & df$HLTH_MHCHLD9 < 0) + 
  (-88) * ((df$HLTH_MHCHLD1 < 0 & df$HLTH_MHCHLD2 < 0 & df$HLTH_MHCHLD9 < 0) | ((df$HLTH_MHCHLD1 > 0 | df$HLTH_MHCHLD2 > 0) & df$HLTH_MHCHLD9 > 0))

df <- replace_with_na(df,
                      replace = list(KidMentalIssue = -88))
df$KidMentalIssue <- factor(df$KidMentalIssue, 
                            labels = c('0', '1'))
table(df$KidMentalIssue)


# preprocessing covariates
df$RHISPANIC <- factor(df$RHISPANIC,
                       labels = c('NotHisp', 'Hispanic'))

df$RRACE <- factor(df$RRACE,
                   labels = c('White', 'Black', 'Asian', 'Others'))
df$RACE <- factor( 
  1 * (df$RHISPANIC == 'NotHisp' & df$RRACE == 'White') + 
    2 * (df$RHISPANIC == 'NotHisp' & df$RRACE == 'Black') + 
    3 * (df$RHISPANIC == 'NotHisp' & df$RRACE == 'Asian') + 
    4 * (df$RHISPANIC == 'Hispanic') +
    5 * (df$RHISPANIC == 'NotHisp' & df$RRACE == 'Others'),
  labels = c('NotHispWhite', 'NotHispBlack', 'NotHispAsian', 'Hispanic', 'Others'))
df$EEDUC <- factor(
  df$EEDUC,
  labels = c('Less_than_high_school', 'Some_high_school', 'High_school_graduate',
             'Some_college', 'Associate', 'Bachelor', 'Graduate'))
df$KIDPROP <- df$THHLD_NUMKID/df$THHLD_NUMPER
df$EST_ST <- factor(df$EST_ST, 
                    labels = c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 
                               'California', 'Colorado', 'Connecticut',
                               'Delaware', 'District_of_Columbia',
                               'Florida',
                               'Georgia',
                               'Hawaii',
                               'Idaho', 'Illinois', 'Indiana', 'Iowa',
                               'Kansas', 'Kentucky',
                               'Louisiana',
                               'Maine', 'Maryland', 'Massachusetts', 'Michigan',
                               'Minnesota', 'Mississippi', 'Missouri', 'Montana',
                               'Nebraska', 'Nevada', 'New_Hampshire', 'New_Jersey',
                               'New_Mexico', 'New_York', 'North_Carolina', 'North_Dakota',
                               'Ohio', 'Oklahoma', 'Oregon',
                               'Pennsylvania',
                               'Rhode_Island',
                               'South_Carolina', 'South_Dakota',
                               'Tennessee', 'Texas',
                               'Utah',
                               'Vermont', 'Virginia',
                               'Washington', 'West_Virginia', 'Wisconsin', 'Wyoming'))

df <- replace_with_na(df,
                      replace = list(WRKLOSSRV = c(-88, -99), 
                                     TENURE = c(-88, -99),
                                     INCOME = c(-88, -99),
                                     MS = -99))

df$WRKLOSSRV <- factor(df$WRKLOSSRV,
                       labels = c('Yes', 'No'))
df$MS <- factor(
  df$MS,
  labels = c('Now_Married', 'Widowed', 'Divorced', 'Separated', 'Never_Married')
)

df$INCOME <- factor(
  df$INCOME,
  labels = c('less_than_25K', '25K_35K', '35K_50K', '50K_75K', '75K_100K',
             '100K_150K', '150K_200K', 'more_than_200K')
)

df$TENURE <- factor(
  df$TENURE,
  labels = c('Owned', 'Loan', 'Rented', 'Occupied')
)

# re-select covariates
df <- df[, c('KidMentalIssue', 'KIDPROP', 'RACE', 'EEDUC', 
             'MS', 'EST_ST', 'WRKLOSSRV', 'TENURE', 'INCOME', 'HWEIGHT')]


# remove missing value for now
df <- na.omit(df)
hweight <- df$HWEIGHT
df <- df[, c('KidMentalIssue', 'KIDPROP', 'RACE', 'EEDUC', 
             'MS', 'EST_ST', 'WRKLOSSRV', 'TENURE', 'INCOME')]

dim(df)
head(df)


#write.csv(df, "hps_cleaned.csv")


