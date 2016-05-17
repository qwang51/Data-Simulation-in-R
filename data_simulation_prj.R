# Qianran (Zoe) Wang

set.seed(123)
# SD = SE * Sqrt(n)

rtnorm <- function(N, mean, sd, min, max) {
  qnorm(runif(N, pnorm(min, mean, sd), pnorm(max, mean, sd)), mean, sd)
}

generate_df <- function(N, age_mean, edu_mean, female_percentage, 
                        sport_mean, sport_se, stress_mean, stress_se,
                        restrained_eating_mean, restrained_eating_se,
                        emotional_eating_mean, emotional_eating_se,
                        external_eating_mean, external_eating_se,
                        gender_social_role_mean, gender_social_role_se,
                        soc_mean, soc_se, family_support_mean, family_support_se,
                        friends_support_mean, friends_support_se,
                        sign_support_mean, sign_support_se){
  age <- rnorm(N, age_mean)
  edu <- rnorm(N, edu_mean)
  gender <- rbinom(N, 1, female_percentage)
  sports <- rnorm(N, sport_mean, sport_se * sqrt(N))
  sports <- ifelse(sports < 0, sport_mean, sports)
  stress <- rtnorm(N, stress_mean, stress_se * sqrt(N), 0, 100)
  restrained_eating <- rtnorm(N, restrained_eating_mean, restrained_eating_se * sqrt(N), 10, 50)
  emotional_eating <- rtnorm(N, emotional_eating_mean, emotional_eating_se * sqrt(N), 13, 65)
  external_eating <- rtnorm(N, external_eating_mean, external_eating_se * sqrt(N), 10, 50)
  gender_social_role <- rtnorm(N, gender_social_role_mean, gender_social_role_se * sqrt(N), 5, 25)
  soc <- rtnorm(N, soc_mean, soc_se * sqrt(N), 13, 91)
  family_support <- rtnorm(N, family_support_mean, family_support_se * sqrt(N), 4, 28)
  friends_support <- rtnorm(N, friends_support_mean, friends_support_se * sqrt(N), 4, 28)
  sign_support <- rtnorm(N, sign_support_mean, sign_support_se * sqrt(N), 4, 28)
  df <- data.frame(age, edu, gender, sports, stress, restrained_eating, emotional_eating, external_eating,
                   gender_social_role, soc, family_support, friends_support, sign_support)
  return(df)
}

#Austria data frame
austria_df <- generate_df(N=421, age_mean = 22.13, edu_mean = 15.13, female_percentage = 0.563,
                          sport_mean = 2.45, sport_se = 0.09, stress_mean = 51, stress_se = 1.47,
                          restrained_eating_mean = 22.63, restrained_eating_se = 0.34,
                          emotional_eating_mean = 29.82, emotional_eating_se = 0.44,
                          external_eating_mean = 21.94, external_eating_se = 0.28,
                          gender_social_role_mean = 10.96, gender_social_role_se = 0.18,
                          soc_mean = 61.74, soc_se = 0.28,
                          family_support_mean = 24.4, family_support_se = 0.32,
                          friends_support_mean = 24.83, friends_support_se = 0.3,
                          sign_support_mean = 25.15, sign_support_se = 0.31)

austria_df$QoL_phy <- .137 * (austria_df$sports - 2.45) + 
  -.086 * (austria_df$age - 22.13) +
  .112 * (austria_df$restrained_eating - 22.63) +
  .142 * (austria_df$friends_support - 24.83) +
  54.02 + rnorm(421, sd=0.38 * sqrt(421))

austria_df$QoL_mt <- -.121 * (austria_df$age - 22.13) + 
  .468 * (austria_df$soc - 2.45) +
  .354 * (austria_df$family_support - 24.4) +
  -.211 * (austria_df$stress - 51) + 54.21 + rnorm(421, sd=0.38 * sqrt(421))
austria_df$QoL_mt <- austria_df$QoL_mt - (mean(austria_df$QoL_mt - 54.21))

austria_df$emotional_strain <- -.243 * (austria_df$gender - mean(austria_df$gender)) +
  -.136 * (austria_df$age - 22.13) + 
  .268 * (austria_df$soc - 2.45) +
  .096 * (austria_df$gender_social_role - 10.96) +
  -.194 * (austria_df$restrained_eating - 22.63) +
  -.253 * (austria_df$stress - 51) + 8.78 + rnorm(421, sd=0.21 * sqrt(421))
austria_df$emotional_strain <- austria_df$emotional_strain - (mean(austria_df$emotional_strain - 8.78))

austria_df$symptom <- -.379 * (austria_df$soc - 2.45) +
  .104 * (austria_df$age - 22.13) +
  .231 * (austria_df$sign_support - 25.15) +
  .185 * (austria_df$emotional_eating - 29.82) + 
  .145 * (austria_df$stress - 51) + 25.56 + rnorm(421, sd=0.47 * sqrt(421))
austria_df$symptom <- austria_df$symptom + (25.56 - mean(austria_df$symptom))

austria_df$gender <- as.factor(ifelse(austria_df$gender==1, 'f', 'm'))

#Japan data frame
japan_df <- generate_df(N=460, age_mean = 18.89, edu_mean = 12.36, female_percentage = 0.437,
                          sport_mean = 3.37, sport_se = 0.13, stress_mean = 52.36, stress_se = 1.39,
                          restrained_eating_mean = 32.03, restrained_eating_se = 0.31,
                          emotional_eating_mean = 35.26, emotional_eating_se = 0.42,
                          external_eating_mean = 27.25, external_eating_se = 0.26,
                          gender_social_role_mean = 11.89, gender_social_role_se = 0.17,
                          soc_mean = 52.55, soc_se = 0.52,
                          family_support_mean = 20.36, family_support_se = 0.3,
                          friends_support_mean = 19.57, friends_support_se = 0.29,
                          sign_support_mean = 19.91, sign_support_se = 0.29)

japan_df$QoL_phy <- .296 * (japan_df$external_eating - 27.25) + 50.27 + rnorm(460, sd=0.36*sqrt(460))

japan_df$QoL_mt <- .365 * (japan_df$soc - 52.55) +
  -.377 * (japan_df$stress - 52.36) + 50.29 + rnorm(460, sd=0.36*sqrt(460))

japan_df$emotional_strain <- .291 * (japan_df$soc - 52.55) +
  -.329 * (japan_df$stress - 52.36) + 
  -.204 * (japan_df$age - 18.89) + 9.23 + rnorm(460, sd=0.11*sqrt(460))

japan_df$symptom <- -.341 * (japan_df$soc - 52.55) +
  .157 * (japan_df$stress - 52.36) + 24.8 + rnorm(460, sd=0.44*sqrt(460))

japan_df$gender <- as.factor(ifelse(japan_df$gender==1, 'f', 'm'))

# Combine Japan and Austria data frame
austria_df$country <- as.factor(rep('Austria', nrow(austria_df)))
japan_df$country <- as.factor(rep('Japan', nrow(japan_df)))
all_df <- rbind(austria_df, japan_df)


library(ggplot2)
ggplot(all_df, aes(x=soc, y=QoL_mt)) + geom_point(aes(color=country)) +
  geom_smooth(method = 'lm')
ggplot(all_df, aes(x=country, y=restrained_eating)) +
  geom_boxplot()
ggplot(all_df, aes(x=soc, y=symptom)) + geom_point(aes(color=country)) +
  geom_smooth(method = 'lm')
ggplot(all_df, aes(x=gender_social_role, fill=country)) + geom_density(alpha=0.5)

ggplot(austria_df, aes(x=age, y=stress)) + geom_point(aes(color=gender)) +
  geom_smooth(method = 'lm')

# Linear Model
austria_phy <- lm(QoL_phy ~ . - QoL_mt - emotional_strain - symptom, data = austria_df[-18])
japan_phy <- lm(QoL_phy ~ . - QoL_mt - emotional_strain - symptom, data = japan_df[-18])
austria_mt <- lm(QoL_mt ~ . - QoL_phy - emotional_strain - symptom, data = austria_df[-18])
japan_mt <- lm(QoL_mt ~ . - QoL_phy - emotional_strain - symptom, data = japan_df[-18])
austria_strain <- lm(emotional_strain ~ . - QoL_mt - QoL_phy - symptom, data = austria_df[-18])
japan_strain <- lm(emotional_strain ~ . - QoL_mt - QoL_phy - symptom, data = japan_df[-18])
austria_symptom <- lm(symptom ~ . - QoL_mt - QoL_phy - emotional_strain, data = austria_df[-18])
japan_symptom <- lm(symptom ~ . - QoL_mt - QoL_phy - emotional_strain, data = japan_df[-18])

summary(austria_phy)
summary(austria_mt)
summary(austria_strain)
summary(austria_symptom)

summary(japan_phy)
summary(japan_mt)
summary(japan_strain)
summary(japan_symptom)

plot(austria_strain, 1)
plot(japan_strain, 1)







