library(haven)
library(texreg)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
library(glm.predict)
library(jtools)
library(broom)
library(shadowtext)

options(scipen = 999)

`%notin%` <- Negate(`%in%`)

ABV_5 <- read_dta("data/ABV_Release_Data.dta") %>% 
  mutate(country = as.character(as_factor(country)),
         religion = as.character(tolower(as_factor(Q1012))),
         gender = as.character(tolower(as_factor(Q1002))),
         employment =as.character( as_factor(Q1005)),
         employed = as.character(as_factor(ifelse(employment %in% c("employed", "self-employed"), "Yes", "No"))),
         uni_imp_men = Q601_4,
         wave = 5) %>% 
  dplyr::select(wt, wave, country, religion, gender, employment, employed, uni_imp_men)
ABV_5[] <- lapply(ABV_5, unclass)

ABV_4 <- read_dta("data/ABIV_English.dta") %>% 
  mutate(country = as.character(as_factor(country)),
         religion = as.character(tolower(as_factor(q1012))),
         gender = as.character(tolower(as_factor(q1002))),
         employment = as.character(as_factor(q1005)),
         employed = as.character(as_factor(q1004)),
         uni_imp_men = q6014,
         wave = 4) %>% 
  dplyr::select(wt, wave, country, religion, gender, employment, employed, uni_imp_men)
ABV_4[] <- lapply(ABV_4, unclass)

ABV_3 <- read_dta("data/ABIII_English.dta") %>% 
  mutate(country = as.character(as_factor(country)),
         religion = as.character(tolower(as_factor(q1012))), # Jewish  (Yemen Only)
         gender = as.character(tolower( as_factor(q1002))),
         employment = as.character(as_factor(q1005)),
         employed = as.character(as_factor(q1004)),
         uni_imp_men = q6014,
         wave = 3) %>% 
  dplyr::select(wt, wave, country, religion, gender, employment, employed, uni_imp_men)
ABV_3[] <- lapply(ABV_3, unclass)

ABV_2 <- read_dta("data/ABII_English.dta") %>% 
  mutate(country = as.character(as_factor(country)),
         religion = as.character(tolower(as_factor(q1012))),
         gender = as.character(tolower(as_factor(q1002))),
         employment = as.character(as_factor(q1005)),
         employed = as.character(as_factor(q1004)),
         uni_imp_men = q60104,
         wave = 2) %>% 
  dplyr::select(wt, wave, country, religion, gender, employment, employed, uni_imp_men)
ABV_2[] <- lapply(ABV_2, unclass)

ABV_1 <- read_dta("data/ABI_English.dta") %>% 
  mutate(country =as.character(as_factor(country)),
         religion = as.character(tolower(as_factor(q711))),
         gender = as.character(tolower(as_factor(q702))),
         employed = as.character(as_factor(q704)),
         employment = as.character(as_factor(q705)),
         uni_imp_men = q5054,
         wt = NA,
         wave = 1) %>% 
  dplyr::select(wt, wave, country, religion, gender, employment, employed, uni_imp_men)
ABV_1[] <- lapply(ABV_1, unclass)

# bind arab barometers by row
AB <- rbind(ABV_5, ABV_4, ABV_3, ABV_2, ABV_1, stringsAsFactors = FALSE) %>% 
  mutate(uni_imp_men = ifelse(uni_imp_men  > 5, NA, uni_imp_men)) %>% 
  mutate(uni_imp_men = ifelse(uni_imp_men == 0, NA, uni_imp_men))

# fix vars with numbers and spaces
AB$gender <- gsub("[^a-zA-Z]", "", AB$gender)
AB$religion <- gsub("[^a-zA-Z]", "", AB$religion)
AB$employed <- tolower(gsub("[^a-zA-Z]", "", AB$employed))
AB$country <- gsub("\\.", "", AB$country)
AB$country <- gsub('[[:digit:]]+', '', AB$country)
AB$country <- tolower(trimws(AB$country))
AB <- AB %>% mutate(religion = ifelse(country %in% c("saudi arabia", "yemen"), "muslim", religion))
AB <- AB %>%  mutate(religion = case_when(
  religion %in% c("notclear", "notprovidednotusable", "refused", "other", "unspecificanswer", "khaki", "declinedtoanswer", "dontknow", "missing", "RA", NA) ~ "unusable",
  religion %in% c("shiitemuslimlebanonbahrain", "sunnimuslimlebanonbahrain", "druzelebanon") ~ "muslim",
  religion == "jewishyemenonly" ~ "jewish",
  TRUE ~ .$religion),
  religion = ifelse(country %in% c("Saudi Arabia", "Yemen"), "muslim", religion),
  employed = ifelse(employed %in% c("yes", "no"), employed, NA))

# load oil and gas data
rorbaek <- read_dta("data/rorbaek.dta") %>% 
  mutate(COUNTRY = tolower(COUNTRY))

# bind with AB 5
ABV_5 <- left_join(ABV_5, rorbaek %>% dplyr::select(COUNTRY, OILGAS) %>% rename("country" = "COUNTRY"))

# bind with joined AB
AB_rorbaek <- left_join(AB, rorbaek %>% dplyr::select(COUNTRY, OILGAS) %>% rename("country" = "COUNTRY"))

# unweighted barplots all waves
AB %>% 
  filter(gender == "female", #& uni_imp_men %notin% c("dontknow", "refused", "notclear", "cantchoosedontknow", "declinetoanswer", NA)
         uni_imp_men < 5 & uni_imp_men != 0) %>% 
  mutate(religion = case_when(
    religion %in% c("notclear", "notprovidednotusable", "refused", NA, "other", "unspecificanswer", "khaki", "declinedtoanswer", "dontknow", "missing") ~ "RA",
    religion %in% c("shiitemuslimlebanonbahrain", "sunnimuslimlebanonbahrain", "druzelebanon") ~ "muslim",
    religion == "jewishyemenonly" ~ "jewish",
    TRUE ~ .$religion)
    ) %>% 
  group_by(religion, uni_imp_men) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  ggplot(aes(x = as_factor(uni_imp_men), y = freq, fill = religion)) +
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  theme_bw() +
  labs(x = "University is More Important for Men than Women", y = "Percent")
  

# ABV_5$Q1012 # religion
# ABV_5$Q1002 # gender
# ABV_5$Q1005 # employment

# religion
# 5 = Q1012
# 4 = q1012
# 3 = q1012
# 2 = Q1012 (1 = Muslim, 2 = Christian, 3 = Other) (not asked in Yemen or Saudi Arabia)
# 1 = q711 (1 - Muslim, 2 = Christian, 3 = Sunni, 4 = Shiite, 5 = Druze, 97 = Not clear, 100 = not usable)

# gender
# 5 = Q1002
# 4 = q1002
# 3 = q1002
# 2 = q1002
# 1 = q702

# uni important
# 5 = Q601_4
# 4 = q601_4
# 3 = q601_4
# 2 = Q601_4
# 1 = q5054

sv_df <- AB_rorbaek %>% filter(!is.na(wt)) %>% as_survey_design(weights = wt) 

# religion and employment among women
sv_df %>% 
  filter(gender == "female" & !is.na(employed)) %>% 
  mutate(employed = as_factor(employed)) %>% 
  group_by(religion, employed) %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  ggplot(aes(x = religion, y = prop, ymax = prop_upp, ymin = prop_low, fill = employed)) +
  geom_col(position = position_dodge2(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  geom_linerange(position = position_dodge2(width = 0.9), size = 1) +
  geom_shadowtext(aes(y = prop/2, label = paste0(round(prop, 3)*100, "%", sep ="")), position = position_dodge2(width = 1)) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.8), legend.background = element_rect(colour = 1)) +
  labs(x = "Religion", y = "Percent")

# importance of university among women of diff religions  
sv_df %>% 
  filter(gender == "female",
         uni_imp_men < 5 & uni_imp_men != 0) %>% 
  # mutate(uni_imp_men = as_factor(uni_imp_men),
  #        uni_imp_men = case_when(
  #          uni_imp_men == 1 ~ "Strongly Disagree",
  #          uni_imp_men == 2 ~ "Disagree",
  #          uni_imp_men == 3 ~ "agree",
  #          uni_imp_men == 4 ~ "Strongly Disagree"
  #        )) %>% 
  group_by(religion, uni_imp_men) %>% 
  summarize(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  ggplot(aes(x = as_factor(uni_imp_men), y =prop, ymax = prop_upp, ymin = prop_low, fill = religion)) +
  geom_col(position = "dodge") + 
  geom_linerange(position = position_dodge2(width = 0.9), size = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  scale_x_discrete(labels = c("Strongly Agree", "Agree", "Disagree", "Strongly Disagree")) +
  theme_bw() +
  coord_cartesian(clip = "off") +
  ylim(0,1) +
  labs(x = "University is More Important for Men than Women", y = "Percent")

# muslim men vs women
sv_df %>% 
filter(religion == "muslim",
       uni_imp_men < 5 & uni_imp_men != 0) %>% 
  mutate(uni_imp_men = as_factor(uni_imp_men)) %>% 
  group_by(gender, uni_imp_men) %>% 
  summarise(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  ggplot(aes(x = uni_imp_men, y = prop, ymin = prop_low, ymax = prop_upp, fill = gender)) +
  geom_col(position = position_dodge2(width = 1)) + 
  geom_linerange(position = position_dodge2(width = 0.9), size = 1) +
  geom_shadowtext(aes(y = prop/2, label = paste0(round(prop, 3)*100, "%", sep ="")), position = position_dodge2(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  theme_bw()  +
  theme(legend.position = c(0.2, 0.7), legend.background = element_rect(colour = 1)) +
  labs(x = "University is More Important for Men than Women", y = "Percent")

# muslim men and women by country
sv_df %>% 
  filter(religion == "muslim",
         uni_imp_men < 5 & uni_imp_men != 0) %>% 
  # mutate(uni_imp_men = as_factor(uni_imp_men)) %>% 
  group_by(gender, country) %>% 
  summarise(prop = survey_mean(uni_imp_men/4, vartype = "ci", na.rm = T)) %>% 
  arrange(prop) %>% 
  mutate(country = forcats::fct_reorder(country, prop)) %>% 
  ggplot(aes(x = country, y = prop, ymin = prop_low, ymax = prop_upp, fill = gender)) +
  geom_col(position = position_dodge2(width = 1)) +
  geom_linerange(position = position_dodge2(width = 0.9), size = 1) +
  geom_shadowtext(aes(y = prop/2, label = round(prop, 2)), position = position_dodge2(width = 1), size = 3) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  theme_bw()  +
  coord_flip() +
  theme(legend.position = "none") +
  labs(y = "University is More Important for Men than Women", caption = "(0 = Agree, 1 = Disagree)", x = "Country")

rorbaek %>% 
  arrange(OILGAS) %>% 
  mutate(COUNTRY = forcats::fct_reorder(COUNTRY, OILGAS)) %>% 
  filter(!is.na(OILGAS) & COUNTRY %in% unique(AB$country)) %>% 
  ggplot(aes(x = COUNTRY, y = OILGAS)) +
  geom_col() +
  geom_shadowtext(aes(y = OILGAS/2, label = round(OILGAS, 2))) +
  coord_flip() +
  theme_bw() +
  labs(x = "Country", y = "Oil and Gas Rents per Capita")
  

sv_df5 <- ABV_5 %>% filter(!is.na(wt)) %>% as_survey_design(weights = wt)

sv_df5 <- sv_df5 %>% mutate(
  country = as_factor(country),
  female = as.factor(ifelse(Q1002 == 2, 1, 0)),
  gender = as_factor(Q1002),
  employment = as_factor(Q1005),
  religion = as_factor(Q1012),
  uni_imp_men = as_factor(Q601_4)
)

# women and men's employment
sv_df5 %>% 
  filter(employment %notin% c("retired", "other", "don't know", "refused", NA)) %>% 
  group_by(female, employment) %>% 
  summarize(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
  ggplot(aes(x = employment, y = prop, fill = female)) +
  geom_col(position = position_dodge(width = 1)) +
  geom_shadowtext(aes(y = prop/2, label = paste0(round(prop, 3)*100, "%", sep = "")), position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  labs(y = "Percent", x = "Employment") +
  theme_bw()

# women - religion -employment
# sv_df5 %>% 
#   filter(female == 1 & employment %notin% c("retired", "other", "don't know", "refused", NA) & !is.na(religion)) %>% 
#   group_by(religion, employment) %>% 
#   summarize(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
#   ggplot(aes(x = employment, y = prop, fill = religion)) +
#   geom_col(position = "dodge") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
#   labs(y = "Percent", x = "Employment") +
#   theme_bw()

# Women of diff religions (importance of uni for men)
# sv_df5 %>% 
#   filter(gender == "female" & uni_imp_men %notin% c("don't know", "refused", NA) & !is.na(religion)) %>%
#   group_by(religion, uni_imp_men) %>% 
#   summarize(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
#   ggplot(aes(x = uni_imp_men, y = prop, fill = religion)) +
#   geom_col(position = "dodge") + 
#   scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
#   theme_bw() +
#   labs(x = "University is More Important for Men than Women", y = "Percent")

# men of diff religions (importance of uni for men)
# sv_df5 %>% 
#   filter(gender == "male" & uni_imp_men %notin% c("don't know", "refused", NA) & !is.na(religion)) %>%
#   group_by(religion, uni_imp_men) %>% 
#   summarize(prop = survey_mean(vartype = "ci", na.rm = T)) %>% 
#   ggplot(aes(x = uni_imp_men, y = prop, fill = religion)) +
#   geom_col(position = "dodge") +
#   scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 5L)) +
#   theme_bw() +
#   labs(x = "University is More Important for Men than Women", y = "Percent")

# Muslim men vs women (importance of uni for men)
# sv_df5 %>%
#   filter(uni_imp_men %notin% c("don't know", "refused", NA) & religion == "muslim") %>%
#   group_by(gender, uni_imp_men) %>%
#   summarize(prop = survey_mean(vartype = "ci", na.rm = T)) %>%
#   ggplot(aes(x = uni_imp_men, y = prop, fill = gender)) +
#   geom_col(position = "dodge") +
#   scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 5L)) +
#   theme_bw() +
#   labs(x = "University is More Important for Men than Women", y = "Percent")

# some regressions
sv_df5_women <- sv_df5 %>% filter(female == 1)

sv_df_women <- sv_df %>% filter(gender == "female")

svyglm(as.numeric(uni_imp_men) ~ religion + employed + OILGAS + as_factor(country), design = sv_df_women) %>% screenreg()

sv_df_women$variables

svyglm(as.numeric(uni_imp_men) ~ religion + country, design = sv_df5_women) %>% screenreg()

ord1 <- MASS::polr(factor(uni_imp_men) ~ religion + gender + OILGAS + employed, data = sv_df, weights = wt, Hess = TRUE)

m_sv_1 <- svyolr(factor(uni_imp_men) ~ religion + gender + employed + OILGAS + country, design = sv_df, na.action = na.omit)

sv_df <- sv_df %>% mutate(uni_imp_men = factor(uni_imp_men))
m_sv_2 <- svyolr(uni_imp_men ~ religion + gender + OILGAS + employed, design = sv_df, na.action = na.omit)

tidy_1 <- tidy(m_sv_1, conf.int = T)
class(m_sv_1) <- 'polr'
class(m_sv_2) <- 'polr'

View(sv_df$variables)


fake <- expand.grid(religion = c("christian", "muslim"),
                    gender = c("male", "female"),
                    OILGAS = seq(0, 20, 1),
                    employed = c("yes", "no"),
                    country = "jordan")

fake_2 <- expand.grid(religion = c("christian", "muslim"),
                      gender = c("male", "female"),
                      OILGAS = seq(0, 20, 1),
                      employed = c("yes", "no"))

preds <- cbind(fake_2, predict(m_sv_2, fake_2, type = "p", interval = "confidence"))

muslim_women <- 
  data.frame(glm.predict::basepredict.polr(ord1, values = c(0, 0, 1, 0, 0, 5, 0))) %>% 
  mutate(religion = "Muslim",
         response = 1:4)# muslim women
christian_women <- 
  data.frame(glm.predict::basepredict.polr(ord1, values = c(1, 0, 0, 0, 0, 5, 0))) %>% 
  mutate(religion = "Christian",
         response = 1:4)# christian women

women_preds <- rbind(muslim_women, christian_women)

# changes in religion
women_preds %>% 
  ggplot(aes(x = religion, y = mean, ymax = X97.5., ymin = X2.5., fill = factor(response))) +
  geom_col(position = "dodge") +
  geom_linerange(size = 1, position = position_dodge(width = 0.9)) +
  geom_shadowtext(aes(y = mean/2, label = round(mean, 2)), position = position_dodge(width = 0.9), size = 3.5) +
  theme_bw() +
  labs(x = "Religion", y = "Probability") +
  theme_bw() +
  scale_fill_discrete(name = "University is More\nImportant for Men", labels = c("Strongly Agree", "Agree", "Disagree", "Strongly Disagree"))

# OILGAS
oilgas_preds <- list()
for(i in 1:20) {
oilgas_preds[[i]] <-  data.frame(glm.predict::basepredict.polr(ord1, values = c(0, 0, 1, 0, 0, i, 0))) %>% 
  mutate(oilgas = i,
         response = 1:4)
}

bind_rows(oilgas_preds) %>% 
  ggplot(aes(x = oilgas, y = mean, ymax = X97.5., ymin = X2.5., color = factor(response))) +
  geom_line() +
  geom_ribbon(alpha = 0.4) +
  theme_bw() +
  labs(x = "Oil and Gas Rents per Capita", y = "Probability") +
  theme_bw() +
  scale_color_discrete(name = "University is More\nImportant for Men", labels = c("Strongly Agree", "Agree", "Disagree", "Strongly Disagree"))



preds_long <- 
preds %>% tidyr::pivot_longer(cols = -c(religion:employed),
                              names_to = "level",
                              values_to = "probs")

# changes in oilgas
preds_long %>% 
  filter(religion == "muslim" & gender == "female" & employed == "no") %>% 
  ggplot(aes(x = OILGAS, y = probs, colour = factor(level)))+
  geom_line(size = 1) +
  theme_bw() +
  labs(x = "Oil and Gas Rents per Capita", y = "Probability") +
  scale_color_discrete(name = "University More is\nImportant for Men", labels = c("Strongly Agree", "Agree", "Disagree", "Strongly Disagree"))

# changes in religion
preds_long %>% 
  filter(gender == "female", OILGAS == 5, employed == "no") %>% 
  ggplot(aes(x = religion, y = probs, fill = factor(level))) +
  geom_col(position = "dodge") +
  geom_shadowtext(aes(y = probs/2, label = round(probs, 3)), position = position_dodge(width = 0.9)) +
  theme_bw() +
  labs(x = "Religion", y = "Probability") +
  theme_bw() +
  scale_fill_discrete(name = "University is More\nImportant for Men", labels = c("Strongly Agree", "Agree", "Disagree", "Strongly Disagree"))

# changes in employment
preds_long %>% 
  filter(gender == "female", OILGAS == 5, religion == "muslim") %>% 
  ggplot(aes(x = employed, y = probs, fill = factor(level))) +
  geom_col(position = "dodge") +
  theme_bw() +
  labs(x = "Oil and Gas Rents per Capita", y = "Probability") +
  theme_bw() +
  scale_fill_discrete(name = "University is More\nImportant for Men", labels = c("Strongly Agree", "Agree", "Disagree", "Strongly Disagree"))

# changes in gender
preds_long %>% 
  filter(employed == "yes", OILGAS == 5, religion == "muslim") %>% 
  ggplot(aes(x = gender, y = probs, fill = factor(level))) +
  geom_col(position = "dodge") +
  geom_shadowtext(aes(y = probs/2, label = round(probs, 3)), position = position_dodge(width = 0.9)) +
  theme_bw() +
  labs(x = "Gender", y = "Probability") +
  theme_bw() +
  scale_fill_discrete(name = "University is More\nImportant for Men", labels = c("Strongly Agree", "Agree", "Disagree", "Strongly Disagree"))



View(predict(m_sv_1, type = "p"))

# as_factor(sv_df$variables$uni_imp_men)

summary(ord1)
screenreg(ord1)
export_summs(ord1)

2*pt(-abs(1.1723), df = nrow(sv_df5_women$variables)-672)

summary(ord1)$tstatistic


tidy_ord1 <- tidy(ord1)

2*pt(-abs(tidy_ord1$statistic), df = nrow(sv_df5_women$variables)-672)


