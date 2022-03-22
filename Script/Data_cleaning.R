# Initial Setting
library(haven)
library(tidyverse)
library(tidytext)
library(Hmisc)
dataset <- read_sav("ESS9e03_1 2.sav")
view(dataset)
view(colnames(dataset))
complementary_dataset_unemployment <- read_csv("unemployment_rate.csv")
view(complementary_dataset_unemployment)

# [Discussion] Datasets for 4 countries
discussion_dataset <- tibble("cntry" = c("DE", "FR", "IT", "PL"),
                             "EU accession(%)" = c(47.0, 42.6, 61.1, 69.5),
                             "Nato accession(%)" = c(31.3, 39.9, 31.3, 49.6))

# Dataset for material variables
dataset_material <- dataset |>
  select(cntry, 
         uemp3m, uemp12m, uemp5yr, 
         uemplap, uemplip, pdwrkp) |>
  group_by(cntry) |>
  summarize("unemployment_3m(%)" = length(which(uemp3m==1))/length(which(uemp3m %in% c(1,2)))*100,
            "unemployment_12m(%)" = length(which(uemp12m==1))/length(which(uemp12m %in% c(1,2)))*100,
            "unemployment_5yr(%)" = length(which(uemp5yr==1))/length(which(uemp5yr %in% c(1,2)))*100,
            "unemployment_partner_actively(%)" = length(which(uemplap==1))/length(which(uemplap %in% c(0,1)))*100,
            "unemployment_partner_nactively(%)" = length(which(uemplip==1))/length(which(uemplip %in% c(0,1)))*100,
            "partner_paid_job(%)" = length(which(pdwrkp==1))/length(which(pdwrkp %in% c(0,1)))*100) |>
  arrange(`unemployment_3m(%)`)

# [Results] Tables for material variables
knitr::kable(head(dataset_material |>
                    select(`unemployment_3m(%)`, `unemployment_12m(%)`)|>
                    arrange(`unemployment_3m(%)`)))
knitr::kable(head(dataset_material |>
                    select(`unemployment_5yr(%)`)|>
                    arrange(`unemployment_5yr(%)`)))
knitr::kable(head(dataset_material |>
                    select(`partner_paid_job(%)`,
                           `unemployment_partner_actively(%)`,
                           `unemployment_partner_nactively(%)`)|>
                    arrange(desc(`partner_paid_job(%)`))))

# [Dicussions] Complementary tables for unemployment rate
complementary_dataset_unemployment_cleaned <- complementary_dataset_unemployment |>
  filter(`Country Name` %in% c("France", "Germany", "Italy", "Poland")) |>
  select(`Country Name`, `2018`) |>
  arrange(`2018`)

knitr::kable(complementary_dataset_unemployment_cleaned,
             col.names = c("Country", "Unemployment rate (2018, %)"))


# Dataset for non material variables
# 1. Social trust
dataset_nm_social_trust <- dataset|>
  select(cntry, 
         ppltrst, ipudrst, iplylfr) |>
  filter(ppltrst %in% c(0:10),
         ipudrst %in% c(1:6), iplylfr %in% c(1:6)) |>
  group_by(cntry) |>
  summarize(people_trust = mean(ppltrst),
            understand_others = mean(ipudrst),
            loyal_to_friends = mean(iplylfr))

# [Results] Tables for social trust
dataset_nm_social_trust |>
  arrange(desc(people_trust))
dataset_nm_social_trust |>
  arrange(understand_others)
dataset_nm_social_trust |>
  arrange(loyal_to_friends)
# By and large, the top countries which have high people trust scores also showed the high scores
# in understanding others, and loyal to friends.
# Especially, Denmark, Finland, Iceland, Switzerland, Ireland were consistently sorted out as top 10
# for the three variables.
dataset_nm_social_trust |>
  arrange(desc(people_trust))|>
  mutate(color = case_when(
    `cntry` %in% c("DK", "FI", "IS", "CH", "IE") ~ "Yes",
    TRUE ~ "No"
  )) |>
  mutate(color = as.factor(color))|>
  ggplot(aes(reorder(cntry, -people_trust, sum), y = people_trust, fill = color)) +
  geom_col() +
  scale_fill_discrete(name = "Top 10 in \nunderstanding others, \nloyal to friends",
                      breaks = c("Yes", "No"),
                      labels = c("Yes", "No")) +
  labs(x = "country code", y = "people trust") 

# [Discussions]
dataset_nm_social_trust_cleaned <- 
  dataset_nm_social_trust |>
  filter(cntry %in% c("FR", "DE", "IT", "PL")) |>
  pivot_longer(cols = c("people_trust","understand_others", "loyal_to_friends"),
               names_to = "social_trust_category",
               values_to = "values") |>
  mutate(social_trust_category = as.factor(social_trust_category),
         cntry = reorder_within(cntry, values, social_trust_category))

dataset_nm_social_trust_cleaned |>
  ggplot(aes(reorder(cntry, -values), values, fill = social_trust_category)) +
  scale_x_reordered()+
  facet_wrap(~social_trust_category, scale = "free_x") +
  geom_col(show.legend = FALSE) +
  labs(x = "Country", y = "Score")
# People Trust: highest=> Germany. Loyal to friends, understanding others
# => for Germany also took the first place. How would it be related to
# the public opinion in Ukraine's EU accession?

# 2. Fairness & Justice
dataset_nm_fairness_justice <- dataset |>
  select(cntry, 
         pplfair, jstprev, pcmpinj, sofrdst, sofrwrk, sofrpr, sofrprv, wltdffr) |>
  filter(pplfair %in% c(0:10), sofrdst %in% c(1:5), sofrwrk %in% c(1:5), sofrpr %in% c(1:5), sofrprv %in% c(1:5), wltdffr %in% c(-4:4), 
         jstprev %in% c(1:5), pcmpinj %in% c(1:5)) |>
  group_by(cntry) |>
  summarize(people_fair = mean(pplfair),
            justice_prevail = mean(jstprev),
            people_compensate_injustice = mean(pcmpinj),
            fair_when_equally_distributed = mean(sofrdst),
            fair_when_hardworking_earn_money = mean(sofrwrk),
            fair_when_takingcare_poor = mean(sofrpr),
            fair_when_highsocial_previlege = mean(sofrprv),
            wealth_difference_fair = mean(wltdffr))

# [Results] Figure for fairness and justice score
dataset_nm_fairness_justice_normalized <- dataset_nm_fairness_justice |>
  select(cntry, people_fair, justice_prevail, fair_when_takingcare_poor)|>
  mutate(justice_prevail = 5-justice_prevail,
         fair_when_takingcare_poor = 5-fair_when_takingcare_poor) |>
  summarize(country = cntry,
            fairness_justice_score = people_fair + justice_prevail + fair_when_takingcare_poor) |>
  arrange(desc(fairness_justice_score)) 

dataset_nm_fairness_justice_normalized|>
  slice(n = c(1:5, 25:29 ))|>
  mutate(top_bottom = case_when(
    fairness_justice_score >10 ~ "Top",
    fairness_justice_score <= 10 ~ "Bottom"
  )) |>
  ggplot(aes(x = reorder(country, -fairness_justice_score), y = fairness_justice_score, fill = top_bottom)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Country", y = "Fairness and Justice Score",
       title = "Fairness and Justice Score by country",
       subtitle = "Top 5 and Bottom 5")

# 3. Equality
dataset_nm_equality_normalized <- dataset |>
  select(cntry,
         ipeqopt) |>
  filter(ipeqopt %in% c(1:6)) |>
  group_by(cntry) |>
  summarize(equality_score = 6-mean(ipeqopt))

colnames(dataset_nm_equality_normalized) <- c("country", "equality_score")


# [Result] Figure for Equality score
dataset_nm_equality_normalized |>
  arrange(desc(equality_score)) |>
  slice(n = c(1:5, 25:29)) |>
  mutate(top_bottom = case_when(
    equality_score > 4 ~ "Top",
    equality_score <= 4 ~ "Bottom"
  )) |>
  ggplot(aes(x = reorder(cntry, -equality_score), y = equality_score, fill = top_bottom)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Country", y = "Equality Score",
       title = "Equality and Justice Score by country",
       subtitle = "Top 5 and Bottom 5")
  
# [Discussions] Table for fairness and justice score & Equality
a <- dataset_nm_fairness_justice_normalized |>
  filter(country %in% c("DE", "FR", "PL", "IT"))
  
b <-dataset_nm_equality_normalized |>
    filter(country %in% c("FR", "DE", "PL", "IT"))

dataset_nm_fairness_justice_equality_normalized <- merge(a,b, by = "country")
dataset_nm_fairness_justice_equality_normalized |>
  knitr::kable(col.names = c("Country", "Fairness and Justice Score", "Equality Score"),
               digits = 2)

# 4. Discrimination
dataset_nm_discrimination <- dataset |>
  select(cntry,
         dscrgrp) |>
  filter(dscrgrp %in% c(1,2)) |>
  group_by(cntry) |>
  summarize(`discrimination_rate(%)` = length(which(dscrgrp==1))/length(which(dscrgrp %in% c(1,2)))*100) |>
  arrange(desc(`discrimination_rate(%)`))

# [Result 1]
dataset_nm_discrimination |>
  slice(c(1:5)) |>
  knitr::kable(digits = 2, col.names = c("Country", "Discrimination rate(%)"))

# [Discussion 1]
median_value <- median(dataset_nm_discrimination $`discrimination_rate(%)`)
dataset_nm_discrimination |>
  filter(cntry %in% c("FR", "PL", "DE", "IT")) |>
  ggplot(aes(x = reorder(cntry, -`discrimination_rate(%)`), y = `discrimination_rate(%)`)) +
  geom_col() +
  geom_hline(yintercept = median_value) +
  geom_text(aes(x = 4, y = median_value + 0.5, label = "Median"),
            size = 3) +
  labs(x = "Country", y = "Discrimination rate(%)",
       title = "Discrimination rates by country")
  

# 4-1. Discrimination type
colnames(dataset_nm_discrimination_type)
dataset_nm_discrimination_type <- dataset |>
  select(cntry,
         dscrgrp,
         dscrrce, dscrntn, dscrrlg, dscrlng, dscretn, dscrage, dscrgnd,
         dscrsex, dscrdsb, dscroth, dscrdk, dscrref, dscrnap, dscrna) |>
  filter(dscrgrp ==1) |>
  group_by(cntry) |>
  summarize("race" = length(which(dscrrce == 1))/length(dscrgrp)*100,
            "nationality" = length(which(dscrntn == 1))/length(dscrgrp)*100,
            "religion" = length(which(dscrrlg == 1))/length(dscrgrp)*100,
            "language" = length(which(dscrlng == 1))/length(dscrgrp)*100,
            "ethnic_group" = length(which(dscretn == 1))/length(dscrgrp)*100,
            "age"= length(which(dscrage == 1))/length(dscrgrp)*100,
            "gender"= length(which(dscrgnd == 1))/length(dscrgrp)*100, 
            "sexuality"= length(which(dscrsex == 1))/length(dscrgrp)*100,
            "disability"= length(which(dscrdsb == 1))/length(dscrgrp)*100 
            ) |>
  pivot_longer(cols = c(`race`, `nationality`, `religion`, `language`,
                        `ethnic_group`,`age`, `gender`, `sexuality`,
                        `disability`),
               names_to = "type_of_discrimination",
               values_to = "rate(%)")

# [Result 2]
dataset_nm_discrimination_type |>
  filter(cntry %in% c("IS", "GB", "ME", "FR", "LV")) |>
  ggplot(aes(x = reorder_within(cntry, -`rate(%)`, type_of_discrimination), 
             y = `rate(%)`, fill = type_of_discrimination)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~type_of_discrimination, scale = "free") +
  labs(x = "Country", y = "Discrimination rate(%)",
       title = "Discrimination rates by category",
       subtitle = "For top 5 countries")

# [Discussion]
dataset_nm_discrimination_type |>
  group_by(type_of_discrimination) |>
  mutate(type_median = median(`rate(%)`)) |>
  filter(cntry %in% c("FR", "IT", "DE", "PL")) |>
  ggplot(aes(x = reorder_within(cntry, -`rate(%)`, type_of_discrimination),
             y = `rate(%)`, fill = type_of_discrimination)) +
  geom_col(show.legend = FALSE) +
  geom_hline(aes(yintercept=type_median))+
  scale_x_reordered() +
  facet_wrap(~type_of_discrimination, scale = "free_x") +
  labs(x = "Country", y = "Discrimination rate(%)",
       title = "Discrimination rates by category")


# 4. Human dignity
dataset_nm_human_dignity <- dataset |>
  select(cntry, 
         impsafe, ppldsrv, pplhlp, iphlppl) |>
  filter(impsafe %in% c(1:6),
         ppldsrv %in% c(1:5),
         pplhlp %in% c(0:10),
         iphlppl %in% c(1:6)) |>
  group_by(cntry)|>
  summarize(important_living_secure_safe = mean(impsafe),
            ppl_get_deserve = mean(ppldsrv),
            ppl_help = mean(pplhlp),
            important_to_help_ppl = mean(iphlppl))

# [Result]
dataset_nm_human_dignity_normalized <- dataset_nm_human_dignity |>
  mutate(important_living_secure_safe = 6 - important_living_secure_safe,
         ppl_get_deserve = 5 - ppl_get_deserve,
         ppl_help = ppl_help,
         important_to_help_ppl = 6 - important_to_help_ppl
         ) |>
  summarize(country = cntry,
            human_dignity_score = 
              important_living_secure_safe + ppl_get_deserve + ppl_help + important_to_help_ppl) |>
  arrange(desc(human_dignity_score))

dataset_nm_human_dignity_normalized |>
  slice(c(1:5, 25:29)) |>
  mutate(top_bottom = case_when(
    human_dignity_score >=14 ~ "Top",
    human_dignity_score < 14 ~ "Bottom"
  )) |>
  ggplot(aes(x = reorder(country, -human_dignity_score), y = human_dignity_score, fill = top_bottom)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Country", y = "Human Dignity Score",
       title = "Human Dignity Score by Country",
       subtitle = "Top 5 and Bottom 5 countries")

# [Discussion]
dataset_nm_human_dignity_normalized |>
  filter(country %in% c("IT", "FR", "PL", "DE")) |>
  knitr::kable(col.names = c("Country", "Human Dignity Score"),
               digits = 2)

# 5. Interest in politics
dataset_nm_political_interest_normalized <- dataset |>
  select(cntry, 
         polintr) |>
  filter(polintr %in% c(1:4)) |>
  group_by(cntry) |>
  summarize(interest_in_politics_score = 4- mean(polintr)) |>
  arrange(desc(interest_in_politics_score))

# [Result]
dataset_nm_political_interest_normalized |>
  slice(c(1:5)) |>
  knitr::kable(col.names = c("Country", "Interest Score"),
               title = "Political Interest",
               subtitle = "Top 5 Countries",
               digits = 2)
dataset_nm_political_interest_normalized |>
  slice(c(25:29)) |>
  knitr::kable(col.names = c("Country", "Interest Score"),
               title = "Political Interest",
               subtitle = "Bottom 5 Countries",
               digits = 2)

# [Discussion]
dataset_nm_political_interest_normalized |>
  filter(cntry %in% c("PL", "IT", "FR", "DE")) |>
  knitr::kable(col.names = c("Country", "Interest Score"),
               title = "Political Interest",
               digits = 2)
  

# 6. Media
dataset_nm_media <- dataset |>
  select(cntry,
         nwspol, pstplonl) |>
  filter(nwspol %in% c(0: 7776),
         pstplonl %in% c(1,2)) |>
  group_by(cntry) |>
  summarize(news_minutes = mean(nwspol),
            "post_share_politics(%)" = length(which(pstplonl==1))/length(which(pstplonl %in% c(1,2)))*100) |>
  arrange(desc(`post_share_politics(%)`)) |>
  mutate(top_10 = case_when(
    `post_share_politics(%)` >19 ~ "Yes",
    `post_share_politics(%)` <=19 ~"No"
  ))


dataset_nm_media |>
  ggplot(aes(x = reorder(cntry, -news_minutes), y = news_minutes, fill = top_10)) +
  geom_col()

# [Result 1]
dataset_nm_media
cor.test(dataset_nm_media$news_minutes, dataset_nm_media$`post_share_politics(%)`,
         method = "pearson")
# The p-value is over 0.05, and it is not significantly correlated,
# Which means it is hard to say that there is a correlation between
# the minutes of watching news and the rates of sharing political posts.

# [Result 2]
dt <- merge(dataset_nm_political_interest_normalized, dataset_nm_media, by = "cntry") |>
  select(-top_10)

correlation_table_politics_media <- dt |>
  select(-cntry) |>
  cor(method = "pearson")

correlation_table_politics_media|>
  knitr::kable()
# There was a positive correlation between interest in politics and the rates of sharing
# political posts The correlation was significant comparing to different variables.
# Therefore, we can induce if a country which has high interest in politic issue by and large,
# the rates of sharing political posts would be higher as well.

# [Discussion]
dataset_nm_media_cleaned <- dataset_nm_media |>
  filter(cntry %in% c("DE", "FR", "IT", "PL")) |>
  select(-top_10) |>
  arrange(desc(news_minutes))

dataset_nm_media_merged <- merge(dataset_nm_media_cleaned, discussion_dataset,
                                 by = "cntry") |>
  select(-cntry, -`Nato accession(%)`)

correlation_table_media_eu <- dataset_nm_media_merged |>
  cor(method = "pearson")

# 7. Attitudes on immigrants
dataset_nm_attitudes_immigrants <- dataset |>
  select(cntry,
         imsmetn, imdfetn, impcntr, imbgeco, imueclt, imwbcnt) |>
  filter(imsmetn %in% c(1:4), imdfetn %in% c(1:4), impcntr %in% c(1:4),
         imbgeco %in% c(0:10), imueclt %in% c(0:10), imwbcnt %in% c(0:10)) |>
  group_by(cntry) |>
  summarize(allow_im_same_group = mean(imsmetn),
            allow_im_different_group = mean(imdfetn),
            allow_im_poor_outside_Europe = mean(impcntr),
            im_for_economy = mean(imbgeco),
            im_for_culture = mean(imueclt),
            im_for_country = mean(imwbcnt))

# [Result]
v1 <- 10- mean(dataset_nm_attitudes_immigrants$allow_im_same_group)
v2 <- 10- mean(dataset_nm_attitudes_immigrants$allow_im_different_group)
v3 <- 10- mean(dataset_nm_attitudes_immigrants$allow_im_poor_outside_Europe)

dataset_nm_attitudes_immigrants_result <- tibble("Allow Immigrants from Same Group" = v1,
       "Allow Immigrants from Different Group" = v2,
       "Allow Immigrants from Poor Countries Outside Europe" = v3)


# 8-3. [GB]
dataset_nm_affiliation_europe_GB <- dataset |>
  select(cntry,
         atcherp,vteumbgb,euftf) |>
  filter(cntry=="GB")|>
  group_by(cntry) |>
  filter(atcherp %in% c(0:10),
         vteumbgb %in% c(1,2),
         euftf %in% c(0:10)) |>
  group_by(cntry)|>
  summarize("emotionally_attached_Europe(%)" = length(which(atcherp==1))/length(which(atcherp %in% c(1,2)))*100,
            "vote_for remain_EU(%)" = length(which(vteumbgb==1))/length(which(vteumbgb %in% c(1,2)))*100,
            EU_should_go_further = mean(euftf))

# Datasets by country
# France
dataset_material_fr <- dataset_material |>
  filter(cntry == "FR")
dataset_non_material_fr <- dataset_non_material |>
  filter(cntry == "FR") |>
  select(-vteubcmb)|>
  na.omit()
summary(dataset_non_material_fr)

# [FR} material indicator
datset_material_indicator_fr <- dataset_material_fr |>
  # respondent's unemployment status
  filter(uemp3m %in% c(1,2)) |>
  filter(uemp12m %in% c(1,2)) |>
  filter(uemp5yr %in% c(1,2)) |>
  # respondent's partner's unemployment status
  group_by(cntry) |>
  summarize(unemployment_indicator = mean(mean(uemp3m)+mean(uemp12m)+mean(uemp5yr)),
            partner_unemployment_indicator = mean(mean(uemplap),mean(uemplip),mean(pdwrkp))
  )
            

# Germany
dataset_material_de <- dataset_material |>
  filter(cntry == "DE")
dataset_non_material_de <- dataset_non_material |>
  filter(cntry == "DE") |>
  select(-vteubcmb)|>
  na.omit()

# [DE] material indicator
datset_material_indicator_de <- dataset_material_de |>
  # respondent's unemployment status
  filter(uemp3m %in% c(1,2)) |>
  filter(uemp12m %in% c(1,2)) |>
  filter(uemp5yr %in% c(1,2)) |>
  # respondent's partner's unemployment status
  group_by(cntry) |>
  summarize(unemployment_indicator = mean(mean(uemp3m)+mean(uemp12m)+mean(uemp5yr)),
            partner_unemployment_indicator = mean(mean(uemplap),mean(uemplip),mean(pdwrkp))
  )

# Italy
dataset_material_it <- dataset_material |>
  filter(cntry == "IT")
dataset_non_material_it <- dataset_non_material |>
  filter(cntry == "IT")|>
  select(-vteubcmb) |>
  na.omit()

# [IT] material_indicator
datset_material_indicator_it <- dataset_material_it |>
  # respondent's unemployment status
  filter(uemp3m %in% c(1,2)) |>
  filter(uemp12m %in% c(1,2)) |>
  filter(uemp5yr %in% c(1,2)) |>
  # respondent's partner's unemployment status
  group_by(cntry) |>
  summarize(unemployment_indicator = mean(mean(uemp3m)+mean(uemp12m)+mean(uemp5yr)),
            partner_unemployment_indicator = mean(mean(uemplap),mean(uemplip),mean(pdwrkp))
  )

# Poland
dataset_material_pl <- dataset_material |>
  filter(cntry == "PL")
dataset_non_material_pl <- dataset_non_material |>
  filter(cntry == "PL") |>
  select(-vteubcmb) |>
  na.omit()

# [PL] material indicator
datset_material_indicator_pl <- dataset_material_pl |>
  # respondent's unemployment status
  filter(uemp3m %in% c(1,2)) |>
  filter(uemp12m %in% c(1,2)) |>
  filter(uemp5yr %in% c(1,2)) |>
  # respondent's partner's unemployment status
  group_by(cntry) |>
  summarize(unemployment_indicator = mean(mean(uemp3m)+mean(uemp12m)+mean(uemp5yr)),
            partner_unemployment_indicator = mean(mean(uemplap),mean(uemplip),mean(pdwrkp))
  )

# Scores for material/non_material factors
