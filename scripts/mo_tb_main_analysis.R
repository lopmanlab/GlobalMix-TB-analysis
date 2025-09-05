# TB-analysis script - Mozambique

# Read data ----------------------------------------------------------------------
mo_locdata <- readRDS("./Mozambique/moz_locations_visited_data_aim1.RDS")

mo.pa <- readRDS("./Mozambique/moz_participant_data_aim1.RDS")
mo.co <- readRDS("./Mozambique/moz_contact_data_aim1.RDS")
mo.we <- read.csv("./Other/moz_pop.csv")

stringency <- read.csv("./Other/OxCGRT_compact_national_v1.csv", header = T)
prem <- read.csv("./Other/synthetic_contacts_2021.csv", header = T)

# Clean the data -----------------------------------------------------------------
# Standardize the data across countries and merge participants information
mo_loc_ed <- mo_locdata%>%
  mutate(place_visited = case_when(place_visited == "Agricultural Field" ~ "Work",
                                   place_visited == "Market/Shop" ~ "Market/essential",
                                   place_visited == "My home" ~ "Home",
                                   place_visited == "Other" ~ "Other",
                                   place_visited == "Other home" ~ "Other home",
                                   place_visited == "Place of worship" ~ "Worship",
                                   place_visited == "Playground" ~ "Other social/leisure",
                                   place_visited == "School" ~ "School",
                                   place_visited == "Street" ~ "Other social/leisure",
                                   place_visited == "Transport/Hub" ~ "Transit",
                                   place_visited == "Well" ~ "Market/essential",
                                   place_visited == "Work" ~ "Work",
                                   TRUE ~ "Unreported"),
         place_visited = factor(place_visited, levels = c("Home", "Other home", "Market/essential", 
                                                          "Other social/leisure", "School", "Transit","Work", "Worship")))%>%
  mutate(num_pax_place = case_when(num_pax_place == ",8" ~ "8",
                                   num_pax_place == "+  50" ~ "50",
                                   num_pax_place == "+ 50" ~ "50",
                                   num_pax_place == "1 NA RUA" ~ "1",
                                   num_pax_place == "1 OUTRA CASA" ~ "1",
                                   num_pax_place == "1+" ~ "1",
                                   num_pax_place == "1i" ~ "1",
                                   num_pax_place == "1r" ~ "1",
                                   num_pax_place == "2 NA RUA" ~ "2",
                                   num_pax_place == "MAIS DE 20" ~ "20",
                                   TRUE ~ num_pax_place),
         num_pax_place = as.numeric(num_pax_place),
         num_pax_place = case_when(num_pax_place >= 50 ~ 50,
                                   TRUE ~ num_pax_place),
         spent_time = case_when(time_visited == "<5 mins" ~ 2.5,
                                time_visited == "5-15 mins" ~ 10,
                                time_visited == "16-30 mins" ~ 23,
                                time_visited == "31 mins-1 hr" ~ 45 ,
                                time_visited == "1-4 hrs" ~ 150,
                                time_visited == ">4 hrs" ~ 240),
         rec_id = as.factor(rec_id))%>%
  left_join(mo.pa, by = "rec_id")

## Modify data for analysis ----
# Merge the <5y group
mo.pa.age <- mo.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))
mo.co.age <- mo.co%>%
  mutate(contact_age = case_when(contact_age == "<6mo" ~ "<5y",
                                 contact_age == "6-11mo" ~ "<5y",
                                 contact_age == "1-4y" ~ "<5y",
                                 TRUE ~ contact_age))

mo.co.pa.counts <-  full_join(mo.co.age, mo.pa.age, 
                              by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))

## Calculate number of participants by age and sex ----
mo.pa.age %>%
  filter(!is.na(participant_age)&!is.na(participant_sex))%>%
  count(participant_age, participant_sex) -> denoms.byagesex.mo

# Figure 1: Exposure-hours by type of contact -----------------------------------------------
mo.ind.type <- mo.co%>%
  group_by(rec_id, study_day, hh_membership)%>%
  summarise(cont_time = sum(cont_time))%>%
  mutate(hh_membership = case_when(hh_membership == "Member" ~ "Close household",
                                   hh_membership == "Non-member" ~ "Close non-household"))

mo.loc.type <- mo_loc_ed%>%
  filter(place_visited != "Home")%>%
  mutate(cont_time = spent_time*num_pax_place)%>%
  group_by(rec_id, study_day)%>%
  summarise(cont_time = sum(cont_time))%>%
  mutate(hh_membership = "Casual")

mo.type.value <- rbind(mo.ind.type, mo.loc.type)%>%
  filter(!is.na(hh_membership))%>%
  mutate(ex_hour = cont_time/60)%>% # make minutes from hour
  group_by(hh_membership)%>%
  summarise(mean_eh = mean(ex_hour, na.rm = T),
            sd_eh = sd(ex_hour, na.rm = TRUE),
            n = sum(!is.na(ex_hour)),
            se = sd_eh / sqrt(n),
            lower_ci = mean_eh - 1.96 * se,
            upper_ci = mean_eh + 1.96 * se
  )%>%
  mutate(country = "Mozambique")

mo.type.value.med <- rbind(mo.ind.type, mo.loc.type)%>%
  filter(!is.na(hh_membership))%>%
  mutate(ex_hour = cont_time/60)%>% # make minutes from hour
  group_by(hh_membership)%>%
  summarise(median_eh = median(ex_hour, na.rm = T),
            lower_percentile = quantile(ex_hour, probs = 0.25, na.rm = T),
            upper_percentile = quantile(ex_hour, probs = 0.75, na.rm = T))%>%
  mutate(country = "Mozambique")

## Summary figure is in the summary script

# Figure 2 and supp figure 1: Exposure profiles by age and sex --------------------------------------
## Calculate age and sex distribution ----
mo.pa.we <- mo.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(study_site, participant_age)%>%
  summarise(n_s = n())%>%
  mutate(prop_s = n_s/sum(n_s))

mo.we.ru <- mo.we%>%
  left_join(mo.pa.we, by = c("participant_age", "study_site"))%>%
  mutate(psweight = prop/prop_s)

## Create a framework for location dataset ----
mo.loc_frame2 <- mo_loc_ed%>%
  filter(!is.na(participant_sex))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(place_visited, participant_age, participant_sex, study_site)%>%
  summarise(count = n())%>%
  ungroup()%>%
  tidyr::expand(participant_age, participant_age, participant_sex, participant_sex, place_visited, study_site) %>%
  setNames(c("participant_age", "contact_age", "participant_sex", "contact_sex","place_visited", "study_site"))%>%
  filter(!is.na(place_visited))%>%
  filter(place_visited != "Home")


mo.pa.ext <- mo.pa%>%
  select(rec_id, study_site, participant_age, participant_sex)%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))

mo.loc_frame_p <- mo.loc_frame2%>%
  filter(!is.na(contact_age) & !is.na(contact_sex))%>%
  right_join(mo.pa.ext, by = c("study_site", "participant_age", "participant_sex"))


mo_loc_we <- mo_loc_ed%>%
  filter(place_visited != "Home")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(mo.we.ru, by = c("participant_age", "study_site"))%>%
  filter(!is.na(participant_age))%>%
  as_survey_design(weights = psweight)

mo_loc_agesex <- mo_loc_we%>%
  group_by(participant_age, participant_sex, place_visited, study_site)%>%
  summarise(count = survey_total(),
            spent_time = survey_mean(spent_time, na.rm = T))%>%
  rename(contact_age = participant_age,
         contact_sex = participant_sex)%>%
  group_by(place_visited, study_site)%>%
  mutate(total_count = sum(count))%>%
  ungroup()%>%
  mutate(agesex_dist = count/total_count)%>%
  right_join(mo.loc_frame2, by = c("contact_age", "contact_sex","place_visited", "study_site"))%>%
  mutate(agesex_dist = replace_na(agesex_dist, 0))%>%
  select(participant_age, participant_sex, contact_age, contact_sex, place_visited, study_site, agesex_dist)

## TB-weight ----
mo.tb.inc <- data.frame(
  contact_age = c("<5y","<5y", "5-9y", "5-9y", "10-19y", "10-19y",
                  "20-29y", "20-29y", "30-39y", "30-39y", "40-59y", "40-59y", "60+y", "60+y"),
  contact_sex = c("Male", "Female", "Male", "Female", "Male", "Female",
                  "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"),
  tb_weight = c(2700/121500, 2300/121500, 5800/121500, 5700/121500, (5800+54000)/2/1215000, (5700+51000)/2/121500,
                54000/121500, 51000/121500, 54000/121500, 51000/121500, 54000/121500, 51000/121500,
                54000/121500, 51000/121500),
  tb_lower = c(0/121500, 0/121500, 0/121500, 0/121500, (0+16000)/2/1215000, (0+15000)/2/121500,
               16000/121500, 15000/121500, 16000/121500, 15000/121500, 16000/121500, 15000/121500,
               54000/121500, 51000/121500),
  tb_upper = c(2700/121500, 2300/121500, 5800/121500, 5700/121500, (5800+54000)/2/1215000, (5700+51000)/2/121500,
               54000/121500, 51000/121500, 54000/121500, 51000/121500, 54000/121500, 51000/121500,
               54000/121500, 51000/121500)
)

mo.tb.inc$norm_tb_weight <- mo.tb.inc$tb_weight / mean(mo.tb.inc$tb_weight)

## Individual contact exposure hours ----
mo.co.group <- mo.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  distinct(participant_age, participant_sex)%>%
  rename(contact_age = participant_age,
         contact_sex = participant_sex)

mo.loc_frame3 <- mo.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  select(rec_id, participant_age, participant_sex)%>%
  crossing(mo.co.group)%>%
  filter(!is.na(contact_age) & !is.na(contact_sex))

mo.co.eh.tbwe <- mo.co.pa.counts%>%
  filter(!is.na(participant_age)&!is.na(participant_sex)&!is.na(contact_age)&!is.na(contact_sex))%>%
  left_join(mo.tb.inc, by = c("contact_age", "contact_sex"))%>%
  group_by(rec_id, participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(cont_time*tb_weight))%>%
  right_join(mo.loc_frame3, by = c("rec_id", "participant_age", "participant_sex", "contact_age", "contact_sex"))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
         contact_age = factor(contact_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  mutate(eh_we = replace_na(eh_we, 0))

## Community contact exposure hours ----
mo_community_tb <- mo_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(mo_loc_agesex, by = c("participant_age", "participant_sex","place_visited", "study_site"))%>%
  left_join(mo.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight)%>%
  filter(!is.na(eh_we))%>%
  right_join(mo.loc_frame_p, by = c("rec_id", "participant_age", "participant_sex", "contact_age", "contact_sex", "study_site", "place_visited"))%>%
  mutate(eh_we = replace_na(eh_we, 0))

mo_tb_eh_comb <- rbind(mo.co.eh.tbwe, mo_community_tb)%>%
  group_by(rec_id, participant_age, contact_age)%>%
  summarise(eh_we = sum(eh_we, na.rm = T))%>%
  filter(!is.na(contact_age) & !is.na(participant_age))%>%
  ungroup()%>%
  group_by(participant_age, contact_age)%>%
  summarise(eh_mean = mean(eh_we, na.rm = T),
            sd_eh   = sd(eh_we, na.rm = TRUE),
            n       = n(),
            se      = sd_eh / sqrt(n),
            lower   = eh_mean - 1.96 * se,
            upper   = eh_mean + 1.96 * se)%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
         contact_age = factor(contact_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))

## Plot in bar plot ----
mo_tb_eh_comb%>%
  filter(!is.na(participant_age) & !is.na(contact_age))%>%
  ggplot(aes(x = participant_age, y = eh_mean, fill = contact_age)) +
  geom_bar(
    position = "fill",
    stat = "identity",
    color = "black"
  ) +
  scale_fill_manual(name = "Contact Age",
                    values = c( "#D6604D","#FDAE61","#91CF60","#7FBFBD","#4393C3","#8073AC","pink")
  ) +
  theme_minimal()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.key.size = unit(0.5, "cm"),
        legend.key = element_rect(fill = "white", colour = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        title = element_text(size = 18)
  )+
  theme(plot.margin = margin(t = 20, r = 10, b = 10, l = 10))+
  labs(title = "Mozambique, Q = -0.072", x = "Participant age", y = "Proportion of exposure-hours")-> mo.age.eh.plot

## Exposure matrix by sex ----
# Individual contact
mo.co.eh.tbwe2 <- mo.co.pa.counts%>%
  filter(!is.na(participant_age)&!is.na(participant_sex)&!is.na(contact_age)&!is.na(contact_sex))%>%
  left_join(mo.tb.inc, by = c("contact_age", "contact_sex"))%>%
  group_by(rec_id, participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(cont_time/60*norm_tb_weight))%>%
  right_join(mo.loc_frame3, by = c("rec_id", "participant_age", "participant_sex", "contact_age", "contact_sex"))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
         contact_age = factor(contact_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  mutate(eh_we = replace_na(eh_we, 0))

# Community contact
mo_community_tb2 <- mo_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(mo_loc_agesex, by = c("participant_age", "participant_sex","place_visited", "study_site"))%>%
  left_join(mo.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time/60*num_pax_place*agesex_dist*norm_tb_weight)%>%
  filter(!is.na(eh_we))%>%
  right_join(mo.loc_frame_p, by = c("rec_id", "participant_age", "participant_sex", "contact_age", "contact_sex", "study_site", "place_visited"))%>%
  mutate(eh_we = replace_na(eh_we, 0))


mo_tb_eh_mat_sex <- rbind(mo.co.eh.tbwe2, mo_community_tb2)%>%
  filter(!is.na(study_day))%>%
  group_by(rec_id, participant_sex, contact_sex, study_day)%>%
  summarise(eh_we = sum(eh_we, na.rm = T))%>%
  group_by(participant_sex, contact_sex)%>%
  summarise(eh_mean = mean(eh_we),
            eh_sd = sd(eh_we),
            n = n(),
            eh_se = eh_sd / sqrt(n),
            ci_lower = eh_mean - 1.96 * eh_se,
            ci_upper = eh_mean + 1.96 * eh_se
  )

mo_tb_eh_mat_sex%>%
  ggplot(aes(x = participant_sex, y = contact_sex, fill = eh_mean))+
  scale_fill_distiller(palette = "Blues",
                       direction = 1,
                       #limits=c(0, 5), 
                       name = "Proportion") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1)+
  geom_shadowtext(
    aes(label = paste0(sprintf("%.1f", eh_mean), "\n(", 
                       sprintf("%.1f", ci_lower), ", ", 
                       sprintf("%.1f", ci_upper), ")")),
    color = "black", 
    bg.color = "white", 
    size = 10, 
    bg.r = 0.15)+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        title = element_text(size = 18))+
  labs(title = "Mozamnique, Q = 0.04", x = "Participant sex", y = "Contact sex") -> mo.mat.sex.plot



## Proportion of exposure-hours to women vs men
rbind(mo.co.eh.tbwe, mo_community_tb)%>%
  group_by(rec_id, participant_age, contact_sex)%>%
  summarise(eh_we = sum(eh_we, na.rm = T))%>%
  filter(!is.na(contact_sex) & !is.na(participant_age))%>%
  ungroup()%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  group_by(participant_age, contact_sex)%>%
  summarise(eh_mean = mean(eh_we, na.rm = T),
            sd_eh   = sd(eh_we, na.rm = TRUE),
            n       = n(),
            se      = sd_eh / sqrt(n),
            lower   = eh_mean - 1.96 * se,
            upper   = eh_mean + 1.96 * se)%>%
  group_by(participant_age)%>%
  mutate(total = sum(eh_mean))%>%
  group_by(participant_age, contact_sex)%>%
  mutate(prop = eh_mean/total)


## Calculate assortativity ----
age_levels <- c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")

mo_age_mat <- mo_tb_eh_comb%>%
  select(participant_age, contact_age, eh_mean)%>%
  pivot_wider(names_from = "participant_age", values_from = "eh_mean")%>%
  arrange(factor(contact_age, levels = age_levels)) %>%
  select(contact_age, all_of(age_levels))

mo_age_mat$contact_age <- NULL
rownames(mo_age_mat) <- colnames(mo_age_mat)

mo_age_ass <- sam_index_q(mo_age_mat)

mo_sex_mat <- mo_tb_eh_mat_sex%>%
  select(participant_sex, contact_sex, eh_mean)%>%
  pivot_wider(names_from = "participant_sex", values_from = "eh_mean")

mo_sex_mat$contact_sex <- NULL
rownames(mo_sex_mat) <- colnames(mo_sex_mat)

mo_sex_ass <- index_q(mo_sex_mat)


# Figure 3 and Supplemental figure 1: Proportion of exposure-hours for location of community contacts ---------------------------
denoms.byagesex.loc.mo <- mo_loc_ed%>%
  filter(place_visited != "Home")%>%
  filter(!is.na(participant_age))%>%
  filter(!is.na(participant_sex))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(place_visited, participant_age, participant_sex)%>%
  summarise(n_part = n())



mo_loc_community_eh <- mo_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  filter(!is.na(participant_sex))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(mo_loc_agesex, by = c("participant_age", "participant_sex","place_visited", "study_site"))%>%
  left_join(mo.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight,
         eh_we_low = spent_time*num_pax_place*agesex_dist*tb_lower,
         eh_we_up = spent_time*num_pax_place*agesex_dist*tb_upper)%>%
  group_by(participant_age, place_visited)%>%
  summarise(ex_hour = sum(eh_we, na.rm = T),
            ex_hour_low = sum(eh_we_low, na.rm = T),
            ex_hour_up = sum(eh_we_up, na.rm = T))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  group_by(participant_age)%>%
  mutate(total = sum(ex_hour),
         prop = ex_hour/total,
         total_low = sum(ex_hour_low),
         prop_low = ex_hour_low/total_low,
         total_up = sum(ex_hour_up),
         prop_up = ex_hour_up/total_up)%>%
  mutate(country = "Mozambique")

# Main text input --------------------------------------------------------------------
nrow(mo.co)
mean(mo.pa.age$participant_age == "<5y", na.rm = TRUE)
mean(mo.pa$participant_sex == "Female", na.rm = T)
mean(mo.pa$high_educ == "Currently enrolled in school", na.rm = T)
median(mo.pa$hh_size, na.rm = T)
quantile(mo.pa$hh_size, na.rm = T)

## Number of types of contacts
sum(mo_loc_ed$num_pax_place, na.rm = T)

## Supplemental materials ##
# Supplemental table 1: Characteristics of participants ---------------------------------
# Number of participants
nrow(mo.pa.age)

## By age
mo.pa.age%>%
  group_by(participant_age) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

## By sex
mo.pa.age%>%
  group_by(participant_sex) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

# Education
#restrict to non-kids only
mo.pa.age %>%
  filter(participant_age != "<5y")%>%
  group_by(high_educ) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

# Occupation
#restrict to adults only
mo.pa.age %>%
  filter(!participant_age %in% c("<5y", "5-9y"))%>%
  group_by(occupation) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

## Household size
mo.pa.age %>%
  group_by(hh_size_cat) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

quantile(mo.pa.age$hh_size, na.rm = T)

## Generations in households
mo.co %>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>%
  group_by(hh.gens) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

## Multi-family household
mo.co %>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>%
  group_by(hh.multfams) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))


# Supplemental table 2: Median number of contacts by participant demographics --------------
mo.co.pa.full <-  full_join(mo.co, mo.pa.age, 
                            by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))%>%
  count(study_day, contact, name = "num_contacts")

mo.co.pa <- left_join(mo.pa.age, mo.co.pa.full, by = "rec_id") %>%
  mutate(num_contacts = ifelse(is.na(num_contacts), 0, num_contacts))%>%
  left_join(mo.we.ru%>%select(psweight, participant_age, study_site), by = c("participant_age", "study_site"))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  filter(!is.na(participant_age))


## Close contacts ----
# Contact overall
mo.co.pa %>%
  as_survey(weights = c(psweight))%>%
  summarise(median = survey_median(num_contacts, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts, c(0.25, 0.75), na.rm = T))

# By age
mo.co.pa%>%
  group_by(participant_age)%>%
  summarise(median = median(num_contacts, na.rm = T),
            lower_q = quantile(num_contacts, 0.25, na.rm = T),
            upper_q = quantile(num_contacts, 0.75, na.rm = T))

# By sex
mo.co.pa%>%
  group_by(participant_sex)%>%
  summarise(median = median(num_contacts, na.rm = T),
            lower_q = quantile(num_contacts, 0.25, na.rm = T),
            upper_q = quantile(num_contacts, 0.75, na.rm = T))

### Statistical test for contact's sex difference ------------------------------
mo.co.pa.test <- mo.co.pa%>%
  filter(!is.na(participant_sex))

wilcox.test(num_contacts ~ participant_sex, data = mo.co.pa.test)

## Casual contacts ----
# Contact overall
mo_loc_ed%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(rec_id, study_day, study_site, participant_age)%>%
  summarise(num_pax_place = sum(num_pax_place))%>%
  ungroup()%>%
  left_join(mo.we.ru%>%select(psweight, participant_age, study_site), by = c("participant_age", "study_site"))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  filter(!is.na(participant_age))%>%
  as_survey(weights = c(psweight))%>%
  summarise(median = survey_median(num_pax_place, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_pax_place, c(0.25, 0.75), na.rm = T))

# By age
mo_loc_ed%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age),
         participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  group_by(rec_id, study_day, study_site, participant_age)%>%
  summarise(num_pax_place = sum(num_pax_place))%>%
  ungroup()%>%
  group_by(participant_age)%>%
  summarise(median = median(num_pax_place, na.rm = T),
            lower_q = quantile(num_pax_place, 0.25, na.rm = T),
            upper_q = quantile(num_pax_place, 0.75, na.rm = T))

# By sex
mo_loc_ed%>%
  group_by(rec_id, study_day, study_site, participant_sex)%>%
  summarise(num_pax_place = sum(num_pax_place))%>%
  ungroup()%>%
  group_by(participant_sex)%>%
  summarise(median = median(num_pax_place, na.rm = T),
            lower_q = quantile(num_pax_place, 0.25, na.rm = T),
            upper_q = quantile(num_pax_place, 0.75, na.rm = T))

## Combined contacts ----
mo.svydate <- mo.co%>%
  select(rec_id, study_day, survey_date)%>%
  distinct()

mo_loc_ed%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(rec_id, study_day)%>%
  summarise(num_pax_place = sum(num_pax_place))%>%
  ungroup() -> mo_loc_num

mo.co.pa%>%
  full_join(mo_loc_num, by = c("rec_id", "study_day"))%>%
  mutate(num_contacts = replace_na(num_contacts, 0),
         num_pax_place = replace_na(num_pax_place, 0),
         total_contact = num_contacts + num_pax_place)%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  filter(!is.na(participant_age))%>%
  left_join(mo.svydate, by = c("rec_id", "study_day")) -> mo_combined_count

mo_combined_count%>%
  as_survey(weights = c(psweight))%>%
  summarise(median = survey_median(total_contact, na.rm = T),
            n = survey_total(),
            q = survey_quantile(total_contact, c(0.25, 0.75), na.rm = T))

# Supplemental figure 2: Daily number of contacts with stringency index -----------------------------------------------------------
mo.stringency <- stringency%>%
  mutate(survey_date = ymd(Date))%>%
  filter(CountryName == "Mozambique")

## Count the number of contacts per survey date
mo_cont_count <- mo_combined_count%>%
  group_by(survey_date)%>%
  summarise(count = sum(total_contact),
            avg_count = mean(total_contact))

mo_str_plot <- ggplot(mo_cont_count, aes(x = survey_date))+
  geom_bar(aes(y = avg_count), stat = "identity")+
  geom_line(data = mo.stringency, aes(y = StringencyIndex_Average * (max(mo_cont_count$avg_count) / 100)), color = "red")+
  scale_x_date(limits = as.Date(c("2021-01-01", "2024-07-30")),
               breaks = seq(as.Date("2021-05-01"), as.Date("2024-07-30"), by = "6 months"),
               date_labels = "%y-%b") +
  scale_y_continuous(
    #name = "Number of participants",
    sec.axis = sec_axis(~ . * (100 / max(mo_cont_count$avg_count)))) +
  labs(title = "Mozambique") +
  xlab(" ")+
  ylab(" ")+
  theme_minimal()+
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20),
        title = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 17),
        plot.background = element_rect(color = "white"))

# Comparison with Prem data -----------------------------------------
## Panel A -------------------------------------------------------
## Prepare Prem data
mo.prem <- prem%>%
  filter(iso3c == "MOZ" & setting == "overall" & location_contact == "all")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")))

mo.prem.table <- mo.prem%>%
  group_by(participant_age)%>%
  summarise(contact_rate = sum(contact_rate))%>%
  mutate(country = "Mozambique")

## Prepare GlobalMix data
mo.gm.table <- mo_combined_count%>%
  mutate(participant_age = case_when(participant_age == "<5y" ~ "0-4y",
                                     TRUE ~ participant_age))%>%
  group_by(participant_age)%>%
  summarise(contact_rate = mean(total_contact, na.rm = T),
            sd = sd(total_contact, na.rm = T),
            n = n())%>%
  mutate(lower_ci = contact_rate - 1.96 * sd/sqrt(n), # Lower bound of 95% CI
         upper_ci = contact_rate + 1.96 * sd/sqrt(n), # Upper bound of 95% CI
         participant_age = factor(participant_age, levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"))) %>%
  arrange(participant_age)%>%
  mutate(country = "Mozambique")

# Take midpoint of age group
mo.prem.table$age_midpoint <- sapply(mo.prem.table$participant_age, get_midpoint)
mo.gm.table$age_midpoint <- sapply(mo.gm.table$participant_age, get_midpoint)

# Add a data source column
mo.prem.table <- mo.prem.table%>%
  mutate(dataset = "Prem et al., 2021",
         lower_ci = NA,
         upper_ci = NA)
mo.gm.table<- mo.gm.table%>%
  mutate(dataset = "GlobalMix")%>%
  # mutate(dataset = case_when(study_site.x == "Rural" ~ "GlobalMix, rural",
  #                            study_site.x == "Urban" ~ "GlobalMix, urban"))%>%
  ungroup()%>%
  dplyr::select(participant_age, contact_rate, country, age_midpoint, dataset, lower_ci, upper_ci)

# Combine the datasets
mo.age.table <- rbind(mo.prem.table, mo.gm.table)%>%
  mutate(dataset = factor(dataset, levels = c("Prem et al., 2021", "GlobalMix")))
# mutate(dataset = factor(dataset, levels = c("Prem et al., 2021", "GlobalMix, rural", "GlobalMix, urban")))

# Plot the line graph
mo.age.plot <- ggplot(mo.age.table, aes(x = age_midpoint, y = contact_rate, color = dataset)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5, size = 0.7) +
  scale_x_continuous("Participant age", breaks = seq(0, 75, by = 5)) +
  #scale_y_continuous("Contact Rate") +
  #ylim(0,25)+
  labs(title = "Mozambique",
       color = "Dataset") +
  ylab("Contact Rate")+
  scale_color_manual(values = c("Prem et al., 2021" = "orange", "GlobalMix" = "steelblue3"))+
  # scale_color_manual(values = c("Prem et al., 2021" = "sienna", "GlobalMix, rural" = 'aquamarine4', "GlobalMix, urban" = "steelblue3"))+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        title = element_text(size = 20))

## Panel B -------------------------------------------------------------------------
# Prepare Prem et al. data
p_moz_school <- prem%>%
  filter(iso3c == "MOZ" & setting == "overall" & location_contact == "school")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)
p_moz_work <- prem%>%
  filter(iso3c == "MOZ" & setting == "overall" & location_contact == "work")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)
p_moz_home <- prem%>%
  filter(iso3c == "MOZ" & setting == "overall" & location_contact == "home")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)
p_moz_other <- prem%>%
  filter(iso3c == "MOZ" & setting == "overall" & location_contact == "others")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)

# Combine the location
p_moz_home_lab <- p_moz_home%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Home")
p_moz_school_lab <- p_moz_school%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "School")
p_moz_work_lab <- p_moz_work%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Work")
p_moz_other_lab <- p_moz_other%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Other")

p_moz_loc <- rbind(p_moz_home_lab, p_moz_school_lab, p_moz_work_lab, p_moz_other_lab)%>%
  group_by(location)%>%
  summarise(contact_rate = sum(contact_rate))%>%
  mutate(contact_rate_tot = sum(contact_rate),
         percentage = contact_rate/contact_rate_tot,
         country = "Mozambique")

## Prepare GlobalMix data
mo.loc.count <- mo_loc_ed%>%
  filter(!is.na(num_pax_place))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age),
         place_visited = case_when(place_visited == "Other home" ~ "Other",
                                   place_visited == "Market/essential" ~ "Other",
                                   place_visited == "Other social/leisure" ~ "Other",
                                   place_visited == "Transit" ~ "Other",
                                   place_visited == "Worship" ~ "Other",
                                   is.na(place_visited) ~ "Unreported",
                                   TRUE ~ place_visited))%>%
  rename(location = place_visited,
         count = num_pax_place)%>%
  left_join(mo.we.ru, by = c("participant_age", "study_site"))%>%
  mutate(country = "Mozambique")%>%
  select(rec_id, participant_age, location, count, study_site, country, psweight)


mo_location <- mo.co%>%
  mutate(location = case_when(location == "Transit" ~ "Other",
                              location == "Market / essential" ~ "Other",
                              location == "Other social / leisure" ~ "Other",
                              location == "Worship" ~ "Other",
                              TRUE ~ location))%>%
  left_join(mo.pa, by = "rec_id")%>%
  rename(study_site = study_site.x)%>%
  dplyr::select(rec_id, study_site, participant_age, participant_sex, contact_age, contact_sex, location)%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age),
         count = 1)%>%
  left_join(mo.we.ru, by = c("participant_age", "study_site"))%>%
  mutate(country = "Mozambique")%>%
  select(rec_id, participant_age, location, count, study_site, country, psweight)

mo.loc.comb <- rbind(mo_location, mo.loc.count)

## Exposure-hour version -----------------------------------------------
### Panel A ---------------------------------------------------
# Prem data will be the same
# Prepare GlobalMix data
mo.gm.eh.table <- mo.co%>%
  group_by(rec_id, study_day)%>%
  summarise(ex_hour = sum(cont_time/60))%>%
  left_join(mo.pa %>% select(rec_id, participant_age), by = "rec_id")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "0-4y",
                                     participant_age == "6-11mo" ~ "0-4y",
                                     participant_age == "1-4y" ~ "0-4y",
                                     TRUE ~ participant_age))

mo.gm.loc.eh.table <- mo_loc_ed%>%
  group_by(rec_id, participant_age, study_day)%>%
  summarise(ex_hour = sum(spent_time*num_pax_place/60))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "0-4y",
                                     participant_age == "6-11mo" ~ "0-4y",
                                     participant_age == "1-4y" ~ "0-4y",
                                     TRUE ~ participant_age))


mo.gm.eh.table.comb <- rbind(mo.gm.eh.table, mo.gm.loc.eh.table)%>%
  group_by(rec_id, participant_age, study_day)%>%
  summarise(ex_hour = sum(ex_hour))%>%
  group_by(participant_age)%>%
  summarise(mean_eh = mean(ex_hour, na.rm = T),
            sd = sd(ex_hour, na.rm = T),
            n = n())%>%
  mutate(lower_ci = mean_eh - 1.96 * sd/sqrt(n), # Lower bound of 95% CI
         upper_ci = mean_eh + 1.96 * sd/sqrt(n), # Upper bound of 95% CI
         participant_age = factor(participant_age, levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"))) %>%
  filter(!is.na(participant_age))%>%
  arrange(participant_age)%>%
  mutate(country = "Mozambique",
         contact_rate = NA,
         dataset = "GlobalMix")%>%
  dplyr::select(participant_age, mean_eh, contact_rate, country, dataset, lower_ci, upper_ci)

# Take midpoint of age group
mo.gm.eh.table.comb$age_midpoint <- sapply(mo.gm.eh.table.comb$participant_age, get_midpoint)

# Combine the datasets
mo.eh.comp.table <- mo.prem.table%>%
  mutate(mean_eh = NA)%>%
  bind_rows(mo.gm.eh.table.comb)%>%
  mutate(dataset = factor(dataset, levels = c("Prem et al., 2021", "GlobalMix")))
# mutate(dataset = factor(dataset, levels = c("Prem et al., 2021", "GlobalMix, rural", "GlobalMix, urban")))

# Plot the line graph
mo_scale <- max(mo.eh.comp.table$contact_rate, na.rm = TRUE) / 
  max(mo.eh.comp.table$mean_eh, na.rm = TRUE)


mo.eh.comp.plot <- ggplot() +
  geom_line(data = mo.eh.comp.table %>% filter(!is.na(contact_rate)),
            aes(x = age_midpoint, y = contact_rate, color = "Prem et al., 2021"),
            linewidth = 1) +
  geom_line(data = mo.eh.comp.table %>% filter(!is.na(mean_eh)),
            aes(x = age_midpoint, y = mean_eh * mo_scale, color = "GlobalMix"),
            linewidth = 1) +
  geom_errorbar(data = mo.eh.comp.table %>% filter(!is.na(mean_eh)),
                aes(x = age_midpoint,
                    ymin = lower_ci * mo_scale,
                    ymax = upper_ci * mo_scale),
                width = 0.5, size = 0.7, color = "steelblue3") +
  scale_x_continuous("Participant age", breaks = seq(0, 75, by = 5)) +
  scale_y_continuous(
    name = "Contact rate (Prem et al., 2021)",
    sec.axis = sec_axis(~ . / in_scale, name = "Exposure-hour (GlobalMix)")
  ) +
  scale_color_manual(values = c("Prem et al., 2021" = "orange", "GlobalMix" = "steelblue3")) +
  labs(title = "Mozambique", color = "Dataset") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    title = element_text(size = 20)
  )

### Panel B ---------------------------------------------------
# Prepare GlobalMix data
mo.loc.eh <- mo_loc_ed%>%
  filter(!is.na(num_pax_place))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age),
         place_visited = case_when(place_visited == "Other home" ~ "Other",
                                   place_visited == "Market/essential" ~ "Other",
                                   place_visited == "Other social/leisure" ~ "Other",
                                   place_visited == "Transit" ~ "Other",
                                   place_visited == "Worship" ~ "Other",
                                   is.na(place_visited) ~ "Unreported",
                                   TRUE ~ place_visited),
         ex_hour = spent_time*num_pax_place/60)%>%
  rename(location = place_visited)%>%
  left_join(mo.we.ru, by = c("participant_age", "study_site"))%>%
  mutate(country = "Mozambique")%>%
  select(rec_id, participant_age, location, ex_hour, study_site, country, psweight)


mo_eh_location <- mo.co%>%
  mutate(location = case_when(location == "Transit" ~ "Other",
                              location == "Market / essential" ~ "Other",
                              location == "Other social / leisure" ~ "Other",
                              location == "Worship" ~ "Other",
                              TRUE ~ location))%>%
  left_join(mo.pa, by = "rec_id")%>%
  rename(study_site = study_site.x)%>%
  dplyr::select(rec_id, study_site, participant_age, participant_sex, contact_age, contact_sex, location, cont_time)%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age),
         ex_hour = cont_time/60)%>%
  left_join(mo.we.ru, by = c("participant_age", "study_site"))%>%
  mutate(country = "Mozambique")%>%
  select(rec_id, participant_age, location, ex_hour, study_site, country, psweight)

mo.loc.eh.comb <- rbind(mo_eh_location, mo.loc.eh)