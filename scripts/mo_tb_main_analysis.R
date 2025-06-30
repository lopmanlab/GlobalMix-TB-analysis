# TB-analysis script - Mozambique

# Read data ----------------------------------------------------------------------
mo_locdata <- readRDS("./Mozambique/moz_locations_visited_data_aim1.RDS")

mo.pa <- readRDS("./Mozambique/moz_participant_data_aim1.RDS")
mo.co <- readRDS("./Mozambique/moz_contact_data_aim1.RDS")
mo.we <- read.csv("./Other/moz_pop.csv")

stringency <- read.csv("./Other/OxCGRT_compact_national_v1.csv", header = T)

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

# Figure 2: Exposure profiles by age and sex --------------------------------------
## Calculate age and sex distribution ----
mo.pa.we <- mo.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(study_site, participant_age)%>%
  summarise(n_s = n())%>%
  mutate(prop_s = n_s/sum(n_s))

mor.we <- mo.we%>%
  left_join(mo.pa.we, by = c("participant_age", "study_site"))%>%
  mutate(psweight = prop/prop_s)%>%
  filter(study_site == "Rural")
mou.we <- mo.we%>%
  left_join(mo.pa.we, by = c("participant_age", "study_site"))%>%
  mutate(psweight = prop/prop_s)%>%
  filter(study_site == "Urban")

## Create a framework for location dataset ----
loc_frame2 <- mo_loc_ed%>%
  filter(!is.na(participant_sex))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(place_visited, participant_age, participant_sex)%>%
  summarise(count = n())%>%
  ungroup()%>%
  tidyr::expand(participant_age, participant_age, participant_sex, participant_sex, place_visited) %>%
  setNames(c("participant_age", "contact_age", "participant_sex", "contact_sex","place_visited"))%>%
  filter(!is.na(place_visited))%>%
  filter(place_visited != "Home")

mor_loc_we <- mo_loc_ed%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Rural")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(mor.we, by = "participant_age")%>%
  as_survey_design(weights = psweight)

mor_loc_agesex <- mor_loc_we%>%
  group_by(participant_age, participant_sex, place_visited)%>%
  summarise(count = survey_total(),
            spent_time = survey_mean(spent_time, na.rm = T))%>%
  rename(contact_age = participant_age,
         contact_sex = participant_sex)%>%
  group_by(place_visited)%>%
  mutate(total_count = sum(count))%>%
  ungroup()%>%
  mutate(agesex_dist = count/total_count)%>%
  right_join(loc_frame2, by = c("contact_age", "contact_sex","place_visited"))%>%
  mutate(agesex_dist = replace_na(agesex_dist, 0))%>%
  select(participant_age, participant_sex, contact_age, contact_sex, place_visited, agesex_dist)

mou_loc_we <- mo_loc_ed%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Urban")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(mou.we, by = "participant_age")%>%
  as_survey_design(weights = psweight)

mou_loc_agesex <- mou_loc_we%>%
  group_by(participant_age, participant_sex, place_visited)%>%
  summarise(count = survey_total(),
            spent_time = survey_mean(spent_time, na.rm = T))%>%
  rename(contact_age = participant_age,
         contact_sex = participant_sex)%>%
  group_by(place_visited)%>%
  mutate(total_count = sum(count))%>%
  ungroup()%>%
  mutate(agesex_dist = count/total_count)%>%
  right_join(loc_frame2, by = c("contact_age", "contact_sex","place_visited"))%>%
  mutate(agesex_dist = replace_na(agesex_dist, 0))%>%
  select(participant_age, participant_sex, contact_age, contact_sex, place_visited, agesex_dist)

## TB-weight ----
mo.tb.inc <- data.frame(
  contact_age = c("<5y","<5y", "5-9y", "5-9y", "10-19y", "10-19y",
                  "20-29y", "20-29y", "30-39y", "30-39y", "40-59y", "40-59y", "60+y", "60+y"),
  contact_sex = c("Male", "Female", "Male", "Female", "Male", "Female",
                  "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"),
  tb_weight = c(2700/121500, 2300/121500, 5800/121500, 5700/121500, (5800+54000)/2/1215000, (5700+51000)/2/121500,
                54000/121500, 51000/121500, 54000/121500, 51000/121500, 54000/121500, 51000/121500,
                54000/121500, 51000/121500)
)

## Individual contact exposure hours ----
mo.co.eh.tbwe <- mo.co.pa.counts%>%
  filter(!is.na(participant_age)&!is.na(participant_sex)&!is.na(contact_age)&!is.na(contact_sex))%>%
  left_join(mo.tb.inc, by = c("contact_age", "contact_sex"))%>%
  group_by(participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(cont_time*tb_weight))%>%
  left_join(denoms.byagesex.mo, by = c("participant_age", "participant_sex"))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
         contact_age = factor(contact_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))

## Community contact exposure hours ----
mor_community_tb <- mo_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Rural")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(mor_loc_agesex, by = c("participant_age", "participant_sex","place_visited"))%>%
  left_join(mo.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight)%>%
  group_by(participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(eh_we, na.rm = T))%>%
  mutate(study_site = "Rural")


mou_community_tb <- mo_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Urban")%>%
  filter(!is.na(participant_sex))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(mou_loc_agesex, by = c("participant_age", "participant_sex","place_visited"))%>%
  left_join(mo.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight)%>%
  group_by(participant_age, participant_sex, contact_age,contact_sex)%>%
  summarise(eh_we = sum(eh_we, na.rm = T))%>%
  mutate(study_site = "Urban")

mo_loc_part2 <- mo_loc_ed%>%
  distinct(rec_id, study_day, study_site, participant_age, participant_sex)%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(study_site, participant_age, participant_sex)%>%
  summarise(n = n())

mo_community_tb <- rbind(mor_community_tb, mou_community_tb)%>%
  left_join(mo_loc_part2, by = c("study_site", "participant_age", "participant_sex"))%>%
  group_by(participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(eh_we, na.rm = T),
            n = sum(n, na.rm = T))

mo_tb_eh_comb <- rbind(mo.co.eh.tbwe, mo_community_tb)%>%
  filter(!is.na(participant_sex) & !is.na(participant_age) & !is.na(contact_age) & !is.na(contact_sex))%>%
  group_by(participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(eh_we, na.rm = T),
            n = sum(n, na.rm = T))%>%
  mutate(eh_mean = eh_we/n)%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
         contact_age = factor(contact_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))

## Plot in bar plot ----
mo_tb_eh_comb%>%
  ggplot(aes(x = interaction(participant_sex, participant_age), y = eh_mean, fill = contact_age, pattern = contact_sex)) +
  geom_bar_pattern(
    position = "fill",
    stat = "identity",
    color = "black",
    pattern_type = "stripe",
    pattern_density = 0.2,
    pattern_spacing = 0.05,
    show.legend = c(pattern = TRUE, fill = TRUE)
  ) +
  scale_fill_discrete(name = "Contact Age", guide = guide_legend(override.aes = list(pattern = "none"))) +
  scale_pattern_manual(name = "Contact Sex", values = c("Male" = "none", "Female" = "stripe"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),
        legend.key.size = unit(0.5, "cm"),
        legend.key = element_rect(fill = "white", colour = "black")
  )+
  theme(plot.margin = margin(t = 20, r = 10, b = 10, l = 10))+
  labs(title = "Mozambique", x = "Participant age and sex", y = "Proportion of exposure-hours")-> mo.agesex.eh.plot

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

mor_loc_community_eh <- mo_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Rural")%>%
  filter(!is.na(participant_sex))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(mor_loc_agesex, by = c("participant_age", "participant_sex","place_visited"))%>%
  left_join(mo.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight)%>%
  group_by(participant_age, participant_sex, place_visited)%>%
  summarise(ex_hour = sum(eh_we, na.rm = T))

mou_loc_community_eh <- mo_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Urban")%>%
  filter(!is.na(participant_sex))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(mou_loc_agesex, by = c("participant_age", "participant_sex","place_visited"))%>%
  left_join(mo.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight)%>%
  group_by(participant_age, participant_sex, place_visited)%>%
  summarise(ex_hour = sum(eh_we, na.rm = T))

mo_loc_community_eh <- rbind(mor_loc_community_eh, mou_loc_community_eh)%>%
  group_by(participant_age, place_visited)%>%
  summarise(ex_hour = sum(ex_hour))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  group_by(participant_age)%>%
  mutate(total = sum(ex_hour),
         prop = ex_hour/total*100)%>%
  mutate(country = "Mozambique")

mo_loc_community_eh_comb <- mo_loc_community_eh%>%
  group_by(place_visited)%>%
  summarise(ex_hour = sum(ex_hour, na.rm = T))%>%
  mutate(total = sum(ex_hour),
         prop = ex_hour/total*100)%>%
  mutate(rank = dense_rank(desc(prop)),
         country = "Mozambique")

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
mo.we.comb <- rbind(mor.we, mou.we)

# add participant data to contact data
mo.co.pa.full <-  full_join(mo.co, mo.pa.age, 
                            by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))%>% #survey_date is complete for all contacts, so this is an indicator variable for whether an observation corresponds to a contact.
  # not all participants reported contacts, so this variable is 0 if the observation represents a participant that did not report any contacts.
  count(study_day, contact, name = "num_contacts")

mo.co.pa <- left_join(mo.pa.age, mo.co.pa.full, by = "rec_id") %>%
  mutate(num_contacts = ifelse(is.na(num_contacts), 0, num_contacts))%>%
  left_join(mo.we.comb%>%select(psweight, participant_age, study_site), by = c("participant_age", "study_site"))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))


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
  left_join(mo.we.comb%>%select(psweight, participant_age, study_site), by = c("participant_age", "study_site"))%>%
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
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        title = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 15),
        plot.background = element_rect(color = "white"))
