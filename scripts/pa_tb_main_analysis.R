# TB-analysis - Pakistan

# Read data ------------------------------------------------------------------------------
pa_locdata <- readRDS("./Pakistan/pak_locations_visited_data_aim1.RDS")

pa.pa <- readRDS("./Pakistan/pak_participant_data_aim1.RDS")
pa.co <- readRDS("./Pakistan/pak_contact_data_aim1.RDS")
pa.we <- read.csv("./Other/pak_pop.csv")

stringency <- read.csv("./Other/OxCGRT_compact_national_v1.csv", header = T)

# Clean the data -------------------------------------------------------------------------
# Standardize the data across countries and merge participants information
pa_loc_ed <- pa_locdata%>%
  mutate(place_visited = case_when(place_visited == "Airport" ~ "Other social/leisure",
                                   place_visited == "Fields" ~ "Work",
                                   place_visited == "Garden/Playground" ~ "Other social/leisure",
                                   place_visited == "Health facilities" ~ "Market/essential",
                                   place_visited == "Local transport station" ~ "Transit",
                                   place_visited == "Market/shop" ~ "Market/essential",
                                   place_visited == "My home" ~ "Home",
                                   place_visited == "Other" ~ "Other",
                                   place_visited == "Others home" ~ "Other home",
                                   place_visited == "Place of worship" ~ "Worship",
                                   place_visited == "Place to keep animals" ~ "Market/essential",
                                   place_visited == "School" ~ "School",
                                   place_visited == "Social gathering/event" ~ "Other social/leisure",
                                   place_visited == "Street" ~ "Transit",
                                   place_visited == "Sugar mill" ~ "Work",
                                   place_visited == "Transport" ~ "Transit",
                                   place_visited == "Well/boring" ~ "Market/essential",
                                   place_visited == "Work" ~ "Work",
                                   TRUE ~ "Unreported"),
         place_visited = factor(place_visited, levels = c("Home", "Other home", "Market/essential", 
                                                          "Other social/leisure", "School", "Transit","Work", "Worship")))%>%
  mutate(num_pax_place = as.numeric(num_pax_place),
         num_pax_place = case_when(num_pax_place >= 50 ~ 50,
                                   TRUE ~ num_pax_place),
         spent_time = case_when(time_visited == "<5 mins" ~ 2.5,
                                time_visited == "5-15 mins" ~ 10,
                                time_visited == "16-30 mins" ~ 23,
                                time_visited == "31 mins-1 hr" ~ 45 ,
                                time_visited == "1-4 hrs" ~ 150,
                                time_visited == ">4 hrs" ~ 240))%>%
  left_join(pa.pa, by = "rec_id")

## Modify data for analysis ----
# Merge the <5y group
pa.pa.age <- pa.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))
pa.co.age <- pa.co%>%
  mutate(contact_age = case_when(contact_age == "<6mo" ~ "<5y",
                                 contact_age == "6-11mo" ~ "<5y",
                                 contact_age == "1-4y" ~ "<5y",
                                 TRUE ~ contact_age))

pa.co.pa.counts <-  full_join(pa.co.age, pa.pa.age, 
                              by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))

## Calculate number of participants by age and sex ----
pa.pa.age %>%
  filter(!is.na(participant_age)&!is.na(participant_sex))%>%
  count(participant_age, participant_sex) -> denoms.byagesex.pa

# Figure 1: Exposure-hours by type of contact -----------------------------------------------
pa.ind.type <- pa.co%>%
  group_by(rec_id, study_day, hh_membership)%>%
  summarise(cont_time = sum(cont_time))%>%
  mutate(hh_membership = case_when(hh_membership == "Member" ~ "Close household",
                                   hh_membership == "Non-member" ~ "Close non-household"))

pa.loc.type <- pa_loc_ed%>%
  filter(place_visited != "Home")%>%
  mutate(cont_time = spent_time*num_pax_place)%>%
  group_by(rec_id, study_day)%>%
  summarise(cont_time = sum(cont_time))%>%
  mutate(hh_membership = "Casual")

pa.type.value <- rbind(pa.ind.type, pa.loc.type)%>%
  mutate(ex_hour = cont_time/60)%>% # make minutes from hour
  group_by(hh_membership)%>%
  summarise(mean_eh = mean(ex_hour, na.rm = T),
            sd_eh = sd(ex_hour, na.rm = TRUE),
            n = sum(!is.na(ex_hour)),
            se = sd_eh / sqrt(n),
            lower_ci = mean_eh - 1.96 * se,
            upper_ci = mean_eh + 1.96 * se
  )%>%
  mutate(country = "Pakistan")

pa.type.value.med <- rbind(pa.ind.type, pa.loc.type)%>%
  mutate(ex_hour = cont_time/60)%>% # make minutes from hour
  group_by(hh_membership)%>%
  summarise(median_eh = median(ex_hour, na.rm = T),
            lower_percentile = quantile(ex_hour, probs = 0.25, na.rm = T),
            upper_percentile = quantile(ex_hour, probs = 0.75, na.rm = T))%>%
  mutate(country = "Pakistan")

## Summary figure is in the summary script

# Figure 2: Exposure profiles by age and sex --------------------------------------
## Calculate age and sex distribution ----
pa.pa.we <- pa.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(study_site, participant_age)%>%
  summarise(n_s = n())%>%
  mutate(prop_s = n_s/sum(n_s))

par.we <- pa.we%>%
  left_join(pa.pa.we, by = c("participant_age", "study_site"))%>%
  mutate(psweight = prop/prop_s)%>%
  filter(study_site == "Rural")
pau.we <- pa.we%>%
  left_join(pa.pa.we, by = c("participant_age", "study_site"))%>%
  mutate(psweight = prop/prop_s)%>%
  filter(study_site == "Urban")

## Create a framework for location dataset ----
loc_frame2 <- pa_loc_ed%>%
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


par_loc_we <- pa_loc_ed%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Rural")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(par.we, by = "participant_age")%>%
  filter(!is.na(participant_age))%>%
  as_survey_design(weights = psweight)

par_loc_agesex <- par_loc_we%>%
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

pau_loc_we <- pa_loc_ed%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Urban")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(pau.we, by = "participant_age")%>%
  filter(!is.na(participant_age))%>%
  as_survey_design(weights = psweight)

pau_loc_agesex <- pau_loc_we%>%
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
pa.tb.inc <- data.frame(
  contact_age = c("<5y", "<5y" ,"5-9y", "5-9y", "10-19y", "10-19y",
                  "20-29y", "20-29y", "30-39y", "30-39y", "40-59y", "40-59y", "60+y", "60+y"),
  contact_sex = c("Male", "Female", "Male", "Female", "Male", "Female",
                  "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"),
  tb_weight = c(22000/685000, 18000/685000, 26000/685000, 27000/685000,
                (26000+66000)/2/685000, (27000+49000)/2/685000, (66000+50000)/2/685000, (49000+42000)/2/685000,
                (50000+59000)/2/685000, (42000+42000)/2/685000, (59000+51000+45000)/3/685000, (42000+38000+33000)/3/685000,
                (45000+66000)/2/685000, (33000+51000)/2/685000
  )
)

## Individual contact exposure hours ----
pa.co.eh.tbwe <- pa.co.pa.counts%>%
  filter(!is.na(participant_age)&!is.na(participant_sex)&!is.na(contact_age)&!is.na(contact_sex))%>%
  left_join(pa.tb.inc, by = c("contact_age", "contact_sex"))%>%
  group_by(participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(cont_time*tb_weight))%>%
  left_join(denoms.byagesex.pa, by = c("participant_age", "participant_sex"))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
         contact_age = factor(contact_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))

## Community contact exposure hours ----
par_community_tb <- pa_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Rural")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(par_loc_agesex, by = c("participant_age", "participant_sex","place_visited"))%>%
  left_join(pa.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight)%>%
  group_by(participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(eh_we, na.rm = T))%>%
  mutate(study_site = "Rural")

pau_community_tb <- pa_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Urban")%>%
  filter(!is.na(participant_sex))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(pau_loc_agesex, by = c("participant_age", "participant_sex","place_visited"))%>%
  left_join(pa.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight)%>%
  group_by(participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(eh_we, na.rm = T))%>%
  mutate(study_site = "Urban")

pa_loc_part2 <- pa_loc_ed%>%
  distinct(rec_id, study_day, study_site, participant_age, participant_sex)%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(study_site, participant_age, participant_sex)%>%
  summarise(n = n())

pa_community_tb <- rbind(par_community_tb, pau_community_tb)%>%
  left_join(pa_loc_part2, by = c("study_site", "participant_age", "participant_sex"))%>%
  group_by(participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(eh_we, na.rm = T),
            n = sum(n, na.rm = T))

pa_tb_eh_comb <- rbind(pa.co.eh.tbwe, pa_community_tb)%>%
  group_by(participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(eh_we, na.rm = T),
            n = sum(n, na.rm = T))%>%
  mutate(eh_mean = eh_we/n)%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
         contact_age = factor(contact_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))

## Plot in bar plot ----
pa_tb_eh_comb%>%
  filter(!is.na(participant_age) & !is.na(participant_sex) & !is.na(contact_age) & !is.na(contact_sex))%>%
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
  labs(title = "Pakistan", x = "Participant age and sex", y = "Proportion of exposure-hours")-> pa.agesex.eh.plot

# Figure 3 and Supplemental figure 1: Proportion of exposure-hours for location of community contacts ---------------------------
denoms.byagesex.loc.pa <- pa_loc_ed%>%
  filter(place_visited != "Home")%>%
  filter(!is.na(participant_age))%>%
  filter(!is.na(participant_sex))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(place_visited, participant_age, participant_sex)%>%
  summarise(n_part = n())

par_loc_community_eh <- pa_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Rural")%>%
  filter(!is.na(participant_sex))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(par_loc_agesex, by = c("participant_age", "participant_sex","place_visited"))%>%
  left_join(pa.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight)%>%
  group_by(participant_age, participant_sex, place_visited)%>%
  summarise(ex_hour = sum(eh_we, na.rm = T))

pau_loc_community_eh <- pa_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Urban")%>%
  filter(!is.na(participant_sex))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(pau_loc_agesex, by = c("participant_age", "participant_sex","place_visited"))%>%
  left_join(pa.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight)%>%
  group_by(participant_age, participant_sex, place_visited)%>%
  summarise(ex_hour = sum(eh_we, na.rm = T))

pa_loc_community_eh <- rbind(par_loc_community_eh, pau_loc_community_eh)%>%
  group_by(participant_age, place_visited)%>%
  summarise(ex_hour = sum(ex_hour))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  group_by(participant_age)%>%
  mutate(total = sum(ex_hour),
         prop = ex_hour/total*100)%>%
  mutate(country = "Pakistan")

pa_loc_community_eh_comb <- pa_loc_community_eh%>%
  group_by(place_visited)%>%
  summarise(ex_hour = sum(ex_hour, na.rm = T))%>%
  mutate(total = sum(ex_hour),
         prop = ex_hour/total*100)%>%
  mutate(rank = dense_rank(desc(prop)),
         country = "Pakistan")

# Main text result --------------------------------------------------------------------------
nrow(pa.co)
mean(pa.pa.age$participant_age == "<5y", na.rm = TRUE)
mean(pa.pa$participant_sex == "Female", na.rm = T)
mean(pa.pa$high_educ == "Currently enrolled in school", na.rm = T)
median(pa.pa$hh_size, na.rm = T)
quantile(pa.pa$hh_size, na.rm = T)

## Number of types of contacts
sum(pa_loc_ed$num_pax_place, na.rm = T)

## Supplemental materials ##
# Supplemental table 1: Characteristics of participants ---------------------------------
# Number of participants
nrow(pa.pa.age)

## By age
pa.pa.age%>%
  group_by(participant_age) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

## By sex
pa.pa.age%>%
  group_by(participant_sex) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

# Education
#restrict to non-kids only
pa.pa.age %>%
  filter(participant_age != "<5y")%>%
  group_by(high_educ) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

# Occupation
#restrict to adults only
pa.pa.age %>%
  filter(!participant_age %in% c("<5y", "5-9y"))%>%
  group_by(occupation) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

## Household size
pa.pa.age %>%
  group_by(hh_size_cat) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

quantile(pa.pa.age$hh_size, na.rm = T)

## Generations in households
pa.co %>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>%
  group_by(hh.gens) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

## Multi-family household
pa.co %>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>%
  group_by(hh.multfams) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

# Supplemental table 2: Median number of contacts by participant demographics --------------
pa.we.comb <- rbind(par.we, pau.we)

# add participant data to contact data
pa.co.pa.full <-  full_join(pa.co, pa.pa.age, 
                            by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))%>% #survey_date is complete for all contacts, so this is an indicator variable for whether an observation corresponds to a contact.
  # not all participants reported contacts, so this variable is 0 if the observation represents a participant that did not report any contacts.
  count(study_day, contact, name = "num_contacts")

pa.co.pa <- left_join(pa.pa.age, pa.co.pa.full, by = "rec_id") %>%
  mutate(num_contacts = ifelse(is.na(num_contacts), 0, num_contacts))%>%
  left_join(pa.we.comb%>%select(psweight, participant_age, study_site), by = c("participant_age", "study_site"))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  filter(!is.na(participant_age))

## Close contacts ----
# Contact overall
pa.co.pa %>%
  as_survey(weights = c(psweight))%>%
  summarise(median = survey_median(num_contacts, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts, c(0.25, 0.75), na.rm = T))

# By age
pa.co.pa%>%
  group_by(participant_age)%>%
  summarise(median = median(num_contacts, na.rm = T),
            lower_q = quantile(num_contacts, 0.25, na.rm = T),
            upper_q = quantile(num_contacts, 0.75, na.rm = T))

# By sex
pa.co.pa%>%
  group_by(participant_sex)%>%
  summarise(median = median(num_contacts, na.rm = T),
            lower_q = quantile(num_contacts, 0.25, na.rm = T),
            upper_q = quantile(num_contacts, 0.75, na.rm = T))

## Casual contacts ----
# Contact overall
pa_loc_ed%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(rec_id, study_day, study_site, participant_age)%>%
  summarise(num_pax_place = sum(num_pax_place))%>%
  ungroup()%>%
  left_join(pa.we.comb%>%select(psweight, participant_age, study_site), by = c("participant_age", "study_site"))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  filter(!is.na(participant_age))%>%
  as_survey(weights = c(psweight))%>%
  summarise(median = survey_median(num_pax_place, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_pax_place, c(0.25, 0.75), na.rm = T))

# By age
pa_loc_ed%>%
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
pa_loc_ed%>%
  group_by(rec_id, study_day, study_site, participant_sex)%>%
  summarise(num_pax_place = sum(num_pax_place))%>%
  ungroup()%>%
  group_by(participant_sex)%>%
  summarise(median = median(num_pax_place, na.rm = T),
            lower_q = quantile(num_pax_place, 0.25, na.rm = T),
            upper_q = quantile(num_pax_place, 0.75, na.rm = T))

## Combined contacts ----
pa.svydate <- pa.co%>%
  select(rec_id, study_day, survey_date)%>%
  distinct()

pa_loc_ed%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(rec_id, study_day)%>%
  summarise(num_pax_place = sum(num_pax_place))%>%
  ungroup() -> pa_loc_num

pa.co.pa%>%
  full_join(pa_loc_num, by = c("rec_id", "study_day"))%>%
  mutate(num_contacts = replace_na(num_contacts, 0),
         num_pax_place = replace_na(num_pax_place, 0),
         total_contact = num_contacts + num_pax_place)%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  filter(!is.na(participant_age))%>%
  left_join(pa.svydate, by = c("rec_id", "study_day")) -> pa_combined_count

pa_combined_count%>%
  as_survey(weights = c(psweight))%>%
  summarise(median = survey_median(total_contact, na.rm = T),
            n = survey_total(),
            q = survey_quantile(total_contact, c(0.25, 0.75), na.rm = T))

# Supplemental figure 2: Daily number of contacts with stringency index -----------------------------------------------------------
# Read stringency data
pa.stringency <- stringency%>%
  mutate(survey_date = ymd(Date))%>%
  filter(CountryName == "Pakistan")

## Count the number of contacts per survey date
pa_cont_count <- pa_combined_count%>%
  group_by(survey_date)%>%
  summarise(count = sum(total_contact),
            avg_count = mean(total_contact))

pa_str_plot <- ggplot(pa_cont_count, aes(x = survey_date))+
  geom_bar(aes(y = avg_count), stat = "identity")+
  geom_line(data = pa.stringency, aes(y = StringencyIndex_Average * (max(pa_cont_count$avg_count) / 100)), color = "red")+
  scale_x_date(limits = as.Date(c("2021-01-01", "2024-07-30")),
               breaks = seq(as.Date("2021-05-01"), as.Date("2024-07-30"), by = "6 months"),
               date_labels = "%y-%b") +
  scale_y_continuous(
    #name = "Number of participants",
    sec.axis = sec_axis(~ . * (100 / max(pa_cont_count$avg_count)))) +
  labs(title = "Pakistan") +
  xlab(" ")+
  ylab(" ")+
  theme_minimal()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        title = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 15),
        plot.background = element_rect(color = "white"))
