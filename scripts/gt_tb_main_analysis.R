# TB-analysis script - Guatemala

# Read data ---------------------------------------------------------------------
gt_locdata <- readRDS("C:/Users/mshiiba/OneDrive - Emory/3.Global Mix/Globalmix clean data/Guatemala/aim_1/gt_locations_visited_data_aim1.RDS")

gt.pa <- readRDS("./Guatemala/gt_participant_data_aim1.RDS")
gt.co <- readRDS("./Guatemala/gt_contact_data_aim1.RDS")
gt.we <- read.csv("./Other/gt_pop.csv")

stringency <- read.csv("./Other/OxCGRT_compact_national_v1.csv", header = T)

# Clean the data ----------------------------------------------------------------
# Standardize the data across countries and merge participants information
gt_loc_ed <- gt_locdata%>%
  mutate(place_visited = case_when(place_visited == "Market/Shop" ~ "Market/essential",
                                   place_visited == "My home" ~ "Home",
                                   place_visited == "Other" ~ "Other",
                                   place_visited == "Other home" ~ "Other home",
                                   place_visited == "Park" ~ "Other social/leisure",
                                   place_visited == "Place of worship" ~ "Worship",
                                   place_visited == "Public services" ~ "Market/essential",
                                   place_visited == "School" ~ "School",
                                   place_visited == "Street" ~ "Transit",
                                   place_visited == "Transport/Hub" ~ "Transit",
                                   place_visited == "Well" ~ "Market/essential",
                                   place_visited == "Work" ~  "Work",
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
  left_join(gt.pa, by = "rec_id")

## Modify the dataset necessary for analysis -------------------------------------
# Merge the <5y group
gt.pa.age <- gt.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age),
         participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))

gt.co.age <- gt.co%>%
  mutate(contact_age = case_when(contact_age == "<6mo" ~ "<5y",
                                 contact_age == "6-11mo" ~ "<5y",
                                 contact_age == "1-4y" ~ "<5y",
                                 TRUE ~ contact_age))

gt.co.pa.counts <-  full_join(gt.co.age, gt.pa.age, 
                              by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))


## Number of participants by age and sex ----
gt.pa.age %>%
  filter(!is.na(participant_age) & !is.na(participant_sex))%>%
  count(participant_age, participant_sex) -> denoms.byagesex.gt

# Figure 1: Exposure-hours by type of contact -----------------------------------------------
gt.ind.type <- gt.co%>%
  group_by(rec_id, study_day, hh_membership)%>%
  summarise(cont_time = sum(cont_time))%>%
  mutate(hh_membership = case_when(hh_membership == "Member" ~ "Close household",
                                   hh_membership == "Non-member" ~ "Close non-household"))

gt.loc.type <- gt_loc_ed%>%
  filter(place_visited != "Home")%>%
  mutate(cont_time = spent_time*num_pax_place)%>%
  group_by(rec_id, study_day)%>%
  summarise(cont_time = sum(cont_time))%>%
  mutate(hh_membership = "Casual")

gt.type.value <- rbind(gt.ind.type, gt.loc.type)%>%
  mutate(ex_hour = cont_time/60)%>% # make minutes from hour
  group_by(hh_membership)%>%
  summarise(mean_eh = mean(ex_hour, na.rm = T),
            sd_eh = sd(ex_hour, na.rm = TRUE),
            n = sum(!is.na(ex_hour)),
            se = sd_eh / sqrt(n),
            lower_ci = mean_eh - 1.96 * se,
            upper_ci = mean_eh + 1.96 * se
  )%>%
  mutate(country = "Guatemala")

gt.type.value.med <- rbind(gt.ind.type, gt.loc.type)%>%
  mutate(ex_hour = cont_time/60)%>% # make minutes from hour
  group_by(hh_membership)%>%
  summarise(median_eh = median(ex_hour, na.rm = T),
            lower_percentile = quantile(ex_hour, probs = 0.25, na.rm = T),
            upper_percentile = quantile(ex_hour, probs = 0.75, na.rm = T))%>%
  mutate(country = "Guatemala")

## Summary figure is in the summary script


# Figure 2 and supp 2: Exposure profiles by age and sex --------------------------------------
## Calculate age and sex distribution ----
gt.pa.we <- gt.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(study_site, participant_age)%>%
  summarise(n_s = n())%>%
  mutate(prop_s = n_s/sum(n_s))

gtr.we <- gt.we%>%
  left_join(gt.pa.we, by = c("participant_age", "study_site"))%>%
  mutate(psweight = prop/prop_s)%>%
  filter(study_site == "Rural")
gtu.we <- gt.we%>%
  left_join(gt.pa.we, by = c("participant_age", "study_site"))%>%
  mutate(psweight = prop/prop_s)%>%
  filter(study_site == "Urban")

## Create a framework for location dataset ----
loc_frame2 <- gt_loc_ed%>%
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


gtr_loc_we <- gt_loc_ed%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Rural")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(gtr.we, by = "participant_age")%>%
  as_survey_design(weights = psweight)

gtr_loc_agesex <- gtr_loc_we%>%
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


gtu_loc_we <- gt_loc_ed%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Urban")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(gtu.we, by = "participant_age")%>%
  as_survey_design(weights = psweight)

gtu_loc_agesex <- gtu_loc_we%>%
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
gt.tb.inc <- data.frame(
  contact_age = c("<5y", "<5y" ,"5-9y", "5-9y", "10-19y", "10-19y",
                  "20-29y", "20-29y", "30-39y", "30-39y", "40-59y", "40-59y", "60+y", "60+y"),
  contact_sex = c("Male", "Female", "Male", "Female", "Male", "Female",
                  "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"),
  tb_weight = c(210/5900, 180/5900, 250/5900, 250/5900, (250+430)/2/5900, (250+290)/2/5900,
                (430+690)/2/5900, (290+400)/2/5900, (690+580)/2/5900, (400+370)/2/5900,
                (580+430+380)/3/5900, (370+380+370)/3/5900,(380+400)/2/5900, (370+290)/2/5900)
)


## Individual contact exposure hours ----
gt.co.eh.tbwe <- gt.co.pa.counts%>%
  filter(!is.na(participant_age)&!is.na(participant_sex)&!is.na(contact_age)&!is.na(contact_sex))%>%
  left_join(gt.tb.inc, by = c("contact_age", "contact_sex"))%>%
  group_by(participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(cont_time*tb_weight))%>%
  left_join(denoms.byagesex.gt, by = c("participant_age", "participant_sex"))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
         contact_age = factor(contact_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))

## Community contact exposure hours ----
gtr_community_tb <- gt_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Rural")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(gtr_loc_agesex, by = c("participant_age", "participant_sex","place_visited"))%>%
  left_join(gt.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight)%>%
  group_by(participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(eh_we, na.rm = T))%>%
  mutate(study_site = "Rural")


gtu_community_tb <- gt_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Urban")%>%
  filter(!is.na(participant_sex))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(gtu_loc_agesex, by = c("participant_age", "participant_sex","place_visited"))%>%
  left_join(gt.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight)%>%
  group_by(participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(eh_we, na.rm = T))%>%
  mutate(study_site = "Urban")

gt_loc_part2 <- gt_loc_ed %>%
  distinct(rec_id, study_day, study_site, participant_age, participant_sex)%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(study_site, participant_age, participant_sex)%>%
  summarise(n = n())

gt_community_tb <- rbind(gtr_community_tb, gtu_community_tb)%>%
  left_join(gt_loc_part2, by = c("study_site", "participant_age", "participant_sex"))%>%
  group_by(participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(eh_we, na.rm = T),
            n = sum(n, na.rm = T))

gt_tb_eh_comb <- rbind(gt.co.eh.tbwe, gt_community_tb)%>%
  group_by(participant_age, contact_age)%>%
  summarise(eh_we = sum(eh_we, na.rm = T),
            n = sum(n, na.rm = T))%>%
  mutate(eh_mean = eh_we/n)%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
         contact_age = factor(contact_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))

## Plot in bar plot ----
gt_tb_eh_comb%>%
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
  labs(title = "Guatemala", x = "Participant age", y = "Proportion of exposure-hours")-> gt.age.eh.plot


## Exposure matrix by sex ----
gt_tb_eh_mat_sex <- rbind(gt.co.eh.tbwe, gt_community_tb)%>%
  group_by(participant_sex, contact_sex)%>%
  summarise(eh_we = sum(eh_we, na.rm = T),
            n = sum(n, na.rm = T))%>%
  mutate(eh_mean = eh_we/n)

gt_tb_eh_mat_sex%>%
  ggplot(aes(x = participant_sex, y = contact_sex, fill = eh_mean))+
  scale_fill_distiller(palette = "Blues",
                       direction = 1,
                       #limits=c(0, 5), 
                       name = "Proportion") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1)+
  geom_shadowtext(aes(label = sprintf("%.1f", eh_mean)),  
                  color = "black", 
                  bg.color = "white", 
                  size = 10, 
                  bg.r = 0.15)+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        title = element_text(size = 18))+
  labs(title = "Guatemala", x = "Participant sex", y = "Contact sex") -> gt.mat.sex.plot


## Calculate assortativity ----
age_levels <- c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")

gt_age_mat <- gt_tb_eh_comb%>%
  select(participant_age, contact_age, eh_mean)%>%
  pivot_wider(names_from = "participant_age", values_from = "eh_mean")%>%
  arrange(factor(contact_age, levels = age_levels)) %>%
  select(contact_age, all_of(age_levels))

gt_age_mat$contact_age <- NULL
rownames(gt_age_mat) <- colnames(gt_age_mat)

gt_age_ass <- sam_index_q(gt_age_mat)

gt_sex_mat <- gt_tb_eh_mat_sex%>%
  select(participant_sex, contact_sex, eh_mean)%>%
  pivot_wider(names_from = "participant_sex", values_from = "eh_mean")

gt_sex_mat$contact_sex <- NULL
rownames(gt_sex_mat) <- colnames(gt_sex_mat)

gt_sex_ass <- index_q(gt_sex_mat)


# Figure 3 and Supplemental figure 1: Proportion of exposure-hours for location of community contacts ---------------------------
gtr_loc_community_eh <- gt_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Rural")%>%
  filter(!is.na(participant_sex))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(gtr_loc_agesex, by = c("participant_age", "participant_sex","place_visited"))%>%
  left_join(gt.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight)%>%
  group_by(participant_age, participant_sex, place_visited)%>%
  summarise(ex_hour = sum(eh_we, na.rm = T))

gtu_loc_community_eh <- gt_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  filter(study_site == "Urban")%>%
  filter(!is.na(participant_sex))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(gtu_loc_agesex, by = c("participant_age", "participant_sex","place_visited"))%>%
  left_join(gt.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight)%>%
  group_by(participant_age, participant_sex, place_visited)%>%
  summarise(ex_hour = sum(eh_we, na.rm = T))

gt_loc_community_eh <- rbind(gtr_loc_community_eh, gtu_loc_community_eh)%>%
  group_by(participant_age, place_visited)%>%
  summarise(ex_hour = sum(ex_hour))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  group_by(participant_age)%>%
  mutate(total = sum(ex_hour),
         prop = ex_hour/total*100)%>%
  mutate(country = "Guatemala")

gt_loc_community_eh_comb <- gt_loc_community_eh%>%
  group_by(place_visited)%>%
  summarise(ex_hour = sum(ex_hour, na.rm = T))%>%
  mutate(total = sum(ex_hour),
         prop = ex_hour/total*100)%>%
  mutate(rank = dense_rank(desc(prop)),
         country = "Guatemala")


# Main text input --------------------------------------------------------------------
nrow(gt.co)
mean(gt.pa.age$participant_age == "<5y", na.rm = TRUE)
mean(gt.pa$participant_sex == "Female", na.rm = T)
mean(gt.pa$high_educ == "Currently enrolled in school", na.rm = T)
median(gt.pa$hh_size, na.rm = T)
quantile(gt.pa$hh_size, na.rm = T)

## Number of types of contacts
sum(gt_loc_ed$num_pax_place, na.rm = T)

## Supplemental materials ##
# Supplemental table 1: Characteristics of participants ---------------------------------
# Number of participants
nrow(gt.pa.age)

## By age
gt.pa.age%>%
  group_by(participant_age) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

## By sex
gt.pa.age%>%
  group_by(participant_sex) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

# Education
#restrict to non-kids only
gt.pa.age %>%
  filter(participant_age != "<5y")%>%
  group_by(high_educ) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

# Occupation
#restrict to adults only
gt.pa.age %>%
  filter(!participant_age %in% c("<5y", "5-9y"))%>%
  group_by(occupation) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

## Household size
gt.pa.age %>%
  filter(!participant_age %in% c("<5y", "5-9y"))%>%
  group_by(hh_size_cat) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

quantile(gt.pa.age$hh_size, na.rm = T)

## Generations in households
gt.co %>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>%
  group_by(hh.gens) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

## Multi-family household
gt.co %>%
  arrange(rec_id) %>%
  filter(row_number()==1) %>%
  group_by(hh.multfams) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

# Supplemental table 2: Median number of contacts by participant demographics --------------
gt.we.comb <- rbind(gtr.we, gtu.we)

# add participant data to contact data
gt.co.pa.full <-  full_join(gt.co, gt.pa.age, 
                            by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))%>% #survey_date is complete for all contacts, so this is an indicator variable for whether an observation corresponds to a contact.
  # not all participants reported contacts, so this variable is 0 if the observation represents a participant that did not report any contacts.
  count(study_day, contact, name = "num_contacts")%>%
  filter(contact == 1)

gt.co.pa <- left_join(gt.pa.age, gt.co.pa.full, by = "rec_id") %>%
  mutate(num_contacts = ifelse(is.na(num_contacts), 0, num_contacts))%>%
  left_join(gt.we.comb%>%select(psweight, participant_age, study_site), by = c("participant_age", "study_site"))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))


## Close contacts ----
# Contact overall
gt.co.pa %>%
  as_survey(weights = c(psweight))%>%
  summarise(median = survey_median(num_contacts, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts, c(0.25, 0.75), na.rm = T))

# By age
gt.co.pa%>%
  group_by(participant_age)%>%
  summarise(median = median(num_contacts, na.rm = T),
            lower_q = quantile(num_contacts, 0.25, na.rm = T),
            upper_q = quantile(num_contacts, 0.75, na.rm = T))

# By sex
gt.co.pa%>%
  group_by(participant_sex)%>%
  summarise(median = median(num_contacts, na.rm = T),
            lower_q = quantile(num_contacts, 0.25, na.rm = T),
            upper_q = quantile(num_contacts, 0.75, na.rm = T))

## Casual contacts ----
# Contact overall
gt_loc_ed%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(rec_id, study_day, study_site, participant_age)%>%
  summarise(num_pax_place = sum(num_pax_place))%>%
  ungroup()%>%
  left_join(gt.we.comb%>%select(psweight, participant_age, study_site), by = c("participant_age", "study_site"))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  filter(!is.na(participant_age))%>%
  as_survey(weights = c(psweight))%>%
  summarise(median = survey_median(num_pax_place, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_pax_place, c(0.25, 0.75), na.rm = T))

# By age
gt_loc_ed%>%
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
gt_loc_ed%>%
  group_by(rec_id, study_day, study_site, participant_sex)%>%
  summarise(num_pax_place = sum(num_pax_place))%>%
  ungroup()%>%
  group_by(participant_sex)%>%
  summarise(median = median(num_pax_place, na.rm = T),
            lower_q = quantile(num_pax_place, 0.25, na.rm = T),
            upper_q = quantile(num_pax_place, 0.75, na.rm = T))

## Combined contacts ----
gt.svydate <- gt.co%>%
  select(rec_id, study_day, survey_date)%>%
  distinct()

gt_loc_ed%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  group_by(rec_id, study_day)%>%
  summarise(num_pax_place = sum(num_pax_place))%>%
  ungroup() -> gt_loc_num

gt.co.pa%>%
  full_join(gt_loc_num, by = c("rec_id", "study_day"))%>%
  mutate(num_contacts = replace_na(num_contacts, 0),
         num_pax_place = replace_na(num_pax_place, 0),
         total_contact = num_contacts + num_pax_place)%>%
  left_join(gt.svydate, by = c("rec_id", "study_day"))-> gt_combined_count 

gt_combined_count%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  filter(!is.na(participant_age))%>%
  as_survey(weights = c(psweight))%>%
  summarise(median = survey_median(total_contact, na.rm = T),
            n = survey_total(),
            q = survey_quantile(total_contact, c(0.25, 0.75), na.rm = T))

# Supplemental figure 3: Daily number of contacts with stringency index -----------------------------------------------------------
gt.stringency <- stringency%>%
  mutate(survey_date = ymd(Date))%>%
  filter(CountryName == "Guatemala")

## Count the number of contacts per survey date
gt_cont_count <- gt_combined_count%>%
  group_by(survey_date)%>%
  summarise(count = sum(total_contact),
            avg_count = mean(total_contact))

gt_str_plot <- ggplot(gt_cont_count, aes(x = survey_date))+
  geom_bar(aes(y = avg_count), stat = "identity")+
  geom_line(data = gt.stringency, aes(y = StringencyIndex_Average * (max(gt_cont_count$avg_count) / 100)), color = "red")+
  scale_x_date(limits = as.Date(c("2021-01-01", "2024-07-30")),
               breaks = seq(as.Date("2021-05-01"), as.Date("2024-07-30"), by = "6 months"),
               date_labels = "%y-%b") +
  scale_y_continuous(
    #name = "Number of participants",
    sec.axis = sec_axis(~ . * (100 / max(gt_cont_count$avg_count)))) +
  labs(title = "Guatemala") +
  xlab(" ")+
  ylab(" ")+
  theme_minimal()+
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20),
        title = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 17),
        plot.background = element_rect(color = "white"))
