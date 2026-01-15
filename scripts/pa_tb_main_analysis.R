# TB-analysis - Pakistan

# Read data ------------------------------------------------------------------------------
pa_locdata <- readRDS("./Pakistan/pak_locations_visited_data_aim1.RDS")

pa.pa <- readRDS("./Pakistan/pak_participant_data_aim1.RDS")
pa.co.o <- readRDS("./Pakistan/pak_contact_data_aim1.RDS")
pa.we <- read.csv("./Other/pak_pop.csv")
pa.hh <- read.csv("./Other/pa_hh.csv")


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
                                time_visited == ">4 hrs" ~ runif(n(), min = 240, max = 840)))%>%
  left_join(pa.pa, by = "rec_id")

## Modify data for analysis ----
pa.co <- pa.co.o%>%
  mutate(cont_time = case_when(cont_time == 240 ~ runif(n(), min = 240, max = 840),
                               TRUE ~ cont_time))
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

## Calculate number of participants by age and sex ----
pa.pa.age %>%
  filter(!is.na(participant_age)&!is.na(participant_sex))%>%
  count(participant_age, participant_sex) -> denoms.byagesex.pa

# Create age and household size weighting design --------------------------------
# Extract the participant info
pa.pa.we.ext <- pa.pa.age%>%
  select(rec_id, participant_age, hh_size_cat, study_site)%>%
  distinct()%>%
  filter(!is.na(participant_age) & !is.na(hh_size_cat))

# Modify the hhsize data
pa.hh.ed <- pa.hh%>%
  select(hhsize_cat, n_individuals)%>%
  mutate(hhsize_cat = case_when(hhsize_cat == "0–2" ~ "[0,2]",
                                hhsize_cat == "3–5" ~ "(2,5]",
                                hhsize_cat == "6–10" ~ "(5,10]",
                                hhsize_cat == "11+" ~ "(10,50]",
                                TRUE ~ NA))%>%
  rename(hh_size_cat = hhsize_cat)

# Base design
pa.base.des <- svydesign(
  ids = ~1,
  data = pa.pa.we.ext
)

# Raked design
pa.des.raked <- rake(
  design = pa.base.des,
  sample.margins = list(
    ~participant_age + study_site,
    ~hh_size_cat
  ),
  population.margins = list(
    pa.we %>% select(participant_age, study_site, Freq = pop),
    pa.hh.ed  %>% select(hh_size_cat, Freq = n_individuals)
  )
)

pa.weights <- pa.pa.we.ext %>%
  mutate(final_weight = weights(pa.des.raked)) %>%
  select(rec_id, final_weight)

# Merge back to participant dataset
pa.pa.we <- pa.pa.age%>%
  left_join(pa.weights, by = "rec_id")

# Merge to contact count dataset
pa.co.pa.counts <-  full_join(pa.co.age, pa.pa.we, 
                              by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))

# Figure 1: Exposure-hours by type of contact -----------------------------------------------
pa.ind.type <- pa.co%>%
  left_join(pa.pa.we %>% select(rec_id, final_weight), by = "rec_id") %>%
  group_by(rec_id, study_day, hh_membership)%>%
  summarise(cont_time = sum(cont_time),
            final_weight = first(final_weight),
            .groups = "drop")%>%
  mutate(hh_membership = case_when(hh_membership == "Member" ~ "Close household",
                                   hh_membership == "Non-member" ~ "Close non-household"))

pa.loc.type <- pa_loc_ed%>%
  left_join(pa.pa.we %>% select(rec_id, final_weight), by = "rec_id") %>%
  filter(place_visited != "Home")%>%
  mutate(cont_time = spent_time*num_pax_place)%>%
  group_by(rec_id, study_day)%>%
  summarise(cont_time = sum(cont_time),
            final_weight = first(final_weight),
            .groups = "drop")%>%
  mutate(hh_membership = "Casual")

pa.type.value <- rbind(pa.ind.type, pa.loc.type)%>%
  filter(!is.na(final_weight))%>%
  mutate(ex_hour = cont_time/60)%>% # make minutes from hour
  as_survey(weights = final_weight) %>%
  group_by(hh_membership) %>%
  summarise(
    mean_eh = survey_mean(ex_hour, na.rm = TRUE),
    lower_ci = survey_mean(ex_hour, vartype = "ci", na.rm = TRUE)[[2]],
    upper_ci = survey_mean(ex_hour, vartype = "ci", na.rm = TRUE)[[3]],
    n = survey_total())%>%
  mutate(country = "Pakistan")

# pa.type.value.med <- rbind(pa.ind.type, pa.loc.type)%>%
#   mutate(ex_hour = cont_time/60)%>% # make minutes from hour
#   group_by(hh_membership)%>%
#   summarise(median_eh = median(ex_hour, na.rm = T),
#             lower_percentile = quantile(ex_hour, probs = 0.25, na.rm = T),
#             upper_percentile = quantile(ex_hour, probs = 0.75, na.rm = T))%>%
#   mutate(country = "Pakistan")

## Summary figure is in the summary script

# Figure 2: Exposure profiles by age --------------------------------------
## Calculate age and sex distribution ----

## Create a framework for location dataset ----
pa.loc_frame2 <- pa_loc_ed%>%
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

pa.pa.ext <- pa.pa%>%
  select(rec_id, study_site, participant_age, participant_sex)%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))

pa.loc_frame_p <- pa.loc_frame2%>%
  filter(!is.na(contact_age) & !is.na(contact_sex))%>%
  right_join(pa.pa.ext, by = c("study_site", "participant_age", "participant_sex"))


pa_loc_agesex <- pa_loc_ed%>%
  left_join(pa.pa.we%>%select(rec_id, final_weight), by = "rec_id")%>%
  filter(!is.na(final_weight))%>%
  as_survey(weights = final_weight)%>%
  group_by(participant_age, participant_sex, place_visited, study_site)%>%
  summarise(count = n(),
            spent_time = sum(spent_time, na.rm = T))%>%
  rename(contact_age = participant_age,
         contact_sex = participant_sex)%>%
  group_by(place_visited, study_site)%>%
  mutate(total_eh  = sum(spent_time),
         total_count = sum(count))%>%
  ungroup()%>%
  mutate(agesex_dist = spent_time/total_eh,
         agesex_dist_cnt = count/total_count)%>%
  right_join(pa.loc_frame2, by = c("contact_age", "contact_sex","place_visited", "study_site"))%>%
  mutate(agesex_dist = replace_na(agesex_dist, 0))%>%
  select(participant_age, participant_sex, contact_age, contact_sex, place_visited, study_site, agesex_dist)


## TB-weight ----
pa.tb.inc <- data.frame(
  contact_age = c("<5y", "<5y" ,"5-9y", "5-9y", "10-19y", "10-19y",
                  "20-29y", "20-29y", "30-39y", "30-39y", "40-59y", "40-59y", "60+y", "60+y"),
  contact_sex = c("Male", "Female", "Male", "Female", "Male", "Female",
                  "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"),
  tb_weight = c(22000/685000, 18000/685000, 26000/685000, 27000/685000,
                (26000+66000)/2/685000, (27000+49000)/2/685000, (66000+50000)/2/685000, (49000+42000)/2/685000,
                (50000+59000)/2/685000, (42000+42000)/2/685000, (59000+51000+45000)/3/685000, (42000+38000+33000)/3/685000,
                (45000+66000)/2/685000, (33000+51000)/2/685000),
  tb_lower = rep(0, 14),
  tb_upper = c(50000/685000, 41000/685000, 59000/685000, 59000/685000,
               (59000+148000)/2/685000, (59000+110000)/2/685000, (148000+112000)/2/685000, (110000+93000)/2/685000,
               (112000+132000)/2/685000, (93000+93000)/2/685000, (132000+113000+101000)/3/685000, (93000+84000+75000)/3/685000,
               (101000+148000)/2/685000, (75000+113000)/2/685000)
)

pa.tb.inc$norm_tb_weight <- pa.tb.inc$tb_weight / mean(pa.tb.inc$tb_weight)

## Individual contact exposure hours ----
pa.co.group <- pa.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  distinct(participant_age, participant_sex)%>%
  rename(contact_age = participant_age,
         contact_sex = participant_sex)

pa.loc_frame3 <- pa.pa%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  select(rec_id, participant_age, participant_sex)%>%
  crossing(pa.co.group)%>%
  filter(!is.na(contact_age) & !is.na(contact_sex))


pa.co.eh.tbwe <- pa.co.pa.counts%>%
  filter(!is.na(participant_age)&!is.na(participant_sex)&!is.na(contact_age)&!is.na(contact_sex))%>%
  left_join(pa.tb.inc, by = c("contact_age", "contact_sex"))%>%
  group_by(rec_id, participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(cont_time*tb_weight))%>%
  right_join(pa.loc_frame3, by = c("rec_id", "participant_age", "participant_sex", "contact_age", "contact_sex"))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
         contact_age = factor(contact_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  mutate(eh_we = replace_na(eh_we, 0))


pa_community_tb <- pa_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(pa_loc_agesex, by = c("participant_age", "participant_sex","place_visited", "study_site"))%>%
  left_join(pa.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight)%>%
  filter(!is.na(eh_we))%>%
  right_join(pa.loc_frame_p, by = c("rec_id", "participant_age", "participant_sex", "contact_age", "contact_sex", "study_site", "place_visited"))%>%
  mutate(eh_we = replace_na(eh_we, 0))



pa_tb_eh_comb <- rbind(pa.co.eh.tbwe, pa_community_tb)%>%
  group_by(rec_id, participant_age, contact_age)%>%
  summarise(eh_we = sum(eh_we, na.rm = T))%>%
  filter(!is.na(contact_age) & !is.na(participant_age))%>%
  ungroup()%>%
  left_join(pa.pa.we %>% select(rec_id, final_weight), by = "rec_id")%>%
  filter(!is.na(final_weight))%>%
  group_by(participant_age, contact_age)%>%
  as_survey(weights = final_weight) %>%
  group_by(participant_age, contact_age) %>%
  summarise(eh_mean = survey_mean(eh_we, na.rm = TRUE),
            lower = survey_mean(eh_we, vartype = "ci", na.rm = TRUE)[[2]],
            upper = survey_mean(eh_we, vartype = "ci", na.rm = TRUE)[[3]],
            n = survey_total()
  )%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
         contact_age = factor(contact_age, levels = c("60+y", "40-59y","30-39y", "20-29y", "10-19y", "5-9y", "<5y")))


## Calculate age assortativity ------
age_levels <- c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")

pa_age_mat <- pa_tb_eh_comb%>%
  filter(!is.na(participant_age)&!is.na(contact_age))%>%
  select(participant_age, contact_age, eh_mean)%>%
  pivot_wider(names_from = "participant_age", values_from = "eh_mean")%>%
  arrange(factor(contact_age, levels = age_levels)) %>%
  select(contact_age, all_of(age_levels))

pa_age_mat$contact_age <- NULL
rownames(pa_age_mat) <- colnames(pa_age_mat)

pa_age_ass <- sam_index_q(pa_age_mat)



## Plot in bar plot ----
pa_tb_eh_comb%>%
  filter(!is.na(participant_age) & !is.na(contact_age))%>%
  ggplot(aes(x = participant_age, y = eh_mean, fill = contact_age)) +
  geom_bar(
    position = "fill",
    stat = "identity",
    color = "black"
  ) +
  scale_fill_manual(name = "Contact Age",
                    values = c("pink", "#8073AC","#4393C3","#7FBFBD","#91CF60","#FDAE61","#D6604D")
  ) +
  theme_minimal()+
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 9),
        legend.key.size = unit(0.5, "cm"),
        legend.key = element_rect(fill = "white", colour = "black"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        title = element_text(size = 9)
  )+
  theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5))+
  labs(title = paste0("Pakistan, Q = ", signif(pa_age_ass, 2)), x = "Participant age", y = "Proportion of exposure-hours")-> pa.age.eh.plot


# Supplemental figure 1: Exposure matrix by sex ----
# Individual contact
pa.co.eh.tbwe2 <- pa.co.pa.counts%>%
  filter(!is.na(participant_age)&!is.na(participant_sex)&!is.na(contact_age)&!is.na(contact_sex))%>%
  left_join(pa.tb.inc, by = c("contact_age", "contact_sex"))%>%
  group_by(rec_id, participant_age, participant_sex, contact_age, contact_sex)%>%
  summarise(eh_we = sum(cont_time/60*norm_tb_weight))%>%
  right_join(pa.loc_frame3, by = c("rec_id", "participant_age", "participant_sex", "contact_age", "contact_sex"))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")),
         contact_age = factor(contact_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  mutate(eh_we = replace_na(eh_we, 0))

# Community contact
pa_community_tb2 <- pa_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(pa_loc_agesex, by = c("participant_age", "participant_sex","place_visited", "study_site"))%>%
  left_join(pa.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time/60*num_pax_place*agesex_dist*norm_tb_weight)%>%
  filter(!is.na(eh_we))%>%
  right_join(pa.loc_frame_p, by = c("rec_id", "participant_age", "participant_sex", "contact_age", "contact_sex", "study_site", "place_visited"))%>%
  mutate(eh_we = replace_na(eh_we, 0))


pa_tb_eh_mat_sex <- rbind(pa.co.eh.tbwe2, pa_community_tb2)%>%
  filter(!is.na(study_day))%>%
  group_by(rec_id, participant_sex, contact_sex, study_day)%>%
  summarise(eh_we = sum(eh_we, na.rm = T))%>%
  left_join(pa.pa.we %>% select(rec_id, final_weight), by = "rec_id")%>%
  filter(!is.na(final_weight))%>%
  group_by(participant_sex, contact_sex)%>%
  as_survey(weights = final_weight) %>%
  group_by(participant_sex, contact_sex) %>%
  summarise(eh_mean = survey_mean(eh_we, na.rm = TRUE),
            ci_lower = survey_mean(eh_we, vartype = "ci", na.rm = TRUE)[[2]],
            ci_upper = survey_mean(eh_we, vartype = "ci", na.rm = TRUE)[[3]],
            n = survey_total())

### Calculate assortativity ----------------------------
pa_sex_mat <- pa_tb_eh_mat_sex%>%
  select(participant_sex, contact_sex, eh_mean)%>%
  pivot_wider(names_from = "participant_sex", values_from = "eh_mean")

pa_sex_mat$contact_sex <- NULL
rownames(pa_sex_mat) <- colnames(pa_sex_mat)

pa_sex_ass <- index_q(pa_sex_mat)


### Plot matrix --------------------------------
pa_tb_eh_mat_sex%>%
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
    size = 4, 
    bg.r = 0.15)+
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        title = element_text(size = 10))+
  labs(title = paste0("Pakistan, Q = ", signif(pa_sex_ass, 2)), x = "Participant sex", y = "Contact sex") -> pa.mat.sex.plot


## Proportion of exposure-hours to women vs men
rbind(pa.co.eh.tbwe, pa_community_tb)%>%
  group_by(rec_id, participant_age, contact_sex)%>%
  summarise(eh_we = sum(eh_we, na.rm = T))%>%
  filter(!is.na(contact_sex) & !is.na(participant_age))%>%
  ungroup()%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  left_join(pa.pa.we %>% select(rec_id, final_weight), by = "rec_id") %>%
  filter(!is.na(final_weight))%>%
  as_survey(weights = final_weight)%>%
  group_by(participant_age, contact_sex)%>%
  summarise(eh_mean = survey_mean(eh_we, na.rm = TRUE),
            lower = survey_mean(eh_we, vartype = "ci", na.rm = TRUE)[[2]],
            upper = survey_mean(eh_we, vartype = "ci", na.rm = TRUE)[[3]],
            n = survey_total()
  )%>%
  group_by(participant_age)%>%
  mutate(total = sum(eh_mean))%>%
  group_by(participant_age, contact_sex)%>%
  mutate(prop = eh_mean/total)



## check
pa.co.exp <- pa.co%>%
  group_by(rec_id, study_day)%>%
  summarise(contact = n())%>%
  right_join(pa.pa, by = "rec_id")%>%
  mutate(contact = replace_na(contact, 0))

pa.co.exp%>%
  group_by(participant_sex)%>%
  summarise(cr = mean(contact))

pa.loc.exp <- pa_loc_ed%>%
  group_by(rec_id, study_day)%>%
  summarise(contact = sum(num_pax_place, na.rm = T))%>%
  right_join(pa.pa, by = "rec_id")%>%
  mutate(contact = replace_na(contact, 0))
pa.loc.exp%>%
  group_by(participant_sex)%>%
  summarise(cr = mean(contact))



# Figure 3: Proportion of exposure-hours for location of community contacts ---------------------------
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



pa_loc_community_eh <- pa_loc_ed%>%
  select(rec_id, place_visited, participant_age, participant_sex, spent_time, num_pax_place, study_day, study_site)%>%
  filter(place_visited != "Home")%>%
  filter(!is.na(participant_sex))%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age))%>%
  left_join(pa_loc_agesex, by = c("participant_age", "participant_sex","place_visited", "study_site"))%>%
  left_join(pa.tb.inc, by = c("contact_age", "contact_sex"))%>%
  mutate(eh_we = spent_time*num_pax_place*agesex_dist*tb_weight,
         eh_we_low = spent_time*num_pax_place*agesex_dist*tb_lower,
         eh_we_up = spent_time*num_pax_place*agesex_dist*tb_upper)%>%
  left_join(pa.pa.we %>% select(rec_id, final_weight), by = "rec_id") %>%
  filter(!is.na(final_weight))%>%
  as_survey(weights = final_weight) %>%
  group_by(participant_age, place_visited) %>%
  summarise(
    ex_hour      = survey_total(eh_we, na.rm = TRUE),
    ex_hour_low  = survey_total(eh_we_low, na.rm = TRUE),
    ex_hour_up   = survey_total(eh_we_up, na.rm = TRUE),
    n            = survey_total()
  ) %>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  group_by(participant_age)%>%
  mutate(total = sum(ex_hour),
         prop = ex_hour/total,
         total_low = sum(ex_hour_low),
         prop_low = ex_hour_low/total_low,
         total_up = sum(ex_hour_up),
         prop_up = ex_hour_up/total_up)%>%
  mutate(country = "Pakistan")


# Main text result --------------------------------------------------------------------------
nrow(pa.co)
mean(pa.pa.age$participant_age == "<5y", na.rm = TRUE)
mean(pa.pa$participant_sex == "Female", na.rm = T)
mean(pa.pa$high_educ == "Currently enrolled in school", na.rm = T)
median(pa.pa$hh_size, na.rm = T)
quantile(pa.pa$hh_size, na.rm = T)

## Number of types of contacts
sum(pa_loc_ed$num_pax_place, na.rm = T)

### Count the number of casual contacts by the number of people present -------------------
pa_loc_ed%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age),
         num_pax_place = case_when(num_pax_place < 50 ~ "<50",
                                   num_pax_place >= 50 ~ "50+",
                                   TRUE ~ NA))%>%
  filter(!is.na(place_visited) & !is.na(num_pax_place))%>%
  group_by(place_visited, num_pax_place)%>%
  summarise(count = n())%>%
  mutate(prop = count/sum(count)*100)%>%
  mutate(country = "Pakistan") -> pa_casual_table

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
# add participant data to contact data
pa.co.pa.full <-  full_join(pa.co, pa.pa.age, 
                            by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))%>% #survey_date is complete for all contacts, so this is an indicator variable for whether an observation corresponds to a contact.
  # not all participants reported contacts, so this variable is 0 if the observation represents a participant that did not report any contacts.
  count(study_day, contact, name = "num_contacts")

pa.co.pa <- left_join(pa.pa.we, pa.co.pa.full, by = "rec_id") %>%
  mutate(num_contacts = ifelse(is.na(num_contacts), 0, num_contacts))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  filter(!is.na(participant_age))

## Close contacts ----
# Contact overall
pa.co.pa %>%
  as_survey(weights = final_weight)%>%
  summarise(median = survey_median(num_contacts, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts, c(0.25, 0.75), na.rm = T))

# By age
pa.co.pa%>%
  as_survey(weights = final_weight)%>%
  group_by(participant_age)%>%
  summarise(median = survey_median(num_contacts, na.rm = T),
            q = survey_quantile(num_contacts, c(0.25, 0.75), na.rm = T))

# By sex
pa.co.pa%>%
  as_survey(weights = final_weight)%>%
  group_by(participant_sex)%>%
  summarise(median = survey_median(num_contacts, na.rm = T),
            q = survey_quantile(num_contacts, c(0.25, 0.75), na.rm = T))

### Statistical test for contact's sex difference ------------------------------
pa.co.pa.test <- pa.co.pa%>%
  as_survey(weights = final_weight)%>%
  filter(!is.na(participant_sex) & !is.na(final_weight))

svyranktest(num_contacts ~ participant_sex, design = pa.co.pa.test)

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
  left_join(pa.pa.we%>%select(rec_id, final_weight), by = "rec_id")%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  filter(!is.na(participant_age) & !is.na(final_weight))%>%
  as_survey(weights = final_weight)%>%
  summarise(median = survey_median(num_pax_place, na.rm = T),
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
  left_join(pa.pa.we%>%select(rec_id, final_weight), by = "rec_id")%>%
  filter(!is.na(final_weight))%>%
  as_survey(weights = final_weight)%>%
  group_by(participant_age)%>%
  summarise(median = survey_median(num_pax_place, na.rm = T),
            q = survey_quantile(num_pax_place, c(0.25, 0.75), na.rm = T))

# By sex
pa_loc_ed%>%
  group_by(rec_id, study_day, study_site, participant_sex)%>%
  summarise(num_pax_place = sum(num_pax_place))%>%
  ungroup()%>%
  left_join(pa.pa.we%>%select(rec_id, final_weight), by = "rec_id")%>%
  filter(!is.na(final_weight))%>%
  as_survey(weights = final_weight)%>%
  group_by(participant_sex)%>%
  summarise(median = survey_median(num_pax_place, na.rm = T),
            q = survey_quantile(num_pax_place, c(0.25, 0.75), na.rm = T))

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
  as_survey(weights = final_weight)%>%
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
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 10),
        title = element_text(size = 10),
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 7),
        plot.background = element_rect(color = "white"))

# Supplemental figure 3 ----------------------------------------------------------------------
## Panel A ----------------------------------------------------------------
# Prepare Prem et al. data
pa.prem <- prem%>%
  filter(iso3c == "PAK" & setting == "overall" & location_contact == "all")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")))

pa.prem.table <- pa.prem%>%
  group_by(participant_age)%>%
  summarise(contact_rate = sum(contact_rate))%>%
  mutate(country = "Pakistan")

# Prepare GlobalMix data
pa.gm.table <- pa_combined_count%>%
  filter(!is.na(final_weight))%>%
  mutate(participant_age = case_when(participant_age == "<5y" ~ "0-4y",
                                     TRUE ~ participant_age))%>%
  as_survey(weights = final_weight)%>%
  group_by(participant_age)%>%
  summarise(contact_rate = survey_mean(total_contact, na.rm = T),
            q = survey_quantile(total_contact, c(0.25, 0.75), na.rm = T))%>%
  mutate(participant_age = factor(participant_age, levels = c("0-4y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y"))) %>%
  arrange(participant_age)%>%
  rename(lower_ci = q_q25,
         upper_ci = q_q75)%>%
  mutate(country = "Pakistan")

# Take midpoint of age group
pa.prem.table$age_midpoint <- sapply(pa.prem.table$participant_age, get_midpoint)
pa.gm.table$age_midpoint <- sapply(pa.gm.table$participant_age, get_midpoint)

# Add a data source column
pa.prem.table <- pa.prem.table%>%
  mutate(dataset = "Prem et al., 2021",
         lower_ci = NA,
         upper_ci = NA)
pa.gm.table<- pa.gm.table%>%
  mutate(dataset = "GlobalMix")%>%
  # mutate(dataset = case_when(study_site.x == "Rural" ~ "GlobalMix, rural",
  #                            study_site.x == "Urban" ~ "GlobalMix, urban"))%>%
  ungroup()%>%
  dplyr::select(participant_age, contact_rate, country, age_midpoint, dataset, lower_ci, upper_ci)

# Combine the datasets
pa.age.table <- rbind(pa.prem.table, pa.gm.table)%>%
  mutate(dataset = factor(dataset, levels = c("Prem et al., 2021", "GlobalMix")))
  # mutate(dataset = factor(dataset, levels = c("Prem et al., 2021", "GlobalMix, rural", "GlobalMix, urban")))

# Plot the line graph
pa.age.plot <- ggplot(pa.age.table, aes(x = age_midpoint, y = contact_rate, color = dataset)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5, size = 0.7) +
  scale_x_continuous("Participant age", breaks = seq(0, 75, by = 5)) +
  #scale_y_continuous("Contact Rate") +
  #ylim(0,25)+
  labs(title = "Pakistan",
       color = "Dataset") +
  ylab("Contact Rate")+
  scale_color_manual(values = c("Prem et al., 2021" = "orange", "GlobalMix" = "steelblue3"))+
  # scale_color_manual(values = c("Prem et al., 2021" = "sienna", "GlobalMix, rural" = 'aquamarine4', "GlobalMix, urban" = "steelblue3"))+
  theme_minimal()+
  theme(axis.text = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        axis.title = element_text(size = 7),
        title = element_text(size = 7))


## Panel B --------------------------------------------------------------------------
# Prepare Prem et al. data
p_pak_school <- prem%>%
  filter(iso3c == "PAK" & setting == "overall" & location_contact == "school")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)
p_pak_work <- prem%>%
  filter(iso3c == "PAK" & setting == "overall" & location_contact == "work")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)
p_pak_home <- prem%>%
  filter(iso3c == "PAK" & setting == "overall" & location_contact == "home")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)
p_pak_other <- prem%>%
  filter(iso3c == "PAK" & setting == "overall" & location_contact == "others")%>%
  rename(country = iso3c,
         participant_age = age_contactor,
         contact_age = age_cotactee,
         contact_rate = mean_number_of_contacts)

# Combine the location
p_pak_home_lab <- p_pak_home%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Home")
p_pak_school_lab <- p_pak_school%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "School")
p_pak_work_lab <- p_pak_work%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Work")
p_pak_other_lab <- p_pak_other%>%
  mutate(participant_age = gsub(" to ", "-", participant_age),
         participant_age = ifelse(participant_age == "75+", "75+y", paste0(participant_age, "y")),
         contact_age = gsub(" to ", "-", contact_age),
         contact_age = ifelse(contact_age == "75+", "75+y", paste0(contact_age, "y")),
         location = "Other")

p_pak_loc <- rbind(p_pak_home_lab, p_pak_school_lab, p_pak_work_lab, p_pak_other_lab)%>%
  group_by(location)%>%
  summarise(contact_rate = sum(contact_rate))%>%
  mutate(contact_rate_tot = sum(contact_rate),
         percentage = contact_rate/contact_rate_tot,
         country = "Pakistan")

# Prepare GlobalMix data
pa.loc.count <- pa_loc_ed%>%
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
  left_join(pa.pa.we%>%select(rec_id, final_weight), by = "rec_id")%>%
  mutate(country = "Pakistan")%>%
  select(rec_id, participant_age, location, count, study_site, country, final_weight)


pa_location <- pa.co%>%
  mutate(location = case_when(location == "Transit" ~ "Other",
                              location == "Market / essential" ~ "Other",
                              location == "Other social / leisure" ~ "Other",
                              location == "Worship" ~ "Other",
                              TRUE ~ location))%>%
  left_join(pa.pa, by = "rec_id")%>%
  rename(study_site = study_site.x)%>%
  dplyr::select(rec_id, study_site, participant_age, participant_sex, contact_age, contact_sex, location)%>%
  mutate(participant_age = case_when(participant_age == "<6mo" ~ "<5y",
                                     participant_age == "6-11mo" ~ "<5y",
                                     participant_age == "1-4y" ~ "<5y",
                                     TRUE ~ participant_age),
         count = 1)%>%
  left_join(pa.pa.we%>%select(rec_id, final_weight), by = "rec_id")%>%
  mutate(country = "Pakistan")%>%
  select(rec_id, participant_age, location, count, study_site, country, final_weight)

pa.loc.comb <- rbind(pa_location, pa.loc.count)
