# TB-analysis script - Mozambique

# Read data ----------------------------------------------------------------------
mo_locdata <- readRDS("./Mozambique/moz_locations_visited_data_aim1.RDS")

mo.pa <- readRDS("./Mozambique/moz_participant_data_aim1.RDS")
mo.co.o <- readRDS("./Mozambique/moz_contact_data_aim1.RDS")
mo.we <- read.csv("./Other/moz_pop.csv")
mo.hh <- read.csv("./Other/mo_hh.csv")


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
                                time_visited == ">4 hrs" ~ runif(n(), min = 240, max = 840)),
         rec_id = as.factor(rec_id))%>%
  left_join(mo.pa, by = "rec_id")

## Modify data for analysis ----
mo.co <- mo.co.o%>%
  mutate(cont_time = case_when(cont_time == 240 ~ runif(n(), min = 240, max = 840),
                               TRUE ~ cont_time))
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


## Calculate number of participants by age and sex ----
mo.pa.age %>%
  filter(!is.na(participant_age)&!is.na(participant_sex))%>%
  count(participant_age, participant_sex) -> denoms.byagesex.mo

# Create age and household size weighting design --------------------------------
# Extract the participant info
mo.pa.we.ext <- mo.pa.age%>%
  select(rec_id, participant_age, hh_size_cat, study_site)%>%
  distinct()%>%
  filter(!is.na(participant_age) & !is.na(hh_size_cat))

# Modify the hhsize data
mo.hh.ed <- mo.hh%>%
  select(hhsize_cat, n_individuals)%>%
  mutate(hhsize_cat = case_when(hhsize_cat == "0–2" ~ "[0,2]",
                                hhsize_cat == "3–5" ~ "(2,5]",
                                hhsize_cat == "6–10" ~ "(5,10]",
                                hhsize_cat == "11+" ~ "(10,50]",
                                TRUE ~ NA))%>%
  rename(hh_size_cat = hhsize_cat)

# Base design
mo.base.des <- svydesign(
  ids = ~1,
  data = mo.pa.we.ext
)

# Raked design
mo.des.raked <- rake(
  design = mo.base.des,
  sample.margins = list(
    ~participant_age + study_site,
    ~hh_size_cat
  ),
  population.margins = list(
    mo.we %>% select(participant_age, study_site, Freq = pop),
    mo.hh.ed  %>% select(hh_size_cat, Freq = n_individuals)
  )
)

mo.weights <- mo.pa.we.ext %>%
  mutate(final_weight = weights(mo.des.raked)) %>%
  select(rec_id, final_weight)

# Merge back to participant dataset
mo.pa.we <- mo.pa.age%>%
  left_join(mo.weights, by = "rec_id")

# Merge to contact count dataset
mo.co.pa.counts <-  full_join(mo.co.age, mo.pa.we, 
                              by = c("rec_id", "study_site")) %>%
  mutate(contact = ifelse(is.na(survey_date), 0, 1))

# Figure 1: Exposure-hours by type of contact -----------------------------------------------
mo.ind.type <- mo.co%>%
  left_join(mo.pa.we %>% select(rec_id, final_weight), by = "rec_id") %>%
  group_by(rec_id, study_day, hh_membership)%>%
  summarise(cont_time = sum(cont_time),
            final_weight = first(final_weight),
            .groups = "drop")%>%
  mutate(hh_membership = case_when(hh_membership == "Member" ~ "Close household",
                                   hh_membership == "Non-member" ~ "Close non-household"))

mo.loc.type <- mo_loc_ed%>%
  left_join(mo.pa.we %>% select(rec_id, final_weight), by = "rec_id") %>%
  filter(place_visited != "Home")%>%
  mutate(cont_time = spent_time*num_pax_place)%>%
  group_by(rec_id, study_day)%>%
  summarise(cont_time = sum(cont_time),
            final_weight = first(final_weight),
            .groups = "drop")%>%
  mutate(hh_membership = "Casual")

mo.type.value <- rbind(mo.ind.type, mo.loc.type)%>%
  filter(!is.na(final_weight))%>%
  mutate(ex_hour = cont_time/60)%>% # make minutes from hour
  as_survey(weights = final_weight) %>%
  group_by(hh_membership) %>%
  summarise(
    mean_eh = survey_mean(ex_hour, na.rm = TRUE),
    lower_ci = survey_mean(ex_hour, vartype = "ci", na.rm = TRUE)[[2]],
    upper_ci = survey_mean(ex_hour, vartype = "ci", na.rm = TRUE)[[3]],
    n = survey_total())%>%
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


mo_loc_agesex <- mo_loc_ed%>%
  left_join(mo.pa.we%>%select(rec_id, final_weight), by = "rec_id")%>%
  filter(!is.na(final_weight))%>%
  as_survey(weights = final_weight)%>%
  group_by(participant_age, participant_sex, place_visited, study_site)%>%
  summarise(count = n(),
            spent_time = survey_total(spent_time, na.rm = T))%>%
  rename(contact_age = participant_age,
         contact_sex = participant_sex)%>%
  group_by(place_visited, study_site)%>%
  mutate(total_eh  = sum(spent_time),
         total_count = sum(count))%>%
  ungroup()%>%
  mutate(agesex_dist = spent_time/total_eh,
         agesex_dist_cnt = count/total_count)%>%
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
  left_join(mo.pa.we %>% select(rec_id, final_weight), by = "rec_id")%>%
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


## Calculate age assortativity -------
age_levels <- c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")

mo_age_mat <- mo_tb_eh_comb%>%
  select(participant_age, contact_age, eh_mean)%>%
  pivot_wider(names_from = "participant_age", values_from = "eh_mean")%>%
  arrange(factor(contact_age, levels = age_levels)) %>%
  select(contact_age, all_of(age_levels))

mo_age_mat$contact_age <- NULL
rownames(mo_age_mat) <- colnames(mo_age_mat)

mo_age_ass <- sam_index_q(mo_age_mat)


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
  labs(title = paste0("Mozambique, Q = ", signif(mo_age_ass, 2)), x = "Participant age", y = "Proportion of exposure-hours")-> mo.age.eh.plot

# Supplemental figure 1: Exposure matrix by sex ----
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
  left_join(mo.pa.we %>% select(rec_id, final_weight), by = "rec_id")%>%
  filter(!is.na(final_weight))%>%
  group_by(participant_sex, contact_sex)%>%
  as_survey(weights = final_weight) %>%
  group_by(participant_sex, contact_sex) %>%
  summarise(eh_mean = survey_mean(eh_we, na.rm = TRUE),
            ci_lower = survey_mean(eh_we, vartype = "ci", na.rm = TRUE)[[2]],
            ci_upper = survey_mean(eh_we, vartype = "ci", na.rm = TRUE)[[3]],
            n = survey_total())


### Calculate assortativity -------------------------
mo_sex_mat <- mo_tb_eh_mat_sex%>%
  select(participant_sex, contact_sex, eh_mean)%>%
  pivot_wider(names_from = "participant_sex", values_from = "eh_mean")

mo_sex_mat$contact_sex <- NULL
rownames(mo_sex_mat) <- colnames(mo_sex_mat)

mo_sex_ass <- index_q(mo_sex_mat)


### Plot matrix --------------------------------------
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
    size = 4, 
    bg.r = 0.15)+
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        title = element_text(size = 10))+
  labs(title = paste0("Mozambique, Q = ", signif(mo_sex_ass, 2)), x = "Participant sex", y = "Contact sex") -> mo.mat.sex.plot



## Proportion of exposure-hours to women vs men
rbind(mo.co.eh.tbwe, mo_community_tb)%>%
  group_by(rec_id, participant_age, contact_sex)%>%
  summarise(eh_we = sum(eh_we, na.rm = T))%>%
  filter(!is.na(contact_sex) & !is.na(participant_age))%>%
  ungroup()%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  left_join(mo.pa.we %>% select(rec_id, final_weight), by = "rec_id") %>%
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


## Contact rate
mo.co.exp <- mo.co%>%
  group_by(rec_id, study_day)%>%
  summarise(contact = n())%>%
  right_join(mo.pa, by = "rec_id")%>%
  mutate(contact = replace_na(contact, 0))

mo.co.exp%>%
  group_by(participant_sex)%>%
  summarise(cr = mean(contact))

mo.loc.exp <- mo_loc_ed%>%
  group_by(rec_id, study_day)%>%
  summarise(contact = sum(num_pax_place, na.rm = T))%>%
  right_join(mo.pa, by = "rec_id")%>%
  mutate(contact = replace_na(contact, 0))
mo.loc.exp%>%
  group_by(participant_sex)%>%
  summarise(cr = mean(contact))


# Figure 3: Proportion of exposure-hours for location of community contacts ---------------------------
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
  left_join(mo.pa.we %>% select(rec_id, final_weight), by = "rec_id") %>%
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

### Count the number of casual contacts by the number of people present -------------------
mo_loc_ed%>%
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
  mutate(country = "Mozambique") -> mo_casual_table

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

mo.co.pa <- left_join(mo.pa.we, mo.co.pa.full, by = "rec_id") %>%
  mutate(num_contacts = ifelse(is.na(num_contacts), 0, num_contacts))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  filter(!is.na(participant_age) & !is.na(final_weight))


## Close contacts ----
# Contact overall
mo.co.pa %>%
  as_survey(weights = final_weight)%>%
  summarise(median = survey_median(num_contacts, na.rm = T),
            n = survey_total(),
            q = survey_quantile(num_contacts, c(0.25, 0.75), na.rm = T))

# By age
mo.co.pa%>%
  as_survey(weights = final_weight)%>%
  group_by(participant_age)%>%
  summarise(median = survey_median(num_contacts, na.rm = T),
            q = survey_quantile(num_contacts, c(0.25, 0.75), na.rm = T))

# By sex
mo.co.pa%>%
  as_survey(weights = final_weight)%>%
  group_by(participant_sex)%>%
  summarise(median = survey_median(num_contacts, na.rm = T),
            q = survey_quantile(num_contacts, c(0.25, 0.75), na.rm = T))

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
  left_join(mo.pa.we%>%select(rec_id, final_weight), by = "rec_id")%>%
  filter(!is.na(final_weight))%>%
  mutate(participant_age = factor(participant_age, levels = c("<5y", "5-9y", "10-19y", "20-29y", "30-39y", "40-59y", "60+y")))%>%
  filter(!is.na(participant_age))%>%
  as_survey(weights = final_weight)%>%
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
  left_join(mo.pa.we%>%select(rec_id, final_weight), by = "rec_id")%>%
  filter(!is.na(final_weight))%>%
  as_survey(weights = final_weight)%>%
  group_by(participant_age)%>%
  summarise(median = survey_median(num_pax_place, na.rm = T),
            q = survey_quantile(num_pax_place, c(0.25, 0.75), na.rm = T))

# By sex
mo_loc_ed%>%
  group_by(rec_id, study_day, study_site, participant_sex)%>%
  summarise(num_pax_place = sum(num_pax_place))%>%
  ungroup()%>%
  left_join(mo.pa.we%>%select(rec_id, final_weight), by = "rec_id")%>%
  filter(!is.na(final_weight))%>%
  as_survey(weights = final_weight)%>%
  group_by(participant_sex)%>%
  summarise(median = survey_median(num_pax_place, na.rm = T),
            q = survey_quantile(num_pax_place, c(0.25, 0.75), na.rm = T))

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
  as_survey(weights = final_weight)%>%
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
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 10),
        title = element_text(size = 10),
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 7),
        plot.background = element_rect(color = "white"))

# Suppplemental figure 3 -----------------------------------------
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
  geom_point(size = 1.5) +
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
  theme(axis.text = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        axis.title = element_text(size = 7),
        title = element_text(size = 7))

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
  left_join(mo.pa.we%>%select(rec_id, final_weight), by = "rec_id")%>%
  mutate(country = "Mozambique")%>%
  select(rec_id, participant_age, location, count, study_site, country, final_weight)


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
  left_join(mo.pa.we%>%select(rec_id, final_weight), by = "rec_id")%>%
  mutate(country = "Mozambique")%>%
  select(rec_id, participant_age, location, count, study_site, country, final_weight)

mo.loc.comb <- rbind(mo_location, mo.loc.count)

