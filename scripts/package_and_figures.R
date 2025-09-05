# Package
pacman::p_load(dplyr, ggplot2, tidyr, scales, srvyr, survey, shadowtext, ggpubr, gridExtra, ggpattern, lubridate, patchwork)

# Functions

# Calculate assortativity -----------------------------------------------------------
index_q <- function(m){
  m = m/sum(m)
  colsum <- colSums(m)
  diagsum = 0
  colsumsq = 0
  for(i in 1:nrow(m)){
    diagsum <- diagsum + m[i,i]
    colsumsq <- colsumsq + (colsum[i]^2)
  }
  
  r <- (diagsum - colsumsq) / (1 - colsumsq)
  return(as.numeric(r))
}



## Taking the adjacent cells into account
sam_index_q <- function(m){
  m = m/sum(m)
  colsum <- colSums(m)
  diagsum = 0
  colsumsq = 0
  for(i in 1:nrow(m)){
    diagsum <- diagsum + m[i,i]
    if(i < nrow(m)){
      diagsum <- diagsum + m[i+1,i] + m[i,i+1]
    }
    colsumsq <- colsumsq + (colsum[i]^2)
  }
  
  r <- (diagsum - 1) / (nrow(m) - 1)
  return(as.numeric(r))
}

# Function to convert age categories to midpoints
get_midpoint <- function(participant_age) {
  age_range <- strsplit(gsub("[^0-9-]", "", participant_age), "-")[[1]]
  if (length(age_range) == 1) {
    return(as.numeric(age_range[1]) + 1)  # for "60+y", assuming 60 is the start
  } else {
    return(mean(as.numeric(age_range)))
  }
}

# Summary figures
# Figure 1: Exposure-hours by type of contact ---------------------------------------
rbind(gt.type.value, in.type.value, mo.type.value, pa.type.value)%>%
  ggplot(aes(x = country, y = mean_eh, fill = hh_membership))+
  geom_bar(position = position_dodge(width = 0.9), stat = "identity")+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 0.9), width = 0.2) +
  scale_fill_manual(values = c("Casual" = "tan1", "Close household" = "royalblue1", "Close non-household" = "skyblue1")) +
  #scale_y_continuous(limits = c(0, 100))+
  theme_minimal()+
  theme(plot.margin = margin(t = 20, r = 10, b = 10, l = 10))+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        title = element_text(size = 20),
        legend.position = "inside",
        legend.position.inside = c(0.12, 0.85), 
        #legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "white", color = "gray80"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)
  )+
  labs(x = "Country", y = "Mean daily exposure hours", fill = "Types of contacts") -> mean.type.contact


# Figure 2: Exposure profiles by age and sex ---------------------------------------
ggarrange(gt.age.eh.plot, in.age.eh.plot, mo.age.eh.plot, pa.age.eh.plot, nrow = 2, ncol = 2, common.legend = T, legend = "right") -> age.plot

# Figure 3: Proportion of exposure-hours by location of community contacts ----------
## Significant digits formatting function
format_sig2 <- function(x) {
  formatted <- signif(x, 2)
  # Convert to character with conditional formatting
  sapply(formatted, function(val) {
    str_val <- format(val, scientific = FALSE, trim = TRUE)
    # Add trailing zero if only one decimal digit
    if (grepl("^\\d+\\.\\d$", str_val)) {
      paste0(str_val, "0")
    } else {
      str_val
    }
  })
}


rbind(gt_loc_community_eh_comb, in_loc_community_eh_comb, mo_loc_community_eh_comb, pa_loc_community_eh_comb)%>%
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 0.45))) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        title = element_text(size = 20),
        plot.background = element_rect(color = "black", linewidth = 2))+
  scale_fill_distiller(palette = "Blues",
                       direction = 1,
                       #limits=c(0, 5), 
                       name = "Proportion") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = format_sig2(prop)),    
                  color = "black", 
                  bg.color = "white", 
                  size = 8, 
                  bg.r = 0.15) +
  labs(x = "", y = "Location", title = "Overall")+
  scale_x_discrete(labels = label_wrap(10)) -> loc.prop.plot


## Proportion of exposure-hours by location of community contacts by age -------------
# <5
rbind(gt_loc_community_eh, in_loc_community_eh, mo_loc_community_eh, pa_loc_community_eh)%>%
  filter(participant_age == "<5y")%>%
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 0.45))) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        title = element_text(size = 20))+
  scale_fill_distiller(palette = "Blues",
                       direction = 1,
                       #limits=c(0, 5), 
                       name = "Proportion") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = format_sig2(prop)),  
                  color = "black", 
                  bg.color = "white", 
                  size = 8, 
                  bg.r = 0.15) +
  labs(x = "", y = "Location", title = "<5 years old")+
  scale_x_discrete(labels = label_wrap(10)) -> loc.prop.u5.plot

# 5-9y
rbind(gt_loc_community_eh, in_loc_community_eh, mo_loc_community_eh, pa_loc_community_eh,
      tibble(
        participant_age = "5-9y",
        place_visited = "Work",
        ex_hour = 0,
        total = 0,
        prop = 0,
        country = "Guatemala"
      ))%>%
  mutate(place_visited = factor(place_visited, levels = c("Other home", "Market/essential", 
                                                          "Other social/leisure", "School", "Transit","Work", "Worship")))%>%
  filter(participant_age == "5-9y")%>%
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 0.45))) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        title = element_text(size = 20))+
  scale_fill_distiller(palette = "Blues",
                       direction = 1,
                       #limits=c(0, 5), 
                       name = "Proportion") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = format_sig2(prop)),  
                  color = "black", 
                  bg.color = "white", 
                  size = 8, 
                  bg.r = 0.15) +
  labs(x = "", y = "Location", title = "5-9 years old")+
  scale_x_discrete(labels = label_wrap(10)) -> loc.prop.5to9.plot

# 10-19y
rbind(gt_loc_community_eh, in_loc_community_eh, mo_loc_community_eh, pa_loc_community_eh)%>%
  filter(participant_age == "10-19y")%>%
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 0.45))) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        title = element_text(size = 20))+
  scale_fill_distiller(palette = "Blues",
                       direction = 1,
                       #limits=c(0, 5), 
                       name = "Proportion") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = format_sig2(prop)),  
                  color = "black", 
                  bg.color = "white", 
                  size = 8, 
                  bg.r = 0.15) +
  labs(x = "", y = "", title = "10-19 years old")+
  scale_x_discrete(labels = label_wrap(10)) -> loc.prop.10to19.plot

# 20-29y
rbind(gt_loc_community_eh, in_loc_community_eh, mo_loc_community_eh, pa_loc_community_eh)%>%
  filter(participant_age == "20-29y")%>%
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 0.45))) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        title = element_text(size = 20))+
  scale_fill_distiller(palette = "Blues",
                       direction = 1,
                       #limits=c(0, 5), 
                       name = "Proportion") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = format_sig2(prop)),  
                  color = "black", 
                  bg.color = "white", 
                  size = 8, 
                  bg.r = 0.15) +
  labs(x = "", y = "Location", title = "20-29 years old")+
  scale_x_discrete(labels = label_wrap(10)) -> loc.prop.20to29.plot

# 30-39y
rbind(gt_loc_community_eh, in_loc_community_eh, mo_loc_community_eh, pa_loc_community_eh,
      tibble(
        participant_age = "30-39y",
        place_visited = "School",
        ex_hour = 0,
        total = 0,
        prop = 0,
        country = "Mozambique"
      ))%>%
  filter(participant_age == "30-39y")%>%
  mutate(place_visited = factor(place_visited, levels = c("Other home", "Market/essential", 
                                                          "Other social/leisure", "School", "Transit","Work", "Worship")))%>%
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 0.45))) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        title = element_text(size = 20))+
  scale_fill_distiller(palette = "Blues",
                       direction = 1,
                       #limits=c(0, 5), 
                       name = "Proportion") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = format_sig2(prop)),  
                  color = "black", 
                  bg.color = "white", 
                  size = 8, 
                  bg.r = 0.15) +
  labs(x = "", y = "", title = "30-39 years old")+
  scale_x_discrete(labels = label_wrap(10)) -> loc.prop.30to39.plot

# 40-59y
rbind(gt_loc_community_eh, in_loc_community_eh, mo_loc_community_eh, pa_loc_community_eh)%>%
  filter(participant_age == "40-59y")%>%
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 0.45))) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        title = element_text(size = 20))+
  scale_fill_distiller(palette = "Blues",
                       direction = 1,
                       #limits=c(0, 5), 
                       name = "Proportion") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = format_sig2(prop)),  
                  color = "black", 
                  bg.color = "white", 
                  size = 8, 
                  bg.r = 0.15) +
  labs(x = "Country", y = "Location", title = "40-59 years old")+
  scale_x_discrete(labels = label_wrap(10)) -> loc.prop.40to59.plot

# 60+y
rbind(gt_loc_community_eh, in_loc_community_eh, mo_loc_community_eh, pa_loc_community_eh,
      tibble(
        participant_age = "60+y",
        place_visited = "School",
        ex_hour = 0,
        total = 0,
        prop = 0,
        country = "Mozambique"
      ))%>%
  filter(participant_age == "60+y")%>%
  mutate(place_visited = factor(place_visited, levels = c("Other home", "Market/essential", 
                                                          "Other social/leisure", "School", "Transit","Work", "Worship")))%>%
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 0.45))) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        title = element_text(size = 20))+
  scale_fill_distiller(palette = "Blues",
                       direction = 1,
                       #limits=c(0, 5), 
                       name = "Proportion") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = format_sig2(prop)),  
                  color = "black", 
                  bg.color = "white", 
                  size = 8, 
                  bg.r = 0.15) +
  labs(x = "Country", y = "", title = "60+ years old")+
  scale_x_discrete(labels = label_wrap(10)) -> loc.prop.m60.plot

## Combine all the age plot ----
ggarrange(loc.prop.plot,loc.prop.u5.plot, loc.prop.5to9.plot, loc.prop.10to19.plot, loc.prop.20to29.plot, loc.prop.30to39.plot, loc.prop.40to59.plot, loc.prop.m60.plot,
          nrow = 4, ncol = 2) -> loc.prop.comb.plot

# Supplemental figure 1: Exposure-hours matrix by sex -------------
ggarrange(gt.mat.sex.plot, in.mat.sex.plot, mo.mat.sex.plot, pa.mat.sex.plot, nrow = 2, ncol = 2) -> mat.sex


# Supplemental figure 2: Daily number of contacts reported with stringency index ----
str_plot <- grid.arrange(mo_str_plot, gt_str_plot, in_str_plot, pa_str_plot, ncol = 1, 
                         #top = textGrob("A", x = 0, just = "left", gp = gpar(fontsize = 30)),
                         left = textGrob("Daily Mean Number of Contacts", rot = 90, just = "centre", gp = gpar(fontsize = 20)),
                         right = textGrob("Stringency Index (Average)", rot = 270, just = "centre", gp = gpar(fontsize = 20)),
                         bottom = textGrob("Survey Date", just = "centre", gp = gpar(fontsize = 20)))


# Comparison with Prem data -------------------------------------------------------
# Panel A
title_grob <- textGrob("A",
                       gp = gpar(fontsize = 30, fontface = "bold"),
                       just = "left",
                       x = 0.02,
                       y = 0.5)

age_line_plot <- grid.arrange(gt.age.plot, in.age.plot, mo.age.plot, pa.age.plot,ncol = 2, top = title_grob)

age_line_plot_2 <- (gt.age.plot + in.age.plot) / (mo.age.plot + pa.age.plot) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


# age_line_plot_2 <- wrap_elements(
#     full = (
#       (gt.age.plot + in.age.plot) / (mo.age.plot + pa.age.plot) +
#         plot_layout(guides = "collect") &
#         theme(legend.position = "bottom")
#     )
#   ) +
#   plot_annotation(title = title_grob)


        
# Panel B
# Combine four country
p_com_loc <- rbind(p_gt_loc, p_ind_loc, p_moz_loc,p_pak_loc)%>%
  mutate(location = factor(location, levels = c("Home", "School", "Work", "Other")),
         dataset = "Prem et al., 2021")%>%
  select(country, location, percentage, dataset)

gm_com_loc <- rbind(gt.loc.comb, in.loc.comb, mo.loc.comb, pa.loc.comb)%>%
  mutate(location = factor(location, levels = c("Home", "School", "Work", "Other", "Unreported")))%>%
  filter(!is.na(psweight))%>%
  as_survey(weights = c(psweight))%>%
  group_by(country, location)%>%
  summarise(count = survey_total(count))%>%
  mutate(percentage = count/sum(count),
         dataset = "GlobalMix")%>%
  select(country, location, percentage, dataset)

# Combine the two datasets
loc_combined <- rbind(gm_com_loc, p_com_loc)%>%
  mutate(country = factor(country, levels = c("Guatemala", "India",  "Mozambique", "Pakistan")))

# Plot
loc_combined_plot <- ggplot(loc_combined, aes(x = dataset, y = percentage, fill = location))+
  geom_bar(stat = "identity", position = "fill")+
  facet_wrap(~ country, nrow = 1) +
  labs(y = "Proportion", x = "Dataset", fill = "Contact location") +
  #labs(y = "Proportion", x = "Dataset", fill = "Contact location", title = "B") +
  scale_fill_manual(values = c("Home"="#F8766D",
                               "Market / essential" = "#E69F00",
                               "Other" = "#7CAE00",
                               "School" = "#00BFC4",
                               "Transit" = "#56B4E9",
                               "Work" = "#C77CFF",
                               "Worship" = "#F564E3",
                               "Unreported" = "#8B4513"))+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white", color = NA),
        #plot.title = element_text(hjust = 0, size = 30, face = "bold")
        )+
  theme(axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        #title = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.position = "top")

# Two-panel plot
multi_plot <- age_line_plot_2 | loc_combined_plot


## Exposure-hours version ------------------------------------
# Panel A
# title_grob <- textGrob("A",
#                        gp = gpar(fontsize = 30, fontface = "bold"),
#                        just = "left",
#                        x = 0.02,
#                        y = 0.5)
# 
# age_line_plot <- grid.arrange(gt.eh.comp.plot, in.eh.comp.plot, mo.eh.comp.plot, pa.eh.comp.plot,ncol = 2, top = title_grob)

age_line_plot_eh <- (gt.eh.comp.plot + in.eh.comp.plot) / (mo.eh.comp.plot + pa.eh.comp.plot) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")



# Panel B
# Combine four country
p_com_loc <- rbind(p_gt_loc, p_ind_loc, p_moz_loc,p_pak_loc)%>%
  mutate(location = factor(location, levels = c("Home", "School", "Work", "Other")),
         dataset = "Prem et al., 2021")%>%
  select(country, location, percentage, dataset)

gm_com_eh_loc <- rbind(gt.loc.eh.comb, in.loc.eh.comb, mo.loc.eh.comb, pa.loc.eh.comb)%>%
  mutate(location = factor(location, levels = c("Home", "School", "Work", "Other", "Unreported")))%>%
  filter(!is.na(psweight))%>%
  filter(!is.na(ex_hour))%>%
  rename(count = ex_hour)%>%
  as_survey(weights = c(psweight))%>%
  group_by(country, location)%>%
  summarise(count = survey_total(count))%>%
  mutate(percentage = count/sum(count),
         dataset = "GlobalMix")%>%
  select(country, location, percentage, dataset)

# Combine the two datasets
loc_eh_combined <- rbind(gm_com_eh_loc, p_com_loc)%>%
  mutate(country = factor(country, levels = c("Guatemala", "India",  "Mozambique", "Pakistan")))

# Plot
loc_eh_combined_plot <- ggplot(loc_eh_combined, aes(x = dataset, y = percentage, fill = location))+
  geom_bar(stat = "identity", position = "fill")+
  facet_wrap(~ country, nrow = 1) +
  labs(y = "Proportion", x = "Dataset", fill = "Contact location") +
  #labs(y = "Proportion", x = "Dataset", fill = "Contact location", title = "B") +
  scale_fill_manual(values = c("Home"="#F8766D",
                               "Market / essential" = "#E69F00",
                               "Other" = "#7CAE00",
                               "School" = "#00BFC4",
                               "Transit" = "#56B4E9",
                               "Work" = "#C77CFF",
                               "Worship" = "#F564E3",
                               "Unreported" = "#8B4513"))+
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white", color = NA),
        #plot.title = element_text(hjust = 0, size = 30, face = "bold")
  )+
  theme(axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        #title = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.position = "top")

# Two-panel plot
multi_eh_plot <- age_line_plot_eh | loc_eh_combined_plot



# Saving figures
ggsave("C:/Users/mshiiba/OneDrive - Emory/Emory University/PHPA/GlobalMix/TB-analysis/output/TB-analysis-Share/sfig1.png", plot = mat.sex, dpi = 300, width = 16, height = 12)
