# Package
pacman::p_load(dplyr, ggplot2, tidyr, scales, srvyr, survey, shadowtext, ggpubr, gridExtra, ggpattern, lubridate)

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
        legend.text = element_text(size = 16))+
  labs(title = "Types of Contacts", x = "Country", y = "Mean daily exposure hours", fill = "Types of contacts") -> mean.type.contact


# Figure 2: Exposure profiles by age and sex ---------------------------------------
ggarrange(gt.age.eh.plot, in.age.eh.plot, mo.age.eh.plot, pa.age.eh.plot, nrow = 2, ncol = 2, common.legend = T, legend = "right") -> age.plot

## Location of community contacts ##
# Figure 3: Proportion of exposure-hours by location of community contacts ----------
rbind(gt_loc_community_eh_comb, in_loc_community_eh_comb, mo_loc_community_eh_comb, pa_loc_community_eh_comb)%>%
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 35))) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 22))+
  scale_fill_distiller(palette = "Blues",
                       direction = 1,
                       #limits=c(0, 5), 
                       name = "Proportion") +
  geom_tile(color = "white", show.legend = FALSE,
            lwd = 1.5,
            linetype = 1) +
  geom_shadowtext(aes(label = sprintf("%.1f", prop)),  
                  color = "black", 
                  bg.color = "white", 
                  size = 10, 
                  bg.r = 0.15) +
  labs(x = "Country", y = "Location", title = "")+
  scale_x_discrete(labels = label_wrap(10)) -> loc.prop.plot


# Supplemental figure 1: Proportion of exposure-hours by location of community contacts by age ----
# <5
rbind(gt_loc_community_eh, in_loc_community_eh, mo_loc_community_eh, pa_loc_community_eh)%>%
  filter(participant_age == "<5y")%>%
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 45))) +
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
  geom_shadowtext(aes(label = sprintf("%.1f", prop)),  
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
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 45))) +
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
  geom_shadowtext(aes(label = sprintf("%.1f", prop)),  
                  color = "black", 
                  bg.color = "white", 
                  size = 8, 
                  bg.r = 0.15) +
  labs(x = "", y = "", title = "5-9 years old")+
  scale_x_discrete(labels = label_wrap(10)) -> loc.prop.5to9.plot

# 10-19y
rbind(gt_loc_community_eh, in_loc_community_eh, mo_loc_community_eh, pa_loc_community_eh)%>%
  filter(participant_age == "10-19y")%>%
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 45))) +
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
  geom_shadowtext(aes(label = sprintf("%.1f", prop)),  
                  color = "black", 
                  bg.color = "white", 
                  size = 8, 
                  bg.r = 0.15) +
  labs(x = "", y = "Location", title = "10-19 years old")+
  scale_x_discrete(labels = label_wrap(10)) -> loc.prop.10to19.plot

# 20-29y
rbind(gt_loc_community_eh, in_loc_community_eh, mo_loc_community_eh, pa_loc_community_eh)%>%
  filter(participant_age == "20-29y")%>%
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 45))) +
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
  geom_shadowtext(aes(label = sprintf("%.1f", prop)),  
                  color = "black", 
                  bg.color = "white", 
                  size = 8, 
                  bg.r = 0.15) +
  labs(x = "", y = "", title = "20-29 years old")+
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
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 45))) +
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
  geom_shadowtext(aes(label = sprintf("%.1f", prop)),  
                  color = "black", 
                  bg.color = "white", 
                  size = 8, 
                  bg.r = 0.15) +
  labs(x = "", y = "Location", title = "30-39 years old")+
  scale_x_discrete(labels = label_wrap(10)) -> loc.prop.30to39.plot

# 40-59y
rbind(gt_loc_community_eh, in_loc_community_eh, mo_loc_community_eh, pa_loc_community_eh)%>%
  filter(participant_age == "40-59y")%>%
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 45))) +
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
  geom_shadowtext(aes(label = sprintf("%.1f", prop)),  
                  color = "black", 
                  bg.color = "white", 
                  size = 8, 
                  bg.r = 0.15) +
  labs(x = "Country", y = "", title = "40-59 years old")+
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
  ggplot(aes(x = country, y = place_visited, fill = pmin(prop, 45))) +
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
  geom_shadowtext(aes(label = sprintf("%.1f", prop)),  
                  color = "black", 
                  bg.color = "white", 
                  size = 8, 
                  bg.r = 0.15) +
  labs(x = "Country", y = "", title = "60+ years old")+
  scale_x_discrete(labels = label_wrap(10)) -> loc.prop.m60.plot

## Combine all the age plot ----
ggarrange(loc.prop.u5.plot, loc.prop.5to9.plot, loc.prop.10to19.plot, loc.prop.20to29.plot, loc.prop.30to39.plot, loc.prop.40to59.plot, loc.prop.m60.plot,
          nrow = 4, ncol = 2) -> loc.prop.age.plot

# Supplemental figure 2: Exposure-hours matrix by sex -------------
ggarrange(gt.mat.sex.plot, in.mat.sex.plot, mo.mat.sex.plot, pa.mat.sex.plot, nrow = 2, ncol = 2) -> mat.sex


# Supplemental figure 3: Daily number of contacts reported with stringency index ----
str_plot <- grid.arrange(mo_str_plot, gt_str_plot, in_str_plot, pa_str_plot, ncol = 1, 
                         #top = textGrob("A", x = 0, just = "left", gp = gpar(fontsize = 30)),
                         left = textGrob("Daily Mean Number of Contacts", rot = 90, just = "centre", gp = gpar(fontsize = 20)),
                         right = textGrob("Stringency Index (Average)", rot = 270, just = "centre", gp = gpar(fontsize = 20)),
                         bottom = textGrob("Survey Date", just = "centre", gp = gpar(fontsize = 20)))
