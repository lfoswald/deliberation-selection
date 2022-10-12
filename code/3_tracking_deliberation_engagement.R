############### Density Plots - Engagement with different Sites

source("code/tracking_packages.R")

# load preprocessed data

user_data_deliberation <- read_excel("data/temp/user_data_deliberation_regr_1.xlsx")

# Duration density  
all_duration <- data.frame(
  Overall=user_data_deliberation$online_duration/1560,
  
  Niche=user_data_deliberation$c2_duration/1560,
  Quality=user_data_deliberation$c1_duration/1560,
  Hubs=user_data_deliberation$c3_duration/1560,
  
  Change=user_data_deliberations$change_duration/1560,
  Twitter=user_data_deliberations$twitter_duration/1560,
  Facebook=user_data_deliberations$facebook_duration/1560,
  Bild=user_data_deliberations$bild_duration/1560,
  Zeit=user_data_deliberations$zeit_duration/1560,
  
  Communication=user_data_deliberation$com_duration/1560,
  Information=user_data_deliberation$info_duration/1560,
  Political=user_data_deliberation$pol_duration/1560)

#### Barplots users vs. non-users

all_duration_na <- data.frame(apply(all_duration, 2, function(x) ifelse(x > 0, 1, 0)))
all_duration_na_rs <- melt(all_duration_na)

all_duration_na_rs$variable <- factor(all_duration_na_rs$variable, levels = c(
  "Political","Information","Communication", "Zeit", "Bild","Facebook","Twitter",
  "Change","Hubs","Quality","Niche","Overall"
   ))

bars <- ggplot(all_duration_na_rs, aes(x = value, fill = factor(variable)))+
  geom_bar(aes(alpha = factor(value)))+
  stat_count(aes(label= round(..count../1282*100,1)), vjust=0.5, 
             geom="text", position="identity", size = 3.5)+
  facet_grid(variable~.)+
  scale_fill_manual(values = c("#17377A","#C5D86D",
                               "#C5D86D", "#DB2763",
                               "#DB2763",
                               "#DB2763",
                               "#DB2763",
                               "#DB2763", "#FC471E",
                               "#FC471E",
                               "#FC471E","#628395"))+
  scale_alpha_manual(values = c(0.3,0.6))+
  theme_minimal()+
  ggtitle("Percent usage")+
  guides(fill = "none", alpha = "none")+
  scale_x_discrete(name = "", limits = c(0,1), labels = c("non users", "users"),
                   expand = c(0.01,0.01))+
  theme(axis.ticks.y=element_blank(), 
        axis.text.y=element_blank(),
        title = element_text(vjust = 2),
        axis.text.x=element_text(size = 10, colour = "black"),
        axis.title.y=element_blank(),
        strip.text = element_blank(),
        panel.border=element_rect(colour = "black",fill=NA, size=0.5),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
  
bars
#ggsave("output/bars_usage.pdf", width = 2, height = 10, dpi = 300, bars)


###### Density Plot - Duration

all_duration_rs <- melt(all_duration)
all_duration_rs$value[all_duration_rs$value < 0.1] <- NA
all_duration_rs <- na.omit(all_duration_rs)

names(all_duration_rs)[1] <- "Variable"

all_duration_rs_summary <- all_duration_rs %>% 
  dplyr::group_by(Variable) %>% 
  dplyr::summarize(mean = round(mean(value),1),
            median = round(median(value, na.rm=T),1))%>%
  as.data.frame()


dp11 <- ggplot(all_duration_rs,aes(x=value, y = Variable)) + 
  stat_density_ridges(quantile_lines = F, quantiles = 2, scale = 1.7,alpha = 0.6, aes(fill=Variable))+
  geom_segment(data = all_duration_rs_summary, aes(x = mean, xend = mean, y = as.numeric(Variable),
                yend = as.numeric(Variable) + 1),linetype = "dashed", colour = "black") +
  geom_segment(data = all_duration_rs_summary, aes(x = median, xend = median, y = as.numeric(Variable),
                                                   yend = as.numeric(Variable) + 1), colour = "black") +
  geom_label(data = all_duration_rs_summary, aes(y = Variable, x = mean, label = mean),
              colour="black", size=3.5, label.size = NA, nudge_y = 0.5)+
  geom_label(data = all_duration_rs_summary, aes(y = Variable, x = median, label = median),
             colour="black", size=3.5, label.size = NA, nudge_y = 0.5)+
  xlab("Engagement duration (minutes/week)")+
  ylab("")+
  ggtitle("Distribution of political engagement online")+
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01,0),
                     trans='log', labels= scales::comma, limits = c(0.1,10000))+
scale_fill_manual(values = c(#overall
                             "#628395",  
                             #latent classes
                             "#FC471E",
                             "#FC471E",
                             "#FC471E", 
                             #domains
                             "#DB2763",
                             "#DB2763",
                             "#DB2763",
                             "#DB2763",
                             "#DB2763",
                             #info/com
                             "#C5D86D",
                             "#C5D86D",
                             #political
                             "#17377A"))+
  theme_ridges()+
  guides(fill="none", alpha = "none")+
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.text.y = element_text(hjust = 0.5,face = "bold"),
        plot.margin=unit(c(0,0,0,-0.9), "cm"))


dp11
#ggsave("output/densitiy_4_duration_all.pdf", width = 6, height = 10, dpi = 300, dp11)

combi_4 <- grid.arrange(bars, dp11, widths = c(1.5,5))
#ggsave("output/combi_4_absolute.pdf", width = 10, height = 12, dpi = 300, combi_4 )

################## Same for clicks #####################

# Click density  
all_clicks <- data.frame(
  Overall=user_data_deliberation$online_clicks,
  
  Niche=user_data_deliberation$c2_clicks,
  Quality=user_data_deliberation$c1_clicks,
  Hubs=user_data_deliberation$c3_clicks,
  
  Change=user_data_deliberations$change_clicks,
  Twitter=user_data_deliberations$twitter_clicks,
  Facebook=user_data_deliberations$facebook_clicks,
  Bild=user_data_deliberations$bild_clicks,
  Zeit=user_data_deliberations$zeit_clicks,
  
  Communication=user_data_deliberation$com_clicks,
  Information=user_data_deliberation$info_clicks,
  Political=user_data_deliberation$pol_clicks)


#### Barplots for Absolute engagement graph

all_clicks_na <- data.frame(apply(all_clicks, 2, function(x) ifelse(x > 0, 1, 0)))
all_clicks_na_rs <- melt(all_clicks_na)

all_clicks_na_rs$variable <- factor(all_clicks_na_rs$variable, levels = c(
  "Political","Information","Communication", "Zeit", "Bild","Facebook","Twitter",
  "Change","Hubs","Quality","Niche","Overall"
))

bars <- ggplot(all_clicks_na_rs, aes(x = value, fill = factor(variable)))+
  geom_bar(aes(alpha = factor(value)))+
  stat_count(aes(label= round(..count../1282*100,1)), vjust=0.5, 
             geom="text", position="identity", size = 3.5)+
  facet_grid(variable~.)+
  scale_fill_manual(values = c("#17377A","#C5D86D",
                               "#C5D86D", "#DB2763",
                               "#DB2763",
                               "#DB2763",
                               "#DB2763",
                               "#DB2763", "#FC471E",
                               "#FC471E",
                               "#FC471E","#628395"))+
  scale_alpha_manual(values = c(0.3,0.6))+
  theme_minimal()+
  ggtitle("Percent usage")+
  guides(fill = "none", alpha = "none")+
  scale_x_discrete(name = "", limits = c(0,1), labels = c("non users", "users"),
                   expand = c(0.01,0.01))+
  theme(axis.ticks.y=element_blank(), 
        axis.text.y=element_blank(),
        title = element_text(vjust = 2),
        axis.text.x=element_text(size = 10, colour = "black"),
        axis.title.y=element_blank(),
        strip.text = element_blank(),
        panel.border=element_rect(colour = "black",fill=NA, size=0.5),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

bars
#ggsave("output/bars_usage_clicks.pdf", width = 2, height = 10, dpi = 300, bars)

###### Density Plot - Clicks

all_clicks_rs <- melt(all_clicks)
all_clicks_rs$value[all_clicks_rs$value < 1] <- NA
all_clicks_rs <- na.omit(all_clicks_rs)

names(all_clicks_rs)[1] <- "Variable"

all_clicks_rs_summary <- all_clicks_rs %>% 
  dplyr::group_by(Variable) %>% 
  dplyr::summarize(mean = round(mean(value),0),
            median = round(median(value, na.rm=T),0))%>%
  as.data.frame()

dp12 <- ggplot(all_clicks_rs,aes(x=value, y = Variable)) + 
  stat_density_ridges(quantile_lines = F, quantiles = 2, scale = 1.7,alpha = 0.6, aes(fill=Variable))+
  geom_segment(data = all_clicks_rs_summary, aes(x = mean, xend = mean, y = as.numeric(Variable),
                                                   yend = as.numeric(Variable) + 1),linetype = "dashed", colour = "black") +
  geom_segment(data = all_clicks_rs_summary, aes(x = median, xend = median, y = as.numeric(Variable),
                                                   yend = as.numeric(Variable) + 1), colour = "black") +
  geom_label(data = all_clicks_rs_summary, aes(y = Variable, x = mean, label = mean),
             colour="black", size=3.5, label.size = NA, nudge_y = 0.5)+
  geom_label(data = all_clicks_rs_summary, aes(y = Variable, x = median, label = median),
             colour="black", size=3.5, label.size = NA, nudge_y = 0.5)+
  xlab("Engagement (number of clicks)")+
  ylab("")+
  ggtitle("Distribution of political engagement online")+
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01,0),
                     trans='log', labels= scales::comma, limits = c(0.5,50000))+
  scale_fill_manual(values = c(#overall
    "#628395",  
    #latent classes
    "#FC471E",
    "#FC471E",
    "#FC471E", 
    #domains
    "#DB2763",
    "#DB2763",
    "#DB2763",
    "#DB2763",
    "#DB2763",
    #info/com
    "#C5D86D",
    "#C5D86D",
    #political
    "#17377A"))+
  theme_ridges()+
  guides(fill="none", alpha = "none")+
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.text.y = element_text(hjust = 0.5,face = "bold"),
        plot.margin=unit(c(0,0,0,-0.9), "cm"))


dp12
#ggsave("output/densitiy_4_clicks_all.pdf", width = 6, height = 10, dpi = 300, dp12)


combi_5 <- grid.arrange(bars, dp12, widths = c(1.5,5))
#ggsave("output/combi_5_absolute.pdf", width = 10, height = 12, dpi = 300, combi_5 )

