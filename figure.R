#load("unLei6_adj.RData")
#m <- unMan6_adj %>% 
#  ma(order = 7) %>%
#  as.data.frame() %>%
#  select(c(2,3,4)) %>%
#  bind_cols(unMan6_adj)
#o <- unOth6_adj %>% 
#  ma(order = 7) %>%
#  as.data.frame() %>%
#  select(c(2,3,4)) %>%
#  bind_cols(unOth6_adj)

#df1 <- select(l, c("date","V2")) %>%
#  mutate(V3 = m$V3, V4 = o$V4)
  
# p <- ggplot() +
#    geom_point(data = ,aes(x = , y = ,), ... ) +
#      labs(x = , y = ,) +
#      scale_y_continuous(limits = ,breaks = , expand = ) +
#      scale_x_date(expand = ) +
#      annotate() +
#      theme_bw() + 
#      theme(...)
    

# original figure5

Source <- paste("Source:", "Authors’ calculations based on data from the U.S. Bureau of Labor Statistics, Current Population Survey")
Note1 <- paste("Notes: Moves between employment in month one of the CPS and",
               "unemployment (red lines) or out of the labor force (blue lines)", 
               "in month four of the CPS.", "Series are seasonally adjusted and",
               "use CPS weights. The shaded bars indicate recessions as defined by", 
               "the National Bureau of Economic Research", sep = " ")
text <- paste(Note1, Source, sep = "\n")

ofigure5_A <- Lei6_adj %>% 
  ma(order = 7) %>%
  as.data.frame() %>%
  select(c(2,3)) %>%
  bind_cols(Lei6_adj)

ofigure5_B <- Man6_adj %>% 
  ma(order = 7) %>%
  as.data.frame() %>%
  select(c(2,3)) %>%
  bind_cols(Man6_adj)

ofigure5_C <- Oth6_adj %>% 
  ma(order = 7) %>%
  as.data.frame() %>%
  select(c(2,3)) %>%
  bind_cols(Oth6_adj)

p5_A <- ggplot(ofigure5_A) +
  geom_line(aes(x = date, y = V2, colour = "brown4")) +
  geom_line(aes(x = date, y = V3, colour = "dodgerblue4")) +
  scale_color_identity(name = NULL,
                       breaks = c("dodgerblue4", "brown4"),
                       labels = c("Not in labor force", "unemployed"),
                       guide = "legend") +
  labs(x = NULL, y = NULL, 
       title = "A. Leisure and hospital",
       subtitle = "percent of tatal in industry, seven-month centered moving average", 
       caption = NULL, tag = NULL) +
  scale_y_continuous(limits = c(0, 12.5), breaks=c(0, 2.5, 5, 7.5, 10, 12.5), 
                     expand = expansion(mult = c(0.05, 0))) +
  scale_x_date(expand = c(0,0)) +
  annotate("rect", xmin = as.Date("2001-03-01", "%Y-%m-%d"), xmax = as.Date
           ("2001-11-01",  "%Y-%m-%d"), ymin = -Inf,ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2007-12-01", "%Y-%m-%d"), xmax = as.Date
           ("2009-6-01",  "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2020-02-01", "%Y-%m-%d"), xmax = as.Date
           ("2020-04-01",  "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  theme_classic() +
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), plot.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(face = "bold",
                                  margin = margin(0, 0, 2, 0),
                                  size = 10),
        plot.subtitle = element_text(margin = margin(0,0,5,0),
                                     size = 9),
        axis.ticks.length= unit(-0.2,'cm'),
        legend.position = "bottom",
        legend.key.size=unit(3,'mm'),
        legend.key.width=unit(10,'mm'),
        legend.margin=margin(t = 0.3, b = 0.3, unit='cm'),
        legend.background = element_rect(inherit.blank = TRUE))


p5_B <- ggplot(ofigure5_B) +
  geom_line(aes(x = date, y = V2, colour = "brown4")) +
  geom_line(aes(x = date, y = V3, colour = "dodgerblue4")) +
  scale_color_identity(name = NULL,
                       breaks = c("dodgerblue4", "brown4"),
                       labels = c("Not in labor force", "unemployed"),
                       guide = "legend") +
  labs(x = NULL, y = NULL, 
       title = "B. Manufacturing",
       subtitle = "percent of tatal in industry, seven-month centered moving average", 
       caption = NULL, tag = NULL) +
  scale_y_continuous(limits = c(0, 12.5), breaks=c(0, 2.5, 5, 7.5, 10, 12.5), 
                     expand = expansion(mult = c(0.05, 0))) +
  scale_x_date(expand = c(0,0)) +
  annotate("rect", xmin = as.Date("2001-03-01", "%Y-%m-%d"), xmax = as.Date
           ("2001-11-01", "%Y-%m-%d"), ymin = -Inf,ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2007-12-01", "%Y-%m-%d"), xmax = as.Date
           ("2009-06-01", "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2020-02-01", "%Y-%m-%d"), xmax = as.Date
           ("2020-04-01", "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  theme_classic()+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), plot.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(face = "bold",
                                  margin = margin(0, 0, 1, 0),
                                  size = 10),
        plot.subtitle = element_text(margin = margin(0,0,5,0),
                                     size = 9),
        axis.ticks.length = unit(-0.2,'cm'),
        legend.position = "bottom",
        legend.key.size=unit(3,'mm'),
        legend.key.width=unit(10,'mm'),
        legend.margin = margin(t = 0.3, b = 0.3, unit='cm'),
        legend.background = element_rect(inherit.blank = TRUE))

p5_C <- ggplot(ofigure5_C) +
  geom_line(aes(x = date, y = V2, colour = "brown4")) +
  geom_line(aes(x = date, y = V3, colour = "dodgerblue4")) +
  scale_color_identity(name = NULL,
                       breaks = c("dodgerblue4", "brown4"),
                       labels = c("Not in labor force", "unemployed"),
                       guide = "legend") +
  labs(x = NULL, y = NULL, 
       title = "C. Other industries",
       subtitle = "percent of tatal in industry, seven-month centered moving average", 
       caption = NULL, tag = NULL) +
  scale_y_continuous(limits = c(0, 12.5), breaks=c(0, 2.5, 5, 7.5, 10, 12.5), 
                     expand = expansion(mult = c(0.05, 0))) +
  scale_x_date(expand = c(0,0)) +
  annotate("rect", xmin = as.Date("2001-03-01", "%Y-%m-%d"), xmax = as.Date
           ("2001-11-01",  "%Y-%m-%d"), ymin = -Inf,ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2007-12-01", "%Y-%m-%d"), xmax = as.Date
           ("2009-06-01",  "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2020-02-01", "%Y-%m-%d"), xmax = as.Date
           ("2020-04-01",  "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  theme_classic() +
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), plot.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(face = "bold",
                                  margin = margin(0, 0, 1, 0),
                                  size = 10),
        plot.subtitle = element_text(margin = margin(0,0,5,0),
                                     size = 9),
        axis.ticks.length = unit(-0.2,'cm'),
        legend.position = "bottom",
        legend.key.size=unit(3,'mm'),
        legend.key.width=unit(10,'mm'),
        legend.margin=margin(t = 0.3, b = 0.3, unit='cm'),
        legend.background = element_rect(inherit.blank = TRUE))

p5 <- ggarrange(p5_A, p5_B, p5_C, ncol = 1, nrow = 3, legend = "bottom", common.legend = TRUE) %>%
      annotate_figure(top = text_grob("5. Employment to nonemployment transitions, by initial industry",
                color = "black", face = "bold", size = 12, hjust = 0, x = 0.025))
#ggexport(p5, filename = "o_figure5.png")
#ggsave(filename = "ofigure5_A.png", plot = p5_A, width = 190, units = "mm", dpi = 300, bg = "White")
#ggsave(filename = "ofigure5_B.png", plot = p5_B, width = 190, units = "mm", dpi = 300, bg = "White")
#ggsave(filename = "ofigure5_C.png", plot = p5_C, width = 190, units = "mm", dpi = 300, bg = "White")
#ggsave(filename = "ofigure5.png", plot = p5, path = "/users/lizhouxin/downloads/", width = 190, units = "mm", dpi = 300, bg = "White")

#orginal figure6

ofigure6_A <- unLei6_adj %>% 
  ma(order = 7) %>%
  as.data.frame() %>%
  select(2) %>%
  bind_cols(unLei6_adj)

ofigure6_B <- unMan6_adj %>% 
  ma(order = 7) %>%
  as.data.frame() %>%
  select(3) %>%
  bind_cols(unMan6_adj)

ofigure6_C <- unOth6_adj %>% 
  ma(order = 7) %>%
  as.data.frame() %>%
  select(4) %>%
  bind_cols(unOth6_adj)

p6 <- ggplot() +
  geom_line(data = ofigure6_A, aes(x = date, y = V2, colour = "dodgerblue4")) +
  geom_line(data = ofigure6_B, aes(x = date, y = V3, colour = "brown4")) +
  geom_line(data = ofigure6_C, aes(x = date, y = V4, colour = "seagreen4")) +
  scale_color_identity(name = NULL,
                       breaks = c("dodgerblue4", "brown4", "seagreen4"),
                       labels = c("Leisure", "Manufacturing", "Other"),
                       guide = "legend") +
  labs(x = NULL, y = NULL, 
       title = "6. Unemployment to employment within the same industry, by industry",
       subtitle = "percent of tatal in industry, seven-month centered moving average", 
       caption = NULL, tag = NULL) +
  scale_y_continuous(limits = c(0, 40), breaks=c(0, 10, 20, 30, 40)) +
  scale_x_date(expand = c(0,0)) +
  annotate("rect", xmin = as.Date("2001-03-01", "%Y-%m-%d"), xmax = as.Date
           ("2001-11-01",  "%Y-%m-%d"), ymin = -Inf,ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2007-12-01", "%Y-%m-%d"), xmax = as.Date
           ("2009-06-01",  "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2020-02-01", "%Y-%m-%d"), xmax = as.Date
           ("2020-04-01",  "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  theme_classic() +
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), plot.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(face = "bold",
                                  margin = margin(0, 0, 1, 0),
                                  size = 10),
        plot.subtitle = element_text(margin = margin(0,0,5,0),
                                     size = 9),
        axis.ticks.length = unit(-0.2,'cm'),
        legend.position = "bottom",
        legend.key.size=unit(3,'mm'),
        legend.key.width=unit(10,'mm'),
        legend.margin=margin(t = 0.3, b = 0.3, unit='cm'),
        legend.background = element_rect(inherit.blank = TRUE))

#ggsave(filename = "ofigure6.png", plot = p6, width = 190, units = "mm", dpi = 300, bg = "White")

#Original figure7

ofigure7_A <- unLei6_adj %>% 
  ma(order = 7) %>%
  as.data.frame() %>%
  select(c(3,4)) %>%
  bind_cols(unLei6_adj)

ofigure7_B <- unMan6_adj %>% 
  ma(order = 7) %>%
  as.data.frame() %>%
  select(c(2,4)) %>%
  bind_cols(unMan6_adj)

ofigure7_C <- unOth6_adj %>% 
  ma(order = 7) %>%
  as.data.frame() %>%
  select(c(2,3)) %>%
  bind_cols(unOth6_adj)

p7_A <- ggplot(ofigure7_A) +
  geom_line(aes(x = date, y = V3, colour = "brown4")) +
  geom_line(aes(x = date, y = V4, colour = "seagreen4")) +
  scale_color_identity(name = NULL,
                       breaks = c("dodgerblue4","brown4", "seagreen4"),
                       labels = c("Leisure", "Manufacturing", "Other"),
                       guide = "legend") +
  labs(x = NULL, y = NULL, 
       title = "A. Leisure and hospitality",
       subtitle = "percent of tatal in industry, seven-month centered moving average", 
       caption = NULL, tag = NULL) +
  scale_y_continuous(limits = c(0, 30), breaks=c(0, 10, 20, 30)) +
  scale_x_date(expand = c(0,0)) +
  annotate("rect", xmin = as.Date("2001-03-01", "%Y-%m-%d"), xmax = as.Date
           ("2001-11-01",  "%Y-%m-%d"), ymin = -Inf,ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2007-12-01", "%Y-%m-%d"), xmax = as.Date
           ("2009-6-01",  "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2020-02-01", "%Y-%m-%d"), xmax = as.Date
           ("2020-04-01",  "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  theme_classic() +
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), plot.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(face = "bold",
                                  margin = margin(0, 0, 1, 0),
                                  size = 10),
        plot.subtitle = element_text(margin = margin(0,0,5,0),
                                     size = 9),
        axis.ticks.length= unit(-0.2,'cm'),
        legend.position = "bottom",
        legend.key.size=unit(3,'mm'),
        legend.key.width=unit(10,'mm'),
        legend.margin=margin(t = 0.3, b = 0.3, unit='cm'),
        legend.background = element_rect(inherit.blank = TRUE))


p7_B <- ggplot(ofigure7_B) +
  geom_line(aes(x = date, y = V2, colour = "dodgerblue4")) +
  geom_line(aes(x = date, y = V4, colour = "seagreen4")) +
  scale_color_identity(name = NULL,
                       breaks = c("dodgerblue4", "seagreen4"),
                       labels = c("leisure", "Other"),
                       guide = "legend") +
  labs(x = NULL, y = NULL, 
       title = "B. Manufacturing",
       subtitle = "percent of tatal in industry, seven-month centered moving average", 
       caption = NULL, tag = NULL) +
  scale_y_continuous(limits = c(0, 30), breaks=c(0, 10, 20, 30)) +
  scale_x_date(expand = c(0,0)) +
  annotate("rect", xmin = as.Date("2001-03-01", "%Y-%m-%d"), xmax = as.Date
           ("2001-11-01", "%Y-%m-%d"), ymin = -Inf,ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2007-12-01", "%Y-%m-%d"), xmax = as.Date
           ("2009-06-01", "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2020-02-01", "%Y-%m-%d"), xmax = as.Date
           ("2020-04-01", "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  theme_classic() +
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), plot.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(face = "bold",
                                  margin = margin(0, 0, 1, 0),
                                  size = 10),
        plot.subtitle = element_text(margin = margin(0,0,5,0),
                                     size = 9),
        axis.ticks.length = unit(-0.2,'cm'),
        legend.position = "bottom",
        legend.key.size=unit(3,'mm'),
        legend.key.width=unit(10,'mm'),
        legend.margin = margin(t = 0.3, b = 0.3, unit='cm'),
        legend.background = element_rect(inherit.blank = TRUE))

p7_C <- ggplot(ofigure7_C) +
  geom_line(aes(x = date, y = V2, colour = "dodgerblue4")) +
  geom_line(aes(x = date, y = V3, colour = "brown4")) +
  scale_color_identity(name = NULL,
                       breaks = c("dodgerblue4", "brown4"),
                       labels = c("Leisure", "Manufacturing"),
                       guide = "legend") +
  labs(x = NULL, y = NULL, 
       title = "C. Other industries",
       subtitle = "percent of tatal in industry, seven-month centered moving average", 
       caption = NULL, tag = NULL) +
  scale_y_continuous(limits = c(0, 30), breaks=c(0, 10, 20, 30)) +
  scale_x_date(expand = c(0,0)) +
  annotate("rect", xmin = as.Date("2001-03-01", "%Y-%m-%d"), xmax = as.Date
           ("2001-11-01",  "%Y-%m-%d"), ymin = -Inf,ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2007-12-01", "%Y-%m-%d"), xmax = as.Date
           ("2009-06-01",  "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2020-02-01", "%Y-%m-%d"), xmax = as.Date
           ("2020-04-01",  "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  theme_classic() +
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), plot.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(face = "bold",
                                  margin = margin(0, 0, 1, 0),
                                  size = 10),
        plot.subtitle = element_text(margin = margin(0,0,5,0),
                                     size = 9),
        axis.ticks.length = unit(-0.2,'cm'),
        legend.position = "bottom",
        legend.key.size=unit(3,'mm'),
        legend.key.width=unit(10,'mm'),
        legend.margin=margin(t = 0.3, b = 0.3, unit='cm'),
        legend.background = element_rect(inherit.blank = TRUE))

ofigure7_legend <- unLei6_adj %>% 
  ma(order = 7) %>%
  as.data.frame() %>%
  select(c(2,3,4)) %>%
  bind_cols(unLei6_adj)
legend <- ggplot(ofigure7_legend) +
  geom_line(aes(x = date, y = V2, colour = "dodgerblue4")) +
  geom_line(aes(x = date, y = V3, colour = "brown4")) +
  geom_line(aes(x = date, y = V4, colour = "seagreen4")) +
  scale_color_identity(name = NULL,
                       breaks = c("dodgerblue4","brown4", "seagreen4"),
                       labels = c("Leisure", "Manufacturing", "Other"),
                       guide = "legend") +
  labs(x = NULL, y = NULL, 
       title = "A. Leisure and hospitality",
       subtitle = "percent of tatal in industry, seven-month centered moving average", 
       caption = NULL, tag = NULL) +
  scale_y_continuous(limits = c(0, 30), breaks=c(0, 10, 20, 30)) +
  scale_x_date(expand = c(0,0)) +
  annotate("rect", xmin = as.Date("2001-03-01", "%Y-%m-%d"), xmax = as.Date
           ("2001-11-01",  "%Y-%m-%d"), ymin = -Inf,ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2007-12-01", "%Y-%m-%d"), xmax = as.Date
           ("2009-6-01",  "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  annotate("rect", xmin = as.Date("2020-02-01", "%Y-%m-%d"), xmax = as.Date
           ("2020-04-01",  "%Y-%m-%d"), ymin = -Inf, ymax = Inf, alpha = 0.5) +
  theme_classic() +
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), plot.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(face = "bold",
                                  margin = margin(0, 0, 1, 0),
                                  size = 10),
        plot.subtitle = element_text(margin = margin(0,0,5,0),
                                     size = 9),
        axis.ticks.length= unit(-0.2,'cm'),
        legend.position = "bottom",
        legend.key.size=unit(3,'mm'),
        legend.key.width=unit(10,'mm'),
        legend.margin=margin(t = 0.3, b = 0.3, unit='cm'),
        legend.background = element_rect(inherit.blank = TRUE))
  
p7 <- ggarrange(p7_A, p7_B, p7_C, ncol = 1, nrow = 3, legend = "bottom", 
                common.legend = TRUE, legend.grob = get_legend(legend, position = "bottom")) %>%
  annotate_figure(top = text_grob("7. Unemployment to employment in a different industry, by initial industry",
                                  color = "black", face = "bold", size = 12, hjust = 0, x = 0.025))
ggexport(p7, filename = "o_figure5.png")
#ggsave(filename = "ofigure5_A.png", plot = p5_A, width = 190, units = "mm", dpi = 300, bg = "White")
#ggsave(filename = "ofigure5_B.png", plot = p5_B, width = 190, units = "mm", dpi = 300, bg = "White")
#ggsave(filename = "ofigure5_C.png", plot = p5_C, width = 190, units = "mm", dpi = 300, bg = "White")
ggsave(filename = "ofigure7.png", plot = p7, path = "/users/lizhouxin/downloads/", width = 190, units = "mm", dpi = 300, bg = "White")


  
  