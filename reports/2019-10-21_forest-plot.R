# dat <- data.frame(group = factor(c("A","B","C","D","E","F","G"), levels=c("F","E","D","C","B","A","G")),
#                   cen = c(3.1,2.0,1.6,3.2,3.6,7.6,NA),
#                   low = c(2,0.9,0.8,1.5,2,4.2,NA),
#                   high = c(6,4,2,6,5,14.5,NA))
# theme_set(theme_bw())
# theme_update(
#   axis.line = element_line(colour = "black"),
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.border = element_blank(),
#   panel.background = element_blank(),
#   axis.text.y = element_blank(),
#   axis.ticks.y = element_blank(),
#   plot.margin = unit(c(0,0,0,0), "lines")
# )
# 
# p <- ggplot(dat,aes(cen,group)) + 
#   geom_point(size=5, shape=18) +
#   geom_errorbarh(aes(xmax = high, xmin = low), height = 0.15) +
#   geom_vline(xintercept = 1, linetype = "longdash") +
#   scale_x_continuous(breaks = seq(0,14,1), labels = seq(0,14,1)) +
#   labs(x="Adjusted Odds Ratio", y="")
# lab <- data.frame(V0 = factor(c("A","B","C","D","E","F","G","A","B","C","D","E","F","G","A","B","C","D","E","F","G","A","B","C","D","E","F","G"),, levels=c("G","F","E","D","C","B","A")),
#                   V05 = rep(c(1,2,3,4),each=7),
#                   V1 = c("Occuption","Active","","Inactive","","Inactive","","Recreation","Inactive","","Active","","Inactive","","Gender","Men","Women","Men","Women","Men","Women","OR",3.1,2.0,1.6,3.2,3.6,7.6)
# )



# t1 <- out.forest %>%
#   select(Outcome, `HDP+ (%)`, `HDP- (%)`, `M.o.A. (95% CI)`)
# rownames(t1) <- NULL
# 
# data_table <- tableGrob(t1[1:4,],
#                     rows = NULL,
#                     theme = ttheme_minimal(
#                       base_size = 12,
#                       base_family = "serif",
#                       core=list(fg_params=list(hjust=adjustments[1:4,], x=adjustments2[1:4,])),
#                       colhead=list(fg_params=list(hjust=c(0,rep(1,3)), x=c(0,rep(0.95,3))))
#                     ))
# grid.newpage()
# grid.draw(data_table)

p1 <- out.forest %>%
  filter(!type %in% c("diasbp", "sysbp")) %>%
  ggplot(aes(HR,-id)) + 
  geom_vline(xintercept = 1, linetype = "longdash") +
  geom_point(size=5, shape=16, col = "darkgreen") +
  ylim(-4.05, -0.95) +
  geom_errorbarh(aes(xmax = CI.u, xmin = CI.l), height = 0) +
  scale_x_continuous(breaks = seq(0.5,2,0.5), labels = seq(0.5,2,0.5), limits = c(0.5,2.1)) +
  labs(x="", y="") + 
  theme(axis.line.y = element_blank(),
        axis.text.x = element_text(#family = "Times",
                                   size = 12))#,
        # plot.margin = unit(c(1,0.1,-0.5,0.1), units = "cm"),
        
  
ggsave(plot = p1,
       filename = here("graphs", "2019-10-30_p1.eps"),
       device = "eps",
       #fontfamily = "Times",
       units = "in",
       width = 3.25,
       height = 4)

p2 <- out.forest %>%
  filter(type %in% c("diasbp", "sysbp")) %>%
  ggplot(aes(HR,-id)) + 
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_point(size=5, shape=16, col = "darkgreen") +
  ylim(-6.075, -4.7) +
  geom_errorbarh(aes(xmax = CI.u, xmin = CI.l), height = 0) +
  scale_x_continuous(breaks = seq(-5,10,5), labels = seq(-5,10,5), limits = c(-5,11)) +
  labs(x="", y="") + 
  theme(axis.line.y = element_blank(),
        axis.text.x = element_text(#family = "Times",
          size = 12))#,
# plot.margin = unit(c(1,0.1,-0.5,0.1), units = "cm"),


ggsave(plot = p2,
       filename = here("graphs", "2019-10-30_p2.eps"),
       device = "eps",
       #fontfamily = "Times",
       units = "in",
       width = 3.25,
       height = 2)

first_row <- arrangeGrob(data_table, p1,
                         ncol = 2,
                         widths = #unit(c(16, 8), "cm"),
                           c(24,8),
                         heights = c(5,13),
                         respect = FALSE,
                         padding = unit(0.1, "line"))
grid.arrange(first_row)

p2 <- out.forest %>%
  filter(type %in% c("diasbp", "sysbp")) %>%
  ggplot(aes(HR,-id)) + 
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_point(size=5, shape=16, col = "darkgreen") +
  geom_errorbarh(aes(xmax = CI.u, xmin = CI.l), height = 0) +
  scale_x_continuous(breaks = seq(-5,10,5), labels = seq(-5,10,5), limits = c(-5,11)) +
  ylim(-6.5,-4.5) +
  labs(x="", y="") + 
  theme(axis.line.y = element_blank())

p3 <- arrangeGrob(p1, p2, 
                  ncol = 1,
                  heights = unit(c(2,1.25), "in"))
grid.arrange(p3)
  
grid.arrange(arrangeGrob(data_table, 
                         p3, 
                         ncol = 2,
                         heights = unit(c(12,10), "cm"),
                         widths = unit(c(18,12), "cm"),
                         respect = FALSE))

lab <- data.frame(V0 = factor(c("A","B","C","D","E","F","G","A","B","C","D","E","F","G","A","B","C","D","E","F","G","A","B","C","D","E","F","G"),, levels=c("G","F","E","D","C","B","A")),
                  V05 = rep(c(1,2,3,4),each=7),
                  V1 = c("Occuption","Active","","Inactive","","Inactive","","Recreation","Inactive","","Active","","Inactive","","Gender","Men","Women","Men","Women","Men","Women","OR",3.1,2.0,1.6,3.2,3.6,7.6)
)

