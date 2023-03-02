source("util.R")

gamedfmf<-get_shots(19748)
ts=clean_shots(gamedfmf)

#PLOT ALL SHOTS
ggplot(ts) +
  annotate_pitch(
    colour = "white",
    fill   = "springgreen4",
    limits = FALSE,
    dimensions = pitch_statsbomb
  ) +
  geom_point(aes(x=x,y=y,color=team),size=3)+
  geom_segment(aes(x = x, y = y, xend = x2, yend = y2,color=team),
               arrow = arrow(length = unit(0.15, "cm"),
                             type = "closed")) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4")) +
  coord_flip(xlim = c(49, 120)) +
  scale_y_reverse() +
  direction_label() +
  ggtitle("Simple passmap", 
          "ggsoccer example")

#PLOT ONE SHOT AND FREEZEFRAME
