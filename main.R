source("util.R")
source("mathUtil.R")

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
# prep data - one shot + shot_ff
gsFF= single_shot_ff(gamedfmf[4,])
gsSh= single_shot(gamedfmf[4,])
gsFF= single_shot_ff(gamedfmf[1,])
gsSh= single_shot(gamedfmf[1,])
gsFF= single_shot_ff(gamedfmf[9,])
gsSh= single_shot(gamedfmf[9,])
gsFF= single_shot_ff(gamedfmf[12,])
gsSh= single_shot(gamedfmf[12,])
gsFF= single_shot_ff(gamedfmf[22,])
gsSh= single_shot(gamedfmf[22,])
gsFF= single_shot_ff(gamedfmf[24,])
gsSh= single_shot(gamedfmf[24,])


ggplot(gsSh) +
  annotate_pitch(
    colour = "black",
    fill   = "white",
    limits = FALSE,
    dimensions = pitch_statsbomb
  ) +
  geom_point(aes(x=x,y=y),size=3)+
  geom_point(data=gsFF,aes(x=x,y=y, color=teammate),size=3)+
  geom_segment(aes(x = x, y = y, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.15, "cm"),
                             type = "closed")) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4")) +
  coord_flip(xlim = c(49, 120)) +
  scale_y_reverse() +
  direction_label() +
  ggtitle("Simple passmap", 
          "ggsoccer example")

