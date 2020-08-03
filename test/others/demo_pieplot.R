

# https://stackoverflow.com/questions/16184188/ggplot-facet-piechart-placing-text-in-the-middle-of-pie-chart-slices
dat = read.table(text = "Channel Volume Cnt
                         AGENT   high   8344
                         AGENT medium   5448
                         AGENT    low  23823
                         KIOSK   high  19275
                         KIOSK medium  13554
                         KIOSK    low  38293", header=TRUE)

# calculate the start and end angles for each pie
dat_pies <- left_join(dat,
                      dat %>%
                        dplyr::group_by(Channel) %>%
                        dplyr::summarize(Cnt_total = sum(Cnt))) %>%
  group_by(Channel) %>%
  mutate(end_angle = 2*pi*cumsum(Cnt)/Cnt_total,      # ending angle for each pie slice
         start_angle = lag(end_angle, default = 0),   # starting angle for each pie slice
         mid_angle = 0.5*(start_angle + end_angle))   # middle of each pie slice, for the text label

rpie = 1 # pie radius
rlabel = 0.6 * rpie # radius of the labels; a number slightly larger than 0.5 seems to work better,
# but 0.5 would place it exactly in the middle as the question asks for.

# draw the pies
ggplot(dat_pies) +
  geom_rect(aes(fill = Channel),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.3) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle, end = end_angle, fill = Volume)) +
  geom_text(aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle), label = Cnt),
            hjust = 0.5, vjust = 0.5) +

  coord_fixed() +
  scale_x_continuous(limits = c(-1, 1), name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1, 1), name = "", breaks = NULL, labels = NULL) +
  facet_grid(Channel~.)


# generate hjust and vjust settings depending on the quadrant into which each
# label falls
dat_pies <- mutate(dat_pies,
                   hjust = ifelse(mid_angle>pi, 1, 0),
                   vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1))

rlabel = 1.05 * rpie # now we place labels outside of the pies

ggplot(dat_pies) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle, end = end_angle, fill = Volume)) +
  geom_text(aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle), label = Cnt,
                hjust = hjust, vjust = vjust)) +
  coord_fixed() +
  scale_x_continuous(limits = c(-1.5, 1.4), name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1, 1), name = "", breaks = NULL, labels = NULL) +
  facet_grid(Channel~.)


#
states <- c(
  'eaten', "eaten but said you didn\'t", 'cat took it', 'for tonight',
  'will decompose slowly'
)
pie <- data.frame(
  state = factor(rep(states, 2), levels = states),
  type = rep(c('Pie', 'Donut'), each = 5),
  r0 = rep(c(0, 0.8), each = 5),
  focus = rep(c(0.2, 0, 0, 0, 0), 2),
  amount = c(4, 3, 1, 1.5, 6, 6, 1, 2, 3, 2),
  stringsAsFactors = FALSE
)

# Look at the cakes
ggplot() + geom_arc_bar(aes(
  x0 = 0, y0 = 0, r0 = r0, r = 1, amount = amount,
  fill = state, explode = focus
),
data = pie, stat = 'pie'
) +
  facet_wrap(~type, ncol = 1) +
  coord_fixed() +
  theme_no_axes() +
  scale_fill_brewer('', type = 'qual')
