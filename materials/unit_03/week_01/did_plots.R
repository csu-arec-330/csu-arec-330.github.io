
library(pacman)
p_load(tidyverse,did,fixest,modelsummary)



data(mpdta)

mw.attgt <- att_gt(yname = "lemp",
                   gname = "first.treat",
                   idname = "countyreal",
                   tname = "year",
                   xformla = ~1,
                   data = mpdta)

# summarize the results
summary(mw.attgt)

ggdid(mw.attgt, ylim = c(-.3,.3))


mw.dyn <- aggte(mw.attgt, type = "dynamic")
summary(mw.dyn)

ggdid(mw.dyn, ylim = c(-.3,.3))



mpdta_new <- mpdta %>%
  mutate(treated=ifelse(treat==1,year>=first.treat,0))

write_csv(mpdta_new,"~/min_wage_data.csv")

m1 <- feols(lemp ~ treated | countyreal + year,
            data=mpdta_new)

summary(m1)

mlm <- lm(lemp ~ treated + I(factor(countyreal)) + I(factor(year)),
            data=mpdta_new)

summary(mlm)

#random sample of counties
samp_co <- sample(unique(mpdta_new$countyreal[mpdta_new$treat==1]),6)

#chosen counties
chos_co <- c(12029,8001)

#plotting treated before and after
mpdta_new %>%
  filter(countyreal %in% c(27009)) %>%
  ggplot(aes(x=year,y=exp(lemp))) +
  geom_line(color="orange") +
  geom_vline(aes(xintercept = first.treat),color="purple") +
  # geom_text(aes()) +
  annotate("text",x=2005.5,y=690,label="Min Wage \nIncrease",color="purple",size=5) +
  ylim(600,700) +
  theme_bw(base_size = 15) +
  labs(x=NULL,y="Youth Employment (1000s)",title = "Benton Co, MN")

ggsave("materials/unit_03/week_01/includes/did_ts.png",width=6,height = 5,units = "in")

#plotting treated before and after
mpdta_new %>%
  filter(year %in% c(2007)) %>%
  mutate(treated=factor(treated,labels = c("Untreated","Treated"))) %>%
  ggplot(aes(x=treated,y=exp(lemp),color=treated)) +
  geom_boxplot(show.legend = F) +
  scale_y_log10() +
  theme_bw(base_size = 15) +
  labs(x=NULL,y="Youth Employment (1000s)",title="Youth Employment in 2007")

ggsave("materials/unit_03/week_01/includes/did_cs.png",width=6,height = 5,units = "in")
