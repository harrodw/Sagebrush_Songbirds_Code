model_data <- brsp_count %>% 
  select(Full.Point.ID, Year, Visit, Count,
         Sagebrush.Cover:TRI, Burned,
         -Burn.Sevarity, -Fire.Distance) %>% 
  mutate(Burned = case_when(Burned == "R" ~ "N",
                            Burned == "B" ~ "Y"))

glimpse(model_data)

#install.packages("expss")

with(model_data, table(Year, useNA = "ifany"))

visit_count <- with(model_data, table(Full.Point.ID, Visit, useNA = "ifany")) %>% 
  data.frame()

with(visit_count, table(Freq, useNA = "ifany"))
unique(model_data$Full.Point.ID)

with(model_data, table(Visit, useNA = "ifany"))

with(model_data, table(Count, useNA = "ifany"))

with(model_data, table(Burned, useNA = "ifany"))

model_data %>% 
  filter(is.na(Burned))

summary(model_data)

pairs(select(model_data, -Full.Point.ID, - Year, - Visit, -Burned))

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
# {
#   par(usr = c(0, 1, 0, 1))
#   txt <- as.character(format(cor(x, y), digits = 2))
#   if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
#   text(0.5, 0.5, txt, cex = cex.cor)
# }
# 
# str(model_data)
# 
# temp <- select(model_data, -Full.Point.ID, - Year, - Visit, -Burned)
# 
# cor(temp[,1:2])

pairs(select(model_data, -Full.Point.ID, - Year, - Visit, -Burned),
      lower.panel = panel.smooth, upper.panel = panel.cor,
      gap=0, row1attop=FALSE)

ggplot(model_data, aes(x = Sagebrush.Cover, y = Shrub.Cover - Sagebrush.Cover)) +
  geom_point()

library(glmmTMB)
library(DHARMa)
library(emmeans)
library(car)
library(ggeffects)

#First basic model -------------------------------------------------------------------
m1 <- glmmTMB(Count ~ Sagebrush.Cover + Elevation, 
              data = model_data,
              family = poisson(link = "log"))
summary(m1)
m1.res <- DHARMa::simulateResiduals(m1, plot = T)
plotResiduals(m1.res, form = model_data$Sagebrush.Cover)
