
### Library
require(ggplot2)
require(tidyverse)
require(gridExtra)
require(stargazer)
require(purrr)
require(multcomp) # may be used to do the exercise (g)

### Download data
d <- read.csv("MENTAL.DAT", sep ="",  header = FALSE)
colnames(d) <- c("drug", "before", "after")

d$druglab <- factor(d$drug,
                    labels = c("Placebo(1)", "Morphine(2)", "Heroin(3)"))


### Formating Wide to Long
long.d <- d %>%
  gather("before", "after", key = "timing", value = "score")

long.d$druglab <- factor(long.d$drug, 
                         labels = c("Placebo(1)", "Morphine(2)", "Heroin(3)"))

### Exercise (a):
# Histograms
hist.a <- long.d %>%
  ggplot(., aes(x = score)
  ) + geom_histogram(binwidth = 2
  ) + facet_grid(druglab~timing
  ) + ggtitle("Histograms of the mental activity scores") 

hist.a + theme(text = element_text(size = 10)
) + theme(plot.title = element_text(size = 10))

# Boxplots of pre-injection
plot.b1 <- long.d %>%
  filter(timing == "before") %>%
  ggplot(., aes(x = as.factor(drug), y = score)
  ) + geom_boxplot(
  ) + xlab("Drug type") + ylab("Mental activity score"
  ) + ggtitle("Boxplot of the score means of pre-injection") 

plot.b1 + theme(text = element_text(size = 10)
) + theme(plot.title = element_text(size = 10))

# Boxplots of post-injection
plot.a <- long.d %>%
  filter(timing == "after") %>%
  ggplot(., aes(x = as.factor(drug), y = score)
         ) + geom_boxplot(
                    ) + xlab("Drug type") + ylab("Mental activity score"
                           ) + ggtitle("Boxplot of the score means of post-injection") 

plot.a + theme(text = element_text(size = 10)
               ) + theme(plot.title = element_text(size = 10))



### Exercise (b): 

loess_fun <- function(span){
  ggplot(data = d, aes(x = before, y = after)) +
    geom_point() +
    geom_smooth(span=span, se=FALSE) +
    facet_grid(druglab~.) +
    ggtitle(paste("Lowess smoother, span=",span)) +
    theme(plot.title = element_text(size=12))
}

map(c(0.3,0.4,0.5,0.6,0.8,0.9), 
    ~ loess_fun(.x)) # "purrr package"


### Exercise (c): 

fit1 <- lm(data = d, after~1)
fit2 <- lm(data = d, after~druglab)
fit3 <- lm(data = d, after~before)
fit4 <- lm(data = d, after~druglab + before)
fit5 <- lm(data = d, after~druglab*before)

stargazer(fit1, fit2, fit3, fit4, fit5,
          type = "text",
          no.space=TRUE,
          omit.stat=c("f"))

### Exercise (e):

fit6 <- glm(data = d, after ~ druglab + before, family = gaussian)

stargazer(fit4, fit6,
          type = "text",
          no.space=TRUE,
          omit.stat=c("f"))

### Exercise (g):

#### Method1
# Create a new variable X1 + X2 which is 1 when either of Morphine or Heroin
# Regress after mental activity on this new variable and a Morphine binary variable
# The coefficient of the Morphine binary variable is the difference and we can automatically do a test.

fit_x <- d %>%
  mutate(PlacMor = ifelse(druglab != "Placebo(1)",1,0)) %>%
  mutate(Heroin = ifelse(druglab == "Heroin(3)",1,0)) %>%
  lm(formula = after~PlacMor + Heroin + before)

# Look at the estimated values for heorin
summary(fit_x) 
confint(fit_x)

### Method2
K <- matrix(c(0,-1,1,0),1)
summary(glht(fit4, linfct = K))
confint(glht(fit4, linfct = K))
