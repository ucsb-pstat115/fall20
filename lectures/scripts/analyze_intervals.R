library(tidyverse)
lower_bounds <- read_csv("data/lower.csv")
upper_bounds <- read_csv("data/upper.csv")


lower <- lower_bounds %>% rename(question=student) %>% gather(key=student, value=lower, -question)
upper <- upper_bounds %>% rename(question=student) %>% gather(key=student, value=upper, -question)

inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question == "Q1") %>% ggplot() + geom_line(aes(x=1:73, y=lower)) + geom_line(aes(x=1:73, y=upper)) + ylim(c(0, 100))




inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question=="Q1") %>%
    ggplot(aes(x=factor(student), group=factor(student))) + geom_hline(yintercept=12, col="red") + geom_linerange(aes(ymin = lower, ymax = upper)) + ylim(c(0, 100)) 


inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question=="Q2") %>%
    ggplot(aes(x=factor(student), group=factor(student))) + geom_hline(yintercept=660000, col="red") + geom_linerange(aes(ymin = lower, ymax = upper)) + ylim(c(0, 1000000))



truth <- c(12, 660000, 59039, 119, 9206250, 24130000, 1720, 1971, 1321.33, 1997)
long_tab <- inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper))
cbind(long_tab, truth = rep(truth, nrow(lower))) %>% group_by(question) %>% summarise(mean = mean(lower < truth & upper > truth, na.rm=TRUE)) %>% arrange(desc(mean))


## Q8
coverage <- inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question == "Q8") %>% summarise(mean = mean(lower < 1971 & upper > 1971, na.rm=TRUE)) %>% as.numeric
inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question=="Q8") %>%
    ggplot(aes(x=factor(student), group=factor(student))) + geom_hline(yintercept=1971, col="red") + geom_linerange(aes(ymin = lower, ymax = upper)) + coord_cartesian(ylim=c(1900, 2018)) +   theme_bw() + theme(axis.title.x=element_blank(), axis.text.x=element_blank()) + ggtitle(sprintf("When was Disney World, Orlando founded? (Coverage = %.2f)", coverage))



## Q4
coverage <- inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question == "Q4") %>% summarise(mean = mean(lower < 119 & upper > 119)) %>% as.numeric

inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question=="Q4") %>%
    ggplot(aes(x=factor(student), group=factor(student))) + geom_hline(yintercept=119, col="red") + geom_linerange(aes(ymin = lower, ymax = upper)) + coord_cartesian(ylim=c(0, 1000)) +   theme_bw() + theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) + ggtitle(sprintf("How many grooves are there on the edge of a quarter? (Coverage = %.2f)", coverage))
        



    filter(question == "Q5") %>% ggplot() + geom_line(aes(x=1:73, y=lower)) + geom_line(aes(x=1:73, y=upper)) + ylim(c(0, 10000000))


inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question == "Q5") 

inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question == "Q3") %>% summarise(mean = mean(lower < 59039 & upper > 59039))

inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question == "Q4") %>% summarise(mean = mean(lower < 119 & upper > 119, na.rm=TRUE))

inner_join(lower, upper) %>% mutate(lower = as.numeric(lower), upper = as.numeric(upper)) %>% filter(question == "Q") %>% summarise(mean = mean(lower < 119 & upper > 119, na.rm=TRUE))



inner_join(lower, upper) %>% mutate(lower = as.numeric(lower))
