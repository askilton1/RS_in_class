library(dplyr)
RS <- (read.csv("RS.csv"))
RS

library(psych)
pairs.panels(RS)
pairs(RS)

source("my_aggr.r")
my_aggr(RS)

select_if(is.numeric)

RS %>%
  select(-Inc_Cat) %>%
  na.omit

sum_cat <- function(...,data){
  data2 <- data %>%
    group_by_(...) %>%
    summarise_all(funs(mean(.,na.rm=T))) %>%
    select_if(is.numeric) %>% select(-Year)
  rownames(data2) <- levels(select_(...))
}

factorNames <- names(select_if(RS,is.factor))
lapply(factorNames,function(x) (sum_cat(x,data=RS)))

lapply(factorNames,function(x) x )
sum_cat("Gender",data=RS)
data <- RS

data2 <- data %>%
  group_by_("Gender") %>%
  summarise_all(funs(mean(.,na.rm=T))) %>% 
  select_if(is.numeric) %>% select(-Year) %>% data.frame
rownames(data2) <- ifelse(is.na(unique(select_(RS,"Gender")[[1]])),"NA",unique((select_(RS,"Gender")[[1]])))
data <- RS
