
####################################
# R code for the article entitled,
# "Smartphone Use for Communication and Learning 
# among University Students in Bangladesh"
# 
# Date: 09/01/2025
####################################

####################################
# R code for reproducing Table 2
####################################

## Communication counts data:
comm.counts <- list(Age=matrix(c(201, 237, 99, 78), nrow = 2, 
                    byrow = TRUE, dimnames=list(Age = c("<=23", ">23"),
                    Comm = c("Below", "Above"))),

                    MotherOcc = matrix(c(236, 273,64, 42), nrow = 2, 
                    byrow = TRUE, dimnames = list(Mother = c("Housewife", "Others"),
                    Comm = c("Below", "Above"))),

                    Wealth = matrix(c(124, 86, 37, 38, 167, 163), nrow=3, 
                    byrow = TRUE, dimnames = list(Wealth = c("Low", "Medium", "High"),
                    Comm = c("Below", "Above"))),

                    StartSmartphone = matrix(c(219, 257, 81, 58), nrow=2, 
                    byrow = TRUE, dimnames = list(StartAge = c("<=18", ">18"), 
                    Comm = c("Below", "Above"))),

					StudyHours = matrix(c(124, 134, 89, 96, 64, 54, 23, 31), nrow = 4, 
                    byrow = TRUE, dimnames = list(Hours = c("<5","5-9","10-14",">14"),
                    Comm = c("Below","Above"))),

                    Extracurricular = matrix(c(150, 185, 149, 130), nrow = 2, 
                    byrow = TRUE, dimnames = list(Extra = c("Yes","No"),
                    Comm = c("Below","Above"))),
  
                    Overuse = matrix(c(135, 94, 165, 221), nrow = 2, 
                    byrow = TRUE, dimnames = list(Overuse = c("Occasional/Less", "Frequent/More"),
                    Comm = c("Below","Above")))
                    )

## Knowledge counts data:
know.counts <- list(Age = matrix(c(142, 177, 165, 131), nrow = 2, byrow = TRUE,
                    dimnames = list(Age = c("<=23", ">23"), Know = c("Below", "Above"))),

                    MotherOcc = matrix(c(257, 252, 50, 56), nrow = 2, byrow = TRUE, 
                    dimnames = list(Mother = c("Housewife","Others"),
                    Know = c("Below","Above"))),

                    Wealth = matrix(c(127, 83, 29, 46, 152, 178), nrow = 3, byrow = TRUE,
                    dimnames = list(Wealth = c("Low","Medium","High"),
                    Know = c("Below","Above"))),

                    StartSmartphone = matrix(c(239, 237, 68, 71), nrow = 2, byrow = TRUE,
                    dimnames = list(StartAge = c("<=18", ">18"),
                    Know = c("Below","Above"))),
  
                    StudyHours = matrix(c(144, 114, 95, 90, 47, 71, 21, 33), nrow = 4, 
                    byrow = TRUE, dimnames = list(Hours = c("<5","5-9","10-14",">14"),
                    Know = c("Below","Above"))),
  
                    Extracurricular = matrix(c(151, 184, 156, 123), nrow = 2, 
                    byrow = TRUE, dimnames = list(Extra = c("Yes","No"),
                    Know = c("Below","Above"))),
  
                    Overuse = matrix(c(134, 95, 173, 213), nrow = 2, byrow = TRUE,
                    dimnames = list(Overuse = c("Occasional/Less","Frequent/More"),
                    Know = c("Below","Above")))
                    )

## This function can be used to 
## calculate p-values shown in Table 2:
pval.calc <- function(counts) {
  res <- chisq.test(counts, correct = FALSE)$p.value
  return(round(res, 3))
}

## This function can produce Table 2 summary:
table.summary <- function(count.list) {
  df.list <- list()
  for(var in names(count.list)) {
    tab <- count.list[[var]]
    total <- rowSums(tab)
    below <- tab[, "Below"]
    above <- tab[, "Above"]
    pval <- pval.calc(tab)
    perc.below <- round(below/total*100, 1)
    perc.above <- round(above/total*100, 1)
    df <- data.frame(
      Variable = rep(var, nrow(tab)),
      Category = rownames(tab),
      Total = total,

      BelowMedian = paste0(below,"(", perc.below,")"),
	  AboveMedian = paste0(above,"(", perc.above,")"),
      p.value = c(pval, rep("", nrow(tab)-1)),
      stringsAsFactors = FALSE
    )
    df.list[[var]] <- df
  }
  summary.df <- do.call(rbind, df.list)
  return(summary.df)
}

## Table 2 outputs:  
comm.summary <- table.summary(comm.counts)
know.summary <- table.summary(know.counts)

cat("Communication Table:\n")
print(comm.summary, row.names = FALSE)

cat("\n Knowledge Acquisition Table:\n")
print(know.summary, row.names = FALSE)


####################################
# R code for reproducing Tables 3-4
####################################

# Counts data for Communication Score.
# Creating a data.frame object consisting 
# of the reponse variable and two predictors: 
data.counts <- data.frame(
  age.group = c("<=23","<=23","<=23","<=23",">23",">23",">23",">23"),
  start.age = c("<=18","<=18",">18",">18","<=18","<=18",">18",">18"),
  communication = c("Below","Above","Below","Above","Below","Above","Below","Above"),
  count    = c(219,257,81,58,99,78,68,71)
)

print(data.counts)
#  age.group start.age communication count
# 1      <=23      <=18         Below   219
# 2      <=23      <=18         Above   257
# 3      <=23       >18         Below    81
# 4      <=23       >18         Above    58
# 5       >23      <=18         Below    99
# 6       >23      <=18         Above    78
# 7       >23       >18         Below    68
# 8       >23       >18         Above    71


# To illustrate the logistic model fit, the counts
# data from Table 2 are expanded to emulate 
# the individual response data observed in the survey

individual.data <- data.counts[rep(1:nrow(data.counts), data.counts$count), 
                               c("age.group","start.age","communication")]

# Recreating the data.frame and fitting the model:
individual.data$communication <- ifelse(individual.data$communication=="Above", 1, 0)
fit <- glm(communication ~ age.group + start.age, data=individual.data, family=binomial)

summary(fit)

# Calculating the odds ratios and 95% CI: 
exp(cbind(OR = coef(fit), confint(fit)))

