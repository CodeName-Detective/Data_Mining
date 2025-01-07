---
title: "EAS509 Homework 6 (50 points)."
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)

library(survival)
library(survminer)

library(ggplot2)
library(plotly)
library(cowplot)
```

Submit your answers as a single pdf attach all R code. Failure to do so will result in grade reduction.

# Question 1 (25 points)
For each question, state whether or not the censoring mechanism is independent. Justify your answer with a short statement. (5 points for each)

### a)	In a study of disease relapse, due to a careless research scientist, all patients whose phone numbers begin with the number “2” are lost to follow up.

The censoring of patients in the study due to phone numbers starting with "2" appears random and not connected to the actual disease relapse. This suggests that such censoring is **INDEPENDENT** and does not impact the study's results.

### b)	In a study of longevity, a formatting error causes all patient ages that exceed 99 years to be lost (i.e. we know that those patients are more than 99 years old, but we do not know their exact ages).

The omission of precise age details for patients over 99 years old, caused by a formatting glitch, is a technicality and isn't related to the actual lifespan of the patients. This indicates that the censoring is **INDEPENDENT** of their longevity.

### c)	Hospital A conducts a study of longevity. However, very sick patients tend to be transferred to Hospital B, and are lost to follow up.

The transfer of patients to Hospital B because of critical illnesses suggests that the censoring is linked to the patients' health condition, a crucial factor in the longevity study. Therefore, the censoring is **NOT INDEPENDENT**.

### d)	In a study of unemployment duration, the people who find work earlier are less motivated to stay in touch with study investigators, and therefore are more likely to be lost to follow up.

Individuals finding work earlier are less motivated to stay in touch, suggesting that the duration of unemployment influences the likelihood of being lost to follow-up. This relationship between the study outcome (unemployment duration) and the likelihood of censoring indicates **NOT INDEPENDENT** censoring.

### e)	In a study of pregnancy duration, women who deliver their babies pre-term are more likely to do so away from their usual hospital, and thus are more likely to be censored, relative to women who deliver full-term babies.

Women who give birth prematurely are more prone to being excluded from the study, which directly ties to the main focus of the research: the length of pregnancy. This indicates that the exclusion is affected by the key variable being examined, showing that the censoring is **NOT INDEPENDENT**.

# Question 2 (25 points)
A data set from "DATA.csv" represents publication times for 244 clinical trials 
funded by the National Heart, Lung, and Blood Institute. Using Log-Rank Test 
in R, estimate if the Kaplan-Meier Survival Curves from two subpopulations 
stratified by “posres” variable are significantly different.


```{r}
data <- readr::read_csv("DATA.csv")
data
```



```{r}
surv_obj <- Surv(time = data$time, event = data$status)
fit.surv <- survfit(surv_obj ~ posres, data = data)

ggsurvplot(fit.surv, data = data, pval = TRUE, risk.table = TRUE, conf.int = TRUE)
```



```{r}
logrank_test <- survdiff(surv_obj ~ posres, data = data)
logrank_test
```

We can see a very high p-value which is much greater than alpha 0.05. So we fail to reject the Null-Hypothesis that there is no significant difference between the survival curves of the two groups.

That means there is no strong evidence to suggest that the "posres" variable significantly affects the survival distributions in your clinical trials dataset.








