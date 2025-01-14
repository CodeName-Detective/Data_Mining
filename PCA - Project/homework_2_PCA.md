---
title: "Homework 2. PCA. (60 Points)"
author: "Shri Harsha"
date: '2023-09-27'
output:
  html_document:
    df_print: paged
  pdf_document: default
always_allow_html: true
---


# Part 1. PCA vs Linear Regression (6 points).

Let's say we have two 'features': let one be $x$ and another $y$.
Recall that in linear regression, we are looking to get a model like:


$$y_i=\beta_0+\beta_1*x_i+\varepsilon_i$$

after the fitting, for each data point we would have:
$$y_i=\hat{\beta_0}+\hat{\beta_1}*x_i+r_i$$
where $r_i$ is residual. It can be rewritten as:

$$\hat{\beta_0}+r_i=y_i-\hat{\beta_1}*x_i\;\;\;\;\;(1)$$

The first principal component $z_1$ calculated on $(x,y)$ is
$$z_{i1}=\phi_{i1}y_i+\phi_{i2}x_i$$
Dividing it by $\phi_{i1}$:
$$\frac{z_{i1}}{\phi_{i1}}=y_i+\frac{\phi_{i2}}{\phi_{i1}}x_i\;\;\;\;\;(2)$$

There is a functional resemblance between equations (1) and (2) (described linear relationship between $y$ and $x$). Is the following true:

$$\hat{\beta_0}+r_i=\frac{z_{i1}}{\phi_{i1}}$$
$$\frac{\phi_{i2}}{\phi_{i1}}=-\hat{\beta_1}$$
**Answer**: *(just yes or no)* : YES


What is the difference between linear regression coefficients optimization and first PCA calculations?


**Answer**:


**Linear Regression Coefficients Optimization**:

Linear regression minimizes the vertical distances between the data points and the hyperplane defined by the regression equation.

The coefficients in linear regression represent the weights assigned to each feature to create a linear combination that best fits the data. 

**Principal Component Analysis (PCA) Calculations**:

PCA seeks to minimize the sum of squared perpendicular distances from the data points to the subspace formed by the principal components.

In PCA, the coefficients represent how much each original feature contributes to each principal component.

*(here should be the answer. help yourself with a plot)*


# Part 2. PCA Exercise (27 points).

In this exercise we will study UK Smoking Data (`smoking.R`, `smoking.rda` or `smoking.csv`):

**Description**

Survey data on smoking habits from the UK. The data set can be used for analyzing the demographic characteristics of smokers and types of tobacco consumed.

**Format**

A data frame with 1691 observations on the following 12 variables.

`gender` - Gender with levels Female and Male.

`age` - Age.

`marital_status` - Marital status with levels Divorced, Married, Separated, Single and Widowed.

`highest_qualification` - Highest education level with levels A Levels, Degree, GCSE/CSE, GCSE/O Level, Higher/Sub Degree, No Qualification, ONC/BTEC and Other/Sub Degree

`nationality` - Nationality with levels British, English, Irish, Scottish, Welsh, Other, Refused and Unknown.

`ethnicity` - Ethnicity with levels Asian, Black, Chinese, Mixed, White and Refused Unknown.

`gross_income` - Gross income with levels Under 2,600, 2,600 to 5,200, 5,200 to 10,400, 10,400 to 15,600, 15,600 to 20,800, 20,800 to 28,600, 28,600 to 36,400, Above 36,400, Refused and Unknown.

`region` - Region with levels London, Midlands & East Anglia, Scotland, South East, South West, The North and Wales

`smoke` - Smoking status with levels No and Yes

`amt_weekends` - Number of cigarettes smoked per day on weekends.

`amt_weekdays` - Number of cigarettes smoked per day on weekdays.

`type` - Type of cigarettes smoked with levels Packets, Hand-Rolled, Both/Mainly Packets and Both/Mainly Hand-Rolled

Source
National STEM Centre, Large Datasets from stats4schools, <https://www.stem.org.uk/resources/elibrary/resource/28452/large-datasets-stats4schools>.

Obtained from <https://www.openintro.org/data/index.php?data=smoking>

## Read and Clean the Data

2.1 Read the data from smoking.R or smoking.rda (3 points)
> hint: take a look at source or load functions
>       there is also smoking.csv file for a refference

```{r setup, results="hide", warning=F, message=F}
# load libraries
library(tibble)
library(readr)
library(dplyr)
library(data.table)

library(broom)
library(cowplot)

library(ggplot2)
library(ggbiplot)
library(fastDummies)

library(plotly)
```

```{r}
# Load data
smoke_data <- fread("smoking.csv")
```

Take a look into data
```{r}
# place holder
head(smoke_data)
```

There are many fields there so for this exercise lets only concentrate on 
smoke, gender, age, marital_status, highest_qualification and gross_income.

Create new data.frame with only these columns.

```{r}
# place holder
smoke_data_new <- smoke_data[ ,c("smoke", "gender", "age", "marital_status", "highest_qualification", "gross_income")]

head(smoke_data_new)
```


2.2 Omit all incomplete records.(3 points)

```{r}
# place holder
smoke_data_new$gross_income[smoke_data_new$gross_income=='Unknown'] <- NA
smoke_data_new$gross_income[smoke_data_new$gross_income=='Refused'] <- NA

smoke_data_new <- na.omit(smoke_data_new)
```

2.3 For PCA feature should be numeric. Some of fields are binary (`gender` and `smoke`) 
and can easily be converted to numeric type (with one and zero).
Other fields like `marital_status` has more than two categories, convert
them to binary (e.g. is_married, is_devorced). Several features in the data 
set are ordinal (`gross_income` and `highest_qualification`), convert them 
to some king of sensible level (note that levels in factors are not in order). 
(3 points)


```{r}
# place holder
smoke_data_new$gender <- as.integer(smoke_data_new$gender == 'Male')

smoke_data_new <- dummy_cols(smoke_data_new, select_columns = c( "marital_status", "highest_qualification", "gross_income"))%>%
  select(-marital_status, -highest_qualification, -gross_income)


head(smoke_data_new)
```

2.4. Do PCA on all columns except smoking status. (3 points)

```{r}
# place holder
pca_fit <- smoke_data_new %>% 
  select(where(is.numeric)) %>% # retain only numeric columns
  prcomp(scale = TRUE)
```


2.5 Make a scree plot (3 points)

```{r}
# place holder
PVE <- tibble(
  PC=1:length(pca_fit$sdev),
  Var=pca_fit$sdev^2,
  PVE=Var/sum(Var),
  CumPVE=cumsum(PVE)
)
PVE

cowplot::plot_grid(
qplot(data=PVE,x=PC,y=PVE,geom=c("point","line"),
      xlab = "Principal Component",
      ylab = "Proportion of Variance Explained"),
qplot(data=PVE,x=PC,y=CumPVE,geom=c("point","line"),
      xlab = "Principal Component",
      ylab = "Cumulative Proportion of Variance Explained")
)
```
Comment on the shape, 
if you need to reduce dimensions home many would you choose


```
After first two principal components there is a sharpe decrease in the varaince explained by the next set of principal components. And the last three principal components are explaining nothing showing those there are highly correlated with other principal components.

The shape of the curve is still elbow for scree plot. If we draw cummulative varaince exlained as function of principal components we can see a linear increase in the explainability later got flattened out.

If I have to reduce the dimensions I will take 19 Principal Components.
```

2.6 Make a biplot color points by smoking field. (3 points)

```{r}
# place holder
smoke <- smoke_data_new$smoke
ggplotly(ggbiplot(pca_fit, scale = 0,labels = pca_fit$x %>% rownames()) + geom_point(aes(color = smoke)))
```

Comment on observed biplot.

```
We can clearly see that Qualification status has more effect on 1st principal component and Martial status has more effect on the 2nd principal component.
```

Can we use first two PC to discriminate smoking?

```
No. Because even after plotting points in frist two principal components we can see that data is cloudy no real seperation between smoking and not smoking.
```

2.7 Based on the loading vector can we name PC with some descriptive name? (3 points)

```
1st Principal Component - Education qualification.
2nd Principal Component - Martial Status.
```

2.8 May be some of splits between categories or mapping to numerics should be revisited, if so what will you do differently? (3 points)

```
May be we can change the encoding of highest_qualification and gross_income from dummy varibles to ordinal encoding.
```

2.9 Follow your suggestion in 2.10 and redo PCA and biplot (3 points)


```{r}
smoke_data_alter <- smoke_data[ ,c("smoke", "gender", "age", "marital_status", "highest_qualification", "gross_income")]

smoke_data_alter$gross_income[smoke_data_alter$gross_income=='Unknown'] <- NA
smoke_data_alter$gross_income[smoke_data_alter$gross_income=='Refused'] <- NA

smoke_data_alter <- na.omit(smoke_data_alter)
smoke_data_alter$gender <- as.integer(smoke_data_alter$gender == 'Male')

# Define the ordinal mapping
qualification_ordinal_mapping <- c("No Qualification" = 1, 
                      "GCSE/CSE" = 2, 
                      "GCSE/O Level" = 3, 
                      "ONC/BTEC" = 4, 
                      "A Levels" = 5, 
                      "Other/Sub Degree" = 6, 
                      "Degree" = 7, 
                      "Higher/Sub Degree" = 8)

# Perform ordinal encoding
smoke_data_alter$highest_qualification <- as.integer(factor(smoke_data_alter$highest_qualification, levels = names(qualification_ordinal_mapping), ordered = TRUE))

# Define the ordinal mapping
income_ordinal_mapping <- c("Under 2,600" = 1, 
                      "2,600 to 5,200" = 2, 
                      "5,200 to 10,400" = 3, 
                      "10,400 to 15,600" = 4, 
                      "15,600 to 20,800" = 5, 
                      "20,800 to 28,600" = 6, 
                      "28,600 to 36,400" = 7, 
                      "Above 36,400" = 8)

# Perform ordinal encoding
smoke_data_alter$gross_income <- as.integer(factor(smoke_data_alter$gross_income, levels = names(income_ordinal_mapping), ordered = TRUE))



smoke_data_alter <- dummy_cols(smoke_data_alter, select_columns = c( "marital_status"))%>%
  select(-marital_status)

pca_fit <- smoke_data_alter %>% 
  select(where(is.numeric)) %>% # retain only numeric columns
  prcomp(scale = TRUE)

ggplotly(ggbiplot(pca_fit, scale = 0,labels = pca_fit$x %>% rownames()))

```


# Part 3. Freestyle. (27 points).

Get the data set from your final project (or find something suitable). The data set should have at least four variables and it shouldn't be used in class PCA examples: iris, mpg, diamonds and so on).

* Convert a columns to proper format (9 points)
* Perform PCA (3 points)
* Make a skree plot (3 points)
* Make a biplot (3 points)
* Discuss your observations (9 points)

```{r}
whl_df <- read_csv("Wholesale customers data.csv")

head(whl_df)
```
```{r}
sum(is.na(whl_df))
```
```{r}
whl_pca_fit <- whl_df %>% 
  select(where(is.numeric)) %>% # retain only numeric columns
  prcomp(scale = TRUE)
whl_pca_fit
```

```{r}
PVE <- tibble(
  PC=1:length(whl_pca_fit$sdev),
  Var=whl_pca_fit$sdev^2,
  PVE=Var/sum(Var),
  CumPVE=cumsum(PVE)
)
PVE

cowplot::plot_grid(
qplot(data=PVE,x=PC,y=PVE,geom=c("point","line"),
      xlab = "Principal Component",
      ylab = "Proportion of Variance Explained"),
qplot(data=PVE,x=PC,y=CumPVE,geom=c("point","line"),
      xlab = "Principal Component",
      ylab = "Cumulative Proportion of Variance Explained")
)
```


```{r}
ggplotly(ggbiplot(whl_pca_fit, scale = 0,labels = whl_pca_fit$x %>% rownames()))
```


**Observations:**
```
1. First two principal components expalain more than 60% of variance in the data.
2. First 5 principal components expalin almost 90% of variance.
3. Upon examining the biplot, it becomes evident that the type of product, such as whether it falls into categories like Grocery, Milk, or Detergent, significantly influences the first principal component. On the other hand, the characteristics of the product itself, such as whether it is categorized as Fresh or Frozen, notably impact the second principal component.
```
