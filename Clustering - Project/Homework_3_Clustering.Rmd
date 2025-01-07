---
title: "Homework 3. Clustering Practice (80 Points)"
author: "Shri Harsha"
date: '2023-10-12'
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(cluster)
library(magrittr)
library(plotly)
library(caret)
library(ggbiplot)
library(cowplot)

#Plotting Dendrogram
library(ggdendro)
```



# Part 1. USArrests Dataset and Hierarchical Clustering (20 Points)

Consider the “USArrests” data. It is a built-in dataset you may directly get 
in RStudio. Perform hierarchical clustering on the observations (states) and 
answer the following questions.


```{r}
head(USArrests)
```


**Q1.1.** Using hierarchical clustering with complete linkage and Euclidean distance, 
cluster the states. (5 points)


```{r}
set.seed(69)
us_arrests <- na.omit(USArrests)
hier_clust <- hclust(dist(us_arrests), method = "complete")
plot(hier_clust, main = "Complete Linkage", xlab = "", sub = "")
```


**Q1.2.** Cut the dendrogram at a height that results in three distinct clusters. 
Interpret the clusters. Which states belong to which clusters? (5 points)


```{r}
clusters <- cutree(hier_clust, k = 3)
state_names <- rownames(us_arrests)
cluster_data <- data.frame(State = state_names, Cluster = clusters)
cluster_states_1 <- cluster_data$State[cluster_data$Cluster == 1]
cluster_states_1
```

```{r}
cluster_states_2 <- cluster_data$State[cluster_data$Cluster == 2]
cluster_states_2
```

```{r}
cluster_states_3 <- cluster_data$State[cluster_data$Cluster == 3]
cluster_states_3
```

```{r}
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  lakecolor = toRGB('white')
)



plot_geo() %>%
  add_trace(
    z = ~cluster_data$Cluster, text = ~cluster_data$State, span = I(0),
    locations = ~state.abb, locationmode = 'USA-states'
  ) %>%
  layout(geo = g)
```


**Q1.3** Hierarchically cluster the states using complete linkage and Euclidean 
distance, after scaling the variables to have standard deviation one. Obtain three clusters. Which states belong to which clusters?(5 points)

```{r}
us_arrests <- na.omit(USArrests)
scaled_arrests <- scale(us_arrests)
hier_clust <- hclust(dist(us_arrests), method = "complete")
plot(hier_clust, main = "Complete Linkage", xlab = "", sub = "")
```


```{r}
clusters <- cutree(hier_clust, k = 3)
state_names <- rownames(us_arrests)
cluster_data <- data.frame(State = state_names, Cluster = clusters)
cluster_states_1 <- cluster_data$State[cluster_data$Cluster == 1]
cluster_states_1
```

```{r}
cluster_states_2 <- cluster_data$State[cluster_data$Cluster == 2]
cluster_states_2
```

```{r}
cluster_states_3 <- cluster_data$State[cluster_data$Cluster == 3]
cluster_states_3
```

```{r}
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  lakecolor = toRGB('white')
)



plot_geo() %>%
  add_trace(
    z = ~cluster_data$Cluster, text = ~cluster_data$State, span = I(0),
    locations = ~state.abb, locationmode = 'USA-states'
  ) %>%
  layout(geo = g)
```


**Q1.4** What effect does scaling the variables have on the hierarchical 
clustering obtained? In your opinion, should the variables be scaled before 
the inter-observation dissimilarities are computed? Provide a justification 
for your answer. *(5 points)*


*Answer:* Scaling in hierarchical clustering is a process that adjusts the numerical values of variables to a common scale. It's done to prevent variables with larger ranges or variances from dominating the clustering process. Scaling ensures that each variable is given equal importance in the clustering analysis, allowing the relationships and patterns within the data to be accurately reflected. However, scaling is not always necessary; if your variables are already on a similar scale or scaling would distort their meaning, you can skip this step. The key is to make clustering results unbiased and based on the actual data patterns. In this particular example scaling made the dendogram a little balanced.


# Part 2. Market Segmentation (60 Points)

An advertisement division of large club store needs to perform customer analysis 
the store customers in order to create a segmentation for more targeted marketing campaign 

You task is to identify similar customers and characterize them (at least some of them). 
In other word perform clustering and identify customers segmentation.

This data-set is derived from https://www.kaggle.com/imakash3011/customer-personality-analysis

```
Colomns description:
People
  ID: Customer's unique identifier
  Year_Birth: Customer's birth year
  Education: Customer's education level
  Marital_Status: Customer's marital status
  Income: Customer's yearly household income
  Kidhome: Number of children in customer's household
  Teenhome: Number of teenagers in customer's household
  Dt_Customer: Date of customer's enrollment with the company
  Recency: Number of days since customer's last purchase
  Complain: 1 if the customer complained in the last 2 years, 0 otherwise

Products

  MntWines: Amount spent on wine in last 2 years
  MntFruits: Amount spent on fruits in last 2 years
  MntMeatProducts: Amount spent on meat in last 2 years
  MntFishProducts: Amount spent on fish in last 2 years
  MntSweetProducts: Amount spent on sweets in last 2 years
  MntGoldProds: Amount spent on gold in last 2 years

Place
  NumWebPurchases: Number of purchases made through the company’s website
  NumStorePurchases: Number of purchases made directly in stores
```

Assume that data was current on 2014-07-01

**Q2.1.** Read Dataset and Data Conversion to Proper Data Format *(12 points)*

Read "m_marketing_campaign.csv" using `data.table::fread` command, examine the data.


```{r}
# fread m_marketing_campaign.csv and save it as df (2 points)
library(data.table)
market_df = fread('m_marketing_campaign.csv')
head(market_df)
```



```{r}
# Convert Year_Birth to Age (assume that current date is 2014-07-01) (2 points)

market_df <- market_df %>%
  mutate(Age = year(as.Date("2014-07-01")) - Year_Birth)


# Dt_Customer is a date (it is still character), convert it to membership days (i.e. number of days person is a member, name it MembershipDays)
# hint: note European date format, use as.Date with proper format argument (2 points)

market_df <- market_df %>%
  mutate(MembershipDays = as.integer(difftime(as.Date("2014-07-01"), as.Date(Dt_Customer, format = "%d-%m-%Y"), units = "days")))

head(market_df)


```

```{r}
# Summarize Education column (use table function) (2 points)


# Lets create a new column EducationLevel from Education
# Lets treat Education column as ordinal categories and use years in education as a levels 
# for distance calculations (2 points)
# Assuming following order and years spend for education:
#    HighSchool (13 years), Associate(15 years), Bachelor(17 years), Master(19 years), PhD(22 years)
# create EducationLevel from Education
# hint: use recode function (in mutate statement)

# Define the years spent for each education level
years_for_education <- c("HighSchool" = 13, "Associate" = 15, "Bachelor" = 17, "Master" = 19, "PhD" = 22)

# Create the "EducationLevel" column based on the specified order and years spent for education
market_df <- market_df %>%
  mutate(EducationLevel = recode(Education, !!!years_for_education))

head(market_df)
```

```{r}
# Summarize Marital_Status column (use table function) 


# Lets convert single Marital_Status categories for 5 separate binary categories  (2 points)
# Divorced, Married, Single, Together and Widow, the value will be 1 if customer 
# is in that category and 0 if customer is not
# hint: use dummy_cols from fastDummies or dummyVars from caret package, model.matrix 
# or simple comparison (there are only 5 groups)
# Keep Marital_Status for later use


# Define the five categories
categories <- c("Divorced", "Married", "Single", "Together", "Widow")

martia_status = market_df$Marital_Status
# Use the pivot_wider function to convert Marital_Status into binary columns
market_df <- market_df %>%
  mutate(Value = 1) %>%   # Add a temporary Value column with 1
  pivot_wider(names_from = Marital_Status, values_from = Value, values_fill = 0) 

market_df$Marital_Status = martia_status
head(market_df)
```

```{r}
# lets remove columns which we will no longer use:
# remove ID, Year_Birth, Dt_Customer, Education, Marital_Status
# and save it as df_sel 

df_sel <- market_df %>%
  select(-ID, -Year_Birth, -Dt_Customer, -Education, -Marital_Status)
head(df_sel)

```


```{r}
# lets scale (2 points)
# run scale function on df_sel and save it as df_scale
# that will be our scaled values which we will use for analysis
df_scale <- as.data.frame(scale(df_sel))
head(df_scale)


```

## PCA

**Q2.2.** Run PCA, make biplot and scree plot *(6 points)*

```{r}
# Run PCA on df_scale, make biplot and scree plot/percentage variance explained plot
# save as pc_out, we will use pc_out$x[,1] and pc_out$x[,2] later for plotting

pc_out <- prcomp(df_scale, center = TRUE, scale. = TRUE)
ggplotly(ggbiplot(pc_out, scale = 0,labels = pc_out$x %>% rownames()))

```

```{r}
PVE <- tibble(
  PC=1:length(pc_out$sdev),
  Var=pc_out$sdev^2,
  PVE=Var/sum(Var),
  CumPVE=cumsum(PVE)
)
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
plot(pc_out$x[,1], pc_out$x[,2])
```

**Q2.3** Comment on observation (any visible distinct clusters?) *(2 points)*
Though there is a overlap I can see two clsters can maybe formed.



## Cluster with K-Means
In questions Q2.4 to Q2.9 use K-Means method for clustering

### Selecting Number of Clusters

**Q2.4** Select optimal number of clusters using elbow method. *(4 points)*


```{r}
km_out_list <- lapply(1:10, function(k) list(
   k=k,
   km_out=kmeans(df_scale, k, nstart = 20)))
km_results <- data.frame(
k=sapply(km_out_list, function(k) k$k),
totss=sapply(km_out_list, function(k) k$km_out$totss), tot_withinss=sapply(km_out_list, function(k) k$km_out$tot.withinss) )
plot_ly(km_results,x=~k,y=~tot_withinss) %>% add_markers() %>% add_paths()
```
We can see sharp dip at two 2 clusters. But as it has no significant meaning because of domain knowledge we will go with other dips where we find elbows 7 and 9. May be as suggested previously 9 clusters can be formed.

**Q2.5** Select optimal number of clusters using Gap Statistic.*(4 points)*

```{r}
suppressWarnings({
   gap_kmeans <- clusGap(df_scale, kmeans, nstart = 20, K.max = 10, B = 100)
 })
 plot(gap_kmeans, main = "Gap Statistic: kmeans")
```

At 2 we have local maximum, so we can select 2 clusters as suggested above, but after that the Gap is continuous to increase there is no definitive peak to decide.

**Q2.6** Select optimal number of clusters using Silhouette method.*(4 points)*

```{r}
results <- lapply(2:20, function(k) {
   kmeans_cluster <- kmeans(df_scale, k, nstart=20)
   si <- silhouette(kmeans_cluster$cluster, dist = dist(df_scale))
   data.frame(k=k,sil_width=mean(si[,'sil_width']),sil_width_min=min(si[,'sil_width']))
 })
 si_df <- bind_rows(results)
 plot_ly(si_df, x=~k,y=~sil_width) %>%
   add_markers() %>% add_lines() %>%
   add_markers(y=~sil_width_min) %>% add_lines(y=~sil_width_min)

```

At 2 we have local maximum, so we can select 2 clusters as suggested above.


**Q2.7** Which k will you choose based on elbow, gap statistics and silhouette 
as well as clustering task (market segmentation for advertisement purposes, that is two groups don't provide sufficient benefit over a single groups)?*(4 points)*

I feel like all there methods agree on 2 being the value of K. But as domain knowledge says having 2 clusters doesn't serve any purpose we will go with 9 clusters.

Though there are some local maximums for 9, 14 and 19 in silhouette method, we can still go with 9 as all three methods agreed on it.

## Clusters Visulalization

**Q2.8** Make k-Means clusters with selected k_kmeans (store result as km_out).
Plot your k_kmeans clusters on biplot (just PC1 vs PC2) by coloring points by their cluster id.*(4 points)*


```{r}
k_kmeans <- 9
 km_out <- kmeans(df_scale, centers = k_kmeans, nstart = 20)
 cluster_ids <- km_out$cluster
 
 plot(pc_out$x[, 1], pc_out$x[, 2], col = cluster_ids, pch = 16, xlab = "PC1", ylab = "PC2", main = "Biplot: PC1 vs PC2 with Cluster IDs")
 legend("topright", legend = unique(cluster_ids), col = unique(cluster_ids), pch = 16)
```

**Q2.9** Do you see any grouping? Comment on you observation.*(2 points)*


*Answer* Actually not. Everything seems pretty clumsy in 2 dimension may be in 3 dimensions we might actually see the clusters.


## Characterizing Cluster

**Q2.10** Perform descriptive statistics analysis on obtained cluster. 
Based on that does one or more group have a distinct characteristics? *(8 points)*
Hint: add cluster column to original df dataframe

```{r}
market_df$cluster<-as.factor(km_out$cluster)
 numerical_df <- market_df %>%
   select(-ID, -Year_Birth) %>%
   select_if(is.numeric)
 numerical_df$cluster <- market_df$cluster
 head(numerical_df)
```


```{r}
cluster_stats_mean <- numerical_df %>%
  group_by(cluster) %>%
  summarise_all(list(mean = mean))
# View the statistics
print(cluster_stats_mean)
```

```{r}
# Median of each numerical columns grouped by cluster ID
cluster_stats_median <- numerical_df %>%
  group_by(cluster) %>%
  summarise_all(list(median = median))
# View the statistics
print(cluster_stats_median)
```


```{r}
# Mode of each numerical columns grouped by cluster ID
calculate_mode <- function(x) {
uniq_x <- unique(x)
uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
cluster_stats_mode <- numerical_df %>%
  group_by(cluster) %>%
  summarise_all(list(mode = ~calculate_mode(.)))
# View the statistics
print(cluster_stats_mode)
```

## Cluster with Hierarchical Clustering


**Q2.11** Perform clustering with Hierarchical method (Do you need to use scaling here?).
Try complete, single and average linkage.
Plot dendagram, based on it choose linkage and number of clusters, if possible, explain your
choice. *(8 points)*

```{r}
hc.complete <- hclust(dist(df_scale), method = "complete")
 ggdendrogram(hc.complete, segements=TRUE, labels=TRUE, leaf_labels = TRUE, rotate=FALSE, theme_dendro = TRUE) +
  labs(title='Complete Linkage')
```


```{r}
hc.single <- hclust(dist(df_scale), method = "single")
ggdendrogram(hc.single, segements=TRUE, labels=TRUE, leaf_labels = TRUE, rotate=FALSE, theme_dendro = TRUE) +
 labs(title='Single Linkage')
```

```{r}
hc.average <- hclust(dist(df_scale), method = "average")
 ggdendrogram(hc.average, segements=TRUE, labels=TRUE, leaf_labels = TRUE, rotate=FALSE, theme_dendro = TRUE) +
  labs(title='Average Linkage')

```

All three Dendograms shows that they are highly imbalanced irrespective of the linkage. And also agrees that  making 2 clusters is the best choice.  But as 2 clusters doesn't have any significant meaning in the domain we can 9 clusters here too.

# Additional grading criteria:

**G3.1** Was all random methods properly seeded? *(2 points)*

Yes I set a global seed of 69 at the beginning of the RMD Files. So it the same seed will be used throughout the file.

