```{r}
library(ggplot2)
library(dplyr)

getwd()
setwd("/Users/daniel/Downloads/")

LondonData <- read.csv('/Users/daniel/Downloads/London_Crimes.csv', header=TRUE)

```


```{r}
#Check if the data has been imported correctly
head(LondonData, n=10)        # Display the first 6 lines of the data frame
tail(LondonData, n=10)        # Display the last 10 lines of the data frame
summary(LondonData)           # Display an overview of the data frame

```

```{r}
#Which are the most dangerous boroughs in London
LondonByYear <- LondonData %>% 
  group_by(borough, year) %>% 
  summarise(value = sum(value)) %>%
  arrange(year,-value)


Analysis_Boroughs <- ggplot(aes(x = borough,y = value/1000, color = year), 
       data = LondonByYear) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Borough") +
    ylab("Number of Occurances in thousands") +
    geom_point() + 
    geom_point(aes(size = value)) +
    stat_summary(
      geom = "point",
      fun.y = "mean",
      col = "black",
      size = 3,
      shape = 24,
      fill = "red"
    )

# Plot Chart 
Analysis_Boroughs
```

````{r}
LondonByCrime <- LondonData %>% 
  group_by(major_category, year) %>% 
  summarise(value = sum(value)) %>%
  arrange(year,-value)

LondonByCrime

Analysis_Occurances_Type <- ggplot(aes(x = major_category, y = value/1000, color = year), 
                             data = LondonByCrime) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Borough") +
  ylab("Number of Occurances in thousands") +
  geom_point() + 
  geom_point(aes(size = value/100)) +
  stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 1,
    shape = 24,
    fill = "red"
  )

# Plot Chart 
Analysis_Occurances_Type

````

````{r}
#Analysis of outlier
WestminsterData <- subset(LondonData, borough == "Westminster") 


Analysis_Outlier_Westminster <- ggplot(WestminsterData, aes(x = major_category, fill = minor_category)) + 
  geom_bar(aes(weight = value/1000)) + 
  xlab("Major Categories") +
  ylab("Number of Occurances in thousands") +
  labs(fill = "Minor Categories") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Plot Chart
Analysis_Outlier_Westminster
````

````{r}
#Analysis of Major Crimes
WestminsterMajor <- WestminsterData %>% 
  group_by(major_category, month) %>% 
  summarise(value = sum(value)) %>%
  arrange(month,-value)


Analysis_Major_Crimes <- ggplot(aes(x=month, y = value/1000, color = major_category),
            data = WestminsterMajor) +
  geom_point() + 
  geom_point(aes(size = value)) +
  xlab("Months") +
  ylab("Number of Occurances in thousands") +
  labs(color = "Major Categories") +
  scale_x_continuous(breaks=seq(1, 12, by = 1)) +
  guides( size = FALSE)


# Plot Chart
Analysis_Major_Crimes
````

