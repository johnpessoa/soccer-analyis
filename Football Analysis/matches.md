# Is the Premier League the most competitive league in the world? 
Let's take a look at the spi matches data to analyze the quality of the english teams



Let's take a look at the data that I have from FiveThirtyEight and see what I'll need to do to clean it up


```r
spi_matches_2 <- spi_matches %>% 
  mutate(match_id = row_number())

spi_matches_2 %>% 
  head() %>% 
  knitr::kable()
```



|date       | league_id|league                  |team1         |team2               |  spi1|  spi2|  prob1|  prob2| probtie| proj_score1| proj_score2| importance1| importance2| score1| score2|  xg1|  xg2| nsxg1| nsxg2| adj_score1| adj_score2| match_id|
|:----------|---------:|:-----------------------|:-------------|:-------------------|-----:|-----:|------:|------:|-------:|-----------:|-----------:|-----------:|-----------:|------:|------:|----:|----:|-----:|-----:|----------:|----------:|--------:|
|2016-08-12 |      1843|French Ligue 1          |Bastia        |Paris Saint-Germain | 51.16| 85.68| 0.0463| 0.8380|  0.1157|        0.91|        2.36|        32.4|        67.7|      0|      1| 0.97| 0.63|  0.43|  0.45|       0.00|       1.05|        1|
|2016-08-12 |      1843|French Ligue 1          |AS Monaco     |Guingamp            | 68.85| 56.48| 0.5714| 0.1669|  0.2617|        1.82|        0.86|        53.7|        22.9|      2|      2| 2.45| 0.77|  1.75|  0.42|       2.10|       2.10|        2|
|2016-08-13 |      2411|Barclays Premier League |Hull City     |Leicester City      | 53.57| 66.81| 0.3459| 0.3621|  0.2921|        1.16|        1.24|        38.1|        22.2|      2|      1| 0.85| 2.77|  0.17|  1.25|       2.10|       1.05|        3|
|2016-08-13 |      2411|Barclays Premier League |Burnley       |Swansea City        | 58.98| 59.74| 0.4482| 0.2663|  0.2854|        1.37|        1.05|        36.5|        29.1|      0|      1| 1.24| 1.84|  1.71|  1.56|       0.00|       1.05|        4|
|2016-08-13 |      2411|Barclays Premier League |Middlesbrough |Stoke City          | 56.32| 60.35| 0.4380| 0.2692|  0.2927|        1.30|        1.01|        33.9|        32.5|      1|      1| 1.40| 0.55|  1.13|  1.06|       1.05|       1.05|        5|
|2016-08-13 |      2411|Barclays Premier League |Southampton   |Watford             | 69.49| 59.33| 0.5759| 0.1874|  0.2367|        1.91|        1.05|        34.1|        30.7|      1|      1| 1.05| 0.22|  1.52|  0.41|       1.05|       1.05|        6|


