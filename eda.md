``` sh
> library(dplyr)
> library(ggplot2)
> library(tidyr)
> heart <- read.csv("~/Downloads/heart.csv")
> glimpse(heart)
```
<p align="center">
  <img width="700" src="https://github.com/akweiss/heart-disease-r/blob/master/images/hdeda1.png">
</p>

This dataset summarizes data on 1,025 patients and includes 14 variables. The target variable is crucial to this dataset,
where 0 = no heart disease present and 1 = heart disease present. There are 13 other variables we can test with this target
variable to understand certain links to heart disease.

Though age and sex are self-explanatory, the cp variable represents chest pain type and can take on one of four values (0, 1,
2, 3), where 0 is Typical Angina, 1 is Atypical Angina, 2 is Non-Anginal, and 3 is Asymptomatic. The variable trestbps is the patient's 
resting blood pressure, chol is serum cholesterol in mg/dl, and ca is the number of major vessels (0, 1, 2, 3) that were colored by
fluoroscopy. In the interest of being succint, I'll omit testing out the other variables in this analysis. 

``` sh
> by_age <- heart %>%
>   group_by(age) %>%
>   summarize(percent_target = mean(target == 1))
> by_age_filt <- by_age %>%
>   filter(frequency >= 10)
> ggplot(by_age_filt, aes(x = age, y = percent_target)) + geom_point() + geom_smooth()
```

<p align="center">
  <img width="500" src="https://github.com/akweiss/heart-disease-r/blob/master/images/hdeda5.png">
</p>

Here we see almost a cubic relationship between age and the presence of heart disease, but a misleading flaw in this
visualization is that it does not consider the relative weights of each observation. To solve this, we can create a
variable for weighted percentage and make another visualization.

``` sh
> dum = as.data.frame(table(heart$age))
> by_age$frequency <- dum$Freq
> by_age$weighted_target <- by_age$percent_target * by_age$frequency
> ggplot(by_age, aes(x = age, y = weighted_target)) + geom_point() + geom_smooth()
```

<p align="center">
  <img width="500" src="https://github.com/akweiss/heart-disease-r/blob/master/images/hdeda6.png">
</p>

This follows more of a bell curve shape that we might expect. The weighted percent chance of having heart disease
increases from 30 to about 55, where we reach the peak of the curve. From 55 onwards, the weighted percent chance 
of heart disease decreases.

We can extend this to explore the relationship between heart disease, age, and biological sex.

``` sh
> age <- by_age$age
> weighted_sex <- by_age$males_weighted
> females_weighted <- by_age$females_weighted
> female <- rep_len("female", length.out = 41)
> sex <- rep_len("male", length.out = 41)
> patient_sex <- data.frame(sex)
> patient_sex <- data.frame(sex = c(patient_sex$sex, female))
> patient_sex[patient_sex == 1] <- NA
> patient_sex$sex <- as.character(patient_sex$sex)
> patient_sex[is.na(patient_sex)] <- "male"
> patient_age <- data.frame(age)
> patient_age <- data.frame(age = c(patient_age$age, age))
> weighted_sex <- data.frame(weighted_sex)
> weighted_sex <- data.frame(weighted_sex = c(weighted_sex$weighted_sex, females_weighted))
> ggplot(by_sex, aes(age, weighted_sex, color = sex)) + geom_line()
```

<p align="center">
  <img width="500" src="https://github.com/akweiss/heart-disease-r/blob/master/images/hdeda8.png">
</p>

From this we can see that - although males have the larger spikes - females tend to mirror the same general shape
of the male plot. Intuitively, we can check the total number of males vs females in our dataset:

<p align="center">
  <img width="200" src="https://github.com/akweiss/heart-disease-r/blob/master/images/hdeda9.png">
</p>

Since there are significantly less females, and because they follow the same general trend as males, there obvious
perceptual conclusion is that there's likely little connection between heart disease and biological sex in this dataset.
Lets move on to chest pain.

``` sh
> ggplot(heart, aes(x = cp)) + geom_histogram(bins = 4)
```
<p align="center">
  <img width="500" src="https://github.com/akweiss/heart-disease-r/blob/master/images/hdeda7.png">
</p>

Where 0 is Typical Angina, 1 is Atypical Angina, 2 is Non-Anginal, and 3 is Asymptomatic. We can further extend this to represent 
the relationship between chest pain and presence of heart disease.

``` sh
> by_cp <- count(heart, cp, target)
> by_cp_spread <- by_cp %>%
> +     spread(cp, n)
> by_cp_mtx <- data.matrix(by_cp_spread)
> colnames(by_cp_mtx) <- c("Typical Angina", "Atypical Angina", "Non-Anginal", "Asymptomatic")
> barplot(by_cp_mtx, ylim = c(0,500), col = c("blue", "red"))
> legend("topright", c("Disease Present","No Disease Present"), fill = c("red","blue"))
```
<p align="center">
  <img width="500" src="https://github.com/akweiss/heart-disease-r/blob/master/images/hdeda10.png">
</p>

From this plot we can see that there's approximately a 20% chance of heart disease linked with typical angina. The other
categories reflect that for atypical angina, non-anginal, and asymptomatic chest pains, heart disease is much more likely
than unlikely. Again, we can extend this to explore the relationship between chest pain and sex.

``` sh
> by_cp_sex <- count(heart, sex == 1, cp)
> colnames(by_cp_sex) <- c("sex", "cp", "n")
> by_cp_sex <- by_cp_sex %>%
>          mutate(sex = replace(sex, sex == 'FALSE', "Female")) %>%
>          mutate(sex = replace(sex, sex == 'TRUE', "Male"))
>          mutate(cp = replace(cp, cp == '0', "Typical Angina")) %>%
>          mutate(cp = replace(cp, cp == '1', "Atypical Angina")) %>%
>          mutate(cp = replace(cp, cp == '2', "Non-Anginal")) %>%
>          mutate(cp = replace(cp, cp == '3', "Asymptomatic"))
> ggplot(by_cp_sex, aes(fill=cp, y=value, x=sex)) + geom_bar(position="dodge", stat="identity")
```
<p align="center">
  <img width="500" src="https://github.com/akweiss/heart-disease-r/blob/master/images/hdeda12.png">
</p>

Though arranged differently, we see that the distributions of different chest pain types are approximately the same between sexes; most notably, there is a large spike for males with typical anginal chest pain which is much more pronounced than the bar for females with typical anginal chest pain. I'm also interested in additionally testing the relationship between chest pain and major vessels colored by fluoroscopy. 

``` sh
> by_cp_ca <- count(heart, ca, cp)
> by_cp_ca <- by_cp_ca %>%
>          mutate(cp = replace(cp, cp == '0', "Typical Angina")) %>%
>          mutate(cp = replace(cp, cp == '1', "Atypical Angina")) %>%
>          mutate(cp = replace(cp, cp == '2', "Non-Anginal")) %>%
>          mutate(cp = replace(cp, cp == '3', "Asymptomatic"))
> colnames(by_cp_ca) <- c("Number of Major Vessels Colored by Fluoroscopy", "Chest Pain", "n")
> ggplot(by_cp_ca, aes(fill=`Chest Pain`, y=n, x=`Number of Major Vessels Colored by Fluoroscopy`)) + 
> geom_bar(position="stack", stat="identity")
```
<p align="center">
  <img width="500" src="https://github.com/akweiss/heart-disease-r/blob/master/images/hdeda13.png">
</p>

This tells us that after typical angina, non-anginal chest pain is the most common type of chest pain as progressively more major 
vessels are colored by fluoroscopy. 

Lastly, we can run a linear model on blood pressure and cholesterol. 

``` sh
> mod <- lm(chol ~ trestbps, data = heart)
> summary(mod)
```
<p align="center">
  <img width="500" src="https://github.com/akweiss/heart-disease-r/blob/master/images/hdeda15.png">
</p>

This table gives us a p-value of 3.97e-5, which is significantly smaller than most levels of alpha that are traditionally chosen.
Therefore we can reject the null hypothesis and conclude that there is some statistically significant relationship between blood
pressure and cholesterol. We can visualize this relationship with the following plot:

``` sh
> plot(mod)
```
<p align="center">
  <img width="500" src="https://github.com/akweiss/heart-disease-r/blob/master/images/hdeda14.png">
</p>

