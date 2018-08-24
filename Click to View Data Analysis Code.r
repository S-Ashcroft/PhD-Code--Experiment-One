# Experiment One Data Analysis

## I wouldn't expect someone to understand this from scratch, given that there needs to be
## ...quite a bit of background knowledge of the subject and hypotheses.
## However, do take a look at the general process, the comments etc.


# Sam Ashcroft
# sam.ashcroft@hotmail.co.uk
# LinkedIn: www.linkedin.com/in/samashcroft
# GitHub: https://github.com/S-Ashcroft
# RPubs: http://rpubs.com/Ashcroft

# here is about 550+ lines of code
# this assumes you have run the data cleaning code, hence no packages are installed here

### now begins a few checks in the data before running full analysis
### never just dive right in!
# 1. did participants respond correctly on coherent test trials? 
# 2. did participants repeatedly choose one button on ambiguous test trials? 
# 3. did participants repeatedly choose one stimulus on ambiguous test trials? 
# 4. did participants repeatedly choose A>C or A<C on ambiguous test trials? 
# 5. did participants respond only in one direction on ICN blocks? 


# 1. did participants get a correctness average over 80%? (80% is 38.4 out of 48)
correctness_one_sample_df_2 <- average_outcomes_df %>% filter(block_type_0A_1C == 1) %>% 
  select(participant, average_sum_correct)

# this code snippet is where you can remove outliers / participants less than a score and run things
# unhash this and edit the '$average_sum_correct>20' to remove participants above or below a certain threshold:
# correctness_one_sample_df_3 <- correctness_one_sample_df_2[correctness_one_sample_df_2$average_sum_correct>20,]
# hist(correctness_one_sample_df_3$average_sum_correct, breaks = 48) # make a histogram
# shapiro.test(correctness_one_sample_df_3$average_sum_correct) # test for normality
# qqnorm(correctness_one_sample_df_3$average_sum_correct) # building qqplots to check
# qqline(correctness_one_sample_df_3$average_sum_correct)
# qqPlot(correctness_one_sample_df_3$average_sum_correct)

# make a histogram of the correctness scores
hist(correctness_one_sample_df_2$average_sum_correct, breaks = 48, col = "grey")

# test for normality
shapiro.test(correctness_one_sample_df_2$average_sum_correct)

# run qqplots to check assumptions
qqnorm(correctness_one_sample_df_2$average_sum_correct)
qqline(correctness_one_sample_df_2$average_sum_correct)
qqPlot(correctness_one_sample_df_2$average_sum_correct, ylab = "Average Sum Correct",
       main = "QQ Plot For Average Sum Correctness")
# looks like there are 8 outliers. These could be removed, but will use wilcox later

# run a one sample t-test to see if participants scored over 80%
t.test(correctness_one_sample_df_2$average_sum_correct, mu = 38.4, alternative="greater")
# confidence interval goes to inf - scale ends and that seems why...
# p value is significant, which suggests that they did indeed respond more than 80

# alternatively do a non parametric one sample Wilcoxon test ---
wilcox.test(correctness_one_sample_df_2$average_sum_correct, mu = 38.4, alternative="greater")
# the assumption of wilcoxon signed rank test is that there are roughly the same number
# of results above and below the mean. 

# Below you can see we have 20 below and 26 above
correctness_median <- median(correctness_one_sample_df_2$average_sum_correct)
sum(correctness_one_sample_df_2$average_sum_correct < 46)
sum(correctness_one_sample_df_2$average_sum_correct > 46)
sum(correctness_one_sample_df_2$average_sum_correct == 46)
# p73 of this thesis https://core.ac.uk/download/pdf/18614332.pdf says wilcox is not robust against
# violation of the symmetry assumption. Accordingly I will inverse the data as recommended on p73

# inversing the data
inverse_corr <- 1/correctness_one_sample_df_2$average_sum_correct

# check the transformation worked
hist(inverse_corr, breaks = 100, col = "grey", xlab = "Inverse Correctness", 
     main = "Histogram of Inverse Correctness")
# add a cutoff line to the histogram
abline(v = 0.02604167, lwd = 3)

# get the new median score for the transformed data
median(inverse_corr)
# median is 0.02173913

# check the new parameters of the transformed data for symmetry above and below median
sum(inverse_corr > 0.02173913) # count how many are now above the median: 27
sum(inverse_corr < 0.02173913) # count how many are now below the median: 26
sum(inverse_corr == 0.02173913) # count how many are equal to the median: 0
# this is as close to symmetry as the data can get
# the transformation was successful, and Wilcoxon signed rank test can continue

# now we need the inverse of the score we wish to do the wilcoxon signed rank test against
# in other words, you can't use 38.4 as the cutoff if youve inversed the data!
inverse_test_value <- 1/38.4
inverse_test_value
# and now do the wilcoxon signed rank test, now testing for "less than" the inverse test value...
# ...due to the inversion.

# see the histogram above to understand the transformation if you are unclear

# now performing a wilcoxon signed rank test on the transformed data
wilcox.test(inverse_corr, mu = inverse_test_value, alternative="less")
# this is significant, indicating that correctness was greater than 80% or 38.4 on test trials
# for more info on this process see:
# Robustness of Wilcoxon Signed-Rank Test Against the Assumption of Symmetry by Jutharath Voraprateep, 2013

# descriptives for test #1. 
describe(correctness_one_sample_df_2$average_sum_correct)
ci.mean(correctness_one_sample_df_2$average_sum_correct)


# 2. did participants repeatedly choose one button on ambiguous test trials?
# to do this we need their 
# average sum of button choices, then take that from 24, turn it absolute, and then 
# run one-sample t-test difference from 0

# get only relevant blocks
button_one_sample_df <- df_limits_removed %>% filter(block_type_0A_1C == 0) %>% 
  # select relevant variables
  select(participant, sum_button_pressed) %>% 
  # get the absolute value compared with 24 (which is right in the middle, indicating chance)
  transmute(button_distance_from_24 = abs(sum_button_pressed - 24)) %>% 
  # get the average absolute value
  summarise(mean_button_distance_from_24 = mean(button_distance_from_24))

# run a one sample t-test on the data to see if participants responded randomly or systematically regarding button choices
t.test(button_one_sample_df$mean_button_distance_from_24, mu = 0)

# histogram to show patterns of responding
hist(button_one_sample_df$mean_button_distance_from_24, breaks = 20)

# shapiro test for normality
shapiro.test(button_one_sample_df$mean_button_distance_from_24)

# run qqplots to check assumptions
qqnorm(button_one_sample_df$mean_button_distance_from_24)
qqline(button_one_sample_df$mean_button_distance_from_24)
qqPlot(button_one_sample_df$mean_button_distance_from_24)

# descriptive statistics for check #2
describe(button_one_sample_df$mean_button_distance_from_24)
describe(df_limits_removed$sum_button_pressed)


# 3. did participants repeatedly choose one stimulus on ambiguous test trials?
# to do this we need their 
# average sum of stimulus choices, then take that from 24, turn it absolute, and then 
# one-sample t-test difference from 0

# select relevant blocks
stim_choice_one_sample_df <- df_limits_removed %>% filter(block_type_0A_1C == 0) %>% 
  # select relevant variables
  select(participant, sum_stim_picked) %>% 
  # create the absolute distance
  transmute(stim_distance_from_24 = abs(sum_stim_picked - 24)) %>% 
  # get the mean of absolute distance
  summarise(mean_stim_distance_from_24 = mean(stim_distance_from_24))

# run a one sample t-test to check whether participants responded randomly or systematically regarding stimulus choices
t.test(stim_choice_one_sample_df$mean_stim_distance_from_24, mu = 0)

# run shapiro to test assumptions
shapiro.test(stim_choice_one_sample_df$mean_stim_distance_from_24)

# run qqplots to test assumptions
qqnorm(stim_choice_one_sample_df$mean_stim_distance_from_24)
qqline(stim_choice_one_sample_df$mean_stim_distance_from_24)
qqPlot(stim_choice_one_sample_df$mean_stim_distance_from_24)

# histogram of the responding for check #3
hist(stim_choice_one_sample_df$mean_stim_distance_from_24, breaks = 24)


# 4. did participants repeatedly choose A>C or A<C on ambiguous test trials?
# this is a bit more complex than the assumption checks above
# given that this looks at a compound including the stimulus choice in conjunction with the discriminative on the screen
# fortunately I have run a big nested-if-statement in the data cleaning to create these compounds
# the compounds are simply 1 or 0: 1 for A>C, 0 for A<C

# first create the absolute means
# filter to get only relevant blocks 
compound_one_sample_df <- df_limits_removed %>% filter(block_type_0A_1C == 0) %>% 
  # select relevant variables
  select(participant, sum_compound_responding) %>% 
  # create the absolute values
  mutate(compound_distance_from_24 = abs(sum_compound_responding - 24)) %>% 
  # average the absolute values
  summarise(mean_sum_compound_responding = mean(compound_distance_from_24))

# this time the test value of the one sample t-test will be mu = 19.2
# because if they were responding A>C or A<C they would have a compound score 
# near 48 or 0, which when you get the abs distance from 24 (48-24 or 24-0) would be 24.
# 80% of 24 is 19.2
# so the question is: "Do participants respond systematically, more than 80% of the time?"

# one sample t-test
t.test(compound_one_sample_df$mean_sum_compound_responding, mu = 0, alternative = "greater")

# shapiro test of assumptions
shapiro.test(compound_one_sample_df$mean_sum_compound_responding)

# qqplot test of assumptions
qqnorm(compound_one_sample_df$mean_sum_compound_responding)
qqline(compound_one_sample_df$mean_sum_compound_responding)
qqPlot(compound_one_sample_df$mean_sum_compound_responding)

# histogram of the results
hist(compound_one_sample_df$mean_sum_compound_responding, breaks = 48)

# descriptives for test #4
describe(compound_one_sample_df$mean_sum_compound_responding)
ci.mean(compound_one_sample_df$mean_sum_compound_responding)


# 5. did participants respond only in one 'direction' on ICN blocks?
# this means, did ALL participants always choose A>C or A<C on block 1
# or always A>C on block 2, or block 3?
# these are three independent questions
# you would expect participants to choose A>C or A<C randomly
# if participants always choose either A>C or A<C, this means there is an experimental artefact

# create dataset for block 1
direction_df_A <- df_limits_removed %>% group_by(block) %>% filter(block_code == 1)
# create dataset for block 2
direction_df_B <- df_limits_removed %>% group_by(block) %>% filter(block_code == 2)
# create dataset for block 3
direction_df_C <- df_limits_removed %>% group_by(block) %>% filter(block_code == 3)

# run t-test against random (24) for block 1
t.test(direction_df_A$sum_compound_responding, mu = 24)
# run t-test against random (24) for block 2
t.test(direction_df_B$sum_compound_responding, mu = 24)
# run t-test against random (24) for block 3
t.test(direction_df_C$sum_compound_responding, mu = 24)

# descriptive statistics for block 1
describe(direction_df_A$sum_compound_responding)
# descriptive statistics for block 2
describe(direction_df_B$sum_compound_responding)
# descriptive statistics for block 3
describe(direction_df_C$sum_compound_responding)

# histogram for block 1
hist(direction_df_A$sum_compound_responding)
# histogram for block 2
hist(direction_df_B$sum_compound_responding)
# histogram for block 3
hist(direction_df_C$sum_compound_responding)



#### code block BEGINS for ALL ANOVAS
#### these will be separated by BEGIN and END so that each anova is independent and clear

### code block BEGIN for rt anovas 
# averaging the reaction times once and once only (averaging an average is naughty!)
rt_av1_aov_df <- mother_df_12 %>% 
  # mutate strings to get values (0 or 1)
  mutate(block_type_0A_1C = ifelse(block %in% c("ambigA.xlsx", "ambigB.xlsx", "ambigC.xlsx"), 0, 1)) %>% 
  # subset to remove outliers
  subset(subset = !(participant %in% limit_reached_df$participant)) %>% 
  group_by(participant, block_type_0A_1C) %>% 
  # get the mean response time
  summarise(average_test_response_rt2 = mean(test_response_rt))

# aov rt
# run the anova
aov_rt2 <- aov(average_test_response_rt2 ~ as.factor(block_type_0A_1C) + 
                 Error(as.factor(participant)/as.factor(block_type_0A_1C)), data = rt_av1_aov_df)
# view the anova
summary(aov_rt2)
# create a value containing specific back-end outputs from the anova
aov_rt2_info <- proj(aov_rt2)
# view the residuals of the anova
aov_rt2_resid <- aov_rt2_info[[3]][, "Residuals"]
# view the qqplot of residuals as is the necessary assumption check
qqPlot(aov_rt2_resid)

# ezanova rt to get effect size of generalised eta squared (GES)
# this checks that the above anova was correct, and also gives GES
# GES is a very powerful measure of effect size and is recommended by...
# Olejnik & Algina, 2003
# Note that a small, medium and large GES would be 0.02, 0.13 and 0.26 respectively (Bakeman, 2005, p383).
ezANOVA(data = rt_av1_aov_df, 
        dv = .(average_test_response_rt2),
        wid = .(participant),
        within = .(block_type_0A_1C),
        type = 3, detailed = TRUE)

# check result using a paired t-test
# spread the data
spread_rt <- rt_av1_aov_df %>% select(participant, block_type_0A_1C, average_test_response_rt2) %>% 
  spread(block_type_0A_1C, average_test_response_rt2)

# paired t-test rt
rt_t_test <- t.test(spread_rt$`0`, spread_rt$`1`, paired = TRUE)
rt_t_test

# descriptives for rt_aov
# for first type of block
describe(spread_rt$`0`)
ci.mean(spread_rt$`0`)
# for second type of block
describe(spread_rt$`1`)
ci.mean(spread_rt$`1`)

# normality for raw data (not particularly useful, but reviewers may wish to view it)
hist(spread_rt$`0`, breaks = 48)
qqnorm(spread_rt$`0`)
qqline(spread_rt$`0`)
shapiro.test(spread_rt$`0`)

hist(spread_rt$`1`, breaks = 48)
qqnorm(spread_rt$`1`)
qqline(spread_rt$`1`)
qqPlot(spread_rt$`1`)
shapiro.test(spread_rt$`1`)

# histogram of the combined dataset
hist(rt_av1_aov_df$average_test_response_rt2)

### code block END for rt anovas 


### code block BEGIN for affect anovas 
# aov affect
aov_affect <- aov(average_block_affect_rating ~ as.factor(block_type_0A_1C) + 
                    Error(as.factor(participant)/as.factor(block_type_0A_1C)), data = average_outcomes_df)
summary(aov_affect)
aov_affect_info <- proj(aov_affect)
aov_affect_resid <- aov_affect_info[[3]][, "Residuals"]
qqPlot(aov_affect_resid)

# ezanova affect
ezANOVA(data = average_outcomes_df, 
        dv = .(average_block_affect_rating),
        wid = .(participant),
        within = .(block_type_0A_1C),
        type = 3, detailed = TRUE)

# check result using a paired t-test
# spread the data
spread_affect <- average_outcomes_df %>% select(participant, block_type_0A_1C, average_block_affect_rating) %>% 
  spread(block_type_0A_1C, average_block_affect_rating)

# paired t-test affect
affect_t_test <- t.test(spread_affect$`0`, spread_affect$`1`, paired = TRUE)
affect_t_test

# descriptives for rt_aov
describe(spread_affect$`0`)
ci.mean(spread_affect$`0`)
describe(spread_affect$`1`)
ci.mean(spread_affect$`1`)
### code block END for affect anovas 


### code block BEGIN for arousal anovas 
# aov arousal
aov_arousal <- aov(average_block_arousal_rating ~ as.factor(block_type_0A_1C) + 
                     Error(as.factor(participant)/as.factor(block_type_0A_1C)), data = average_outcomes_df)
summary(aov_arousal)
aov_arousal_info <- proj(aov_arousal)
aov_arousal_resid <- aov_arousal_info[[3]][, "Residuals"]
qqPlot(aov_arousal_resid)

# ezanova arousal to get effect size
ezANOVA(data = average_outcomes_df, 
        dv = .(average_block_arousal_rating),
        wid = .(participant),
        within = .(block_type_0A_1C),
        type = 3, detailed = TRUE)

# check result using a paired t-test
# spread the data
spread_arousal <- average_outcomes_df %>% select(participant, block_type_0A_1C, average_block_arousal_rating) %>% 
  spread(block_type_0A_1C, average_block_arousal_rating)

# paired t-test arousal
arousal_t_test <- t.test(spread_arousal$`0`, spread_arousal$`1`, paired = TRUE)
arousal_t_test

# descriptives for rt_aov
describe(spread_arousal$`0`)
ci.mean(spread_arousal$`0`)
describe(spread_arousal$`1`)
ci.mean(spread_arousal$`1`)
### code block END for arousal anovas 


### code block BEGIN for sense anovas 
# aov sense
aov_sense <- aov(average_block_sense_making_rating ~ as.factor(block_type_0A_1C) + 
                   Error(as.factor(participant)/as.factor(block_type_0A_1C)), data = average_outcomes_df)
summary(aov_sense)
aov_sense_info <- proj(aov_sense)
aov_sense_resid <- aov_sense_info[[3]][, "Residuals"]
qqPlot(aov_sense_resid)

# ezanova sense to get effect size
ezANOVA(data = average_outcomes_df, 
        dv = .(average_block_sense_making_rating),
        wid = .(participant),
        within = .(block_type_0A_1C),
        type = 3, detailed = TRUE)

# check result using a paired t-test
# spread the data
spread_sense <- average_outcomes_df %>% select(participant, block_type_0A_1C, average_block_sense_making_rating) %>% 
  spread(block_type_0A_1C, average_block_sense_making_rating)

# paired t-test sense
sense_t_test <- t.test(spread_sense$`0`, spread_sense$`1`, paired = TRUE)
sense_t_test

describe(spread_sense$`0`)
ci.mean(spread_sense$`0`)
describe(spread_sense$`1`)
ci.mean(spread_sense$`1`)
### code block END for sense anovas 


### code block BEGIN for loop attempts (how many attempts!) anovas

loop_df <- df_limits_removed %>% group_by(participant, block_type_0A_1C) %>%
    summarise(mean_loops = mean(block_attempts_this_block))

aov_loop <- aov(mean_loops ~ as.factor(block_type_0A_1C) +
                   Error(as.factor(participant)/as.factor(block_type_0A_1C)), data = loop_df)
summary(aov_loop)
aov_loop_info <- proj(aov_loop)
aov_loop_resid <- aov_loop_info[[3]][, "Residuals"]
qqPlot(aov_loop_resid)
# qq looks great!

# ezanova sense to get effect size
ezANOVA(data = loop_df,
        dv = .(mean_loops),
        wid = .(participant),
        within = .(block_type_0A_1C),
        type = 3, detailed = TRUE)

# check result using a paired t-test
# spread the data
spread_loop <- loop_df %>%
  spread(block_type_0A_1C, mean_loops)

# paired t-test sense
loop_t_test <- t.test(spread_loop$`0`, spread_loop$`1`, paired = TRUE)
loop_t_test

describe(spread_loop$`0`)
ci.mean(spread_loop$`0`)
describe(spread_loop$`1`)
ci.mean(spread_loop$`1`)

### code block END for loop anovas

#### code block END for ALL ANOVAS



# Now, two bivariate correlations found that affect and sense-making scores 
# correlated significantly on both coherent (r=.521, p<.0001) 
# and part-ambiguous blocks (r=.376, p<.001).

# filter the blocks for type 1
coh_correlation_df <- average_outcomes_df %>% filter(block_type_0A_1C == 1)
# create a histogram of affect scores
hist(coh_correlation_df$average_block_affect_rating, breaks = 30)
# create a histogram of sense-making scores
hist(coh_correlation_df$average_block_sense_making_rating, breaks = 30)
# based on these hists the data is not normally distributed, so spearman corr is more suitable
coh_cor <- cor.test(coh_correlation_df$average_block_affect_rating, 
                    coh_correlation_df$average_block_sense_making_rating,
                    method = "spearman",
                    # exact = FALSE used because otherwise ties become an issue
                    # https://stackoverflow.com/questions/10711395/spearman-correlation-and-ties
                    exact = FALSE)
# view the correlation output
coh_cor

# do the same process for block type 0
# create the data set
amb_correlation_df <- average_outcomes_df %>% filter(block_type_0A_1C == 0)
# view block 1 histogram
hist(amb_correlation_df$average_block_affect_rating, breaks = 30)
# view block 2 histogram
hist(amb_correlation_df$average_block_sense_making_rating, breaks = 30)
# based on these hists the data is not normally distributed, so spearman is more suitable
amb_cor <- cor.test(amb_correlation_df$average_block_affect_rating, 
                    amb_correlation_df$average_block_sense_making_rating,
                    method = "spearman",
                    # exact = FALSE used because otherwise ties become an issue
                    # https://stackoverflow.com/questions/10711395/spearman-correlation-and-ties
                    exact = FALSE)
# view the corr output
amb_cor



### Creating Graphs of the Data

# boxplots for all outcome variables

# block attempts boxplot
boxplot(average_block_attempts_this_block ~ block_type_0A_1C, data=average_outcomes_df,
        col = "grey",
        xlab = "Block Type", ylab = "Block Attempts", main = "Mean Block Attempts by Block Type",
        cex.main=1.5, cex.lab=1.3, cex.axis=1, 
        names = c("ICN", "CN"))

# affect rating boxplot
boxplot(average_block_affect_rating ~ block_type_0A_1C, data=average_outcomes_df,
        col = "grey",
        xlab = "Block Type", ylab = "Affect", main = "Mean Affect by Block Type",
        cex.main=1.5, cex.lab=1.3, cex.axis=1, 
        names = c("ICN", "CN"))

# arousal rating boxplot
boxplot(average_block_arousal_rating ~ block_type_0A_1C, data=average_outcomes_df,
        col = "grey",
        xlab = "Block Type", ylab = "Arousal", main = "Mean Arousal by Block Type",
        cex.main=1.5, cex.lab=1.3, cex.axis=1, 
        names = c("ICN", "CN"))

# sense-making boxplot
boxplot(average_block_sense_making_rating ~ block_type_0A_1C, data=average_outcomes_df,
        col = "grey",
        xlab = "Block Type", ylab = "Sense-making", main = "Mean Sense-Making by Block Type",
        cex.main=1.5, cex.lab=1.3, cex.axis=1, 
        names = c("ICN", "CN"))

# histogram of the systematic responding scores
hist(compound_one_sample_df$mean_sum_compound_responding,
     col = "grey",
     main = "Histogram to Show Mean SRS Frequency",
     xlab = "Mean SRS",
     breaks = 40,
     xlim=c(0,25))
# add a line showing 80% cutoff
abline(v = 19.2, lwd = 3)

# histogram of correctness scores
hist(correctness_one_sample_df_2$average_sum_correct,
     col = "grey",
     main = "Histogram to Show Mean Correctness Frequency",
     xlab = "Mean Correctness",
     breaks = 20,
     xlim=c(0,50))
# add a line showing 80% cutoff
abline(v = 38.4, lwd = 3)



##### ggplot boxplot template I wrote (which I doubt I'll use!)
gg_rt_boxplot <- ggplot(average_outcomes_df,
                     aes(x = as.factor(block_type_0A_1C),
                         y = average_test_response_rt)) +
              geom_boxplot(fill='#A4A4A4', color="black") +
              theme_bw() +
              ggtitle("Plot of reaction time per block") +
              labs(x = "Block", y = "Reaction Time (s)") +
  theme(
    plot.title = element_text(color="black", size=18, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold"),
    axis.text = element_text(color="black", size=12, face="bold")
    )
gg_rt_boxplot



# Additional Code that may wish to be used to explore the data (appendix)
##### FILTERING AND RUNNING STATS FOR PEOPLE WITH SCORES OVER A CERTAIN PERCENT CORRECT
# edit the values in here to create your own cutoffs
ss_more_than_seventy_four_pc <- correct_output_df %>% group_by(participant) %>%
  summarise(mean_coh_correct = mean(correct_sum)) %>%
  mutate(mean_coh_correct = 100*(mean_coh_correct / 8)) %>%
  filter(mean_coh_correct >= 80)

# subset the whole dataset to include anyone in the above table
full_table_for_seventy_five_pc <- filtered_df %>%
  subset(subset = participant %in% ss_more_than_seventy_four_pc$participant)

### End of Code

# Sam Ashcroft
# sam.ashcroft@hotmail.co.uk
# LinkedIn: www.linkedin.com/in/samashcroft
# GitHub: https://github.com/S-Ashcroft
# RPubs: http://rpubs.com/Ashcroft
