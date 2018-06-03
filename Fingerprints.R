# The variables in "printratings_dat.Rda" are:
# bag - a number which matches rows in the data frame to the images
# finger - which finger (1-10. e.g. thumb) was used to make the print
# donor - the number of the donor which contributed the print (0 means no contributor)
# day - 1-4 indicating the day this print was contributed
# weight1 - the weight of the bag before they were numbered
# weight2 - the weight of the bag after numbering but before treatment
# weight3 - the weight of the bag after being treated with one of four fingerprint enhancing methods
# treatment - which treatment was applied
# printed - an indicator variable included for convenience. It is 1 if there was a donor (1-4), 0 if no donor.
# Score.1 - The numeric score (0-4) assigned to the print by examiner 1 
# Score.2 - The score assigned to the print by examiner 2
# Score.3 and Score.4 are defined similarly. 
library(GGally)
load("C:/Users/nmajo/Desktop/CMU SURE/printratings_dat.Rda")
p = printratings_dat
# View(p)

# check for missing data
table(is.na(p)) 

# new metrics
# meanWeight is the mean of the 3 weights
# meanScore is the mean of the 4 scores
# fingerNumeric is the finger recoded as a number
p["meanWeight"] = (weight1 + weight2 + weight3) / 3 
p["meanScore"] = (Score.1 + Score.2 + Score.3 + Score.4) / 4
p["fingerNumeric"] = NA

# possible additions: weighted mean score based on examiner's proficiency? 
#   (number of false positives)


p$fingerNumeric[p$finger == "L Pinky"] = 0
p$fingerNumeric[p$finger == "L Ring"] = 1
p$fingerNumeric[p$finger == "L Middle"] = 2
p$fingerNumeric[p$finger == "L Index"] = 3
p$fingerNumeric[p$finger == "L Thumb"] = 4

p$fingerNumeric[p$finger == "R Pinky"] = 5
p$fingerNumeric[p$finger == "R Ring"] = 6
p$fingerNumeric[p$finger == "R Middle"] = 7
p$fingerNumeric[p$finger == "R Index"] = 8
p$fingerNumeric[p$finger == "R Thumb"] = 9

levels(as.factor(p$fingerNumeric))

attach(p)

# View(p)

# EDA
# metrics of interest: mean weight, mean score, finger, treatment
levels(as.factor(finger))
levels(as.factor(p$meanScore))

# finger vs mean score: appears to be no strong variation between groups
boxplot(meanScore ~ finger, par(cex.axis = .75))
plot(fingerNumeric, meanScore) 
anova(aov(meanScore ~ finger))

# finger vs mean weight: appears to be no strong variation between groups
boxplot(meanWeight ~ finger)
plot(fingerNumeric, meanWeight)
anova(aov(meanWeight ~ finger))

# treatment vs mean score: 
# statistically significant variation between treatment groups and mean score 
#   (p-value = 4.08e-8)
boxplot(meanScore ~ treatment)
anova(aov(meanScore ~ treatment))

# donor vs mean score
# statistically significant variation between donor and mean score 
#   (p-value < 2.2e-16)
boxplot(meanScore ~ donor)
anova(aov(meanScore ~ donor))

# donor vs mean score (0 donor dropped)
# reasoning: clear from boxplot that variation between groups is strongest with donor 0.
# Is ANOVA still statistically significant upon removing this donor? 
# statistically signification variation between donor and mean score
#   (p-value = 3.637e-10)
no0 = subset(p, donor != 0)
# View(no0)
boxplot(no0$meanScore ~ no0$donor)
anova(aov(no0$meanScore ~ no0$donor))

# Michaela's barplots
barplot(table(printratings_dat$Score.1))
barplot(table(printratings_dat$Score.2))
barplot(table(printratings_dat$Score.3))
barplot(table(printratings_dat$Score.4))

# subsetted treatments
levels(as.factor(treatment))
MP = subset(p, treatment == "Magnetic Powder")
DFO = subset(p, treatment == "DFO")
N = subset(p, treatment == "Ninhydrin")
DFON = subset(p, treatment == "DFO + Ninhydrin")

nrow(p) == nrow(MP) + nrow(DFO) + nrow(N) + nrow(DFON) # verification

# False positives occur when donor is 0 and the score is something other than 0.
# Examiner 1 had 1 false positive
sc1 = subset(p, donor == 0 & Score.1 > 0, select = c(donor, Score.1))
nrow(sc1) 

# Examiner 2 had 4 false positives
sc2 = subset(p, donor == 0 & Score.2 > 0, select = c(donor, Score.2))
nrow(sc2)

# Examiner 3 had 3 false positives
sc3 = subset(p, donor == 0 & Score.3 > 0, select = c(donor, Score.3))
nrow(sc3)

# Examiner 4 had 4 false positives
sc4 = subset(p, donor == 0 & Score.4 > 0, select = c(donor, Score.4))
nrow(sc4)

# correlation matrix
ggpairs(p, 
        columns = c("day", "weight1", "weight2", "weight3", "Score.1",
                    "Score.2", "Score.3", "Score.4"), 
        lower = list(continuous = "smooth"))

# Further Analysis: general contrasts/multiple comparisons for treatment vs. mean score
#   and donor vs. mean score