# VARIABLE INFORMATION
# The variables in "printratings_dat.Rda" are:
# bag - a number which matches rows in the data frame to the images
# finger - which finger was used to make the print
# donor - the number of the donor which contributed the print (0 means no contributor)
# day - 1-4 indicating the day this print was contributed
# weight1 - the weight of the bag before they were numbered
# weight2 - the weight of the bag after numbering but before treatment
# weight3 - the weight of the bag after being treated with one of four fingerprint 
#   enhancing methods
# treatment - which treatment was applied
# printed - an indicator variable included for convenience. It is 1 if there was a donor
#   (1-4), 0 if no donor.
# Score.1 - The numeric score (0-4) assigned to the print by examiner 1 
# Score.2 - The score assigned to the print by examiner 2
# Score.3 and Score.4 are defined similarly. 

library(GGally)
load("C:/Users/nmajo/Desktop/CMU SURE/printratings_dat.Rda")
p = printratings_dat
# View(p)

# check for missing data
table(is.na(p)) 

# NEW METRICS
# meanScore is the mean of the 4 examiner's scores
# fingerNumeric is the finger recoded as a number
# printWeight is the weight of the bag after the treatment is applied minus the 
#   weight of the bag before the treatment is applied. Used to determine the weight of
#   the treatment. 
p["meanScore"] = (Score.1 + Score.2 + Score.3 + Score.4) / 4
p["printWeight"] = weight3 - weight2

# recode fingers as numbers
p["fingerNumeric"] = NA
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

levels(as.factor(p$fingerNumeric)) # verification (0-9)

attach(p)
# View(p)

# EDA
# finger vs mean score: appears to be no strong variation between groups (p-value = .23)
boxplot(meanScore ~ finger, par(cex.axis = .75), xlab = "Finger", ylab = "Mean Score",
        main = "Average Fingerprint Quality Score v. Finger")
plot(fingerNumeric, meanScore, xlab = "Finger (Numeric)", ylab = "Mean Score", 
     main = "Average Fingerprint Quality v. Finger (Numeric)") 
mSvF = aov(meanScore ~ finger)
anova(mSvF)

# check assumptions
# anova analysis is invalid because residuals are non-normal. Try Kruskal-Wallis
qqnorm(mSvF$residuals)
qqline(mSvF$residuals)

kruskal.test(finger, meanScore)


# treatment vs mean score: 
# statistically significant variation between treatment groups and mean score 
#   (p-value = 8.605e-6)
boxplot(meanScore ~ treatment, xlab = "Treatment", ylab = "Mean Score", 
        main = "Average Fingerprint Quality Score v. Treatment")
mSvT = aov(meanScore ~ treatment)
anova(mSvT)

# check assumptions
# anova analysis is invalid because residuals are non-normal. Try Kruskal-Wallis
qqnorm(mSvT$residuals)
qqline(mSvT$residuals)

kruskal.test(treatment, meanScore)


# donor vs mean score
# statistically significant variation between donor and mean score 
#   (p-value < 2.2e-16)
plot(donor, meanScore, xlab = "Donor", ylab = "Avg. Quality Score", 
     main = "Avg. Fingerprint Quality Score v. Donor")
boxplot(meanScore ~ donor)
mSvD = aov(meanScore ~ donor)
anova(mSvD)

# check assumptions
# anova analysis is invalid because residuals are non-normal. Try Kruskal-Wallis
qqnorm(mSvD$residuals)
qqline(mSvD$residuals)

kruskal.test(donor, meanScore)


# donor vs mean score (0 donor dropped)
# reasoning: clear from boxplot that variation between groups is strongest with donor 0.
# Is ANOVA still statistically significant upon removing this donor? 
# statistically signification variation between donor and mean score
#   (p-value = 2.277e-5)
no0 = subset(p, donor != 0)
# View(no0)
boxplot(no0$meanScore ~ no0$donor, xlab = "Donor")
mSvD0 = aov(no0$meanScore ~ no0$donor)
anova(mSvD0)

# why does 0 come up as a level when it's not in this new dataset?
levels(as.factor(no0$donor)) 
View(no0)

# check assumptions
# anova analysis is invalid because residuals are non-normal. Try Kruskal-Wallis
qqnorm(mSvD0$residuals)
qqline(mSvD0$residuals)

kruskal.test(no0$donor, no0$meanScore)

# DONORS (subset)
levels(as.factor(donor))
d0 = subset(p, donor == 0)
d1 = subset(p, donor == 1)
d2 = subset(p, donor == 2)
d3 = subset(p, donor == 3)
d4 = subset(p, donor == 4)
d5 = subset(p, donor == 5)

d0.avg = mean(d0$meanScore) # sixth
d1.avg = mean(d1$meanScore) # fourth
d2.avg = mean(d2$meanScore) # fifth 
d3.avg = mean(d3$meanScore) # third 
d4.avg = mean(d4$meanScore) # second 
d5.avg = mean(d5$meanScore) # first highest

# TREATMENTS (subset)
levels(as.factor(treatment))
MP = subset(p, treatment == "Magnetic Powder")
DFO = subset(p, treatment == "DFO")
N = subset(p, treatment == "Ninhydrin")
DFON = subset(p, treatment == "DFO + Ninhydrin")

nrow(p) == nrow(MP) + nrow(DFO) + nrow(N) + nrow(DFON) # verification

# including donor 0
MP.avg = mean(MP$meanScore) # highest scores
DFO.avg = mean(DFO$meanScore) # second highest scores
N.avg = mean(N$meanScore) # second lowest scores
DFON.avg = mean(DFON$meanScore) # lowest scores

# excluding donor 0
MPE = subset(p, donor != 0 & treatment == "Magnetic Powder")
DFOE = subset(p, donor != 0 & treatment == "DFO")
NE = subset(p, donor != 0 & treatment == "Ninhydrin")
DFONE = subset(p, donor != 0 & treatment == "DFO + Ninhydrin")

MPE.avg = mean(MPE$meanScore) # highest score
DFOE.avg = mean(DFOE$meanScore) # second highest score
NE.avg = mean(NE$meanScore) # second lowest score
DFONE.avg = mean(DFONE$meanScore) # lowest score

# scores for MP (weight vs score)
plot(MP$meanScore, MP$printWeight, xlab = "Mean Score", ylab = "Print Weight", 
     main = "Printed Weight v. Mean Score \n for Magnetic Powder Treated Fingerprints")
abline(h = 0, col = "red")
boxplot(MP$printWeight ~ MP$meanScore)
abline(h = 0, col = "blue")

# scores for DFO (weight vs score)
plot(DFO$meanScore, DFO$printWeight, xlab = "Mean Score", ylab = "Print Weight", 
     main = "Printed Weight v. Mean Score \n for DFO Treated Fingerprints")
abline(h = 0, col = "red")
boxplot(DFO$printWeight ~ DFO$meanScore)
abline(h = 0, col = "blue")

# NEGATIVE PRINT WEIGHTS
negativeWeight = subset(p, printWeight < 0)
# View(negativeWeight)
nrow(negativeWeight)

# Breakdown of negative weights
#   Magnetic Powder: 21 -> 30% of MP had negative weights
#   DFO: 55 -> 79% of DFO had negative weights
#   Ninhydrin: 0 -> 0% of N had negative weights
#   DFO + Ninhydrin: 1 -> 1% of DFON had negative weights
#   About 71% of negative weights can be attributed to DFO-treated fingerprints. 
nw.MP = nrow(subset(negativeWeight, treatment == "Magnetic Powder"))
nw.DFO = nrow(subset(negativeWeight, treatment == "DFO")) 
nw.N = nrow(subset(negativeWeight, treatment == "Ninhydrin"))
nw.DFON = nrow(subset(negativeWeight, treatment == "DFO + Ninhydrin"))

nw.MP + nw.DFO + nw.N + nw.DFON == nrow(negativeWeight) # verification

# FALSE POSITIVES
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

# Mean Scores had 8 false positives
mS = subset(p, donor == 0 & meanScore > 0, select = c(donor, treatment, meanScore))
nrow(mS)
View(mS)

# remove false positives for new dataset
nFP = subset(p, donor == 0 & meanScore == 0 | donor != 0)
nrow(nFP)
nrow(nFP) + nrow(mS) == nrow(p) # Verification

MPnFP = subset(nFP, treatment == "Magnetic Powder")
DFOnFP = subset(nFP, treatment == "DFO")
NnFP = subset(nFP, treatment == "Ninhydrin")
DFONnFP = subset(nFP, treatment == "DFO + Ninhydrin")

MPnFP.avg = mean(MPnFP$meanScore) # highest score
DFOnFP.avg = mean(DFOnFP$meanScore) # second highest score
NnFP.avg = mean(NnFP$meanScore) # second lowest score
DFONnFP.avg = mean(DFONnFP$meanScore) # lowest score

# data frame for averages
# Mean Score 1: mean score including donor 0
# Mean Score 2: mean score excluding donor 0
# Mean Score 3: mean score excluding false positives
donor0 = c(MP.avg, DFO.avg, N.avg, DFON.avg)
donor0e = c(MPE.avg, DFOE.avg, NE.avg, DFONE.avg)
nFP = c(MPnFP.avg, DFOnFP.avg, NnFP.avg, DFONnFP.avg)
averages = data.frame(c("Magnetic Powder", "DFO", "Ninhyndrin", "DFO + Ninhydrin"), 
                      donor0, donor0e, nFP)
colnames(averages) = c("Treatment", "Mean Score 1", "Mean Score 2", "Mean Score 3")
View(averages)

# TRUE NEGATIVES
# true negatives: all examiners scored the print as 0. 
tN = subset(p, meanScore == 0)
nrow(tN) # 107

# true negatives that didn't come from donor 0
# Why are these true negatives? 
tN.not0 = subset(tN, donor != 0)
nrow(tN.not0) # 35

# true negatives that came from donor 0
tN.donor0 = subset(tN, donor == 0)
nrow(tN.donor0) # 72

nrow(tN) == nrow(tN.not0) + nrow(tN.donor0) # verification

# true positives: true positives are when none of the scores are 0. 

# CORRELATION MATRICES
# correlation matrix on raw data
ggpairs(p, 
        columns = c("weight1", "weight2", "weight3", "Score.1",
                    "Score.2", "Score.3", "Score.4"), 
        lower = list(continuous = "smooth"))

# correlation matrix on modified data
ggpairs(p, 
        columns = c("fingerNumeric", "donor", "meanScore"), 
        lower = list(continuous = "smooth"))

############################ End of statistical analysis ############################


# FURTHER ANALYSIS
#   general contrasts/multiple comparisons for treatment vs. mean score 
#     and donor vs. mean score
#   Respective scores based on distortion (weight1 and weight2)
#   Machine Learning: minutiae of fingerprints v. scores

# Questions
# 1) How do distortions (weights and images) affect scores? 
#     - need to determine threshold for distortion. 
#     - look at differences between donor 5 and donor 2 to find threshold. Change image
#       quality of higher scoring prints to find threshold of 0. 
#     - use machine learning to create a rubric for what contributes to different 
#       scores. Use false positives and true negatives against this rubric. 
#       Training data is true positives

# Where do we go from here?
# PART I
# Discrimination strength:
#   - image quality of donor 5 vs. donor 2, donor 5 vs. donor 1, donor 5 vs. donor 3, 
#     donor 5 vs. donor 4. Sharpest differences to dullest. 
#   - new subset = only one score is equal to 0 (mostly positive). Used to compare with
#     true positive subset to find threshold. 
#   - treatment: respective quality vs. treatment (Think On It)

# Distortions: 
#   - weight discrepancy's effects on scores. Looking across donors and treatments. 
#     DFO had higher weights and lower scores. 
#   - images
#   - distortion metric

# PART 2
# Consistency of Markup
#   - what caused variability in scores in images
#   - defining rubric with machine learning (false posiives and false negatives)

# Treatment Bias
#   - treatment bias within scorer (anova)

# Image Quality
#   - based upon the former 3 results, consider machine learning for image quality.
#   Compare to examiner's scores. 

# TO-DO
# Look at 35 false negatives (true negatives that didn't come from donor 0)
# true positives where no score = 0
# mostly positive subset (only one score = 0)
# review NIST Special Publication 1151
# donor 2 vs weight --> lower scores

# OUTLINE
# average weight doesn't mean anything
# negative weights
