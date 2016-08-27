# DominanceSurvival

# These R-Script does all the analysis presented in the MSc-Thesis by Dominic Martin

# TITLE: "High social status is associated with improved first-winter survival in a wild passerine population"
# SUPERVISOR: Dr. Julia Schroeder
# INSTITUTION: Imperial College London, Silwood Park Campus
# DATA SOURCE: Lundy Sparrow Project (University of Sheffield, Imperial College London)
# AUTHOR: Dominic Martin 1) 2) 3)
# 1) Current address: PhD Student, Biodiversity, Macroecology & Conservation Biogeography, Georg-August University of Goettingen
# 2) E-Mail: dominic.martin@uni-goettingen.de
# 3) Github Profile: https://github.com/DominicMart

# The script RFID_comb_20160722.R has only 1 part:
# Putting together the RFID (i.e. PIT) files from each single day to one (huge) file with all the PIT readings 

# The script analysis_dominance_20160820.R has 8 parts:
# Part 1: Getting last live record data for each bird recorded in the dominance videos
# Part 2: Combining yuhu database with last live record data
# Part 3: Converts Last live record date for each BirdID into Season when last recorded (Summer/Winter)
# Part 4: Puts together Yuhu database and llrComplete 4 based on BirdID to yuhu_llr
# Part 5: Subsetting, overview metrics & Repeatability
# Part 6: Builds mixed models with the binomial response variable "Alive" which are shown in the Thesis
# Part 7: Odd ratios and plots shown in Thesis
# Part 8: Counting number of breeding pairs in study period

