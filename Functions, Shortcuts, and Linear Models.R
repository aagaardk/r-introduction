#===============================================================
# Linear models, functions, shortcuts
# Created By: Kevin Aagaard
# Last modified: 11/16/2015
#===============================================================

#Setting the working directory
work.dir = file.path(paste0(getwd(),"/Data"))
setwd(work.dir)
x = c("data.table", "plyr")
lapply(x,require,character.only=T)

#===============================================================
# Linear Models
#===============================================================

#The command:

?lm()
# "lm is used to fit linear models. It can be used to carry 
# out regression, single stratum analysis of variance and 
# analysis of covariance (although aov may provide a more 
# convenient interface for these)."

# lm(formula, data, subset, weights, na.action,
#    method = "qr", model = TRUE, x = FALSE, y = FALSE, 
#    qr = TRUE, singular.ok = TRUE, contrasts = NULL, 
#    offset, ...)


# formula: an object of class "formula" (or one that can be 
#          coerced to that class): a symbolic description of 
#          the model to be fitted. The details of model 
#          specification are given under 'Details'.
# data:    an optional data frame, list or environment (or 
#          object coercible by as.data.frame to a data frame) 
#          containing the variables in the model. If not found 
#          in data, the variables are taken from 
#          environment(formula), typically the environment from 
#          which lm is called.
# subset:  an optional vector specifying a subset of 
#          observations to be used in the fitting process.
# weights: an optional vector of weights to be used in the 
#          fitting process. Should be NULL or a numeric vector. 
#          If non-NULL, weighted least squares is used with 
#          weights weights (that is, minimizing sum(w*e^2)); 
#          otherwise ordinary least squares is used. See also 
#          'Details',
# na.action:  a function which indicates what should happen 
#             when the data contain NAs. The default is set by 
#             the na.action setting of options, and is na.fail 
#             if that is unset. The 'factory-fresh' default is 
#             na.omit. Another possible value is NULL, no 
#             action. Value na.exclude can be useful.
# method:  the method to be used; for fitting, currently only 
#          method = "qr" is supported; method = "model.frame" 
#          returns the model frame (the same as with model = 
#          TRUE, see below).
# model, x, y, qr:  logicals. If TRUE the corresponding 
#                   components of the fit (the model frame, 
#                   the model matrix, the response, the QR 
#                   decomposition) are returned.
# singular.ok:   logical. If FALSE (the default in S but not 
#                in R) a singular fit is an error.
# contrasts:   an optional list. See the contrasts.arg of 
#              model.matrix.default.
# offset:   this can be used to specify an a priori known 
#           component to be included in the linear predictor 
#           during fitting. This should be NULL or a numeric 
#           vector of length equal to the number of cases. One 
#           or more offset terms can be included in the 
#           formula instead or as well, and if more than one 
#           are specified their sum is used. See model.offset.
# ...:      additional arguments to be passed to the low level 
#           regression fitting functions (see below).

tree_data = read.table("Trees.txt", header=T, sep="\t")

# formula (is DBH related to Species?): DBH ~ index(BOTANICAL)
# data: Trees

tree_data$species_id = id(tree_data["BOTANICAL"], drop=T)

plot(tree_data)

plot(tree_data$DBH~tree_data$BOTANICAL,at=rank(tapply(tree_data$DBH, tree_data$BOTANICAL, median)))

example_lm = lm(DBH ~ species_id, data=tree_data)

# Output
example_lm
# > example_lm
# 
# Call:
#   lm(formula = DBH ~ species_id, data = tree_data)
# 
# Coefficients:
#   (Intercept)   species_id  
# 47.8526       0.1987 

summary(example_lm)
# Call:
#   lm(formula = DBH ~ species_id, data = tree_data)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -7.230 -4.051 -1.442  2.167 19.167 
# 
# Coefficients:
#             Estimate  Std. Error t value Pr(>|t|)    
# (Intercept) 47.85263    0.82114  58.276   <2e-16 ***
# species_id  0.19866     0.08315   2.389   0.0181 *  
#   ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 5.522 on 147 degrees of freedom
# Multiple R-squared:  0.03738,	Adjusted R-squared:  0.03083 
# F-statistic: 5.709 on 1 and 147 DF,  p-value: 0.01815

plot(x=tree_data$species_id,y=tree_data$DBH,type="p")
abline(example_lm)
abline(a=47.8526, b = 0.1987, col="red", lwd=5, lty=2)

plot(tree_data$DBH~tree_data$BOTANICAL, ylim=c(40,75),
     at=rank(tapply(tree_data$DBH, tree_data$BOTANICAL, median)))
abline(example_lm)
abline(a=47.8526, b = 0.1987, col="red", lwd=5, lty=2)

plot(example_lm)
# 1) Possible non-linear trend
# 2) Possibly non-normal (or, "non-Gaussian") residuals
# 3) Potential non-constant error variance)
# 4) Trend may be driven by a single point



# Multiple responses
example_lm2 = lm(DBH ~ species_id+SITE, data=tree_data)

# Output
example_lm2
# > example_lm2
# 
# Call:
#   lm(formula = DBH ~ species_id + SITE, data = tree_data)
# 
# Coefficients:
#   (Intercept)   species_id         SITE  
# 47.58010      0.20138      0.02282  

summary(example_lm2)
# > summary(example_lm2)
# 
# Call:
#   lm(formula = DBH ~ species_id + SITE, data = tree_data)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -7.374 -4.006 -1.244  2.196 19.376 
# 
# Coefficients:
#             Estimate  Std. Error t value Pr(>|t|)    
# (Intercept) 47.58010    0.85631  55.564   <2e-16 ***
# species_id   0.20138    0.08312   2.423   0.0166 *  
# SITE         0.02282    0.02053   1.112   0.2680    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 5.518 on 146 degrees of freedom
# Multiple R-squared:  0.04547,	Adjusted R-squared:  0.03239 
# F-statistic: 3.477 on 2 and 146 DF,  p-value: 0.03348

plot(example_lm2)
# 1) Possible non-linear trend
# 2) Possibly non-normal (or, "non-Gaussian") residuals
# 3) Potential non-constant error variance)
# 4) Low leverage, high resisual
#===============================================================
# Analysis of Variance
#===============================================================

#The command:

?aov()
# "Fit an analysis of variance model by a call to lm for each 
# stratum."

# aov(formula, data = NULL, projections = FALSE, qr = TRUE,
#     contrasts = NULL, ...)



# formula:   A formula specifying the model.
# data:      A data frame in which the variables specified in 
#            the formula will be found. If missing, the 
#            variables are searched for in the standard way.
# projections:  Logical flag: should the projections be returned?
# qr:           Logical flag: should the QR decomposition be 
#               returned?
# contrasts:    A list of contrasts to be used for some of the 
#               factors in the formula. These are not used for 
#               any Error term, and supplying contrasts for 
#               factors only in the Error term will give a 
#               warning.
# ...:      Arguments to be passed to lm, such as subset or 
#           na.action. See 'Details' about weights.

# formula (is DBH different among different Species groups?): DBH ~ index(BOTANICAL)
# data: Trees

example_aov = aov(example_lm)

# Output
example_aov
# > example_aov
# Call:
#   aov(formula = example_lm)
# 
# Terms:
#                 species_id Residuals
# Sum of Squares     174.101  4483.134
# Deg. of Freedom          1       147
# 
# Residual standard error: 5.522455
# Estimated effects may be unbalanced

summary(example_aov)
# > summary(example_aov)
#              Df  Sum Sq Mean Sq F value Pr(>F)  
# species_id    1    174   174.1   5.709 0.0181 *
# Residuals   147   4483    30.5                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



# Multiple responses
example_aov2 = aov(example_lm2)

# Output
example_aov2
# > example_aov2
# Call:
#   aov(formula = example_lm2)
# 
# Terms:
#                 species_id     SITE Residuals
# Sum of Squares     174.101   37.641  4445.493
# Deg. of Freedom          1        1       146
# 
# Residual standard error: 5.518024
# Estimated effects may be unbalanced

summary(example_aov2)
# > summary(example_aov2)
#                Df  Sum Sq Mean Sq F value Pr(>F)  
# species_id      1    174  174.10   5.718 0.0181 *
#   SITE          1     38   37.64   1.236 0.2680  
# Residuals     146   4445   30.45                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

rm(tree_data,example_lm,example_lm2,example_aov,example_aov2,x)



#===============================================================
# Functions
#===============================================================

# Why use functions? 

# For speed!
# To time stuff: proc.time()

test = rnorm(1000, 10, 1)

z_test = vector()

timer = proc.time()
for(i in 1:length(test)) {
  z_test[i] = (test[i] - mean(test)) / sd(test)
}
proc.time() - timer
# > proc.time() - timer
# user  system elapsed 
# 81.78    4.81   88.08 

zscore.func = function(x) (x - mean(x))/sd(x)
timer = proc.time()
z_test_fun = zscore.func(test)
proc.time() - timer
# > proc.time() - timer
# user  system elapsed 
# 0.0     0.0     0.9 



# For flexibility! Not everything has been made for R...

bird_data = read.table("Waterfowl.txt", header=T, sep="\t")

bird_data$Observed_Date = as.Date(bird_data$Observed_Date,
                                  "%m/%d/%Y")

# Bird-use-days function
BUDS = function(x, day) {
  birdusedays = array(NA, dim=c(dim(x)[1],dim(x)[2]))
  for (j in 1:dim(x)[1]) {
    for (k in 2:dim(x)[2]) {
      birdusedays[j,k] = (day[k]-day[k-1])*((x[j,k]+x[j,k-1])/2)
    } # k
  } # j
  results = rowSums(birdusedays, na.rm=TRUE)
  return(round(results))
}

bird_data_bud = 
  dcast.data.table(data.table(bird_data),
                   Waterbody_ID~Observed_Date, fun.aggregate=sum,
                   fill=0, value.var="Observed")

bird_data_days = 
  as.numeric(round(difftime(colnames(bird_data_bud)[-1],
                            min(colnames(bird_data_bud)[-1]), 
                            units="days"))) + 1

waterfowl_buds = BUDS(data.frame(bird_data_bud)[,-1], 
                      bird_data_days)

waterfowl_buds = data.table(waterfowl_buds)

rm(bird_data, bird_data_bud, bird_data_days, test, waterfowl_buds,
   i, timer, z_test_fun, z_test, BUDS, zscore.func)



#===============================================================
# Shortcuts
#===============================================================

# Loading multiple libraries at once

# require(data.table)
# require(plyr)
# ...
# require(...)

x = c("data.table", "plyr")
lapply(x,require,character.only=T)



# Reading in large data files

x = c("readr", "readxl", "microbenchmark")
# lapply(x,install.packages,character.only=T)
lapply(x,require,character.only=T)

# data_test1 = read.csv("big_Trees.csv")
# data_test2 = read_delim("big_Trees.csv", delim=",")
# data_test3 = read_excel("big_Trees.xlsx")

timer = proc.time()
data_test1 = read.csv("big_Trees.csv")
proc.time() - timer
# > proc.time() - timer
# user  system elapsed 
# 8.86    0.25    9.13 

timer = proc.time()
data_test2 = read_delim("big_Trees.csv", delim=",")
proc.time() - timer
# > proc.time() - timer
# user  system elapsed 
# 1.59    0.16    2.25

timer = proc.time()
data_test3 = read_excel("big_Trees.xlsx")
proc.time() - timer
# > proc.time() - timer
# user  system elapsed 
# 16.62    1.64   18.57 



# For loops, Vectorization, Functions

test = rnorm(1000000, 10, 1)

add_test = vector()

timer = proc.time()
for(i in 1:length(test)) {
  add_test[i] = test[i] + 1
}
proc.time() - timer
# > proc.time() - timer
# user  system elapsed 
# 1837.85  494.32 2394.03 <- 39 minutes!

add.func = function(x) x+1
timer = proc.time()
add_test_fun = add.func(test)
proc.time() - timer
# > proc.time() - timer
# user  system elapsed 
# 0.00    0.02    0.02  

timer = proc.time()
add_test_vector = test + 1
proc.time() - timer
# > proc.time() - timer
# user  system elapsed 
# 0.00    0.02    0.02



# Dynamically saving tables and graphs

for(
  i in 1:#NUMBER OF FILES TO SAVE
    ) 
  {
  filename = paste(names(
    #R OBJECTS TO SAVE
    )[i], 
    #" SOME EXTRA DESCRIPTOR OF EACH FILE", 
    ".jpeg", sep="")
  save_files_to = file.path(paste(getwd()), filename)
  
  jpeg(file = 
        #R OBJECT
        , width = 960, height = 480)
  
  plot(
    #R OBJECT
    [[i]], main = names(
      #R OBJECT
      )[i])
  
  dev.off()
}



# Easier subsetting with data.tables

subset_example = read_csv("Trees.csv")
subset_example_sites = subset_example[subset_example$SITE==1,]

# AND
subset_example_large = 
  subset_example[subset_example$SITE==1 &
                   subset_example$DBH>=50,]

# OR
subset_example_species = 
  subset_example[subset_example$BOTANICAL=="Quercus alba" | 
                   subset_example$BOTANICAL=="Acer saccharinum",]

# %in%

"stump" %in% subset_example$"COMMON NAME"



# Doing stuff to every element of two arrays

array_test = c(1:10)
# [1]  1  2  3  4  5  6  7  8  9 10
array_test1 = seq(0.1, 1, 0.1)
# [1] 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0

array_test - array_test1
#[1] 0.9 1.8 2.7 3.6 4.5 5.4 6.3 7.2 8.1 9.0

cbind(array_test, array_test1, array_test - array_test1)
#           array_test  array_test1
# [1,]          1         0.1         0.9
# [2,]          2         0.2         1.8
# [3,]          3         0.3         2.7
# [4,]          4         0.4         3.6
# [5,]          5         0.5         4.5
# [6,]          6         0.6         5.4
# [7,]          7         0.7         6.3
# [8,]          8         0.8         7.2
# [9,]          9         0.9         8.1
# [10,]         10         1.0        9.0

array_test2=vector("list")
for(i in 1:length(array_test)){
  array_test2[[i]] = array_test[i] - array_test1
}
array_test2
# [[1]]
# [1] 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1 0.0
# 
# [[2]]
# [1] 1.9 1.8 1.7 1.6 1.5 1.4 1.3 1.2 1.1 1.0
# 
# [[3]]
# [1] 2.9 2.8 2.7 2.6 2.5 2.4 2.3 2.2 2.1 2.0
# 
# [[4]]
# [1] 3.9 3.8 3.7 3.6 3.5 3.4 3.3 3.2 3.1 3.0
# 
# [[5]]
# [1] 4.9 4.8 4.7 4.6 4.5 4.4 4.3 4.2 4.1 4.0
# 
# [[6]]
# [1] 5.9 5.8 5.7 5.6 5.5 5.4 5.3 5.2 5.1 5.0
# 
# [[7]]
# [1] 6.9 6.8 6.7 6.6 6.5 6.4 6.3 6.2 6.1 6.0
# 
# [[8]]
# [1] 7.9 7.8 7.7 7.6 7.5 7.4 7.3 7.2 7.1 7.0
# 
# [[9]]
# [1] 8.9 8.8 8.7 8.6 8.5 8.4 8.3 8.2 8.1 8.0
# 
# [[10]]
# [1] 9.9 9.8 9.7 9.6 9.5 9.4 9.3 9.2 9.1 9.0

data_frame_test2 = data.frame(matrix(unlist(array_test2), 
                                     nrow = 10, byrow=T))

# What if array_test and array_test1 aren't the same length?

timer = proc.time()
array_test3 = outer(array_test, array_test1, `-`)
proc.time() - timer
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#  [1,]  0.9  0.8  0.7  0.6  0.5  0.4  0.3  0.2  0.1     0
#  [2,]  1.9  1.8  1.7  1.6  1.5  1.4  1.3  1.2  1.1     1
#  [3,]  2.9  2.8  2.7  2.6  2.5  2.4  2.3  2.2  2.1     2
#  [4,]  3.9  3.8  3.7  3.6  3.5  3.4  3.3  3.2  3.1     3
#  [5,]  4.9  4.8  4.7  4.6  4.5  4.4  4.3  4.2  4.1     4
#  [6,]  5.9  5.8  5.7  5.6  5.5  5.4  5.3  5.2  5.1     5
#  [7,]  6.9  6.8  6.7  6.6  6.5  6.4  6.3  6.2  6.1     6
#  [8,]  7.9  7.8  7.7  7.6  7.5  7.4  7.3  7.2  7.1     7
#  [9,]  8.9  8.8  8.7  8.6  8.5  8.4  8.3  8.2  8.1     8
# [10,]  9.9  9.8  9.7  9.6  9.5  9.4  9.3  9.2  9.1     9

timer = proc.time()
array_test4 = lapply(array_test, "-", array_test1)
proc.time() - timer

timer = proc.time()
x = rep(array_test, each=length(array_test))
array_test5 = split(x-array_test1, x)
proc.time() - timer

timer = proc.time()
array_test6 = CJ(array_test, array_test1)[, V:=V1-V2]
proc.time() - timer








