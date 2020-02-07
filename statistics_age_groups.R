###############################################################
# install needed libraries (only for first run):
install.packages("readxl")
install.packages("writexl")
install.packages("lawstat")
install.packages("DescTools")
install.packages("rlist")

# load the needed libraries:
library(readxl)
library(writexl)
library(lawstat)
library(DescTools)
library(rlist)



###############################################################
# enter here the wanted parameters:
setwd("~/Desktop/Manuscript/LMS/data_processing")
data_file_name = 'data_reference_values_v17.xlsx'
save_output_file_name0 = 'data/statistics_age_groups.xlsx'
save_output_file_name1 = 'data/statistics_age_groups_male.xlsx'
save_output_file_name2 = 'data/statistics_age_groups_female.xlsx'
save_output_file_name3 = 'data/statistics_age_groups_gender_test.xlsx'
save_output_file_name4 = 'data/statistics_age_groups_agegroup_test.xlsx'
x.col = 'age'  # the name of the column in the excel file of the predictor variable (usually age or time)
min.x = 18  # the minimum age
max.x = 82  # the max age
y.cols = c('BMI', 'FM', 'LM', 'FMI', 'LMI', 'LMI_g1_splitby_FMI_p0.25', 'LMI_g1_splitby_FMI_p25.75', 'LMI_g1_splitby_FMI_p75.100') # an arry with the names of the columns in the excel file of the response variable (e.g. BMI) (algorithm loops over all columns). Of the columns with "splitby" in their name, only one gender has to be specified here, the other one is selected automatically.

###ATTENTION###
#keep in mind that all rows with nans will be deleted and therefore not be included in analysis --> load them extra if you have nans in some rows and you want to keep those IDs in different rows.
###ATTENTION###

g.col = 'gender'  # the name of the column in the excel file of the gender variable
gender.symbols = c(0,1)  # first symbol should be male, second female
id.col = 'ID'  # the name of the column with the IDs of the samples, or NULL if no ID
ages.to.split = c(18,30,40,50,60,70,82)

# params for testing
normality.test = 'KS'  # which test to use to test for normality, see test.groups.for.mean.difference-function-description
var.homogeneity.test = 'L'  # which test to us to test for homogeneous variance, see test.groups.for.mean.difference-function-description
level.for.normality.test = 0.01  # the level whith which it is decided that a sample is not normally distributed
level.for.var.homogeneous.test = 0.01  # the level with which it is decided that samples of different groups do not have same variance
level.for.all.over.test = 0.01  # the level with which it is decided that the all-aver test (groups used vs. no groups used) is significant
p.adjust = 'holm'  # the wanted correction method for the p-values due to multiple testing, one of: 'bonf', 'holm'

# helper functions:
get.pretty.names = function(name){
  name = gsub('_', ' ', name)
  name = gsub('percent', '%', name)
  name = gsub('quotient', '/', name)
  name = gsub ('g0', 'male', name)
  name = gsub ('g1', 'female', name)
  name = gsub ('p0.25', 'low', name)
  name = gsub ('p25.75', 'normal', name)
  name = gsub ('p75.100', 'high', name)
  return(name)
}
##############################################################################################
##########                    function definition                          ###################
##############################################################################################
test.groups.for.mean.difference = function(values, groups, normality.test = 'KS',
                                           var.homogeneity.test = 'L',
                                           level.for.normality.test = 0.01,
                                           level.for.var.homogeneous.test = 0.01,
                                           level.for.all.over.test = 0.01,
                                           p.adjust = 'bonf'){
  ###############################################################################
  # normality.test: one of: 'KS' (Kolmogorov-Smirnov-Test),
  #                         'SW' (Shapiro-Wilk-Test)
  # var.homogeneity.test: one of: 'B, (Barlett's Test, the univariate version of Box-M-Test),
  #                               'L' (Levene's test)
  # p.adjust: one of: 'bonf' (bonferroni), 'holm', "hochberg", "hommel", "bonferroni", "BH", "BY"
  ###############################################################################

  diff.groups = sort(na.omit(unique(groups)))

  # first perform a Test to check for normalility in each group:
  normality.pvals = c()
  for (group in diff.groups) {
    x = values[which(groups == group)]
    if (normality.test == 'KS' | length(x) > 5000){
      test = ks.test(x = x, y = pnorm, mean(x), sd(x))
    } else {
      if (normality.test == 'SW'){
        test = shapiro.test(x)
      } else {
        stop('No valid normality test selected')
      }
    }
    normality.pvals = c(normality.pvals, test$p.value)
  }

  # then perform a Test to check vor homogeneity of variance:
  if (var.homogeneity.test == 'B'){
    test = bartlett.test(values, groups, na.action = na.omit)
  } else {
    if (var.homogeneity.test == 'L'){
      test = levene.test(values[which(!is.na(groups))], groups[which(!is.na(groups))])
    } else {
      stop('No valid variance homogeneity test selected')
    }
  }
  var.homogeneity.pval = test$p.value

  # choose which test to perform:
  if (min(normality.pvals) > level.for.normality.test/length(diff.groups)){  # make multiple testing correction for the normality-tests using bonferroni
    print('each group is normally distributed -> scheffe-test or t-test can be used')
    if (var.homogeneity.pval > level.for.var.homogeneous.test){
      print('all groups have same variance -> standarnd variance analysis (anova) & if significant the post-hoc scheffe-test used')
      which.test = 'scheffe'
    } else {
      print('not all groups have same variance -> (non-parametric) Kruskall-Wallis-Test & if significant pairwise t-tests with non-equal variance and p.value-correction used')
      which.test = 'ttest'
    }
  } else {
    print('not every group is normally distributed -> (non-parametric) Kruskall-Wallis-Test & if significant (non-parametric) Wilcoxon-Rank-Sum-Test with p.value-correction used')
    which.test = 'wilcox'
  }

  # do the test
  if (which.test == 'scheffe'){
    res.aov = aov(values ~ as.factor(groups), na.action = na.omit)
    overall.pval = summary(res.aov)[[1]][['Pr(>F)']][1]
    if (overall.pval < level.for.all.over.test){
      print('analysis of variance showed significant difference between groups -> do Scheffe-Test')
      test = ScheffeTest(res.aov, which = NULL, contrasts = NULL, conf.level = NA)[[1]]
    } else {
      test = NULL
    }
  }
  if (which.test == 'ttest'){
    res.KW = kruskal.test(values, groups, na.action = na.omit)
    overall.pval = res.KW$p.value
    if (overall.pval < level.for.all.over.test){
      print('Kruskal-Wallis all-over test showed significant difference between groups -> do pairwise ttest')
      test = pairwise.t.test(values, groups, paired = F, var.equal = F, p.adjust.method = p.adjust, pool.sd = F, na.action = na.omit)$p.value
    } else {
      test = NULL
    }
  }
  if (which.test == 'wilcox'){
    res.KW = kruskal.test(values, groups, na.action = na.omit)
    overall.pval = res.KW$p.value
    if (overall.pval < level.for.all.over.test){
      print('Kruskal-Wallis all-over test showed significant difference between groups -> do pairwise wilcoxon-rank-sum test')
      test = pairwise.wilcox.test(values, groups, paired = F, p.adjust.method = p.adjust, na.action = na.omit)$p.value
    } else {
      test = NULL
    }
  }

  return(list(overall.pval, which.test, test))
}








###############################################################
##### load the data:                                  #########
###############################################################
dat = data.frame(read_excel(path = data_file_name))

# extract the data without missing values
y.cols_ = c(x.col, g.col, id.col)
for (y.col in y.cols){
  if (grepl('splitby', y.col, fixed = TRUE)){
    y.cols_ = c(y.cols_, gsub('g1', 'g0', y.col), gsub('g0', 'g1', y.col))
  } else{
    y.cols_ = c(y.cols_, y.col)
  }
}
dat =  dat[,y.cols_]
dat = na.omit(dat)



###############################################################
###### main loop to produce standard statistics       #########
###############################################################
dat.out = data.frame()  # dataframe to store output
dat.out_ = data.frame()

for (y.col in y.cols){  # loop over all y-columns
  print('')
  row = list()  # arrays to store everything from this row
  row1 = c()
  names = c()  # arrays to store the names of the columns for the genders seperately
  names_ = c()
  names0 = c()
  names1 = c()
  names2 = c()
  names3 = c()
  names4 = c()
  names5 = c()

  gender.test.p.vals = c()
  gender.test.names = c()
  gender.test.which = c()
  gender.test.which.names = c()
  for (i.age in 1:length(ages.to.split)){
    if (i.age == length(ages.to.split)){
      min.age = ages.to.split[1]
      max.age = ages.to.split[i.age]
    } else {
      min.age = ages.to.split[i.age]
      max.age = ages.to.split[i.age + 1]
      dat[which(dat[,x.col] >= min.age & dat[,x.col] < max.age),'age.group'] = i.age
    }
    dat1 = dat
    dat1 = dat1[which(dat1[,x.col] >= min.age & dat1[,x.col] < max.age), ]  # keep only the correct age group
    for (gender in gender.symbols){  # loop over the gender symbols
      # keep wanted gender and wanted column
      col.name = paste(min.age, max.age, paste('g', gender, sep = ''), sep = '_')
      col.names = paste(col.name, c('size', 'mean', 'std'), sep = '_')
      names = c(names, col.names)
      names_ = c(names_, col.name)
      if (gender == gender.symbols[1]){
        if (grepl('splitby', y.col, fixed = TRUE)){  # if the column was splitted (hence only one gender in the column) get also the column of the other gender
          y.col_ = gsub(paste('g', gender.symbols[2], sep = ''), paste('g', gender.symbols[1], sep = ''), y.col)
        } else {
          y.col_ = y.col
        }
        male = dat1[which(dat1[,g.col] == gender),y.col_]
        male = male[which(male > 0)]
        names0 = c(names0, col.names)
        names3 = c(names3, col.name)
        y = male
      } else {
        if (grepl('splitby', y.col, fixed = TRUE)){
          y.col_ = gsub(paste('g', gender.symbols[1], sep = ''), paste('g', gender.symbols[2], sep = ''), y.col)
        } else {
          y.col_ = y.col
        }
        female = dat1[which(dat1[,g.col] == gender),y.col_]
        female = female[which(female > 0)]
        names1 = c(names1, col.names)
        names4 = c(names4, col.name)
        y = female
      }
      # ----------------------------------------------
      row = list.append(row, length(y), mean(y), sd(y))
      row1 = c(row1, paste(round(mean(y), digits = 1), round(sd(y), digits = 1), sep = '+/-'))
    }

    # ------------------------------------------------
    # make tests between genders
    # ------------------------------------------------
    # first perform a Test to check for normalility in both group:
    normality.pvals = c()
    for (i in gender.symbols) {
      if (i == gender.symbols[1]){
        x = male
      } else {
        x = female
      }
      if (normality.test == 'KS' | length(x) > 5000){
        test = ks.test(x = x, y = pnorm, mean(x), sd(x))
      } else {
      if (normality.test == 'SW'){
        test = shapiro.test(x)
      } else {
        stop('No valid normality test selected')
      }
      }
      normality.pvals = c(normality.pvals, test$p.value)
    }

    # then perform a Test to check vor homogeneity of variance:
    if (var.homogeneity.test == 'B'){
      test = bartlett.test(c(male, female), c(rep(1, times = length(male)), rep(2, times = length(female))), na.action = na.omit)
    } else {
    if (var.homogeneity.test == 'L'){
      test = levene.test(c(male, female), c(rep(1, times = length(male)), rep(2, times = length(female))))
    } else {
      stop('No valid variance homogeneity test selected')
    }
    }
    var.homogeneity.pval = test$p.value

    # choose which test and perform it:
    if (min(normality.pvals) > level.for.normality.test/2){  # make multiple testing correction for the normality-tests
      if (var.homogeneity.pval > level.for.var.homogeneous.test){
        # make t-test with same var between genders:
        gender.test = t.test(x = male, y = female, var.equal = TRUE)
        gender.test.p.vals = c(gender.test.p.vals, gender.test$p.value)
        gender.test.which = c(gender.test.which, 'ttest_equ_var')
      } else {
        # make t-test with different var between genders:
        gender.test = t.test(x = male, y = female, var.equal = FALSE)
        gender.test.p.vals = c(gender.test.p.vals, gender.test$p.value)
        gender.test.which = c(gender.test.which, 'ttest_diff_var')
      }
    } else {
      # make wilcoxon test between genders:
      gender.test = wilcox.test(x = male, y = female, paired = FALSE)
      gender.test.p.vals = c(gender.test.p.vals, gender.test$p.value)
      gender.test.which = c(gender.test.which, 'wilcox')
    }
    gender.test.names = c(gender.test.names, paste(paste('group', i.age, sep='.'), 'gender_test_adjusted_pval', sep='_'))
    gender.test.which.names = c(gender.test.which.names, paste(paste('group', i.age, sep='.'), 'used_test', sep = '_'))

    #### old code version ###
    # # make t-test between genders:
    # gender.test = t.test(x = male, y = female, var.equal = FALSE)
    # row = list.append(row, gender.test$p.value, min(1,gender.test$p.value*length(ages.to.split)))
    # names = c(names, paste(min.age, max.age, c('gender_ttest_pvalue', 'gender_ttest_pvalue_bonf_adj'), sep = '_'))
    # names2 = c(names2, paste(min.age, max.age, c('gender_ttest_pvalue', 'gender_ttest_pvalue_bonf_adj'), sep = '_'))
    #
    # # make wilcoxon-test between genders:
    # gender.test1 = wilcox.test(x = male, y = female, paired = FALSE)
    # row = list.append(row, gender.test1$p.value, min(1,gender.test1$p.value*length(ages.to.split)))
    # names = c(names, paste(min.age, max.age, c('gender_WRST_pvalue', 'gender_WRST_pvalue_bonf_adj'), sep = '_'))
    # names2 = c(names2, paste(min.age, max.age, c('gender_WRST_pvalue', 'gender_WRST_pvalue_bonf_adj'), sep = '_'))

  }
  # correct the p-values of the gender tests
  if (p.adjust == 'bonf' | p.adjust == 'bonferroni'){
    for (i in 1:length(gender.test.p.vals)){
      p.val = min(1, gender.test.p.vals[i]*length(ages.to.split))
      row = list.append(row, gender.test.which[i], p.val)
      names = c(names, gender.test.which.names[i], gender.test.names[i])
      names2 = c(names2, gender.test.which.names[i], gender.test.names[i])
    }
  } else {
  if (p.adjust == 'holm'){
    p.order = order(gender.test.p.vals, decreasing = TRUE)
    ordered.gender.p.vals = gender.test.p.vals[p.order] * seq(1, length(gender.test.p.vals))
    # adjust the p-values s.t. they are still decreasing
    for (i.pval in (length(gender.test.p.vals)-1):1){
      ordered.gender.p.vals[i.pval] = max(ordered.gender.p.vals[i.pval], ordered.gender.p.vals[i.pval+1])
    }
    reorder = order(seq(1, length(gender.test.p.vals))[p.order])
    gender.test.p.vals = ordered.gender.p.vals[reorder]
    for (i in 1:length(gender.test.p.vals)){
      p.val = min(1, gender.test.p.vals[i])
      row = list.append(row, gender.test.which[i], p.val)
      names = c(names, gender.test.which.names[i], gender.test.names[i])
      names2 = c(names2, gender.test.which.names[i], gender.test.names[i])
    }
  } else {
    stop('no valid p.adjust (multiple-testing-correction) selected')
  }
  }



  # --------------------------------------------------
  # test between age groups:
  # ------------------------------------------------
  n.groups = length(ages.to.split) - 1
  n.tests = n.groups * (n.groups -1) / 2
  for (gender in c(gender.symbols, NA)){
    # get the correct data for the wanted gender
    if (is.na(gender)){
      values = dat[, y.col]
      groups = dat[,'age.group']
      if (grepl('splitby', y.col, fixed = TRUE)){  # if the column was splitted (hence only one gender in the column) get also the column of the other gender
        if (grepl('g0', y.col, fixed = TRUE)){
          y.col_ = gsub('g0', 'g1', y.col)
        } else {
          y.col_ = gsub('g1', 'g0', y.col)
        }
        values = c(values, dat[, y.col_])
        groups = c(groups, dat[, 'age.group'])
      }
    } else {
      if (grepl('splitby', y.col, fixed = TRUE)){  # if the column was splitted (hence only one gender in the column) get also the column of the other gender
        y.col_ = gsub(paste('g', gender.symbols[1], sep = ''), paste('g', gender, sep = ''), y.col)
        y.col_ = gsub(paste('g', gender.symbols[2], sep = ''), paste('g', gender, sep = ''), y.col_)
      } else {
        y.col_ = y.col
      }
      values = dat[which(dat[,g.col] == gender), y.col_]
      groups = dat[which(dat[,g.col] == gender), 'age.group']
    }
    # keep only entries with values > 0 (important for splitted rows, where rows not belonging to the split have values -1)
    groups = groups[which(values > 0)]
    values = values[which(values > 0)]


    # do the tests
    print(paste(y.col, 'gender:', gender))
    res.list = test.groups.for.mean.difference(values = values, groups = groups,
                                               normality.test = normality.test,
                                               var.homogeneity.test = var.homogeneity.test,
                                               level.for.normality.test = level.for.normality.test,
                                               level.for.var.homogeneous.test = level.for.var.homogeneous.test,
                                               level.for.all.over.test = level.for.all.over.test,
                                               p.adjust = p.adjust)
    test.res = res.list[[3]]
    overall.pval = res.list[[1]]
    which.test = res.list[[2]]

    # save which test was used
    row = list.append(row, which.test)
    name_app = paste(paste('gender', gender, sep = '.'), 'used_test', sep = '_')
    names = c(names, name_app)
    names5 = c(names5, name_app)

    # save overall pval
    row = list.append(row, overall.pval)
    name_app = paste(paste('gender', gender, sep = '.'), 'overall_pval', sep = '_')
    names = c(names, name_app)
    names5 = c(names5, name_app)

    # put the p.values in the wanted line-format with correct labels
    for (age.g1 in 2:(length(ages.to.split)-1)){
      for (age.g2 in 1:(age.g1-1)){
        if (is.null(test.res)){
          pval = NA
        } else {
          pval = test.res[age.g1-1, age.g2]
        }
        row = list.append(row, pval)
        name_app = paste(paste('gender', gender, sep = '.'), paste('group', age.g1, age.g2, sep = '.'), 'adjusted_pval', sep = '_')
        names = c(names, name_app)
        names5 = c(names5, name_app)
      }
    }
  }

  # append the out file
  dat.out = rbind(dat.out, row, stringsAsFactors = F)
  names(dat.out) = names

  dat.out_ = rbind(dat.out_, row1, stringsAsFactors = F)
  names(dat.out_) = names_
}


# get the correct row names and sort the columns
y.cols_ = c()
for (y.col in y.cols){
  if (grepl('splitby', y.col, fixed = TRUE)){  # if the column was splitted (hence only one gender in the column) get also the column of the other gender
    y.col = gsub('g0_', '', y.col)
    y.col = gsub('g1_', '', y.col)
  }
  y.cols_ = c(y.cols_, y.col)
}

dat.out = cbind(data.frame(row_names=y.cols_, stringsAsFactors = F), dat.out, stringsAsFactors = F)
dat.out_ = cbind(data.frame(row_names=y.cols_, stringsAsFactors = F), dat.out_, stringsAsFactors = F)
dat.out = dat.out[,c('row_names', names0, names1, names2, names5)]
dat.out1 = dat.out_[,c('row_names', names3)]
dat.out2 = dat.out_[,c('row_names', names4)]
dat.out3 = dat.out[,c('row_names', names2)]
dat.out4 = dat.out[,c('row_names', names5)]

# load already saved files, append them and then save them
file_names = c(save_output_file_name0, save_output_file_name1, save_output_file_name2, save_output_file_name3, save_output_file_name4)
dats = list(dat.out, dat.out1, dat.out2, dat.out3, dat.out4)
for (i in 1:5){  # loop through all files that should be appended
  # load the file if it exists
  if (file.exists(file_names[i])){
    saved_dat = data.frame(read_excel(path = file_names[i]))
  } else {
    saved_dat = data.frame()
  }
  # change names
  new_names = c()
  for (name in names(saved_dat)){
    new_names = c(new_names, gsub('X', '', name))
  }
  names(saved_dat) = new_names
  # append the file
  saved_dat = rbind(saved_dat, dats[[i]], stringsAsFactors = F)
  # save the file
  write_xlsx(saved_dat, path = file_names[i])
}









###############################################################
###### script to compare LMI subgroups                #########
###############################################################
# parameters to be specified:
#y.cols = c('LMI_g1_splitby_FMI_p0.25', 'LMI_g1_splitby_FMI_p25.75', 'LMI_g1_splitby_FMI_p75.100')  # put here the names of the columns that should be compared
#y.cols = c('LMI_g1_splitby_BMI_normal', 'LMI_g1_splitby_BMI_overweight', 'LMI_g1_splitby_BMI_obesity')  # put here the names of the columns that should be compared
y.cols = c('appendicular_LMI_g1_splitby_BMI_normal', 'appendicular_LMI_g1_splitby_BMI_overweight', 'appendicular_LMI_g1_splitby_BMI_obesity')  # put here the names of the columns that should be compared
gender.symbols = c(0, 1)
out_file_name = 'data/statistics_BMI_subgroups_test1.xlsx'
#out_file_name = 'data/statistics_LMI_subgroups_test.xlsx'
normality.test = 'SW'  # which test to use to test for normality, see test.groups.for.mean.difference-function-description
var.homogeneity.test = 'L'  # which test to us to test for homogeneous variance, see test.groups.for.mean.difference-function-description
level.for.normality.test = 0.01  # the level whith which it is decided that a sample is not normally distributed
level.for.var.homogeneous.test = 0.01  # the level with which it is decided that samples of different groups do not have same variance
level.for.all.over.test = 0.01  # the level with which it is decided that the all-aver test (groups used vs. no groups used) is significant
p.adjust = 'holm'  # the wanted correction method for the p-values due to multiple testing, one of: 'bonf', 'holm'


###############################################################
# code to run:
dat.out = data.frame()  # dataframe to store output

for (gender in gender.symbols){
  for (i.age in 1:length(ages.to.split)){
    row = list()
    names = character()
    
    # save the gender:
    row = list.append(row, gender)
    names = c(names, 'gender')
    
    print('-------------------------------------------------------------------------------------------------')
    
    # get the values of all subgroups with different group labels:
    values = numeric(0)
    groups = numeric(0)
    if (i.age == length(ages.to.split)){  # all ages together
      row = list.append(row, ages.to.split[1], ages.to.split[i.age])
      names = c(names, 'age_from', 'age_to')
      print(paste('gender:', gender, 'age group:', i.age, 'from:', ages.to.split[1], 'to:', ages.to.split[i.age]))
      for (i.col in 1:length(y.cols)){
        # get correct column name for the gender
        y.col_ = gsub(paste('g', gender.symbols[1], sep = ''), paste('g', gender, sep = ''), y.cols[i.col])
        y.col_ = gsub(paste('g', gender.symbols[2], sep = ''), paste('g', gender, sep = ''), y.col_)
        
        vals = dat[, y.col_]
        vals = vals[which(vals > 0)]
        values = c(values, vals)
        groups = c(groups, rep(i.col, length(vals)))
      }
      
    } else {  # age group specific
      row = list.append(row, ages.to.split[i.age], ages.to.split[i.age+1])
      names = c(names, 'age_from', 'age_to')
      print(paste('gender:', gender, 'age group:', i.age, 'from:', ages.to.split[i.age], 'to:', ages.to.split[i.age+1]))
      for (i.col in 1:length(y.cols)){
        # get correct column name for the gender
        y.col_ = gsub(paste('g', gender.symbols[1], sep = ''), paste('g', gender, sep = ''), y.cols[i.col])
        y.col_ = gsub(paste('g', gender.symbols[2], sep = ''), paste('g', gender, sep = ''), y.col_)
        
        vals = dat[which(dat[,'age.group'] == i.age), y.col_]
        vals = vals[which(vals > 0)]
        values = c(values, vals)
        groups = c(groups, rep(i.col, length(vals)))
      }
    }
    
    # make the test between the groups:
    res.list = test.groups.for.mean.difference(values = values, groups = groups,
                                               normality.test = normality.test,
                                               var.homogeneity.test = var.homogeneity.test,
                                               level.for.normality.test = level.for.normality.test,
                                               level.for.var.homogeneous.test = level.for.var.homogeneous.test,
                                               level.for.all.over.test = level.for.all.over.test,
                                               p.adjust = p.adjust)
    test.res = res.list[[3]]
    overall.pval = res.list[[1]]
    which.test = res.list[[2]]
    
    print(paste('which test:', which.test))
    print(paste('overall p-value:', overall.pval))
    print('test results:')
    print(test.res)
    
    # save results:
    # save which test was used
    row = list.append(row, which.test)
    names = c(names, 'used_test')
    
    # save overall pval
    row = list.append(row, overall.pval)
    names = c(names, 'overall_pval')
    
    # put the p.values in the wanted line-format with correct labels
    for (col.g1 in 2:(length(y.cols))){
      for (col.g2 in 1:(col.g1-1)){
        if (is.null(test.res)){
          pval = NA
        } else {
          pval = test.res[col.g1-1, col.g2]
        }
        row = list.append(row, pval)
        name_app = paste(paste('group', col.g1, col.g2, sep = '.'), 'adjusted_pval', sep = '_')
        names = c(names, name_app)
      }
    }
  
    # append the out file
    dat.out = rbind(dat.out, row, stringsAsFactors = F)
    names(dat.out) = names
  }
}

# save the file
write_xlsx(dat.out, path = out_file_name)
































##############################################################################################
##### old code versions
##############################################################################################
# # test between age groups:
# n.groups = length(ages.to.split) - 1
# n.tests = n.groups * (n.groups -1) / 2
# for (gender in c(gender.symbols, NA)){
#   for (age.group1 in 2:n.groups){
#     for (age.group2 in 1:(age.group1-1)){
#       if (is.na(gender)){
#         y1 = dat[which(dat[,'age.group'] == age.group1), y.col]
#         y2 = dat[which(dat[,'age.group'] == age.group2), y.col]
#         if (grepl('splitby', y.col, fixed = TRUE)){  # if the column was splitted (hence only one gender in the column) get also the column of the other gender
#           if (grepl('g0', y.col, fixed = TRUE)){
#             y.col_ = gsub('g0', 'g1', y.col)
#           } else {
#             y.col_ = gsub('g1', 'g0', y.col)
#           }
#           y1 = c(y1, dat[which(dat[,'age.group'] == age.group1), y.col_])
#           y2 = c(y2, dat[which(dat[,'age.group'] == age.group2), y.col_])
#         }
#       } else {
#         if (grepl('splitby', y.col, fixed = TRUE)){  # if the column was splitted (hence only one gender in the column) get also the column of the other gender
#           y.col_ = gsub(paste('g', gender.symbols[1], sep = ''), paste('g', gender, sep = ''), y.col)
#           y.col_ = gsub(paste('g', gender.symbols[2], sep = ''), paste('g', gender, sep = ''), y.col_)
#         } else {
#           y.col_ = y.col
#         }
#         y1 = dat[which(dat[,'age.group'] == age.group1 & dat[,g.col] == gender), y.col_]
#         y2 = dat[which(dat[,'age.group'] == age.group2 & dat[,g.col] == gender), y.col_]
#       }
#       # keep only entries with values > 0 (important for splitted rows, where rows not belonging to the split have values -1)
#       y1 = y1[which(y1 > 0)]
#       y2 = y2[which(y2 > 0)]
#
#       # make t-test between age groups:
#       age.group.test = t.test(x = y1, y = y2, var.equal = FALSE)
#       row = c(row, age.group.test$p.value, min(1, age.group.test$p.value*n.tests))
#       name_app = paste(paste('gender', gender, sep = '.'), paste('group', age.group1, age.group2, sep = '.'), c('ttest_pvalue', 'ttest_pvalue_bonf_adj'), sep = '_')
#       names = c(names, name_app)
#       names5 = c(names5, name_app)
#
#       # make wilcoxon-test between age groups:
#       age.group.test2 = wilcox.test(x = y1, y = y2, paired = FALSE)
#       row = c(row, age.group.test2$p.value, min(1, age.group.test2$p.value*n.tests))
#       name_app = paste(paste('gender', gender, sep = '.'), paste('group', age.group1, age.group2, sep = '.'), c('WRST_pvalue', 'WRST_pvalue_bonf_adj'), sep = '_')
#       names = c(names, name_app)
#       names5 = c(names5, name_app)
#     }
#   }
# }



