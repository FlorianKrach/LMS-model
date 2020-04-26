###############################################################
# install needed libraries (only for first run):
install.packages("readxl")
install.packages("writexl")
install.packages('VGAM')

# load the needed libraries:


library(readxl)
library(writexl)
library(VGAM)



###############################################################
# enter here the wanted parameters:
setwd("~/Code/R/LMS")
data_file_name = 'data_reference_values_v17.xlsx'  # the name of the data file
save_output_file_name = 'data/auto_overview.xlsx'
x.col = 'age'  # the name of the column in the excel file of the predictor variable (usually age or time)
min.x = 6  # the minimum age
max.x = 18  # the max age
y.cols = c('FM', 'FMI', 'percentBF', 'appendicular_BF', 'appendicular_FMI')  # an arry with the names of the columns in the excel file of the response variable (e.g. BMI) (algorithm loops over all columns)
g.col = 'gender'  # the name of the column in the excel file of the gender variable
wanted.gender.symbols = c(0,1)  # array with the symbols of the wanted gender, e.g. 'm', 'f', 0, 1, NULL (if all)
id.col = 'ID'  # the name of the column with the IDs of the samples, or NULL if no ID
std.units = 10  # this is the number of standard deviations units to dedect outliers (i.e. a linear model is fit to the data and all datapoints with standardized residuals bigger than this number are excluded for the LMS-model fitting part)

tol = 0.001  # a small positive number, the tolerance to test whether lambda is equal zero (if smaller than tol then it is set to be zero)
edf.lambda = seq(from=1, to=5, by=1)  #1,5,1  # effective degree of freedome for lambda (L), either a single value or an array
edf.mu = seq(from=3, to=4, by=1)  #3,10,1  # effective degree of freedome for mu (M), this is usually chosen higher than for L and S, either a single value or an array
edf.sigma = seq(from=1, to=5, by=1)  #1,5,1  # effective degree of freedome for sigma (S), either a single value or an array
ilambdas = c(1, 0.5, 0.3, 1.5, 2)  # values that are used as initial lambda values for fitting the LMS-model. The success of the fitting can depend on these values. The values are used in their given order and as soon as one of them works, the others are not tried any more.
criterion = 'BIC'  # model selection criterion, one of: 'AIC', 'BIC' (same as SBC, this is suggested to be used), 'logLikelihood' (should not be used, since it is not the penalized version -> the more degrees of freedome the better the score)

t.scale = seq(from=min.x, to=max.x, by=1)  # an array containing all x-values for which the L,M,S values should be computed
percentiles = c(3,10,50,90,97)  # enter here the wanted percentiles (in %)
perc_color_default = c('dark green', 'dark green', 'dark green', 'dark green', 'dark green')  # colors of the percentile curves, if gender is neither 0 nor 1
perc_color_g1 = c('dark red', 'dark red', 'dark red', 'dark red', 'dark red')  # colors of the percentile curves if gender is 1
perc_color_g0 = c('dark blue', 'dark blue', 'dark blue', 'dark blue', 'dark blue')  # colors of the percentile curves if gender is 0
perc_line_width = c(2,2,2,2,2,2,2)  # line widths of the percentile curves, either 1dim or same as dim as color
perc_line_style = c(1,2,3,2,1)  # line styles of the percentile curves, either 1dim or same as dim as color
perc_label_dist = c(0,0,0,0,0)  # the extra dist between the end of line and the label
min.y.label.diff = 0.01  # minimum distance on y-axis (in percent of the entire displayed y-axis part) that has to be between two labels on the perc plot (change this if some of the labels intersect)

# grid: first entry for small steps, second entry for bigger steps
grid.col = c('light gray', 'light gray')
grid.lty = c(1,1)
grid.lwd = c(0.5,1.5)
n.x.ticks = 12
n.y.ticks.mult = 1
grid.x.dist = c(0.25, 1)
grid.y.dist = c(0.25, 1)
point_color = 'light gray'  # color of the points in the percentile curve plot with points
point_style = 20  # style of the points in the percentile curve plot with points, suggested: 1, 20
xdist = 0.4 # distance on x-axis, suggested: 1 for children, 5 for adults
x.plot.min = 6
x.plot.max = 18
length.t.scale = 500

  
plot.height = 6  # the height of the saved plots
plot.width = 10  # the width of the saved plots
plot.res = 300  # the resolution of the saved plots

# here is the parameters for splitting the data:
splitting.y.cols = c('')  # array of the column names by which the data is split up (i.e. for these columns a LMS model is fitted and the ids of all datapoints belonging to each percentile are extracted seperately. With the extracted ids, other data columns (specified below), having the same id-universe, can be split up)
data_file_name_with_cols_to_split = 'data_reference_values_v17_childrenonly.xlsx'  # file name where the columns that should be split up are taken from and saved to
y.cols.to.split = c('LMI')  # array of the column names in data_file_name_with_cols_to_split which should be split up according to the split computed by columns above
percentiles_for_data_split = c(25,75)  # these are the percentiles by which the data is split up, the split data is save in an xlsx file, where which_percentile is an integer representing the percentile a datapoint belongs to. The lowest percentile has number 0, the next 1 and so on. A point falls in the upper percentile if it is '>' then the percentile curve at its t-value.
# NOTE: the name of the new column will be: '[y.col.to.split]_g[wanted.gender.symbol]_splitby_[splitting.y.col]_p[from_percentile].[to_percentile]'




###############################################################
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

# function to plot the percentile curves
plot_percentiles = function(fit, percentiles, gender, x.col, y.col, y.lim, length.t.scale,
                            perc_line_width, perc_color, perc_line_style,
                            xdist, add, add.grid = T){
  t.scale.plot = seq(from=x.plot.min, to=x.plot.max, length.out = length.t.scale)
  percentile.values = fit@family@linkinv(eta = predict(fit, data.frame(t = t.scale.plot)),
                                         extra = list(percentiles = percentiles))
  # plot
  if (!(add)){
    plot(x=t.scale.plot, y=percentile.values[,1], type='n', las=1,
         main = if(gender==0){'Male'}else{'Female'},
         xlab = get.pretty.names(x.col), 
         xlim = c(min(t.scale.plot), max(t.scale.plot)+xdist), ylab = get.pretty.names(y.col),
         ylim = y.lim)
    if (add.grid){
      abline(v = seq(x.plot.min, x.plot.max, by=grid.x.dist[1]), lwd = grid.lwd[1], col = grid.col[1], lty = grid.lty[1])
      abline(h = seq(round(y.lim[1]), round(y.lim[2]), by=grid.y.dist[1]), lwd = grid.lwd[1], col = grid.col[1], lty = grid.lty[1])
      abline(v = seq(x.plot.min, x.plot.max, by=grid.x.dist[2]), lwd = grid.lwd[2], col = grid.col[2], lty = grid.lty[2])
      abline(h = seq(round(y.lim[1]), round(y.lim[2]), by=grid.y.dist[2]), lwd = grid.lwd[2], col = grid.col[2], lty = grid.lty[2])
      # grid(nx = grid.n.x.ticks, ny = (y.lim[2] - y.lim[1])*grid.y.ticks.mult, lwd = grid.lwd, col = grid.col, lty = grid.lty)
    }
  }
  y.pos.labels = c()
  y.plot.labels = c()
  for (i in 1:length(percentiles)){
    lines(x=t.scale.plot, y=percentile.values[,i],
          lwd = perc_line_width[i],
          col = perc_color[i], lty = perc_line_style[i])
    add.y.label.diff = 0
    if (i > 1){
      check.y.diff = (percentile.values[length(t.scale.plot),i] - y.pos.labels[i-1])/(y.lim[2] - y.lim[1])
      if (check.y.diff < min.y.label.diff){
        add.y.label.diff = (min.y.label.diff - check.y.diff)*(y.lim[2] - y.lim[1])
      }
    }
    y.pos.labels = c(y.pos.labels, percentile.values[length(t.scale.plot),i] + add.y.label.diff)
    y.plot.labels = c(y.plot.labels, paste(percentiles[i], "%", sep = ''))
  }
  text(x=perc_label_dist+max(t.scale.plot), y=y.pos.labels, pos=4, labels=y.plot.labels, col = perc_color)
}





###############################################################
# load the data:
dat = data.frame(read_excel(path = data_file_name))

# extract the data without missing values
y.cols_ = intersect(c(y.cols, x.col, g.col, id.col), names(dat))
dat =  dat[,y.cols_]
dat = na.omit(dat)

# extract the data with wanted age (min.x, max.x)
dat = dat[which(dat[,x.col] >= min.x),]
dat = dat[which(dat[,x.col] <= max.x),]

###############################################################
###### main loop to produce all the wanted LMS-models #########
###############################################################
# loop over all wanted y.cols
for (y.col in y.cols){
  # loop over all genders
  for (wanted.gender.symbol in wanted.gender.symbols){
    
    # get the correct plotting color
    if (wanted.gender.symbol==0){
      perc_color=perc_color_g0
    } else{
      if (wanted.gender.symbol==1){
        perc_color=perc_color_g1
      } else{
        perc_color=perc_color_default
      }
    }
    
    
    print('')
    print(paste(y.col, 'gender:', wanted.gender.symbol, sep = ' '))
    if (!(dir.exists('data/'))){
      dir.create(path='data/')
    }
    path = paste('data/', y.col, '_gender', wanted.gender.symbol, sep = '')
    if (!(dir.exists(path))){
      dir.create(path=path)
    }
    # create file names:
    out_file_LMS_name = paste(path, 'LMS_params.xlsx', sep = "/")  # the name of the output file for LMS values and percentile values
    out_file_Zscore_name = paste(path, 'Zscore.xlsx', sep = "/")  # the name of the output file for Z-scores of the data samples
    out_file_data_split = paste(path, 'data_split.xlsx', sep = "/")  # the name of the output file where the data splitted up by wanted percentiles is saved
    percentile_plot_name1 = paste(path, 'percentile_plot1.png', sep = "/")  # the name of the file with the percentile plot
    percentile_plot_name2 = paste(path, 'percentile_plot2.png', sep = "/")  # the name of the file with the percentile plot
    percentile_plot_name3 = paste(path, 'percentile_plot_data_split.png', sep = "/")  # the name of the file with the percentile plot
    histogram_plot_name = paste(path, 'histogram_zscores.png', sep = "/")  # the name of the plot of the histogram of the Z-scores
    saved_model_name = paste(path, 'saved_LMS_model.rda', sep = "/")  # the name of the file where the fitted model is saved

    # get data:
    y = dat[,y.col]
    t = dat[,x.col]
    if (is.null(wanted.gender.symbol)){
      g.ids = 1:length(t)
    } else {
      g = dat[,g.col]
      g.ids = which(g == wanted.gender.symbol)
    }
    y = y[g.ids]
    t = t[g.ids]

    if (!(is.null(id.col))){
      ids = dat[g.ids, id.col]
    }

    # exclude datapoints with values <= 0:
    which.to.keep = which(y > 0)
    if (length(which.to.keep) <= 0){  # check whether data is left, otherwise go to next iteration (this can happen if a column has only entries for men but the current wanted.gender is female)
      print('no data left after excluding wrong values => skipping')
      unlink(path, recursive = TRUE)
      next
    }
    y = y[which.to.keep]
    t = t[which.to.keep]
    if (!(is.null(id.col))){
      ids = ids[which.to.keep]
    }

    # exclude outliers:
    lm.fit = lm(y ~ t)
    #plot(lm.fit)
    #plot(t,y)
    #abline(lm.fit, col=2, lwd=1.5)
    standardized.residuals = rstandard(lm.fit)
    which.not.outliers = which(abs(standardized.residuals) <= std.units)
    which.outliers = which(abs(standardized.residuals) > std.units)
    y.all = y
    t.all = t
    y = y[which.not.outliers]
    t = t[which.not.outliers]
    if (!(is.null(id.col))){
      ids.all = ids
      ids = ids[which.not.outliers]
    }
    number.outliers = length(y.all) - length(y)
    str.outlier.ids = ''
    if (!(is.null(id.col))){
      if (number.outliers > 0){
        str.outlier.ids = as.character(which.outliers[1])
        if (number.outliers > 1){
          for (i.outlier in 2:number.outliers){
            str.outlier.ids = paste(str.outlier.ids, as.character(which.outliers[i.outlier]), sep = ',')
          }
        }
      }
    }

    # fit the LMS model:
    distances = data.frame(edf.lambda = numeric(0), edf.mu = numeric(0),
                           edf.sigma = numeric(0), logLikelihood = numeric(0),
                           AIC = numeric(0), BIC = numeric(0))
    best.dist = Inf
    fit = NULL

    # loop over all combinations of edf's:
    for (edfL in edf.lambda){
      for (edfM in edf.mu){
        for (edfS in edf.sigma){
          formula = as.formula(paste('y ~', 's(t, df = c(', edfL,
                                     ',', edfM, ',', edfS, '))'))
          for (ilambda in ilambdas){  # try out different start values for the power, if necessary (i.e. if the first value does not work try the second and so on)
            # ------------
            tryCatch({
              print(paste('initial-lambda:', ilambda))
              fit1 = NULL
              fit1 <- vgam(formula = formula, lms.bcn(zero = NULL, tol0 = tol, ilambda = ilambda),
                           control = vgam.control(maxit=30, epsilon = 1e-07))
              break
            }, 
            error=function(cond){
              message(cond)
            })
            # ------------
          }
          
          if (is.null(fit1)){
            print('None of the provided initial-lambda-values worked')
          } 
          else{
            tryCatch({
              logLik1 = logLik(fit1)  # the log-likelihood of the fitted model (this is maximized)
              AIC1 = AIC(fit1)
              BIC1 = BIC(fit1)
              
              # save the AIC, BIC and log-likelihood of the model
              distances = rbind(distances, data.frame(edf.lambda = edfL, edf.mu = edfM,
                                                      edf.sigma = edfS, logLikelihood = -2*logLik1,
                                                      AIC = AIC1, BIC = BIC1))
              
              # if the new model has better log-likelihood than the previously best model, save it
              if (criterion == 'loglik' & logLik1 < best.dist){
                best.dist = logLik1
                fit = fit1
              } else
                if (criterion == 'AIC' & AIC1 < best.dist){
                  best.dist = AIC1
                  fit = fit1
                } else
                  if (criterion == 'BIC' & BIC1 < best.dist){
                    best.dist = BIC1
                    fit = fit1
                  }
            }, 
            error=function(cond){
              message(cond)
            })
          }
          
        }
      }
    }

    #distances
    dist = distances[which.min(distances[,criterion]),]  # the configuration minimizing the criterion
    #distances[c(42, 49),]  # to compare distances

    # save the model:
    save(fit, file = saved_model_name)


    ###############################################################
    # compute the LMS parameters and the percentiles (specified in percentiles) for the values
    # in t.scale and save them:
    LMS = predict(fit, newdata = data.frame(t = t.scale))
    percentile.values = fit@family@linkinv(eta = predict(fit, data.frame(t = t.scale)),
                                           extra = list(percentiles = percentiles))
    out_dat1 = cbind(data.frame(t = t.scale), LMS, percentile.values)
    out_dat1[,4] = exp(out_dat1[,4])
    names(out_dat1)[1] = x.col
    names(out_dat1)[4] = 'sigma'
    write_xlsx(out_dat1, path = out_file_LMS_name)


    ###############################################################
    # compute the Z-scores for all samples and save them
    zscores = qnorm(cdf(fit, newdata = data.frame(t = t.all, y = y.all)))
    if (!(is.null(id.col))){
      out_dat2 = cbind(data.frame(id = ids.all), data.frame(Zscore = zscores))
      names(out_dat2)[1] = id.col
    } else{
      out_dat2 = cbind(data.frame(id = 1:length(zscores)), data.frame(Zscore = zscores))
    }
    write_xlsx(out_dat2, path = out_file_Zscore_name)

    # histogram of z-scores and density of standard normal distribution (in theory they should be very
    # similar)
    par(mfrow=c(1,1))
    hist(zscores, breaks = 50, freq = F)
    ts = seq(-4,4,length.out = 1000)
    lines(ts, dnorm(ts), col='red')
    # save the plot:
    png(histogram_plot_name, units='in', width=plot.width, height=plot.height, res=plot.res)
    par(mfrow=c(1,1))
    hist(zscores, breaks = 50, freq = F)
    ts = seq(-4,4,length.out = 1000)
    lines(ts, dnorm(ts), col='red')
    dev.off()


    ###############################################################
    # plot percentile curves with data points:
    t.all.plot = t.all[which(t.all<=x.plot.max & t.all>=x.plot.min)]
    y.all.plot = y.all[which(t.all<=x.plot.max & t.all>=x.plot.min)]
    
    n.y.ticks = round(max(y.all.plot) - min(y.all.plot))*n.y.ticks.mult
    par(bty = "l", mar = c(5, 4, 4, 3) + 0.1, xpd = FALSE, mfrow=c(1,1), lab = c(n.x.ticks,n.y.ticks,7))
    plot_percentiles(fit=fit, percentiles = percentiles, gender = wanted.gender.symbol,
                     x.col = x.col, y.col = y.col,
                     y.lim = c(min(y.all.plot), max(y.all.plot)), 
                     add = F, 
                     length.t.scale = length.t.scale, perc_line_width = perc_line_width,
                     perc_line_style = perc_line_style, perc_color = perc_color, xdist = xdist)
    points(t.all.plot, y.all.plot, col = point_color, pch = point_style)
    plot_percentiles(fit=fit, percentiles = percentiles, gender = wanted.gender.symbol,
                     x.col = x.col, y.col = y.col, 
                     y.lim = c(min(y.all.plot), max(y.all.plot)), 
                     add = T, 
                     length.t.scale = length.t.scale, perc_line_width = perc_line_width,
                     perc_line_style = perc_line_style, perc_color = perc_color, xdist = xdist)
    # save the plot:
    png(percentile_plot_name1, units='in', width=plot.width, height=plot.height, res=plot.res)
    par(bty = "l", mar = c(5, 4, 4, 3) + 0.1, xpd = FALSE, mfrow=c(1,1), lab = c(n.x.ticks,n.y.ticks,0))
    plot_percentiles(fit=fit, percentiles = percentiles, gender = wanted.gender.symbol,
                     x.col = x.col, y.col = y.col,
                     y.lim = c(min(y.all.plot), max(y.all.plot)), 
                     add = F, 
                     length.t.scale = length.t.scale, perc_line_width = perc_line_width,
                     perc_line_style = perc_line_style, perc_color = perc_color, xdist = xdist)
    points(t.all.plot, y.all.plot, col = point_color, pch = point_style)
    plot_percentiles(fit=fit, percentiles = percentiles, gender = wanted.gender.symbol,
                     x.col = x.col, y.col = y.col, 
                     y.lim = c(min(y.all.plot), max(y.all.plot)), 
                     add = T, 
                     length.t.scale = length.t.scale, perc_line_width = perc_line_width,
                     perc_line_style = perc_line_style, perc_color = perc_color, xdist = xdist)
    dev.off()

    # plot percentile curves without data points:
    # get ylim that is the same for both genders:
    ylim.dat = dat[,y.col]
    t.ylim = dat[,x.col]
    if (grepl('splitby', y.col, fixed = TRUE)){  # if the y.col has only one gender (since it was splitted), also get the other gender
      split = strsplit(y.col, '_')[[1]]
      str = split[1]
      for (ii in 2:(length(split)-1)){
        str = paste(str, split[ii], sep = '_')
      }
      dat.names = names(dat)
      w = which(grepl(str, dat.names, fixed=TRUE))
      if (length(w) > 0){
        ylim.dat = c()
        t.ylim = c()
      }
      for (ind in w){
        ylim.dat = c(ylim.dat, dat[,dat.names[ind]])
        t.ylim = c(t.ylim, dat[,x.col])
      }
      
      # # to get other gender column:
      # if (grepl('g0', y.col, fixed = TRUE)){
      #   y.col2 = gsub('g0', 'g1', y.col)
      # }
      # if (grepl('g1', y.col, fixed = TRUE)){
      #   y.col2 = gsub('g1', 'g0', y.col)
      # }
      # ylim.dat = c(ylim.dat, dat[,y.col2])
      # t.ylim = c(t.ylim, dat[,x.col])
    }
    which.to.keep = which(ylim.dat > 0)
    ylim.dat = ylim.dat[which.to.keep]
    t.ylim = t.ylim[which.to.keep]
    ylim.fit = lm(ylim.dat ~ t.ylim)
    ylim.std.residuals = rstandard(ylim.fit)
    which.not.outliers = which(abs(ylim.std.residuals) <= 4)
    ylim.dat = ylim.dat[which.not.outliers]
    ylim = c(min(ylim.dat), max(ylim.dat))
    
    # plot
    n.y.ticks = round(max(ylim) - min(ylim))*n.y.ticks.mult
    par(bty = "l", mar = c(5, 4, 4, 3) + 0.1, xpd = FALSE, mfrow=c(1,1), lab = c(n.x.ticks,n.y.ticks,0))
    plot_percentiles(fit=fit, percentiles = percentiles, gender = wanted.gender.symbol,
                     x.col = x.col, y.col = y.col, y.lim = ylim, add = F, 
                     length.t.scale = length.t.scale, perc_line_width = perc_line_width,
                     perc_line_style = perc_line_style, perc_color = perc_color, xdist = xdist)
    # save the plot:
    png(percentile_plot_name2, units='in', width=plot.width, height=plot.height, res=plot.res)
    par(bty = "l", mar = c(5, 4, 4, 3) + 0.1, xpd = FALSE, mfrow=c(1,1), lab = c(n.x.ticks,n.y.ticks,0))
    plot_percentiles(fit=fit, percentiles = percentiles, gender = wanted.gender.symbol,
                     x.col = x.col, y.col = y.col, y.lim = ylim, add = F, 
                     length.t.scale = length.t.scale, perc_line_width = perc_line_width,
                     perc_line_style = perc_line_style, perc_color = perc_color, xdist = xdist)
    dev.off()


    ###############################################################
    # check percentile curves, i.e. compute the percentage of data lying below the respective percentiles
    #check_perc = 100 * colMeans(depvar(fit, drop = TRUE) < fitted(fit, percentiles = percentiles))  # this is the version which works if there are no outliers
    check_perc = 100 * colMeans(y.all < fit@family@linkinv(eta = predict(fit, data.frame(t = t.all)),
                                                            extra = list(percentiles = percentiles)))
    check_perc = data.frame(t(as.matrix(check_perc)))
    check_perc


    ###############################################################
    # split up the wanted data by the wanted percentiles of the wanted splitting-data and save it:
    if (y.col %in% splitting.y.cols){
      which_percentile = rep(0, times=length(y.all))
      percentiles_for_data_split = sort(percentiles_for_data_split, decreasing = FALSE)
      for (i.perc in 1:length(percentiles_for_data_split)){
        percentile.curve = fit@family@linkinv(eta = predict(fit, data.frame(t = t.all)),
                                              extra = list(percentiles=percentiles_for_data_split[i.perc]))
        which_percentile[which(y.all > percentile.curve)] = i.perc
      }

      split_dat = data.frame(read_excel(path = data_file_name_with_cols_to_split))
      if (!(is.null(id.col))){
        split_ids = split_dat[,id.col]
        # loop over all columns which should be splitted up
        for (col.to.split in y.cols.to.split){
          y.to.split = split_dat[,col.to.split]
          # split the column and save all splits as new column to the data_file_name_with_cols_to_split,
          # if the column does not exist yet:
          for (i.perc in 0:length(percentiles_for_data_split)){
            # get the new column name:
            if (i.perc==0){
              new.column.name = paste(col.to.split, '_g', wanted.gender.symbol, '_splitby_', y.col,
                                      '_p', '0.',percentiles_for_data_split[i.perc+1], sep = '')
            } else {
              if (i.perc == length(percentiles_for_data_split)){
                new.column.name = paste(col.to.split, '_g', wanted.gender.symbol, '_splitby_', y.col,
                                        '_p', percentiles_for_data_split[i.perc], '.100', sep = '')
              }else {
                new.column.name = paste(col.to.split, '_g', wanted.gender.symbol, '_splitby_', y.col,
                                        '_p', percentiles_for_data_split[i.perc], '.',
                                        percentiles_for_data_split[i.perc+1], sep = '')
              }
            }

            # get the ids in the split:
            ids.in.split = ids.all[which(which_percentile == i.perc)]

            # check whether new column already exists
            if (!(new.column.name %in% names(split_dat))){
              new.col = rep(-1, length = length(split_ids))
              # go through all ids corresponding to the column that should be split:
              for (ii in 1:length(split_ids)){
                # check whether the id falls in the current split, if so, set the
                # column value of the new column to the value of the column that is split up:
                if (split_ids[ii] %in% ids.in.split){
                  new.col[ii] = y.to.split[ii]
                }
              }

              # save the column:
              split_dat[new.column.name] = new.col
              write_xlsx(split_dat, path = data_file_name_with_cols_to_split)
            }
          }
        }
      }

      # reload the dat data.frame, in case the new column was added and should be used later on:
      # load the data:
      dat = data.frame(read_excel(path = data_file_name))
      
      # reload the data
      y.cols_ = intersect(c(y.cols, x.col, g.col, id.col), names(dat))
      dat =  dat[,y.cols_]
      dat = na.omit(dat)
      # extract the data with wanted age (min.x, max.x)
      dat = dat[which(dat[,x.col] >= min.x),]
      dat = dat[which(dat[,x.col] <= max.x),]

      # also save the splitting column with the different percentiles the data-points belong to
      if (!(is.null(id.col))){
        out_dat3 = data.frame(id=ids.all, x = t.all, y=y.all, which_percentile=which_percentile)
        names(out_dat3) = c(id.col, x.col, y.col, 'which_percentile')
      } else{
        out_dat3 = data.frame(id=1:length(y.all), x = t.all, y=y.all, which_percentile=which_percentile)
        names(out_dat3) = c('id', x.col, y.col, 'which_percentile')
      }
      write_xlsx(out_dat3, path = out_file_data_split)

      # plot the points with different colors depending on percentile and the percentiles for splitting
      # the data:
      qtplot(fit, percentiles = percentiles_for_data_split,
             main = if(wanted.gender.symbol==0){'Male'}else{'Female'},
             xlab = get.pretty.names(x.col), label = T,
             xlim = c(min(t.scale), max(t.scale)+xdist), las = 1, ylab = get.pretty.names(y.col),
             llwd = perc_line_width,
             lcol = 1, llty = 1, tcol = 1, y = FALSE)
      points(t.all, y.all, col = (which_percentile+2), pch=20)
      qtplot(fit, percentiles = percentiles_for_data_split,
             main = if(wanted.gender.symbol==0){'Male'}else{'Female'},
             xlab = get.pretty.names(x.col), label = T,
             xlim = c(min(t.scale), max(t.scale)+xdist), las = 1, ylab = get.pretty.names(y.col),
             llwd = perc_line_width,
             lcol = 1, llty = 1, tcol = 1, y = FALSE, add=TRUE)
      # save the plots
      png(percentile_plot_name3, units='in', width=plot.width, height=plot.height, res=plot.res)
      par(bty = "l", mar = c(5, 4, 4, 3) + 0.1, xpd = FALSE, mfrow=c(1,1))
      qtplot(fit, percentiles = percentiles_for_data_split,
             main = if(wanted.gender.symbol==0){'Male'}else{'Female'},
             xlab = get.pretty.names(x.col), label = T,
             xlim = c(min(t.scale), max(t.scale)+xdist), las = 1, ylab = get.pretty.names(y.col),
             llwd = perc_line_width,
             lcol = 1, llty = 1, tcol = 1, y = FALSE)
      points(t.all, y.all, col = (which_percentile+2), pch=point_style)
      qtplot(fit, percentiles = percentiles_for_data_split,
             main = if(wanted.gender.symbol==0){'Male'}else{'Female'},
             xlab = get.pretty.names(x.col), label = T,
             xlim = c(min(t.scale), max(t.scale)+xdist), las = 1, ylab = get.pretty.names(y.col),
             llwd = perc_line_width,
             lcol = 1, llty = 1, tcol = 1, y = FALSE, add=TRUE)
      dev.off()
    }



    ###############################################################
    # save overview over saved model
    if (file.exists(save_output_file_name)){
      saved_dat = data.frame(read_excel(path = save_output_file_name))
    } else {
      saved_dat = data.frame()
    }
    append = data.frame(cbind(data.frame(parameter = y.col, gender=wanted.gender.symbol), dist,
                              check_perc, data.frame(number_samples=length(y.all), number_outliers=number.outliers,
                                                     outlier_ids=str.outlier.ids,
                                                     file_name=data_file_name)))
    saved_dat = rbind(saved_dat, append)
    row.names(saved_dat) = NULL
    write_xlsx(saved_dat, path = save_output_file_name)

  }
}




###############################################################
#### get number of samples
###############################################################
for (y.col in y.cols){
  # loop over all genders
  for (wanted.gender.symbol in wanted.gender.symbols){
    # get data:
    y = dat[,y.col]
    t = dat[,x.col]
    if (is.null(wanted.gender.symbol)){
      g.ids = 1:length(t)
    } else {
      g = dat[,g.col]
      g.ids = which(g == wanted.gender.symbol)
    }
    y = y[g.ids]
    t = t[g.ids]
    
    if (!(is.null(id.col))){
      ids = dat[g.ids, id.col]
    }
    
    # exclude datapoints with values <= 0:
    which.to.keep = which(y > 0)
    if (length(which.to.keep) <= 0){  # check whether data is left, otherwise go to next iteration (this can happen if a column has only entries for men but the current wanted.gender is female)
      print('no data left after excluding wrong values => skipping')
      next
    }
    y = y[which.to.keep]
    t = t[which.to.keep]
    if (!(is.null(id.col))){
      ids = ids[which.to.keep]
    }
    
    print(paste(y.col, 'gender:', wanted.gender.symbol, 'number samples:', length(y)))
  }
}





###############################################################
### combine columns of 2 files by ID ###
###############################################################
# enter here the wanted parameters:
file_name1 = 'DXA_Children_data.xlsx'  # the file that is appended with new columns (file1)
file_name2 = 'NeueParameter_100519.xlsx'  # the file with the new columns (file2)
file_name_to_save = 'appended_file.xlsx'  # the name of the file where the appended file is saved
id.col1 = 'ID'  # the name of the id-column in file1
id.col2 = 'ID'  # the name of the id-column in file2
cols.to.append = c('FM_trunk_quotient_limb', 'percent_FM_trunk_quotient_limb')  # the columns in file2 which are appended

##############################################################
# loop to append the file:
dat1 = data.frame(read_excel(path = file_name1))
dat2 = data.frame(read_excel(path = file_name2))

ids1 = dat1[,id.col1]
ids2 = dat2[,id.col2]

count = 0
ids.no.match = c()
for (i in 1:length(ids2)){
  w = which(ids1 == ids2[i])
  if (length(w) != 1){
    count = count + 1
    ids.no.match = c(ids.no.match, ids2[i])
    # print(paste(ids2[i], ' matches: ', length(w), w, sep = ''))
  } else {
    dat1[w, cols.to.append] = dat2[i, cols.to.append]
  }
}
count
ids.no.match
write_xlsx(dat1, path = file_name_to_save)

# count2 = 0
# count3 = 0
# for (i in 1:dim(dat1)[1]){
#   if (is.na(dat1[i,'FM_trunk_quotient_limb'])){
#     count2 = count2 + 1
#   }
#   if (is.na(dat1[i,'FM'])){
#     count3 = count3 + 1
#   }
#   
# }
# count2
# count3





###############################################################
### functions ###
###############################################################

###############################################################
# function to plot new data-points in the percentile curves
plot_new_data = function(x.vals, y.vals, saved_model_file = saved_model_name,
                         gender = 0,
                         plot_file_name = 'plot_new.png', color_data_points='black', 
                         percentile_color=perc_color){
  # x.vals should contain the predictor variable (e.g. age)
  # y.vals should contain the measured response variable (e.g. BMI)
  # x.vals[i] and y.vals[i] should be a pair

  # load LMS-model
  load(file = saved_model_file)

  png(plot_file_name, units='in', width=plot.height, height=plot.width, res=plot.res)
  par(bty = "l", mar = c(5, 4, 4, 3) + 0.1, xpd = TRUE, mfrow=c(1,1))
  plot_percentiles(fit=fit, percentiles = percentiles, gender = gender,
                   x.col = x.col, y.col = y.col, y.lim = y.lim, add = F, 
                   length.t.scale = length.t.scale, perc_line_width = perc_line_width,
                   perc_line_style = perc_line_style, perc_color = perc_color, xdist = xdist)
  points(x.vals, y.vals, col=color_data_points)
  dev.off()
}

plot_new_data(c(25, 30, 65, 40), c(10, 20, 30, 40), gender=0)



###############################################################
# function to compute z-score for new data-points, using the true params from the saved model
zscore = function(x.vals, y.vals, saved_model_file = saved_model_name){
  # x.vals should contain the predictor variable (e.g. age)
  # y.vals should contain the measured response variable (e.g. BMI)
  # x.vals[i] and y.vals[i] should be a pair

  # load LMS-model:
  load(file = saved_model_file)

  # compute z-scores and the cdfs:
  cdf = cdf(fit, newdata = data.frame(t = x.vals, y = y.vals))
  z = qnorm(cdf)

  # the following is another way to compute it, but only works for lambda!=0:
  # params = predict(fit, newdata = data.frame(t = x.vals))
  # z = ((y.vals/params[,2])**(params[,1]) - 1)/(params[,1]* exp(params[,3]))
  # cdf = pnorm(z, lower.tail = T)

  out = data.frame(zscore = z, cdf = cdf)
  return(out)
}

# maximum deviation of cdf values when using t
#z = zscore(t, y)
#max(abs(z[,2] - cdf(fit)))

# some new values
#z = zscore(50, 27)



###############################################################
# function to compute z-score for new data-points, using the table of LMS params
zscore1 = function(x.vals, y.vals, LMS_param_file = out_file_LMS_name){
  # x.vals should contain the predictor variable (e.g. age)
  # y.vals should contain the measured response variable (e.g. BMI)
  # x.vals[i] and y.vals[i] should be a pair

  out = data.frame(zscore = numeric(length(x.vals)), cdf = numeric(length(x.vals)))

  # load file
  LMS = data.frame(read_excel(path = LMS_param_file))[,1:4]
  for (i in 1:length(x.vals)){
    j = which.min(abs(LMS[,1] - x.vals[i]))
    if (abs(LMS[j,2]) > tol){
      z = ((y.vals[i]/LMS[j,3])**(LMS[j,2]) - 1)/(LMS[j,2]* LMS[j,4])
    } else {
      z = log(y.vals[i]/LMS[j,3])/LMS[j,4]
    }
    cdf = pnorm(z, lower.tail = T)
    out[i,] = c(z, cdf)
  }

  return(out)
}

# maximum deviation of the computed cdf's when using the LMS-params of the table instead of the true values
#z = zscore1(t, y)
#max(abs(z[,2] - cdf(fit)))

# plot of the histograms of z-scores computed with LMS-params of table
#hist(z[,1], breaks = 50, freq = F)
#ts = seq(-4,4,length.out = 1000)
#lines(ts, dnorm(ts), col='red')

###################################descriptive statistics
######split according to age groups


######################################calculate mean
mean(dat[,'FMI']) #enter wanted parameter
mean(y)
write_xlsx(mean, path = tempfile(fileext = 'descriptive_data.xlsx'), col_names = TRUE, format_headers = TRUE)
