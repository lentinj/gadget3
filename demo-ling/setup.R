library(mfdb)
library(tidyverse)
library(gadget3)

year_range <- 1982:lubridate::year(Sys.Date())
read_data <- FALSE

reitmapping <-
  read.table(
    system.file("demo-data", "reitmapping.tsv", package="mfdb"),
    header=TRUE,
    as.is=TRUE)

defaults <- list(
  area = mfdb::mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
  timestep = mfdb::mfdb_timestep_quarterly,
  year = year_range,
  species = 'LIN')

# Map area names to integer area numbers (in this case only "1" ==> 1, but could be anything)
areas <- structure(
    seq_along(defaults$area),
    names = names(defaults$area))

##### Configure model stocks/fleets ###########################################

## modelled stocks
ling_imm <-
  g3_stock('ling_imm', seq(4, 156, 4)) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(3, 10)

ling_mat <-
  g3_stock('ling_mat', seq(20, 156, 4)) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(5, 15)

## fleets
lln <-
  g3_fleet('lln') %>%
  g3s_livesonareas(areas[c('1')])

bmt <-
  g3_fleet('bmt') %>%
  g3s_livesonareas(areas[c('1')])

gil <-
  g3_fleet('gil') %>%
  g3s_livesonareas(areas[c('1')])

foreign <-
  g3_fleet('foreign') %>%
  g3s_livesonareas(areas[c('1')])

igfs <-
  g3_fleet('igfs') %>%
  g3s_livesonareas(areas[c('1')])

# Load required data objects
if(read_data){
  mdb<-mfdb('Iceland', db_params = list(host = 'mfdb.hafro.is'))
  source('demo-ling/setup-fleet-data.R')
  source('demo-ling/setup-catchdistribution.R')
  source('demo-ling/setup-indices.R')
  source('demo-ling/setup-initial_parameters.R')
} else {
  fs::dir_ls('demo-ling/data') %>%
    stringr::str_subset('.Rdata') %>%
    lapply(load,.GlobalEnv)
}

##### Configure model actions #################################################

# Timekeeping for the model, i.e. how long we run for
time_actions <- list(
    g3a_time(start_year = min(defaults$year),
        end_year = max(defaults$year),
        defaults$timestep),
    list())

source('demo-ling/setup-fleets.R')  # Generates fleet_actions
source('demo-ling/setup-model.R')  # Generates ling_mat_actions / ling_imm_actions
source('demo-ling/setup-likelihood.R')  # Generates ling_likelihood_actions

## set up reporting:

# Disaggregated report storage for ling_imm (we store with same age/step/length as ling itself)
imm_report <- g3s_clone(ling_imm, 'imm_report') %>%
  g3s_time(year = local(year_range), step = 1:4)

# Disaggregated report storage for ling_mat (we store with same age/step/length as ling itself)
mat_report <- g3s_clone(ling_mat, 'mat_report') %>%
  g3s_time(year = local(year_range), step = 1:4)

report_actions <- list(
       # Report ling numbers
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__num)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__num)),
       # Report ling mean weight
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__wgt)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__wgt)),
       # Report ling biomass caught by igfs
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__igfs)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__igfs)),
       # Report ling biomass caught by bmt
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__bmt)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__bmt)),
       # Report ling biomass caught by lln
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__lln)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__lln)),
       # Report ling biomass caught by gil
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__gil)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__gil)),
       # Report ling biomass caught by foreign
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__foreign)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__foreign)),
       ## recruitment
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__renewalnum)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__renewalwgt)),

       # Report ling suitability caught by igfs
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__suit_igfs)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__suit_igfs)))

##### Run r-based model #######################################################

# Turn actions into an R function
ling_model <- g3_to_r(c(
  ling_mat_actions,
  ling_imm_actions,
  fleet_actions,
  ling_likelihood_actions,
  report_actions,
  time_actions), strict = TRUE)

# Get pararameter template attached to function, fill it in
ling_param <- attr(ling_model, 'parameter_template')
ling_param[["lingimm.walpha"]] <- 2.27567436711055e-06
ling_param[["lingimm.wbeta"]] <- 3.20200445996187
ling_param[["ling.bbin"]] <- 6
#ling_param[["ling.rec.1982"]] <- 1
ling_param[["lingmat.walpha"]] <- 2.27567436711055e-06
ling_param[["lingmat.wbeta"]] <- 3.20200445996187
ling_param[["ling_si_beta1"]] <- 1
ling_param[["ling_si_beta2"]] <- 1
ling_param[grepl('^lingimm\\.M\\.', names(ling_param))] <- 0.15
ling_param[grepl('^lingmat\\.M\\.', names(ling_param))] <- 0.15
ling_param[grepl('^ling\\.rec\\.', names(ling_param))] <- 1
ling_param[grepl('^lingimm\\.init\\.sd', names(ling_param))] <- init.sigma$ms[3:10]
ling_param[grepl('^lingmat\\.init\\.sd', names(ling_param))] <- init.sigma$ms[5:15]

ling_param[grepl('si_igfs_si.+weight$',names(ling_param))] <- 1


## Old weights

ling_param['ldist_lln_weight'] <- 3331
ling_param['aldist_lln_weight'] <- 2512
ling_param['ldist_bmt_weight'] <- 1247
ling_param['aldist_bmt_weight'] <- 1515
ling_param['ldist_gil_weight'] <- 781
ling_param['aldist_gil_weight'] <- 719
ling_param['ldist_igfs_weight'] <- 6869
ling_param['aldist_igfs_weight'] <- 11087
ling_param['matp_igfs_weight'] <- 9
ling_param['si_igfs_si1_weight'] <- 40
ling_param['si_igfs_si2a_weight'] <- 8
ling_param['si_igfs_si2b_weight'] <- 36
ling_param['si_igfs_si3a_weight'] <- 19

ling_param['si_igfs_si3b_weight'] <- 16

ling_param['si_igfs_si3c_weight'] <- 13
ling_param['si_igfs_si3d_weight'] <- 14.5
#




# You can edit the model code with:
#ling_model <- edit(ling_model)

# Run model with params above
result <- ling_model(ling_param)
result[[1]]
# List all available reports
print(names(attributes(result)))

##### Run TMB-based model #####################################################

# Turn actions into C++ objective function code
tmb_ling <- g3_to_tmb(c(
  ling_mat_actions,
  ling_imm_actions,
  fleet_actions,
  ling_likelihood_actions,
  time_actions))

# Get the parameter template to fill in
tmb_param <- attr(tmb_ling, 'parameter_template')

# Copy initial guesses from R model
tmb_param$value <- I(ling_param[rownames(tmb_param)])

# Configure lower and upper bounds
tmb_param$lower <- vapply(tmb_param$value, function (x) 0.5 * x[[1]], numeric(1))
tmb_param$upper <- vapply(tmb_param$value, function (x) 2 * x[[1]], numeric(1))
#tmb_param[grepl('^ling\\.rec\\.', rownames(tmb_param)),]$lower <- 0.0001
#tmb_param[grepl('^ling\\.rec\\.', rownames(tmb_param)),]$upper <- 1e3
tmb_param[grepl('^ling\\.init\\.', rownames(tmb_param)),]$lower <- 0.0001
tmb_param[grepl('^ling\\.init\\.', rownames(tmb_param)),]$upper <- 1e3
tmb_param[grepl('^ling\\.init\\.', rownames(tmb_param)),]$lower <- 0.0001
tmb_param[grepl('^ling\\.init\\.', rownames(tmb_param)),]$upper <- 1e3

# Disable optimisation for some parameters, to make life easier
tmb_param[c('lingimm.walpha',
            'lingimm.wbeta',
            'lingmat.walpha',
            #"ling_si_alpha1",
            #"ling_si_beta1" ,
            #'ling_si_alpha2',
            #"ling_si_beta2" ,
            #'ling_si_alpha3',
            #'ling_si_alpha4',
            #'ling_si_alpha5',
            #'ling_si_alpha6',
            #'ling_si_alpha7',
            'lingmat.wbeta'),]$optimise <- FALSE
tmb_param[grepl('^lingimm\\.M\\.', rownames(tmb_param)),]$optimise <- FALSE
tmb_param[grepl('^lingmat\\.M\\.', rownames(tmb_param)),]$optimise <- FALSE
tmb_param[grepl('^lingimm\\.init\\.sd', rownames(tmb_param)),]$optimise <- FALSE
tmb_param[grepl('^lingmat\\.init\\.sd', rownames(tmb_param)),]$optimise <- FALSE

# Compile and generate TMB ADFun (see ?TMB::MakeADFun)
ling_model_tmb <- g3_tmb_adfun(tmb_ling,tmb_param)
# writeLines(TMB::gdbsource(g3_tmb_adfun(tmb_ling, tmb_param, compile_flags = "-g", output_script = TRUE)))
# Run model once, using g3_tmb_par to reshape tmb_param into param vector.
# Will return nll
ling_model_tmb$fn(g3_tmb_par(tmb_param))

# Run model once, returning model report
ling_model_tmb$report(g3_tmb_par(tmb_param))

# Run model through R optimiser, using bounds set in tmb_param


# cl <- parallel::makeCluster(spec=parallel::detectCores(), outfile="")
# parallel::setDefaultCluster(cl=cl)
#
# optimParallel::optimParallel(par=g3_tmb_par(tmb_param),
#                              fn=ling_model_tmb$fn,
#                              gr=ling_model_tmb$gr),
#                              control = list(trace = 2, fnscale = 1e3,maxit = 200, reltol = .Machine$double.eps),
#                              parallel=list(loginfo=TRUE))
#tmp <- g3_tmb_par(tmb_param)

#tmp <- set_names(rnorm(length(tmp), sd = 3),names(tmp))

fit.opt <- optim(#g3_tmb_par(tmb_param),
                 ling_model_tmb$fn,
                 ling_model_tmb$gr,
                 method = 'BFGS',
                 control = list(trace = 2,maxit = 1000, reltol = .Machine$double.eps^2))
if(FALSE){
fit.nelder <- optim(fit.opt$par,
                    ling_model_tmb$fn,
                    #ling_model_tmb$gr,
                    method = "Nelder-Mead",
                    control = list(trace = 2,fnscale = 1e3, maxit = 1000))

# fit <- nlminb(g3_tmb_par(tmb_param),
#               ling_model_tmb$fn, ling_model_tmb$gr,
#              # upper = g3_tmb_upper(tmb_param),
#             #  lower = g3_tmb_lower(tmb_param),
#               control = list(trace = TRUE, eval.max=200, iter.max=100))



fit.opt2 <- optim(fit.nelder$par,
                 ling_model_tmb$fn,
                 ling_model_tmb$gr,
                 method = 'BFGS',
                 control = list(trace = 2,maxit = 1000, reltol = .Machine$double.eps^2))


fit.opt3 <- optim(fit.opt2$par,
                  ling_model_tmb$fn,
                  ling_model_tmb$gr,
                  method = 'BFGS',
                  control = list(trace = 2, maxit = 1000, reltol = .Machine$double.eps))

 fit.opt4 <- nlminb(fit.opt3$par,
                 ling_model_tmb$fn,
                 ling_model_tmb$gr,
                 method = 'BFGS',
                 control = list(trace = TRUE, eval.max = 200, rel.tol = .Machine$double.eps))

}
#
# fit.nlminb <- nlminb(fit.opt$par,
#               ling_model_tmb$fn, ling_model_tmb$gr,
#               # upper = g3_tmb_upper(tmb_param),
#               #  lower = g3_tmb_lower(tmb_param),
#               control = list(trace = TRUE, eval.max=200, iter.max=100))
# #
#
# fit.sann <- optim(fit.opt$par,
#                   ling_model_tmb$fn, method = 'SANN',
#                   control = list(trace = 1))
#
# fit.hjkb <- dfoptim::hjkb(fit.opt$par,#g3_tmb_par(tmb_param),
#               ling_model_tmb$fn)#,
#             #ling_model_tmb$gr,
#                upper = g3_tmb_upper(tmb_param),
#                lower = g3_tmb_lower(tmb_param)),
#               control = list(trace = TRUE, eval.max=2000, iter.max=1000))
#
#
#
#
# # Turn fit parameters back into list, which can be used in table or R function
# param_list <- g3_tmb_relist(tmb_param, fit$par)
