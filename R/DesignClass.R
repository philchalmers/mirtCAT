Design <- setClass(Class = "Design",
                   slots = c(method = 'character',
                             criteria = 'character',
                             criteria_estimator = 'character',
                             classify = 'numeric',
                             classify_alpha = 'numeric',
                             delta_thetas = 'numeric',
                             min_SEM = 'numeric',
                             met_SEM = 'logical',
                             met_delta_thetas = 'logical',
                             met_classify = 'logical',
                             min_items = 'integer',
                             max_items = 'integer',
                             items_not_scored = 'integer',
                             stop_now = 'logical',
                             exposure = 'numeric',
                             exposure_type = 'character',
                             weights = 'numeric',
                             KL_delta = 'numeric',
                             start_item = 'integer',
                             preCAT_max_items = 'integer',
                             preCAT_min_items = 'integer',
                             preCAT_criteria = 'character',
                             preCAT_method = 'character',
                             preCAT_response_var = 'logical',
                             CAT_criteria = 'character',
                             CAT_method = 'character',
                             max_time = 'numeric',
                             use_content = 'logical',
                             content = 'factor',
                             content_prop = 'numeric',
                             content_prop_empirical = 'numeric',
                             constraints = 'list',
                             excluded = 'integer',
                             customNextItem = 'function',
                             test_properties = 'data.frame',
                             person_properties = 'data.frame',
                             Update.thetas = 'function',
                             constr_fun = 'function',
                             stage = 'integer'),
                   validity = function(object) return(TRUE)
)

setMethod("initialize", signature(.Object = "Design"),
          function(.Object, method, criteria, nfact, design,
                   start_item, preCAT, nitems){
              Update_thetas <- function(design, person, test){
                  responses2 <- person$responses
                  responses2[design@items_not_scored] <- NA
                  if(person$score){
                      method <- design@method
                      if(last_item(person$items_answered) %in% design@items_not_scored)
                          method <- 'fixed'
                      if(method == 'ML')
                          if(length(unique(na.omit(responses2))) < 2L) 
                              method <- 'MAP'
                      if(method != 'fixed'){
                          if(all(is.na(responses2))){
                              person$Update_thetas(person$thetas,
                                                   person$thetas_SE_history[nrow(person$thetas_SE_history),])
                          } else {
                              suppressWarnings(tmp <- fscores(test@mo, method=method, response.pattern=responses2,
                                                              theta_lim=test@fscores_args$theta_lim,
                                                              MI = test@fscores_args$MI, quadpts = test@quadpts, 
                                                              mean = test@fscores_args$mean, cov = test@fscores_args$cov,
                                                              QMC=test@fscores_args$QMC, max_theta=test@fscores_args$max_theta,
                                                              custom_den=test@fscores_args$custom_den,
                                                              start = person$thetas))
                              if(all(is.finite(tmp[,paste0('F', 1L:test@nfact), drop=FALSE]))){
                                  person$Update_thetas(tmp[,paste0('F', 1L:test@nfact), drop=FALSE],
                                                       tmp[,paste0('SE_F', 1L:test@nfact), drop=FALSE])
                              } else {
                                  person$Update_thetas(person$thetas,
                                                       person$thetas_SE_history[nrow(person$thetas_SE_history),])
                              }
                          }
                      } else {
                          person$Update_thetas(person$thetas,
                                               person$thetas_SE_history[nrow(person$thetas_SE_history),])
                      }
                  }
                  invisible()
              }
              
              .Object@method <- method
              .Object@criteria <- criteria
              .Object@criteria_estimator <- 'MAP'
              if(criteria %in% c('Drule', 'Trule', 'Erule', 'Wrule', 'Arule')){
                  .Object@criteria_estimator <- 'ML'
              } else if(criteria %in% c('DPrule', 'TPrule', 'EPrule', 'WPrule',
                                        'MEPV', 'APrule')){
                  .Object@criteria_estimator <- 'MAP'
              }
              .Object@CAT_criteria <- criteria
              .Object@CAT_method <- method
              .Object@start_item <- as.integer(start_item)
              if(!is.nan(start_item) && .Object@start_item != 1 && criteria == 'seq' && 
                 is.null(design$customNextItem))
                  stop('start_item must equal 1 with seq criteria', call.=FALSE)
              if(nfact > 1L && 
                     !any(criteria %in% c('Drule', 'Trule', 'Wrule', 'KL', 'KLn',
                                          'Erule', 'seq', 'random', 'Arule', 'APrule',
                                          'DPrule', 'TPrule', 'EPrule', 'WPrule')))
                  stop('Selected criteria not valid for multidimensional tests', call.=FALSE)
              .Object@min_SEM <- .3
              .Object@met_SEM <- rep(FALSE, nfact)
              .Object@met_delta_thetas <- rep(FALSE, nfact)
              .Object@met_classify <- rep(FALSE, nfact)
              .Object@weights <- rep(1, nfact)
              .Object@min_items <- 1L
              .Object@max_items <- nitems
              .Object@max_time <- Inf
              .Object@stop_now <- FALSE
              .Object@delta_thetas <- rep(0, nfact)
              .Object@preCAT_min_items <- 0L
              .Object@preCAT_max_items <- 0L
              .Object@preCAT_response_var <- FALSE
              .Object@KL_delta <- 0.1
              .Object@use_content <- FALSE
              .Object@content_prop_empirical <- 1
              .Object@classify <- NaN
              .Object@classify_alpha <- .05
              .Object@exposure <- rep(1, nitems)
              .Object@exposure_type <- 'none'
              .Object@constraints <- list()
              .Object@items_not_scored <- integer(0L)
              .Object@test_properties <- data.frame()
              .Object@person_properties <- data.frame()
              .Object@Update.thetas <- Update_thetas
              .Object@stage <- 2L
              if(length(design)){
                  dnames <- names(design)
                  gnames <- c('min_SEM', 'thetas.start', 'min_items', 'max_items', 'quadpts', 'max_time',
                              'theta_range', 'weights', 'KL_delta', 'content', 'content_prop',
                              'classify', 'classify_CI', 'exposure', 'delta_thetas', 'constraints',
                              'customNextItem', 'test_properties', 'person_properties', 'constr_fun',
                              'customUpdateThetas')
                  if(!all(dnames %in% gnames))
                      stop('The following inputs to design are invalid: ',
                           paste0(dnames[!(dnames %in% gnames)], ' '), call.=FALSE)
                  if(!is.null(design$content)){
                      .Object@use_content <- TRUE
                      .Object@content <- factor(design$content)
                      if(!mirt:::closeEnough(sum(design$content_prop)-1, -1e-6, 1e-6))
                          stop('content_prop does not sum to 1', call.=FALSE)
                      tmp <- design$content_prop
                      tmp <- tmp[match(names(table(.Object@content)), names(tmp))]
                      .Object@content_prop <- tmp
                      tmp[1L:length(tmp)] <- 0
                      .Object@content_prop_empirical <- tmp
                  }
                  if(!is.null(design$KL_delta))
                      .Object@KL_delta <- design$KL_delta
                  if(!is.null(design$weights)) 
                      .Object@weights <- design$weights
                  if(!is.null(design$min_SEM))
                      .Object@min_SEM <- design$min_SEM
                  if(!is.null(design$delta_thetas))
                      .Object@delta_thetas <- design$delta_thetas
                  if(!is.null(design$min_items))
                      .Object@min_items <- as.integer(design$min_items)
                  if(!is.null(design$max_items))
                      .Object@max_items <- as.integer(design$max_items)
                  if(!is.null(design$classify))
                      .Object@classify <- design$classify
                  if(!is.null(design$constr_fun))
                      .Object@constr_fun <- design$constr_fun
                  if(!is.null(design$max_time))
                      .Object@max_time <- design$max_time
                  if(!is.null(design$customUpdateThetas))
                      .Object@Update.thetas <- design$customUpdateThetas
                  if(!is.null(design$test_properties)){
                      .Object@test_properties <- design$test_properties
                      if(nrow(.Object@test_properties) != nitems)
                          stop('test_properties input does not have the same number of rows as items', 
                               call.=FALSE)
                  }
                  if(!is.null(design$person_properties))
                      .Object@person_properties <- design$person_properties
                  if(!is.null(design$classify_CI)){
                      if(design$classify_CI > 1 || design$classify_CI < 0)
                          stop('classify_CI criteria must be between 0 and 1', call.=FALSE)
                      .Object@classify_alpha <- (1 - design$classify_CI)/2
                  }
                  if(!is.null(design$exposure)){
                      if(length(design$exposure) != nitems)
                          stop('exposure vector length not equal to number of items', call.=FALSE)
                      exposure_type <- ifelse(all(design$exposure <= 1 && design$exposure >= 0), 
                                              'SH', 'sample')
                      exposure <- if(exposure_type == 'SH') design$exposure 
                      else as.integer(design$exposure)
                      if(exposure_type == 'sample')
                          if(!all(exposure == design$exposure & exposure >= 1)) 
                              stop('sampling exposure method does not contain integer 
                                   values greater than or equal to 1', call.=FALSE)
                      .Object@exposure <- exposure
                      .Object@exposure_type <- exposure_type
                  }
                  if(!is.null(design$constraints))
                      if(!all(names(design$constraints) %in% 
                              c("independent", "unordered", "ordered", "not_scored", "excluded")))
                          stop('Named element in constraints list not suppored', call.=FALSE)
                  if(!is.null(design$constraints$excluded)){
                      .Object@excluded <- design$constraints$excluded
                      design$constraints$excluded <- NULL
                  }
                  if(!is.null(design$constraints)){
                      if(any(names(design$constraints) == 'not_scored')){
                          .Object@items_not_scored <- 
                              as.integer(design$constraints$not_scored)
                          design$constraints$not_scored <- NULL
                      }
                      nms <- names(design$constraints)
                      design$constraints[nms == 'unordered'] <- 
                          lapply(design$constraints[nms == 'unordered'], 
                             function(x) sample(x, length(x)))
                      nms[nms == 'unordered'] <- 'ordered'
                      names(design$constraints) <- nms
                      .Object@constraints <- design$constraints
                      for(i in seq_len(length(start_item))){
                          if(is.nan(start_item[i])) next
                          pick <- sapply(design$constraints, 
                                         function(x, si) any(x == si),
                                         si=start_item[i])
                          if(any(pick) && names(pick)[pick] == 'independent')
                              stop('The first item can not be used in an \'independent\' constraint. 
                              Consider removing the items that will not be used from the test.', 
                                   call.=FALSE)
                      }
                  }
                  if(!is.null(design$customNextItem)){
                      .Object@customNextItem <- design$customNextItem
                      .Object@CAT_criteria <- 'custom'
                      .Object@criteria <- 'custom'
                  }
              }
              if(.Object@use_content && criteria == 'seq')
                  stop('content designs are not supported for seq criteria', call.=FALSE)
              if(length(.Object@min_SEM) != 1L && length(.Object@min_SEM) != nfact)
                  stop('min_SEM criteria is not a suitable length', call.=FALSE)
              if(length(preCAT)){
                  if(!is.null(design$customNextItem))
                      stop('preCAT input not supported when customNextItem function supplied',
                           call.=FALSE)
                  dnames <- names(preCAT)
                  gnames <- c('min_items', 'max_items', 'criteria', 'method', 'response_variance')
                  if(!all(dnames %in% gnames))
                      stop('The following inputs to preCAT are invalid: ',
                           paste0(dnames[!(dnames %in% gnames)], ' '), call.=FALSE)
                  if(is.null(preCAT$max_items))
                      stop('preCAT max_items must be specified', call.=FALSE)
                  else .Object@preCAT_max_items <- as.integer(preCAT$max_items)
                  if(!is.null(preCAT$min_items))
                      .Object@preCAT_min_items <- as.integer(preCAT$min_items)
                  if(is.null(preCAT$method))
                      .Object@preCAT_method <- 'MAP'
                  else .Object@preCAT_method <- preCAT$method
                  if(is.null(preCAT$criteria))
                      .Object@preCAT_criteria <- 'random'
                  else .Object@preCAT_criteria <- preCAT$criteria
                  if(!is.null(preCAT$response_variance)) 
                      .Object@preCAT_response_var <- preCAT$response_variance
                  if(.Object@preCAT_min_items > .Object@preCAT_max_items)
                      stop('preCAT_min_items > preCAT_max_items', call.=FALSE)
                  .Object@criteria <- .Object@preCAT_criteria
                  .Object@method <- .Object@preCAT_method
                  .Object@stage <- 1L
              }
              .Object@min_items <- .Object@min_items + .Object@preCAT_min_items
              .Object
          }
)

setGeneric('Update.stop_now', function(.Object, ...) standardGeneric("Update.stop_now"))

setMethod("Update.stop_now", signature(.Object = "Design"),
          function(.Object, person){
              nanswered <- sum(!is.na(person$items_answered))
              if(person$score){
                  if(nanswered >= .Object@min_items){
                      diff <- person$thetas_SE_history[nrow(person$thetas_SE_history), ]
                      if(!is.nan(.Object@classify[1L])){
                          z <- -abs(person$thetas - .Object@classify) / diff
                          .Object@met_classify <- as.vector(z < qnorm(.Object@classify_alpha))
                          if(.Object@stage > 1L && all(.Object@met_classify)) 
                              .Object@stop_now <- TRUE
                      } else {
                          .Object@met_SEM <- diff < .Object@min_SEM
                          if(.Object@stage > 1L && !any(is.nan(diff)) && all(.Object@met_SEM)) 
                              .Object@stop_now <- TRUE
                      }
                      diff2 <- abs(person$thetas_history[nrow(person$thetas_history),] - 
                                       person$thetas_history[nrow(person$thetas_history)-1L,])
                      .Object@met_delta_thetas <- as.vector(diff2 < .Object@delta_thetas)
                      if(.Object@stage > 1L && all(.Object@met_delta_thetas)) 
                          .Object@stop_now <- TRUE
                  }
              }
              if(.Object@stage > 1L && nanswered == .Object@max_items) 
                  .Object@stop_now <- TRUE
              if(.Object@max_time <= sum(person$item_time)) .Object@stop_now <- TRUE
              .Object
          }
)

setGeneric('Next.stage', function(.Object, ...) standardGeneric("Next.stage"))

setMethod("Next.stage", signature(.Object = "Design"),
          function(.Object, person, test, item){
              if(.Object@stage < 2L){
                  if(item >= .Object@preCAT_min_items){
                      if(.Object@preCAT_response_var){
                          suppressWarnings(tmp <- try(fscores(test@mo, method='ML', 
                                                              max_theta=Inf,
                                                              start=person$thetas,
                                                              response.pattern=person$responses), 
                                                      silent=TRUE))
                          if(all(is.finite(na.omit(tmp[1L, ])))){
                              .Object@criteria <- .Object@CAT_criteria
                              .Object@method <- .Object@CAT_method
                              .Object@min_items <- .Object@min_items + item
                              .Object@max_items <- min(c(.Object@max_items + item, test@length))
                              .Object@stage <- 2L
                              return(.Object)
                          }
                      }
                      if(item == .Object@preCAT_max_items){
                          .Object@criteria <- .Object@CAT_criteria
                          .Object@method <- .Object@CAT_method
                          .Object@min_items <- .Object@min_items + item
                          .Object@max_items <- min(c(.Object@max_items + item, test@length))
                          .Object@stage <- 2L
                          return(.Object)
                      }
                  }
            }
        .Object
        }
)
