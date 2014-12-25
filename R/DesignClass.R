Design <- setClass(Class = "Design",
                   slots = c(method = 'character',
                             criteria = 'character',
                             criteria_estimator = 'character',
                             classify = 'numeric',
                             classify_alpha = 'numeric',
                             min_SEM = 'numeric',
                             met_SEM = 'logical',
                             min_items = 'integer',
                             max_items = 'integer',
                             stop_now = 'logical',
                             exposure = 'numeric',
                             SH = 'logical',
                             Wrule_weights = 'numeric',
                             KL_delta = 'numeric',
                             start_item = 'integer',
                             preCAT_nitems = 'integer',
                             preCAT_criteria = 'character',
                             preCAT_method = 'character',
                             CAT_criteria = 'character',
                             CAT_method = 'character',
                             max_time = 'numeric',
                             use_content = 'logical',
                             content = 'factor',
                             content_prop = 'numeric',
                             content_prop_empirical = 'numeric',
                             numerical_info = 'logical'),
                   validity = function(object) return(TRUE)
)

setMethod("initialize", signature(.Object = "Design"),
          function(.Object, method, criteria, nfact, design,
                   start_item, preCAT, nitems){
              .Object@numerical_info <- FALSE
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
              if(!is.nan(start_item) && .Object@start_item != 1 && criteria == 'seq')
                  stop('start_item must equal 1 with seq criteria')
              if(nfact > 1L && 
                     !any(criteria %in% c('Drule', 'Trule', 'Wrule', 'KL', 'KLn',
                                          'Erule', 'seq', 'random', 'Arule', 'APrule',
                                          'DPrule', 'TPrule', 'EPrule', 'WPrule')))
                  stop('Selected criteria not valid for multidimensional tests')
              .Object@min_SEM <- .3
              .Object@met_SEM <- rep(FALSE, nfact)
              .Object@Wrule_weights <- rep(1/nfact, nfact)
              .Object@min_items <- 1L
              .Object@max_items <- nitems
              .Object@stop_now <- FALSE 
              .Object@preCAT_nitems <- 0L
              .Object@KL_delta <- 0.1
              .Object@max_time <- Inf
              .Object@use_content <- FALSE
              .Object@content_prop_empirical <- 1
              .Object@classify <- NaN
              .Object@classify_alpha <- .05
              .Object@exposure <- rep(1, nitems)
              .Object@SH <- FALSE
              if(length(design)){
                  if(!is.null(design$content)){
                      .Object@use_content <- TRUE
                      .Object@content <- factor(design$content)
                      if(!mirt:::closeEnough(sum(design$content_prop)-1, -1e-6, 1e-6))
                          stop('content_prop does not sum to 1')
                      tmp <- design$content_prop
                      tmp <- tmp[match(names(table(.Object@content)), names(tmp))]
                      .Object@content_prop <- tmp
                      tmp[1L:length(tmp)] <- 0
                      .Object@content_prop_empirical <- tmp
                  }
                  if(!is.null(design$max_time))
                      .Object@max_time <- design$max_time
                  if(!is.null(design$KL_delta))
                      .Object@KL_delta <- design$KL_delta
                  if(!is.null(design$Wrule_weights)) 
                      .Object@Wrule_weights <- design$Wrule_weights
                  if(!is.null(design$min_SEM))
                      .Object@min_SEM <- design$min_SEM
                  if(!is.null(design$min_items))
                      .Object@min_items <- as.integer(design$min_items)
                  if(!is.null(design$max_items))
                      .Object@max_items <- as.integer(design$max_items)
                  if(!is.null(design$numerical_info))
                      .Object@numerical_info <- design$numerical_info
                  if(!is.null(design$classify))
                      .Object@classify <- design$classify
                  if(!is.null(design$classify_CI)){
                      if(design$classify_CI > 1 || design$classify_CI < 0)
                          stop('classify_CI criteria must be between 0 and 1')
                      .Object@classify_alpha <- (1 - design$classify_CI)/2
                  }
                  if(!is.null(design$exposure)){
                      if(length(design$exposure) != nitems)
                          stop('exposure vector length not equal to number of items')
                      .Object@exposure <- design$exposure
                      .Object@SH <- all(design$exposure <= 1 && design$exposure >= 0)
                  }
              }
              if(.Object@use_content && criteria == 'seq')
                  stop('content designs are not supported for seq criteria')
              if(!mirt:::closeEnough(sum(.Object@Wrule_weights)-1, -1e-6, 1e-6))
                  stop('Wrule_weights does not sum to 1')
              if(length(.Object@min_SEM) != 1L && length(.Object@min_SEM) != nfact)
                  stop('min_SEM criteria is not a suitable length')
              if(length(preCAT)){
                  if(is.null(preCAT$nitems))
                      stop('preCAT nitems must be specified')
                  else .Object@preCAT_nitems <- as.integer(preCAT$nitems)
                  if(is.null(preCAT$method))
                      .Object@preCAT_method <- 'MAP'
                  else .Object@preCAT_method <- preCAT$method
                  if(is.null(preCAT$criteria))
                      .Object@preCAT_criteria <- 'random'
                  else .Object@preCAT_criteria <- preCAT$criteria
                  .Object@criteria <- .Object@preCAT_criteria
                  .Object@method <- .Object@preCAT_method
              }
              .Object
          }
)

setGeneric('Update.stop_now', function(.Object, ...) standardGeneric("Update.stop_now"))

setGeneric('Next.stage', function(.Object, ...) standardGeneric("Next.stage"))

setMethod("Update.stop_now", signature(.Object = "Design"),
          function(.Object, person){
              nanswered <- sum(!is.na(person$responses))
              if(person$score){
                  if(nanswered >= .Object@min_items){
                      diff <- person$thetas_SE_history[nrow(person$thetas_SE_history), ]
                      if(!is.nan(.Object@classify[1L])){
                          z <- -abs(person$thetas - .Object@classify) / diff
                          if(all(z < qnorm(.Object@classify_alpha))) .Object@stop_now <- TRUE
                          } else {
                              .Object@met_SEM <- diff < .Object@min_SEM
                              if(all(.Object@met_SEM)) .Object@stop_now <- TRUE
                          }
                  }
              }
              if(nanswered == .Object@max_items) .Object@stop_now <- TRUE
              if(.Object@max_time <= sum(person$item_time)) stop_now <- TRUE
              .Object
          }
)

setMethod("Next.stage", signature(.Object = "Design"),
          function(.Object, item){
              if(item == .Object@preCAT_nitems){
                  .Object@criteria <- .Object@CAT_criteria
                  .Object@method <- .Object@CAT_method
              }
              .Object
          }
)