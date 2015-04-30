Test <- setClass(Class = "Test",                     
                    slots = c(
                        mo = 'SingleGroupClass',
                        EIs = 'list',
                        ThetaGrid = 'matrix',
                        density = 'numeric',
                        quadpts = 'numeric',
                        theta_range = 'numeric',
                        item_answers = 'list',
                        item_options = 'list',
                        item_class = 'character',
                        itemnames = 'character',
                        nfact = 'integer',
                        length = 'integer',
                        itemloc2 = 'integer',
                        gp = 'list',
                        fscores_args = 'list'),
                 validity = function(object) return(TRUE)
)

setMethod("initialize", signature(.Object = "Test"),
          function(.Object, mo, item_answers_in, item_options,
                   quadpts_in, theta_range_in, dots){
              mo@exploratory <- FALSE
              .Object@itemnames <- colnames(mo@Data$data)
              mo@Data$mins <- rep(0L, length(mo@Data$min))
              .Object@mo <- mo
              .Object@item_class <- sapply(mo@pars, class)
              if(is.null(item_answers_in))
                  item_answers_in <- as.character(rep(NA, length(.Object@itemnames)))
              
              .Object@item_answers <- as.list(item_answers_in)
              .Object@item_options <- item_options
              .Object@length <- length(.Object@item_answers)
              .Object@nfact <- mo@nfact
              if(is.null(quadpts_in)) 
                  .Object@quadpts <- switch(as.character(mo@nfact), 
                                     '1'=61, '2'=31, '3'=15, '4'=9, '5'=7, 3)
              else .Object@quadpts <- quadpts_in
              if(is.null(theta_range_in)) .Object@theta_range <- c(-6, 6)
              else .Object@theta_range <- theta_range_in
              gp <- mirt:::ExtractGroupPars(mo@pars[[.Object@length + 1L]])
              if(mo@nfact == 1L){
                  .Object@ThetaGrid <- mirt:::thetaComb(seq(.Object@theta_range[1L],
                                                            .Object@theta_range[2L], 
                                                            length.out=.Object@quadpts),
                                                        mo@nfact)
                  .Object@density <- mirt:::mirt_dmvnorm(.Object@ThetaGrid, mean=gp$gmeans, 
                                                  sigma=gp$gcov)
              }
              .Object@gp <- gp
              tmp <- mo@itemloc
              .Object@itemloc2 <- as.integer(tmp[-length(tmp)])
              tmp <- list(rotate = 'none', theta_lim = c(-6,6), mean = gp$gmean,
                          cov=gp$gcov, MI = 0, QMC=FALSE, custom_den=NULL)
              if(length(dots)){
                  if(!is.null(dots$rotate))
                      warning('rotation not supported in mirtCAT. Using fixed
                                           slope coefficients', call.=FALSE)
                  if(!is.null(dots$theta_lim))
                      tmp$theta_lim <- dots$theta_lim
                  if(!is.null(dots$mean))
                      tmp$mean <- dots$mean
                  if(!is.null(dots$cov))
                      tmp$cov <- dots$cov
                  if(!is.null(dots$MI))
                      tmp$MI <- dots$MI
                  if(!is.null(dots$QMC))
                      tmp$QMC <- dots$QMC
                  if(!is.null(dots$custom_den))
                      tmp$custom_den <- dots$custom_den
              } 
              .Object@fscores_args <- tmp
              .Object@EIs <- lapply(1L:.Object@length, 
                                    function(x, test) extract.item(test, x), test=.Object@mo)
              .Object
          }
)
