as_dgCMatrix <- function( x, ... ) 
  sparseMatrix(i=x$i, j=x$j, x=x$v, dims=c(x$nrow, x$ncol))


to_dense_vector <- function(x, len) {
  y <- rep.int(0L, len)
  if ( is.null(x$ind) ) return(y)
  y[x$ind] <- x$val
  return(y)
}

V_bound_2_matrix <- function(bound, len) {
  blx <- rep.int(0L, len)
  bux <- rep.int(Inf, len)
  if ( length(bound$lower$ind) > 0 ) {
    blx[bound$lower$ind] <- bound$lower$val
  }
  if ( length(bound$upper$ind) > 0 ) {
    bux[bound$upper$ind] <- bound$upper$val
  }
  return(rbind(blx, bux))
}



## problem
## The optimization problem.
## problem                      LIST
## ..$sense                     STRING
## ..$c                         NUMERIC VECTOR
## ..$c0                        NUMERIC                  (OPTIONAL)
## ..$A                         SPARSE MATRIX
## ..$bc                        NUMERIC MATRIX (2 rows)
## ..$bx                        NUMERIC MATRIX (2 rows)
## ..$cones                     LIST MATRIX (2 rows)     (OPTIONAL)
## ..$intsub                    NUMERIC VECTOR           (OPTIONAL)
## ..$qobj                      LIST                     (OPTIONAL)
## ..$scopt                     LIST                     (OPTIONAL)
## ..$iparam/$dparam/$sparam    LIST                     (OPTIONAL)
## ....$<MSK_PARAM>             STRING / NUMERIC         (OPTIONAL)
## ..$sol                       LIST                     (OPTIONAL)
## ....$itr/$bas/$int           LIST                     (OPTIONAL)
##
## opts
## The interface options.
## opts               LIST                 (OPTIONAL)
## ..$verbose         NUMERIC              (OPTIONAL)
## ..$usesol          BOOLEAN              (OPTIONAL)
## ..$useparam        BOOLEAN              (OPTIONAL)
## ..$soldetail       NUMERIC              (OPTIONAL)
## ..$getinfo         BOOLEAN              (OPTIONAL)
## ..$writebefore     STRING (filepath)    (OPTIONAL)
## ..$writeafter      STRING (filepath)    (OPTIONAL)

is.LP <- function(x) {
  if ( inherits(objective(x), "L_objective")
     & ( is.NO_constraint(constraints(x)) | is.L_constraint(constraints(x)) )
     & all(types(x) == "C") ) {
    return(TRUE)
  }    
  return(FALSE)
}

.control_args  <- c("verbose", "usesol", "useparam", "soldetail", "getinfo", "scofile")
.control_types <- c("numeric", "logical", "logical", "numeric",   "logical", "character")

mosek_cones <- c("zero" = 1L, "nonneg" = 2L, "soc" = 3L, "expp" = 5L, "expd" = 6L) #, "DEXP" = 6L, "PPOW" = 7L, "DPOW" = 8L)

calc_exp_dims <- function(x, exp) {
    y <- x$id[x$cone == mosek_cones[exp]]
    if ( !length(y) )
        return(NULL)
    length(unique(y))
}

calc_soc_dims <- function(x) {
    y <- x$id[x$cone == mosek_cones["soc"]]
    if ( !length(y) )
        return(NULL)
    as.integer(table(y))
}

calc_dims <- function(cones) {
  dims <- list()
  dims$ep <- calc_exp_dims(cones, "expp")
  dims$ed <- calc_exp_dims(cones, "expd")
  dims$q <- calc_soc_dims(cones)

  dims
}

##
solve_OP <- function(x, control = list()) {
  solver <- "mosek"

  constr <- as.C_constraint(constraints(x))

  ## check if "mosek" supports the provided cone types
  stopifnot(all(constr$cones$cone %in% mosek_cones))

  leno <- length(objective(x))
  m <- list()
  m$sense <- c("min", "max")[1 + x$maximum]
  
  ## objective
  m$c <- as.vector(terms(objective(x))$L)
  if ( !is.null(terms(objective(x))$Q) ) {
    m$qobj <- unclass(terms(objective(x))$Q)[c("i", "j", "v")]
    i <- which(m$qobj$j <= m$qobj$i)
    m$qobj$i <- m$qobj$i[i]
    m$qobj$j <- m$qobj$j[i]
    m$qobj$v <- m$qobj$v[i]
  }

  ## constraints
  m$A <- sparseMatrix(i=1, j=1, x=1, dims=c(1,leno))
  m$bc <- rbind(-Inf, Inf)

  if ( nrow(constr) ) {
    b <- constr$cones$cone %in% mosek_cones[c("zero", "nonneg")]
    if ( any(b) ) {
      m$A <- as_dgCMatrix(constr$L[b,])
      m$bc <- rbind(rep.int(-Inf, nrow(m$A)),
                    constr$rhs[b])
      zero_b = constr$cones$cone == mosek_cones["zero"]
      if ( any(zero_b) ) m$bc[1,which(zero_b)] = constr$rhs[zero_b] 
    } 

    if ( any(!b) ) {
      m$F <- as_dgCMatrix(-constr$L[!b,])
      m$g <- constr$rhs[!b]
      cones <- constr$cones[!b]
      dims <- calc_dims(cones)

      ## Since mosek uses a different cone definition we have to 
      ## permutate the cones.
      ## Change the order from c(1, 2, 3) to c(3, 2, 1)
      k <- which(cones$cone %in% mosek_cones[c('expp', 'expd')])
      if ( length(k) ) {
        cids <- aggregate(id ~ cone_id, 
                  data = list(cone_id = cones$id[k], id = k), 
                  FUN = c, simplify = FALSE)
        for (i in seq_len(nrow(cids))) {
          j <- cids[[i, 2]]
          cones$id[j] <- cones$id[j] + c(0.3, 0.2, 0.1)
        }
      }
      i <- with(cones, order(cone, id))

      m$F <- m$F[i,]
      m$g <- m$g[i]

      ep <- NULL
      ed <- NULL
      qc <- NULL

      if ( !is.null(dims$ep) )
        ep <- matrix(list("PEXP", 3, NULL), nrow = 3, ncol = dims$ep)
      if ( !is.null(dims$ed) )
        ed <- matrix(list("DEXP", 3, NULL), nrow = 3, ncol = dims$ed)
      if ( !is.null(dims$q) ) {
        qc <- matrix(list(), nrow = 3, ncol = length(dims$q))
        qc[1,] <- "QUAD"
        qc[2,] <- dims$q
      }

      m$cones <- do.call(cbind, list(ep, ed, qc))
      rownames(m$cones) <- c("type","dim","conepar")
    }
  }


  ## bounds
  m$bx <- V_bound_2_matrix(bounds(x), leno)

  ## types
  if ( !all(types(x) == "C") ) {
    i <- which(types(x) %in% c("B", "I"))
    m$intsub <- i
    ## binary are defined as integer and 0 <= x <= 1
    i <- which(types(x) == "B")
    if ( length(i) ) {
      m$bx[1, i] <- sapply(m$bx[1, i], max, 0)
      m$bx[2, i] <- sapply(m$bx[2, i], min, 1)
    }
  } 

  ## control
  control$verbose <- if (length(control$verbose)) control$verbose else 0L

  ## tmp <- Rmosek:::mosek(problem=m, 
  ##                       opts=control[intersect(names(control), .control_args)])
  ## str(tmp)

  m_call <- list(Rmosek::mosek, problem=m, 
           opts = control[intersect(names(control), .control_args)])
  mode(m_call) <- "call"
  if ( isTRUE(control$dry_run) )
    return(m_call)

  out <- eval(m_call)

  ##' mosek destinguishes between 3 types of solutions
  ##' 1. itr - Interior solution
  ##' 2. bas - Basic solution
  ##' 3. int - Integer solution
  if ( !is.null(out$sol$bas$xx) ) {
    x.solution <- out$sol$bas$xx
  } else if ( !is.null(out$sol$itr$xx) ) {
    x.solution <- out$sol$itr$xx
  } else {
    x.solution <- out$sol$int$xx
  }

  optimum <- tryCatch({as.numeric(objective(x)(x.solution))}, 
            error=function(e) as.numeric(NA))

  return( ROI_plugin_canonicalize_solution( solution = x.solution, 
                         optimum  = optimum,
                         status   = as.integer(out$response$code),
                         solver   = "mosek", 
                         message  = out ) 
  )
}
