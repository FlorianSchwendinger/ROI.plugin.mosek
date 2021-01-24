library(ROI)
library(ROI.plugin.mosek)

check <- function(domain, condition, level=1, message="", call=sys.call(-1L)) {
    if ( isTRUE(condition) ) return(invisible(NULL))
    msg <- sprintf("in %s", domain)
    if ( all(nchar(message) > 0) ) msg <- sprintf("%s\n\t%s", msg, message)
    stop(msg)
    return(invisible(NULL))
}

## LP - Example - 1
## max:  2 x_1 + 4 x_2 + 3 x_3
## s.t.
## 3 x_1  +  4 x_2  +  2 x_3  <= 60
## 2 x_1  +    x_2  +  2 x_3  <= 40
##   x_1  +  3 x_2  +  2 x_3  <= 80 
## x_1, x_2, x_3 >= 0
test_lp_01 <- function(solver) {
    mat <- matrix(c(3, 4, 2,
                    2, 1, 2,
                    1, 3, 2), nrow=3, byrow=TRUE)
    x <- OP(objective = c(2, 4, 3),
            constraints = L_constraint(L = mat,
                                       dir = c("<=", "<=", "<="),
                                       rhs = c(60, 40, 80)),
            maximum = TRUE)


    opt <- ROI_solve(x, solver = solver)
    check("LP-01@01", equal(opt$solution, c(0, 20/3, 50/3), tol=1e-4))
    check("LP-01@02", equal(opt$objval, 230/3, tol=1e-4))
}

## Test if ROI can handle empty constraint matrix.
test_lp_02 <- function(solver) {
    x <- OP(objective = c(2, 4, 3),
            constraints = L_constraint(L=matrix(0, nrow=0, ncol=3), 
                                       dir=character(), rhs=double()),
            maximum = FALSE)

    opt <- ROI_solve(x, solver = solver)
    check("LP-02@01", equal(opt$solution, c(0, 0, 0), tol=1e-4))
    check("LP-02@02", equal(opt$objval, 0, tol=1e-4))
}

## Test if ROI can handle when the constraint is equal to NULL.
test_lp_03 <- function(solver) {
    x <- OP(objective = c(2, 4, 3), constraints = NULL, maximum = FALSE)

    opt <- ROI_solve(x, solver = solver)
    check("LP-03@03", equal(opt$solution, c(0, 0, 0), tol=1e-4))
    check("LP-03@03", equal(opt$objval, 0, tol=1e-4))
}

## MILP - Example - 1
## min:  3 x + 1 y + 3 z
## s.t.
##      -1 x  +    y  +   z  <=  4
##               4 y  - 3 z  <=  2
##         x  -  3 y  + 2 z  <=  3
##     x, z \in Z_+
##     y >= 0
test_milp_01 <- function(solver) {
    obj <- c(3, 1, 3)
    A <- rbind(c(-1,  2,  1),
               c( 0,  4, -3),
               c( 1, -3,  2))
    b <- c(4, 2, 3)
    bounds <- V_bound(li = c(1L, 3L), ui = c(1L, 2L),
                  lb = c(-Inf, 2), ub = c(4, 100))

    x <- OP(objective = obj,
         constraints = L_constraint(L = A,
                                    dir = c("<=", "<=", "<="),
                                    rhs = b),
         types = c("I", "C", "I"),
         bounds = bounds,
         maximum = TRUE)

    control <- list()

    opt <- ROI_solve(x, solver=solver, control=control)    
    check("MILP-01@01", equal(opt$solution , c(4, 2.5, 3), tol=1e-01))
}


## MILP - Example - 2
## min:  3 x + 1 y + 3 z
## s.t.
##      -1 x  +    y  +   z  <=  4
##               4 y  - 3 z  <=  2
##         x  -  3 y  + 2 z  <=  3
##     x, z \in Z_+
##     y >= 0
test_milp_02 <- function(solver) {
    obj <- c(3, 1, 3)
    A <- rbind(c(-1,  2,  1),
               c( 0,  4, -3),
               c( 1, -3,  2))
    b <- c(4, 2, 3)

    x <- OP(objective = obj,
         constraints = L_constraint(L = A,
                                    dir = c("<=", "<=", "<="),
                                    rhs = b),
         types = c("I", "C", "I"),
         maximum = TRUE)

    control <- list()

    opt <- ROI_solve(x, solver=solver, control=control)
    check("MILP-02@01", all(A %*% opt$solution <= b))
    check("MILP-02@02", equal(opt$solution , c(5, 2.75, 3), tol=1e-01))
}

## QP - Example - 1
##
## from the quadprog package
## (c) S original by Berwin A. Turlach R port by Andreas Weingessel
## GPL-3
##
## min: -(0 5 0) %*% x + 1/2 x^T x
## under the constraints:      A^T x >= b
## with b = (-8,2,0)^T
## and      (-4  2  0)
##      A = (-3  1 -2)
##          ( 0  0  1)
## we can use solve.QP as follows:
##
## library(quadprog)
## D <- diag(1, 3)
## d <- c(0, 5, 0)
## A <- cbind(c(-4, -3, 0), 
##            c( 2,  1, 0), 
##            c( 0, -2, 1))
## b <- c(-8, 2, 0)
## 
## sol <- solve.QP(D, d, A, bvec=b)
## deparse(sol$solution)
## deparse(sol$value)
test_qp_01 <- function(solver) {
    A <- cbind(c(-4, -3, 0), 
               c( 2,  1, 0), 
               c( 0, -2, 1))
    x <- OP(Q_objective(diag(3), L =  c(0, -5, 0)),
            L_constraint(L = t(A),
                         dir = rep(">=", 3),
                         rhs = c(-8, 2, 0)))

    opt <- ROI_solve(x, solver=solver)
    x.solution <- c(0.476190476190476, 1.04761904761905, 2.0952380952381)
    check("QP-01@01", equal(opt$solution, x.solution, tol=1e-4) )
    check("QP-01@02", equal(opt$objval, -2.38095238095238) )
}

## This Test detects non-conform objective functions.
## minimize 0.5 x^2 - 2 x + y
## s.t. x <= 3
## Type 1:   0.5 x'Qx + c'Lx => c(2, 0)  objval=-2
## Type 2:       x'Qx + c'Lx => c(3, 0)  objval=-3.75
test_qp_02 <- function(solver) {
    zero <- .Machine$double.eps * 100
    qo <- Q_objective(Q=rbind(c(1, 0), c(0, zero)), L=c(-2, 1))
    lc1 <- L_constraint(L=matrix(c(1, 0), nrow=1), dir="<=", rhs=3)
    lc2 <- L_constraint(L=matrix(c(1, 0), nrow=1), dir=">=", rhs=0)
    x <- OP(qo, c(lc1, lc2))

    opt <- ROI_solve(x, solver=solver)
    x.solution <- c(2, 0)
    check("QP-02@01", equal(opt$solution, x.solution, tol=1e-4) )
    check("QP-02@02", equal(opt$objval, -2, tol=1e-4) )
}


## SOCP - Example - 1
## min:  1 x1 + 1 x2 + 1 x3
## s.t.     x1 == sqrt(2)
##          x1 >= ||(x2, x3)||
##
## c(sqrt(2), -1, -1)
test_cp_01 <- function(solver) {
    obj <- c(1, 1, 1)
    A <- rbind(c(1, 0, 0))
    b <- c(sqrt(2))
    G <- diag(x=-1, 3)
    h <- rep(0, 3)

    bound <- V_bound(li = 1:3, lb = rep(-Inf, 3))

    lc <- C_constraint(L = rbind(A, G), 
                       cones = c(K_zero(1), K_soc(3)), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc, types = rep("C", 3),
            bounds =  bound, maximum = FALSE)

    opt <- ROI_solve(x, solver = solver)
    
    check("CP-01@01", equal(sum(abs(opt$solution - c(sqrt(2), -1, -1))), 0))
    check("CP-01@02", equal(opt$objval, (sqrt(2) - 2)))
}

## SOCP - Example - 2
## min:  0 x1 - 2 x2 - 2 x3 + 0 x4 - 2 x5 - 2 x6
## s.t.     x1 == sqrt(2)
##          x4 == sqrt(2)
##          x1 >= ||(x2, x3)||
##          x4 >= ||(x5, x6)||
##
## c(sqrt(2), 1, 1, sqrt(2), 1, 1)
test_cp_02 <- function(solver) {
    obj <- c(0, -2, -2, 0, -2, -2)
    A <- rbind(c(1, 0, 0, 0, 0, 0),
               c(0, 0, 0, 1, 0, 0))
    b <- c(sqrt(2), sqrt(2))
    G <- diag(x=-1, 6)
    h <- rep(0, 6)

    lc <- C_constraint(L = rbind(A, G), 
                       cones = c(K_zero(2), K_soc(c(3, 3))), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc)

    opt <- ROI_solve(x, solver=solver)
    check("CP-02@01", equal(sum(abs(opt$solution - c(sqrt(2), 1, 1, sqrt(2), 1, 1))), 0))
}

## EXPP - Example - 1
## min:  x + y + z
## s.t.
## y e^(x/y) <= z
## y > 0
## x := 1
## y := 2
## c(1, 2, 2*exp(1/2))
test_cp_03 <- function(solver) {
    obj <- c(1, 1, 1)
    A <- rbind(c(1, 0, 0),
               c(0, 1, 0))
    b <- c(1, 2)
    G <- -diag(3)
    h <- rep(0, 3)

    lc <- C_constraint(L = rbind(A, G), 
                       cones = c(K_zero(2), K_expp(1)), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc)

    opt <- ROI_solve(x, solver = solver)
    check("CP-03@01", equal(opt$solution , c(1, 2, 2*exp(1/2))))
}

## EXPP - Example - 2
## max:  x + y + z
## s.t.
## y e^(x/y) <= z
## y > 0
## y == 2
## z == 2 * exp(1/2)
## c(1, 2, 2*exp(1/2))
test_cp_04 <- function(solver) {
    obj <- c(1, 1, 1)
    A <- rbind(c(0, 1, 0),
               c(0, 0, 1))
    b <- c(2, 2*exp(1/2))
    G <- diag(x=-1, 3)
    h <- rep(0, 3)

    lc <- C_constraint(L = rbind(A, G), 
                       cones = c(K_zero(2), K_expp(1)), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc, maximum = TRUE)

    opt <- ROI_solve(x, solver=solver)
    check("CP-04@01", equal(opt$solution , c(1, 2, 2*exp(1/2))))
}

## EXPP - Example - 3
## max:  x + y + z
## s.t.
## y e^(x/y) <= z
## y > 0
## y == 1
## z == exp(1)
## c(1, 1, exp(1))
test_cp_05 <- function(solver) {
    obj <- c(1, 1, 1)
    A <- rbind(c(0, 1, 0),
               c(0, 0, 1))
    b <- c(1, exp(1))
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    ## cones <- list("free"=c(1, 2), "expp"=list(3:5))
    ## bound <- as.C_bound(cones)

    lc <- C_constraint(L = rbind(A, G), 
                       cones = c(K_zero(2), K_expp(1)), 
                       rhs = c(b, h))
    x <- OP(objective = obj, constraints = lc, 
            types = rep("C", 3), maximum = TRUE)

    opt <- ROI_solve(x, solver = solver)
    check("CP-05@01", equal(opt$solution , c(1, 1, exp(1))))
}

## EXPD - Example - 1
## min:  u + v + w
## s.t.
##      -u * e^(v/u) <= e * w; u < 0; v >= 0; w >= 0
##      u == -1
##      v ==  1
##
##      c(-1, 1, exp(-2))
test_cp_06 <- function(solver) {
    x <- OP(c(1, 1, 1))
    A <- rbind(c(1,  0, 0),
               c(0,  1, 0))
    b <- c(-1, 1)
    G <- diag(x=-1, 3)
    h <- rep(0, 3)
    constraints(x) <- C_constraint(L = rbind(A, G), 
                                   cones = c(K_zero(2), K_expd(1)), 
                                   rhs = c(b, h))
    bounds(x) <- V_bound(li=1:3, lb=rep(-Inf, 3))

    opt <- ROI_solve(x, solver=solver)

    check("CP-06@01", equal(opt$solution , c(-1, 1, exp(-2))))
}


if ( !any("mosek" %in% names(ROI_registered_solvers())) ) {
    ## This should never happen.
    cat("ROI.plugin.mosek cloud not be found among the registered solvers.\n")
} else {
    print("Start Testing!")
    local({test_lp_01("mosek")})
    local({test_lp_02("mosek")})
    local({test_lp_03("mosek")})
    local({test_milp_01("mosek")})
    local({test_milp_02("mosek")})
    local({test_qp_01("mosek")})
    local({test_qp_02("mosek")})
    local({test_cp_01("mosek")})
    local({test_cp_02("mosek")})
    local({test_cp_03("mosek")})
    local({test_cp_04("mosek")})
    local({test_cp_05("mosek")})
    local({test_cp_06("mosek")})
}

