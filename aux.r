require(MASS)

chartime <- function() as.character(format(Sys.time(), "%Y-%m-%d_%H:%M"))

roundprint <- function(x, d)
{
    x <- as.character(as.list(round(x, d)))
    unlist(
    lapply(x, function(y){
                temp <- strsplit(y, "\\.")[[1]][2]
                if(is.na(temp)) dp <- 0
                else            dp <- nchar(temp)
                if(dp == 0) y <- paste(y, ".", sep="")
                paste(y, paste(rep("0", d-dp), sep="", collapse=""), sep="")
              })
          )
}

timestamper <- function() # not to be used on the last day of the month!
    as.numeric(format(Sys.time(), "%OS6")) +
    60*(as.numeric(format(Sys.time(), "%M")) +
        60*(as.numeric(format(Sys.time(), "%H")) +
            24*as.numeric(format(Sys.time(), "%d"))))

normalize <- function(X, X2=NULL, type="mm")
{
    q <- dim(X)[1]
    q2 <- dim(X2)[1]
    if(type == "mm") {
        mi <- apply(X, 2, min)
        ma <- apply(X, 2, max)
        mi[mi == ma] <- 0
        nX <- (X - matrix(1, q, 1) %*% mi)/(matrix(1, q, 1) %*% (ma - mi))

        if (!is.null(X2)) {
            obj <- NULL
            obj$nX <- nX
            nX2 <- (X2 - matrix(1, q2, 1) %*% mi)/(matrix(1, q2, 1) %*% (ma - mi))
            obj$nX2 <- nX2
        } else {
            obj <- nX
        }
        return(obj)
    } else if(type == "ms") {
        m <- apply(X, 2, mean)
        s <- apply(X, 2, sd)
        s[s == 0] <- 1
        nX <- (X - matrix(1, q, 1) %*% m)/(matrix(1, q, 1) %*% s)

        if (!is.null(X2)) {
            obj <- NULL
            obj$nX <- nX
            nX2 <- (X2 - matrix(1, q2, 1) %*% m)/(matrix(1, q2, 1) %*% s)
            obj$nX2 <- nX2
        } else {
            obj <- nX
        }
        return(obj)
    }
}

UAR <- function(y, y_hat) {
    tmp <- 0
    groups <- unique(y)
    for (g in groups) {
        ind <- y == g
        tmp <- tmp + sum(y_hat[ind] == g)/sum(ind)
    }
    tmp <- tmp/length(groups)
    return(tmp)
}

mult <- function(...)
{
    input <- list(...)
    argc <- length(input)

    if(argc < 1){
        stop("insufficient number of inputs")
    } else if(argc < 2){
        return(input_list[1])
    }

    dyn.load("mult.so")
    A <- input[[1]]
    for(i in 2:argc)
    {
        B <- input[[i]]

        if(dim(A)[2] != dim(B)[1]) stop("dimensions don't match")

        l <- dim(A)[1]
        m <- dim(A)[2]
        n <- dim(B)[2]

        temp <- .C("mult",
                   l=as.integer(l),
                   m=as.integer(m),
                   n=as.integer(n),
                   A=as.double(t(A)),
                   B=as.double(t(B)),
                   C=as.double(rep(0, l*n))
                   )
        A <- matrix(temp$C, l, n, byrow=T)
    }

    return(A);
}

kernel <- function(X, Z=NULL, kernel="lin", gamma=1, degree=1, offset=1, scale=1)
{
    dyn.load("kernel.so")

    X <- as.matrix(X)
    n1 <- dim(as.matrix(X))[1]
    p <- dim(as.matrix(X))[2]
    K <- as.vector(matrix(0, n1, n1))

    type <- 0
    n2 <- 0
    if (!is.null(Z)) {
        type <- 1
        n2 <- dim(as.matrix(Z))[1]
        X <- rbind(X, as.matrix(Z))
        K <- as.vector(matrix(0, n1, n2))
    }

    K <- .C("kernel",
            type=as.integer(type),
            dim=as.integer(c(n1, p, n2)),
            X=as.double(as.vector(t(X))),
            kernel_code=as.integer(switch(kernel, "lin"=0, "rbf"=1,"pol"=2)),
            param=as.double(switch(kernel, "lin"=0,
                                           "rbf"=as.numeric(gamma),
                                           "pol"=c(as.numeric(degree),
                                                    as.numeric(offset),
                                                    as.numeric(scale)))),
            K=as.double(K)
           )

    if (type == 0) {
        return(matrix(K$K, n1, n1))
    } else if (type == 1) {
        return(matrix(K$K, n1, n2, byrow=T))
    }
}
