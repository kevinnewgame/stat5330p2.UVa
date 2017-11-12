svm
methods(svm)
getAnywhere(svm.formula)

function (formula, data = NULL, ..., subset, na.action = na.omit, 
          scale = TRUE) 
{
    call <- match.call()
    if (!inherits(formula, "formula")) 
        stop("method is only for formula objects")
    m <- match.call(expand.dots = FALSE)
    if (identical(class(eval.parent(m$data)), "matrix")) 
        m$data <- as.data.frame(eval.parent(m$data))
    m$... <- NULL
    m$scale <- NULL
    m[[1L]] <- quote(stats::model.frame)
    m$na.action <- na.action
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    attr(Terms, "intercept") <- 0
    x <- model.matrix(Terms, m)
    y <- model.extract(m, "response")
    attr(x, "na.action") <- attr(y, "na.action") <- attr(m, "na.action")
    if (length(scale) == 1) 
        scale <- rep(scale, ncol(x))
    if (any(scale)) {
        remove <- unique(c(which(labels(Terms) %in% names(attr(x, 
                                                               "contrasts"))), which(!scale)))
        scale <- !attr(x, "assign") %in% remove
    }
    ret <- svm.default(x, y, scale = scale, ..., na.action = na.action)
    ret$call <- call
    ret$call[[1]] <- as.name("svm")
    ret$terms <- Terms
    if (!is.null(attr(m, "na.action"))) 
        ret$na.action <- attr(m, "na.action")
    class(ret) <- c("svm.formula", class(ret))
    return(ret)
}