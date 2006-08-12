"llbb7" <-
function (param, u, v) 
{
    n <- sum(u >= -1)
    s <- 0
    for (i in 1:n) {
        s <- s + log(dcbb7(param[1], param[2], u[i], v[i]))
        if (is.nan(s)) {
            break
        }
    }
    if (is.finite(s)) {
        res <- s
    }
    else {
        res <- -10^(64)
    }
}
