"dcbb2" <-
function (theta, delta, u, v) 
{
    k <- exp(theta * (u^(-delta) - 1))
    t <- exp(theta * (v^(-delta) - 1))
    S <- k + t - 1
    h <- (1/theta) * log(S)
    duk <- -k * (theta * delta) * u^(-delta - 1)
    dvt <- -t * (theta * delta) * v^(-delta - 1)
    duS <- duk
    dvS <- dvt
    duh <- (1/theta) * duS/S
    dvh <- (1/theta) * dvS/S
    dvuh <- (-1/theta) * duS * dvS/S^2
    densi <- (1/delta) * (1 + 1/delta) * (1 + h)^(-2 - 1/delta) * 
        dvh * duh - (1/delta) * (1 + h)^(-1 - 1/delta) * dvuh
}
