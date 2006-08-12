"dcbb7" <-
function (theta, delta, u, v) 
{
    WU <- (1 - (1 - u)^(theta))^(-delta)
    WV <- (1 - (1 - v)^(theta))^(-delta)
    duWU <- (-delta * theta) * (1 - (1 - u)^(theta))^(-delta - 
        1) * (1 - u)^(theta - 1)
    dvWV <- (-delta * theta) * (1 - (1 - v)^(theta))^(-delta - 
        1) * (1 - v)^(theta - 1)
    K <- WU + WV
    duK <- duWU
    dvK <- dvWV
    S <- K - 1
    duS <- duK
    dvS <- dvK
    h <- 1 - S^(-1/delta)
    duh <- (1/delta) * S^(-1/delta - 1) * duS
    dvh <- (1/delta) * S^(-1/delta - 1) * dvS
    dvuh <- (1/delta) * (-1/delta - 1) * (S)^(-1/delta - 2) * 
        (dvS) * (duS)
    densi <- (-1/theta) * (1/theta - 1) * h^(1/theta - 2) * dvh * 
        duh - (1/theta) * h^(1/theta - 1) * dvuh
}
