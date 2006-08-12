"dcbb3" <-
function (theta, delta, u, v) 
{
    W <- exp(theta * (-log(u))^(delta))
    P <- exp(theta * (-log(v))^(delta))
    S <- W + P - 1
    h <- (1/theta) * log(S)
    F <- exp(-h^(1/delta))
    duS <- exp((-log(u))^(delta) * theta) * (-theta * delta/u) * 
        (-log(u))^(delta - 1)
    dvS <- exp((-log(v))^(delta) * theta) * (-theta * delta/v) * 
        (-log(v))^(delta - 1)
    dvuh <- (-1/theta)/S^2 * duS * dvS
    duh <- (1/theta) * duS/S
    dvh <- (1/theta) * dvS/S
    densi <- F * (1/delta^2) * (h^(1/delta - 1))^2 * dvh * duh + 
        F * (-1/delta) * (1/delta - 1) * (h)^(1/delta - 2) * 
            (dvh) * (duh) + F * (-1/delta) * (h)^(1/delta - 1) * 
        dvuh
}
