"dcbb10" <-
function (theta, delta, u, v) 
{
    S <- 1 - u^(1/delta)
    T <- 1 - v^(1/delta)
    W <- theta * S * T
    C <- u * v * (1 - W)^(-delta)
    DuS <- (-1/delta) * u^(1/delta - 1)
    DuW <- theta * T * DuS
    DvT <- (-1/delta) * v^(1/delta - 1)
    DvW <- theta * S * DvT
    DvuW <- theta * DuS * DvT
    densi <- (1 - W)^(-delta) + v * (-delta) * (1 - W)^(-delta - 
        1) * (-1) * DvW + u * delta * (1 - W)^(-delta - 1) * 
        DuW + u * v * delta * (-delta - 1) * (1 - W)^(-delta - 
        2) * (-1) * DvW * DuW + u * v * delta * (1 - W)^(-delta - 
        1) * DvuW
}
