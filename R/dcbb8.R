"dcbb8" <-
function (theta, delta, u, v) 
{
    S <- 1 - (1 - delta * u)^(theta)
    T <- 1 - (1 - delta * v)^(theta)
    K <- (1 - (1 - delta)^(theta))^(-1)
    W <- 1 - K * S * T
    DuS <- theta * delta * (1 - delta * u)^(theta - 1)
    DuW <- -K * T * DuS
    DvT <- theta * delta * (1 - delta * v)^(theta - 1)
    DvW <- -K * S * DvT
    DvuW <- -K * DuS * DvT
    densi <- -delta^(-1) * (1/theta) * (1/theta - 1) * W^(1/theta - 
        2) * DvW * DuW - delta^(-1) * (1/theta) * W^(1/theta - 
        1) * DvuW
}
