"dcbb9" <-
function (theta, delta, u, v) 
{
    S <- delta - log(u)
    T <- delta - log(v)
    W <- S^(theta) + T^(theta) - delta^(theta)
    C <- exp(-W^(1/theta) + delta)
    DuS <- -1/u
    DuW <- theta * S^(theta - 1) * DuS
    DvT <- -1/v
    DvW <- theta * T^(theta - 1) * DvT
    densi <- C * (1/theta^2) * (W^(1/theta - 1))^2 * DvW * DuW + 
        C * (-1/theta) * (1/theta - 1) * W^(1/theta - 2) * DvW * 
            DuW
}
