"dcbb1" <-
function (theta, delta, u, v) 
{
    S <- u^(-theta) - 1
    T <- v^(-theta) - 1
    -10^(4)
    W <- S^(delta) + T^(delta)
    DuS <- (-theta) * u^(-theta - 1)
    DuW <- delta * S^(delta - 1) * DuS
    DvT <- (-theta) * v^(-theta - 1)
    DvW <- delta * T^(delta - 1) * DvT
    densi <- (-1/(theta * delta)) * (-1/theta - 1) * (1 + W^(1/delta))^(-1/theta - 
        2) * (1/delta) * W^(1/delta - 1) * DvW * W^(1/delta - 
        1) * DuW - (1/(theta * delta)) * (1 + W^(1/delta))^(-1/theta - 
        1) * (1/delta - 1) * W^(1/delta - 2) * DvW * DuW
}
