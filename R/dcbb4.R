"dcbb4" <-
function (theta, delta, u, v) 
{
    VV <- (u^(-theta) - 1)^(-delta)
    WW <- (v^(-theta) - 1)^(-delta)
    h <- VV + WW
    S <- u^(-theta) + v^(-theta) - 1
    duh <- delta * theta * u^(-theta - 1) * (u^(-theta) - 1)^(-delta - 
        1)
    dvh <- delta * theta * v^(-theta - 1) * (v^(-theta) - 1)^(-delta - 
        1)
    duS <- (-theta) * u^(-theta - 1)
    dvS <- (-theta) * v^(-theta - 1)
    densi <- (1/theta) * (1/theta + 1) * (S - h^(-1/delta))^(-1/theta - 
        2) * (dvS + 1/delta * h^(-1/delta - 1) * dvh) * (duS + 
        1/delta * h^(-1/delta - 1) * duh) + (-1/theta) * (S - 
        h^(-1/delta))^(-1/theta - 1) * ((-1/delta) * (1/delta + 
        1) * (h)^(-1/delta - 2) * dvh * duh)
}
