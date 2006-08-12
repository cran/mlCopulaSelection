"dcbb5" <-
function (theta, delta, u, v) 
{
    t <- (-log(u))^(-theta * delta) + (-log(v))^(-theta * delta)
    dut <- (theta * delta/u) * (-log(u))^(-theta * delta - 1)
    dvt <- (theta * delta/v) * (-log(v))^(-theta * delta - 1)
    S <- (-log(u))^(theta) + (-log(v))^(theta)
    duS <- (-theta/u) * (-log(u))^(theta - 1)
    dvS <- (-theta/v) * (-log(v))^(theta - 1)
    h <- S - t^(-1/delta)
    duh <- duS + (1/delta) * (t)^(-1/delta - 1) * (dut)
    dvh <- dvS + (1/delta) * (t)^(-1/delta - 1) * (dvt)
    dvuh <- -1/delta * (1/delta + 1) * t^(-1/delta - 2) * dut * 
        dvt
    densi <- exp(-h^(1/theta)) * (1/theta)^2 * (h^(1/theta - 
        1))^2 * dvh * duh + exp(-h^(1/theta)) * (-1/theta) * 
        (1/theta - 1) * h^(1/theta - 2) * dvh * duh + exp(-h^(1/theta)) * 
        (-1/theta) * h^(1/theta - 1) * dvuh
}
