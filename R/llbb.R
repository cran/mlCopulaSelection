"llbb" <-
function (param, u, v, model = c("CBB1", "CBB2", "CBB3", "CBB4", 
    "CBB5", "CBB6", "CBB7", "CBB8", "CBB9", "CBB10", "CMM1", 
    "CMM2", "CMM3", "CMM4", "CMM5", "CMM6", "CMM7", "CMM8", "CMM9", 
    "CMM10", "CBM1", "CBM2", "CBM3", "CBM4", "CBM5", "CBM6", 
    "CBM7", "CBM8", "CBM9", "CBM10", "CMB1", "CMB2", "CMB3", 
    "CMB4", "CMB5", "CMB6", "CMB7", "CMB8", "CMB9", "CMB10")) 
{
    n <- sum(u >= -1)
    ss <- c(1:n) * 0
    s <- 0
    for (i in 1:n) {
        ss[i] <- log(cbbmodel(param[1], param[2], u[i], v[i], 
            model))
    }
    res <- ss
}
