"mlcbbsel" <-
function (U, V) 
{
    model = c("CBB1", "CBB2", "CBB3", "CBB4", "CBB5", "CBB6", 
        "CBB7", "CBB8", "CBB9", "CBB10", "CMM1", "CMM2", "CMM3", 
        "CMM4", "CMM5", "CMM6", "CMM7", "CMM8", "CMM9", "CMM10", 
        "CBM1", "CBM2", "CBM3", "CBM4", "CBM5", "CBM6", "CBM7", 
        "CBM8", "CBM9", "CBM10", "CMB1", "CMB2", "CMB3", "CMB4", 
        "CMB5", "CMB6", "CMB7", "CMB8", "CMB9", "CMB10")
    respmodel = c("CBB1", "CBB2", "CBB3", "CBB4", "CBB5", "CBB6", 
        "CBB7", "CBB8", "CBB9", "CBB10", "180 degree rotation of CBB1", 
        "180 degree rotation of CBB2", "180 degree rotation of CBB3", 
        "180 degree rotation of CBB4", "180 degree rotation of CBB5", 
        "180 degree rotation of CBB6", "180 degree rotation of CBB7", 
        "180 degree rotation of CBB8", "180 degree rotation of CBB9", 
        "180 degree rotation of CBB10", "90 degree rotation of CBB1", 
        "90 degree rotation of CBB2", "90 degree rotation of CBB3", 
        "90 degree rotation of CBB4", "90 degree rotation of CBB5", 
        "90 degree rotation of CBB6", "90 degree rotation of CBB7", 
        "90 degree rotation of CBB8", "90 degree rotation of CBB9", 
        "90 degree rotation of CBB10", "270 degree rotation of CBB1", 
        "270 degree rotation of CBB2", "270 degree rotation of CBB3", 
        "270 degree rotation of CBB4", "270 degree rotation of CBB5", 
        "270 degree rotation of CBB6", "270 degree rotation of CBB7", 
        "270 degree rotation of CBB8", "270 degree rotation of CBB9", 
        "270 degree rotation of CBB10")
    ncop <- 1
    TODOCOP <- c(1:40)
    TODOPV <- c(1:2) * 0
    TODOTET <- c(1:2) * 0
    TODODEL <- c(1:2) * 0
    PVMAX <- -10^(100)
    pmax <- 10
    n <- sum(U != -10^200)
    for (nmodel in 1:40) {
        RES <- mlcbb(U, V, copulamodel = model[nmodel])
        PV <- RES$value
        PAR <- RES$par
        if (PV > PVMAX) {
            PVMAX <- PV
            PARMAX <- PAR
            COPMAX <- nmodel
            LLMAX <- PV
        }
        TODOPV[nmodel] <- PV
        TODOTET[nmodel] <- PAR[1]
        TODODEL[nmodel] <- PAR[2]
    }
    ORDEN <- order(TODOPV, TODOCOP, decreasing = TRUE)
    TODO <- matrix(c(1:40 * 4) * 0, 40, 4)
    TODO[, 1] <- respmodel[TODOCOP[ORDEN]]
    TODO[, 2] <- TODOPV[ORDEN]
    TODO[, 3] <- TODOTET[ORDEN]
    TODO[, 4] <- TODODEL[ORDEN]
    result <- list(todo = TODO, copmax = respmodel[COPMAX], parmax = PARMAX, 
        llmax = LLMAX)
}
