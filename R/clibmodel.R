"clibmodel" <-
function (param, x, y, model = c("CBB1", "CBB2", "CBB3", "CBB4", 
    "CBB5", "CBB6", "CBB7", "CBB8", "CBB9", "CBB10", "CMM1", 
    "CMM2", "CMM3", "CMM4", "CMM5", "CMM6", "CMM7", "CMM8", "CMM9", 
    "CMM10", "CBM1", "CBM2", "CBM3", "CBM4", "CBM5", "CBM6", 
    "CBM7", "CBM8", "CBM9", "CBM10", "CMB1", "CMB2", "CMB3", 
    "CMB4", "CMB5", "CMB6", "CMB7", "CMB8", "CMB9", "CMB10")) 
{
    model <- match.arg(model)
    switch(model, CBB1 = llbb1(param, x, y), CBB2 = llbb2(param, 
        x, y), CBB3 = llbb3(param, x, y), CBB4 = llbb4(param, 
        x, y), CBB5 = llbb5(param, x, y), CBB6 = llbb6(param, 
        x, y), CBB7 = llbb7(param, x, y), CBB8 = llbb8(param, 
        x, y), CBB9 = llbb9(param, x, y), CBB10 = llbb10(param, 
        x, y), CMM1 = llbb1(param, 1 - x, 1 - y), CMM2 = llbb2(param, 
        1 - x, 1 - y), CMM3 = llbb3(param, 1 - x, 1 - y), CMM4 = llbb4(param, 
        1 - x, 1 - y), CMM5 = llbb5(param, 1 - x, 1 - y), CMM6 = llbb6(param, 
        1 - x, 1 - y), CMM7 = llbb7(param, 1 - x, 1 - y), CMM8 = llbb8(param, 
        1 - x, 1 - y), CMM9 = llbb9(param, 1 - x, 1 - y), CMM10 = llbb10(param, 
        1 - x, 1 - y), CBM1 = llbb1(param, x, 1 - y), CBM2 = llbb2(param, 
        x, 1 - y), CBM3 = llbb3(param, x, 1 - y), CBM4 = llbb4(param, 
        x, 1 - y), CBM5 = llbb5(param, x, 1 - y), CBM6 = llbb6(param, 
        x, 1 - y), CBM7 = llbb7(param, x, 1 - y), CBM8 = llbb8(param, 
        x, 1 - y), CBM9 = llbb9(param, x, 1 - y), CBM10 = llbb10(param, 
        x, 1 - y), CMB1 = llbb1(param, 1 - x, y), CMB2 = llbb2(param, 
        1 - x, y), CMB3 = llbb3(param, 1 - x, y), CMB4 = llbb4(param, 
        1 - x, y), CMB5 = llbb5(param, 1 - x, y), CMB6 = llbb6(param, 
        1 - x, y), CMB7 = llbb7(param, 1 - x, y), CMB8 = llbb8(param, 
        1 - x, y), CMB9 = llbb9(param, 1 - x, y), CMB10 = llbb10(param, 
        1 - x, y))
}
