
require(compositions)


# # basic data ----
# compo_names = c("VPA", "MPA", "LPA", "SB", "Sleep")

# mean composition and ilrs at baseline ----
m4 = c(10.44407, 55.27578, 347.65288, 511.51217, 515.11511)
sbp <- matrix(c(  1, -1, -1, -1, -1,
                  0,  1, -1, -1, -1,
                  0,  0,  1, -1, -1,
                  0,  0,  0,  1, -1),
              ncol = 5, byrow = TRUE)
psi <- compositions::gsi.buildilrBase(t(sbp))

ilr.m4 = compositions::ilr(m4, V = psi)

# new compositions for predictions ----
# change in the ilr of interest (a = change in ilr of interest; b = change in the others to compensate a)

# Change in ILR 1 (from -10 to +30 min of VPA, min by min)
a = c(1, seq(m4[1] - 10, m4[1] + 30) / m4[1])
b = (1440 - (m4[1] * a)) / (m4[2] + m4[3] + m4[4] + m4[5])
ab_ilr1 = data.frame(ilr = c(0, rep(1, length(a) - 1)), a = a, b = b,
                     # time-use composition at 4 years old
                     VPA4 = m4[1]*a, MPA4 = m4[2]*b, LPA4 = m4[3]*b, 
                     SB4 = m4[4]*b, Sleep4 = m4[5]*b)
rm(a,b)

# Change in ILR 2 (from -30 to +30 min of MPA by 2 min)
# 1440 = 10.44407*(a*b*b*b)^(1/4) + 55.27578*a + 347.65288*b + 511.51217*b + 515.11511*b
# b values calculated in https://www.symbolab.com/solver/equation-calculator
a = seq(m4[2] - 30, m4[2] + 30, by = 2) / m4[2]
b = c(1.02307, 1.0215, 1.01994, 1.01838, 1.01683, 1.01528,
      1.01374, 1.0122, 1.01067, 1.00913, 1.00761, 1.00608,
      1.00608, 1.00304,1.00152, 1.000000000, 0.998486,0.996973,
      0.995462, 0.993953, 0.992446, 0.99094, 0.989435, 0.987932,
      0.98643, 0.98493, 0.983431, 0.981932, 0.980435, 0.978939,
      0.977444)
ab_ilr2 = data.frame(ilr = 2, a = a, b = b,
                     # time-use composition at 4 years old
                     VPA4 = m4[1]*(a*b^3)^(1/4), MPA4 = m4[2]*a, LPA4 = m4[3]*b, 
                     SB4 = m4[4]*b, Sleep4 = m4[5]*b)

# Change in ILR 3 (from -90 to +90 min of LPA)
# 1440 = 10.44407(((a·b^2)^(1/3))^(1/4) · (a·b^2)^(1/4)) + 55.27578((a·b^2)^(1/3)) + 347.65288·a + 1026.627b
# b values calculated in https://www.symbolab.com/solver/equation-calculator
a = seq(m4[3] - 90, m4[3] + 90, by = 5) / m4[3]
b = c(1.09031, 1.08524, 1.08017, 1.07512, 1.07006, 1.06502, 1.05998,
      1.05495, 1.04993, 1.04491, 1.03989, 1.03489, 1.02989, 1.02489,
      1.0199,  1.01492, 1.00994, 1.00497, 1.00000, 0.995037,0.99008,
      0.985127,0.980179,0.975236,0.970297,0.965363,0.960434,0.955509,
      0.950588,0.945672,0.94076, 0.935853,0.930949,0.92605, 0.921154,
      0.916263,0.911375)
ab_ilr3 = data.frame(ilr = 3, a = a, b = b,
                     # time-use composition at 4 years old
                     VPA4 = m4[1]*((a*b*b)^(1/3))^(1/4) * (a*b*b)^(1/4), MPA4 = m4[2]*(a*b*b)^(1/3), LPA4 = m4[3]*a, SB4 = m4[4]*b, Sleep4 = m4[5]*b)
rm(a,b)

# Change in ILR 4 (from -300 to +300 min of SB)
# 1440=(10.44407(ab)^(1/4))(sqrt(ab)^(1/4))((sqrt(ab)(ab))^(1/3))^(1/4)+55.27578(sqrt(ab)·(ab))^(1/3)+347.65288·sqrt(ab)+511.51217·a+515.11511b
# b values calculated in https://www.symbolab.com/solver/equation-calculator
a = seq(m4[4] - 300, m4[4] + 300, by = 15) / m4[4]
b = c(1.71006670229299, 1.66640191455040, 1.62394854148532, 1.58261212859228, 1.54231162605595, 1.50297737901422, 1.46454833997031,
      1.42697109098268, 1.39019849515969, 1.35418872980947, 1.31890450514219, 1.28431230994092, 1.25038235623840, 1.21708743618891,
      1.18440303206502, 1.15230689069605, 1.12077876394383, 1.08980008292947, 1.05935418943891, 1.02942553932630, 1.00000000000000,
      0.971064552474187,0.942607323564974,0.914617354422225,0.887084495238876,0.859999693825665,0.833354369296217,0.807140733529092,
      0.781351647358753,0.755980577100591,0.731021471655306,0.706469067493431,0.682318347884886,0.658564858123805,0.635204596272442,
      0.612233992576200,0.589649891480090,0.567449460973740,0.545630480697629,0.524190876360710,0.503129014405917)
ab_ilr4 = data.frame(ilr = 4, a = a, b = b,
                     # time-use composition at 4 years old
                     VPA4 = m4[1]*(a*b)^(1/4)*(sqrt(a*b)^(1/4))*((sqrt(a*b)*(a*b))^(1/3))^(1/4), 
                     MPA4 = m4[2]*(sqrt(a*b)*(a*b))^(1/3), LPA4 = m4[3]*sqrt(a*b), 
                     SB4 = m4[4]*a, Sleep4 = m4[5]*b)

out = rbind(ab_ilr1, ab_ilr2, ab_ilr3, ab_ilr4)

# ilr for the new compositions
out[, 9:12] = compositions::ilr(out[,4:8], V = psi)
colnames(out)[9:12] = c("ilr1_bl", "ilr2_bl", "ilr3_bl", "ilr4_bl")

# clean environment
rm(a, b, ab_ilr1, ab_ilr2, ab_ilr3, ab_ilr4)


# -------------------------------------------------------------------------
# Predictions of outcomes at 9 years old ----------------------------------
# -------------------------------------------------------------------------

# Mean predictions ILRS at follow up (predicted from lavaan) ----
attach(dat)
for (i in 1:nrow(out)) {
  ilr.m9_bc = predict_lavaan(fit = fCompo1_BC.sex.age, 
                             newdata = cbind.data.frame(FMI4 = mean(FMI4),
                                                        FFMI4 = mean(FFMI4),
                                                        VPA4_MPA4.LPA4.SB4.SLEEP4 = out[i, "ilr1_bl"],
                                                        MPA4_LPA4.SB4.SLEEP4 = out[i, "ilr2_bl"],
                                                        LPA4_SB4.SLEEP4 = out[i, "ilr3_bl"],
                                                        SB4_SLEEP4 = out[i, "ilr4_bl"],
                                                        sex = mean(sex),
                                                        age4 = mean(age4),
                                                        age9 = mean(age9)))
  
  ilr.m9_pf = predict_lavaan(fit = fCompo1_PF.sex.age, 
                             newdata = cbind.data.frame(laps20m4 = mean(laps20m4),
                                                        Speed4 = mean(Speed4),
                                                        Str4 = mean(Str4), 
                                                        VPA4_MPA4.LPA4.SB4.SLEEP4 = out[i, "ilr1_bl"],
                                                        MPA4_LPA4.SB4.SLEEP4 = out[i, "ilr2_bl"],
                                                        LPA4_SB4.SLEEP4 = out[i, "ilr3_bl"],
                                                        SB4_SLEEP4 = out[i, "ilr4_bl"],
                                                        sex = mean(sex),
                                                        age4 = mean(age4),
                                                        age9 = mean(age9)))
  
  
  if (i == 1) {
    preds9 = as.data.frame(matrix(NA, nrow = nrow(out), ncol = 9))
    colnames(preds9) = c("ilr1_fu", "ilr2_fu", "ilr3_fu", "ilr4_fu",
                         "VPA9", "MPA9", "LPA9", "SB9", "Sleep9")
  }
  
  # store output
  preds9[i,] = c(as.numeric(ilr.m9_bc[1, c("VPA9_MPA9.LPA9.SB9.SLEEP9", "MPA9_LPA9.SB9.SLEEP9",
                                           "LPA9_SB9.SLEEP9", "SB9_SLEEP9")]), 
                 compositions::clo(compositions::ilrInv(ilr.m9_bc[1, c("VPA9_MPA9.LPA9.SB9.SLEEP9", "MPA9_LPA9.SB9.SLEEP9",
                                                                       "LPA9_SB9.SLEEP9", "SB9_SLEEP9")], V = psi), total = 1440))
  
}




# Predictions of outcomes (BC and PF) at follow up ----
for (i in 1:nrow(out)) {
  ilr.m9_bc = predict_lavaan(fit = fCompo1_BC.sex.age, 
                             newdata = cbind.data.frame(FMI4 = mean(FMI4),
                                                        FFMI4 = mean(FFMI4),
                                                        VPA4_MPA4.LPA4.SB4.SLEEP4 = mean(VPA4_MPA4.LPA4.SB4.SLEEP4),
                                                        MPA4_LPA4.SB4.SLEEP4 = mean(MPA4_LPA4.SB4.SLEEP4),
                                                        LPA4_SB4.SLEEP4 = mean(LPA4_SB4.SLEEP4),
                                                        SB4_SLEEP4 = mean(SB4_SLEEP4),
                                                        VPA9_MPA9.LPA9.SB9.SLEEP9 = preds9[i, "ilr1_fu"],
                                                        MPA9_LPA9.SB9.SLEEP9 = preds9[i, "ilr2_fu"],
                                                        LPA9_SB9.SLEEP9 = preds9[i, "ilr3_fu"],
                                                        SB9_SLEEP9 = preds9[i, "ilr4_fu"],
                                                        sex = mean(sex),
                                                        age4 = mean(age4),
                                                        age9 = mean(age9)))
  
  ilr.m9_pf = predict_lavaan(fit = fCompo1_PF.sex.age, 
                             newdata = cbind.data.frame(laps20m4 = mean(laps20m4),
                                                        Speed4 = mean(Speed4),
                                                        Str4 = mean(Str4), 
                                                        VPA4_MPA4.LPA4.SB4.SLEEP4 = mean(VPA4_MPA4.LPA4.SB4.SLEEP4),
                                                        MPA4_LPA4.SB4.SLEEP4 = mean(MPA4_LPA4.SB4.SLEEP4),
                                                        LPA4_SB4.SLEEP4 = mean(LPA4_SB4.SLEEP4),
                                                        SB4_SLEEP4 = mean(SB4_SLEEP4),
                                                        VPA9_MPA9.LPA9.SB9.SLEEP9 = preds9[i, "ilr1_fu"],
                                                        MPA9_LPA9.SB9.SLEEP9 = preds9[i, "ilr2_fu"],
                                                        LPA9_SB9.SLEEP9 = preds9[i, "ilr3_fu"],
                                                        SB9_SLEEP9 = preds9[i, "ilr4_fu"],
                                                        sex = mean(sex),
                                                        age4 = mean(age4),
                                                        age9 = mean(age9)))
  
  
  if (i == 1) {
    preds9[, (ncol(preds9) + 1):(ncol(preds9) + 5)] = NA
    colnames(preds9)[(ncol(preds9) - 4):(ncol(preds9))] = c("FMI9", "FFMI9","laps20m9", "Speed9", "Str9")
  }
  
  # store output
  preds9[i, (ncol(preds9) - 4):(ncol(preds9))] = c(as.numeric(ilr.m9_bc[1, c("FMI9", "FFMI9")]),
                                                   as.numeric(ilr.m9_pf[1, c("laps20m9", "Speed9", "Str9")]))
  
}
detach(dat)

out = cbind(out, preds9)
out = round(out, 3)
for (i in 2:nrow(out)) out[i,4:8] = out[i,4:8] - out[1,4:8]
for (i in 2:nrow(out)) out[i,17:26] = out[i,17:26] - out[1,17:26]



usethis::use_data(out, dat, fCompo1_BC.sex.age, fCompo1_PF.sex.age, internal = TRUE, overwrite = TRUE)
