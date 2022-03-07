#' get_MS_predictions
#'
#' @description Get predictions from SEM as presented in paper.
#' @return
#' @export
#'
#' @examples
get_MS_predictions = function(get_preds = TRUE) {
  # packages
  require(compositions)
  
  # # basic data ----
  # compo_names = c("VPA", "MPA", "LPA", "SB", "Sleep")
  
  # mean compositions and ilrs ----
  m4 = c(10.12452, 54.14555, 347.96264, 513.26918, 514.49811)
  m9 = c(12.71212, 55.71468, 293.33485, 536.14679, 542.09155)
  sbp <- matrix(c(  1, -1, -1, -1, -1,
                    0,  1, -1, -1, -1,
                    0,  0,  1, -1, -1,
                    0,  0,  0,  1, -1),
                ncol = 5, byrow = TRUE)
  psi <- compositions::gsi.buildilrBase(t(sbp))
  
  ilr.m4 = compositions::ilr(m4, V = psi)
  ilr.m9 = compositions::ilr(m9, V = psi)
  
  # new compositions for predictions ----
  # change in the ilr of interest (a = change in ilr of interest; b = change in the others to compensate a)
  
  # ILR 1
  a = c(1, seq((m4[1] - 10)/m4[1], (m4[1]+30)/m4[1], length.out = 20))
  b = (1440 - (m4[1]*a)) / (m4[2] + m4[3] + m4[4] + m4[5])
  ab_ilr1 = data.frame(ilr = c(0, rep(1, 20)), a = a, b = b,
                       # time-use composition at 4 years old
                       VPA4 = m4[1]*a, MPA4 = m4[2]*b, LPA4 = m4[3]*b, SB4 = m4[4]*b, Sleep4 = m4[5]*b)
  rm(a,b)
  
  # ILR 2
  # for the rest, b values calculated in https://www.symbolab.com/solver/equation-calculator
  a = c(seq((m4[2] - 30)/m4[2], (m4[2] + 30)/m4[2], length.out = 20))
  b = c(1.02305, 1.02057, 1.01812, 1.01567, 1.01324, 1.01081, 1.0084, 1.00599, 1.00359, 1.0012, 0.998806, 0.996421,0.994041, 0.991664, 0.989292, 0.986923, 0.984556, 0.982193, 0.979833, 0.977475)
  ab_ilr2 = data.frame(ilr = 2, a = a, b = b,
                       # time-use composition at 4 years old
                       VPA4 = m4[1]*(a*b^3)^(1/4), MPA4 = m4[2]*a, LPA4 = m4[3]*b, SB4 = m4[4]*b, Sleep4 = m4[5]*b)
  rm(a,b)
  
  # ILR 3
  # b values calculated in https://www.symbolab.com/solver/equation-calculator
  a = c(seq((m4[3] - 90)/m4[3], (m4[3] + 90)/m4[3], length.out = 20))
  b = c(1.09015, 1.08057, 1.07101, 1.06147, 1.05195, 1.04246, 1.03299, 1.02354, 1.01411, 1.0047, 0.995305, 0.985929, 0.97657, 0.967228, 0.957901, 0.94859, 0.939294, 0.930013, 0.920746, 0.911493)
  ab_ilr3 = data.frame(ilr = 3, a = a, b = b,
                       # time-use composition at 4 years old
                       VPA4 = m4[1]*((a*b*b)^(1/3))^(1/4) * (a*b*b)^(1/4), MPA4 = m4[2]*(a*b*b)^(1/3), LPA4 = m4[3]*a, SB4 = m4[4]*b, Sleep4 = m4[5]*b)
  rm(a,b)
  
  # ILR 4
  # b values calculated in https://www.symbolab.com/solver/equation-calculator
  a = c(seq((m4[4] - 300)/m4[4], (m4[4] + 300)/m4[4], length.out = 20))
  b = c(1.70747129300068, 1.61765, 1.53258396271130, 1.45163158459809, 1.37429607331649, 1.30019763836909, 1.22903672376019, 1.16057215920667, 1.09460734053028, 1.03098011732903, 0.969555539924622, 0.910221021541094, 0.852881845533020, 0.797458632034325, 0.743885001892015, 0.692105785227467, 0.642076049762841, 0.593759607742649, 0.547128623508218, 0.502163009780822)
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
  
  
  # Predictions of outcomes at 9 years old ----
  out[, 13:14] = NA; colnames(out)[13:14] = c("a9", "b9")
  out[, 15:19] = NA; colnames(out)[15:19] = c("VPA9", "MPA9", "LPA9", "SB9", "Sleep9")
  out[, 20:23] = NA; colnames(out)[20:23] = c("ilr1_fu", "ilr2_fu", "ilr3_fu", "ilr4_fu")
  
  # mean composition at 9
  out[1, 13:14] = 1; out[1, 15:19] = m9; out[1, 20:23] = ilr.m9
  
  # for changes in ilr1 at baseline
  what = which(out$ilr == 1)
  out$ilr1_fu[what] = ilr.m9[1] + (0.205*(out$ilr1_bl[what] - ilr.m4[1]))
  out$ilr2_fu[what] = ilr.m9[2]
  out$ilr3_fu[what] = ilr.m9[3]
  out$ilr4_fu[what] = ilr.m9[4]
  
  out$a9[what] = ilr.m9[1] / out$ilr1_fu[what]
  out$b9[what] = (1440 - (m9[1]*out$a9[what])) / (m9[2] + m9[3] + m9[4] + m9[5])
  
  out$VPA9[what] = m9[1] * out$a9[what]
  out$MPA9[what] = m9[2] * out$b9[what]
  out$LPA9[what] = m9[3] * out$b9[what]
  out$SB9[what] = m9[4] * out$b9[what]
  out$Sleep9[what] = m9[5] * out$b9[what]
  
  # for changes in ilr2 at baseline
  what = which(out$ilr == 2)
  out$ilr1_fu[what] = ilr.m9[1]
  out$ilr2_fu[what] = ilr.m9[2] + (0.181*(out$ilr2_bl[what] - ilr.m4[2]))
  out$ilr3_fu[what] = ilr.m9[3]
  out$ilr4_fu[what] = ilr.m9[4]
  
  # b: 1440 = 12.71212*(a*b*b*b)^(1/4) + 55.71468*a + 293.33485*b + 536.14679*b + 542.09155*b
  out$a9[what] = ilr.m9[2] / out$ilr2_fu[what]
  out$b9[what] = c(1.0029, 1.00248, 1.00211, 1.00176, 1.00144, 1.00115, 1.00087, 1.0006, 1.00035, 1.00011, 0.999888, 0.999671, 0.999463, 0.999263, 0.99907, 0.998884, 0.998704, 0.99853, 0.998361, 0.998197)
  
  out$VPA9[what] = m9[1] * (out$a9[what]*out$b9[what])^(1/4)
  out$MPA9[what] = m9[2] * out$a9[what]
  out$LPA9[what] = m9[3] * out$b9[what]
  out$SB9[what] = m9[4] * out$b9[what]
  out$Sleep9[what] = m9[5] * out$b9[what]
  
  # for changes in ilr3 at baseline
  what = which(out$ilr == 3)
  out$ilr1_fu[what] = ilr.m9[1]
  out$ilr2_fu[what] = ilr.m9[2] 
  out$ilr3_fu[what] = ilr.m9[3] + (0.046*(out$ilr3_bl[what] - ilr.m4[3]))
  out$ilr4_fu[what] = ilr.m9[4]
  
  # b: 1440 = 12.71212(((a·b^2)^(1/3))^(1/4) · (a·b^2)^(1/4)) + 55.71468((a·b^2)^(1/3)) + 293.33485·a + 1078.238b
  out$a9[what] = ilr.m9[3] / out$ilr3_fu[what]
  out$b9[what] = c(1.00798, 1.00707, 1.00618, 1.00531, 1.00446, 1.00362, 1.00279, 1.00198, 1.00118, 1.00039, 0.999612, 0.998843, 0.998084, 0.997333, 0.996589, 0.995853, 0.995124, 0.994402, 0.993685, 0.992974)
  
  out$VPA9[what] = m9[1]*((out$a9[what]*out$b9[what]*out$b9[what])^(1/3))^(1/4) * (out$a9[what]*out$b9[what]*out$b9[what])^(1/4)
  out$MPA9[what] = m9[2]*(out$a9[what]*out$b9[what]*out$b9[what])^(1/3)
  out$LPA9[what] = m9[3] * out$a9[what]
  out$SB9[what] = m9[4] * out$b9[what]
  out$Sleep9[what] = m9[5] * out$b9[what]
  
  # for changes in ilr4 at baseline
  what = which(out$ilr == 4)
  out$ilr1_fu[what] = ilr.m9[1]
  out$ilr2_fu[what] = ilr.m9[2] 
  out$ilr3_fu[what] = ilr.m9[3] 
  out$ilr4_fu[what] = ilr.m9[4] + (0.068*(out$ilr4_bl[what] - ilr.m4[4]))
  
  # b: 1440=(12.71212(ab)^(1/4))(sqrt(ab)^(1/4))((sqrt(ab)(ab))^(1/3))^(1/4)+55.71468(sqrt(ab)·(ab))^(1/3)+293.33485·sqrt(ab)+536.14679·a+542.09155b
  out$a9[what] = ilr.m9[4] / out$ilr4_fu[what]
  out$b9[what] = c(2.23452572344702, 2.20142290998172, 2.16392797754044, 2.12028600406237, 2.06791236173201,
                   2.00272094605176, 1.91770520479911, 1.79953880751793, 1.61887029173173, 1.29407698707066,
                   NA, NA, NA, NA, NA,
                   NA, NA, NA, NA, NA)
  
  out$VPA9[what] = m9[1]*(out$a9[what]*out$b9[what])^(1/4)*(sqrt(out$a9[what]*out$b9[what])^(1/4))*((sqrt(out$a9[what]*out$b9[what])*(out$a9[what]*out$b9[what]))^(1/3))^(1/4)
  out$MPA9[what] = m9[2]*(sqrt(out$a9[what]*out$b9[what])*(out$a9[what]*out$b9[what]))^(1/3)
  out$LPA9[what] = m9[3]*sqrt(out$a9[what]*out$b9[what])
  out$SB9[what] = m9[4] * out$a9[what]
  out$Sleep9[what] = m9[5] * out$b9[what]
  
  # clean out
  out = out[complete.cases(out), ]
  
  
  # BODY COMPOSITION AND PHYSICAL FITNESS PREDICTIONS ----
  out[, 24:27] = NA; colnames(out)[24:27] = c("FMI9", "CRF9", "Motor9", "Str9")
  
  # mean composition at 9
  out[1, 24:27] = 0
  
  # for changes in ilr1
  what = which(out$ilr == 1)
  out$FMI9[what] = (-0.174*(out$ilr1_fu[what] - ilr.m9[1]))
  out$CRF9[what] = (1.520*(out$ilr1_fu[what] - ilr.m9[1]))
  out$Motor9[what] = (-0.099*(out$ilr1_fu[what] - ilr.m9[1]))
  out$Str9[what] = (0.087*(out$ilr1_fu[what] - ilr.m9[1]))
  
  # for changes in ilr2
  what = which(out$ilr == 2)
  out$FMI9[what] = (-0.070*(out$ilr1_fu[what] - ilr.m9[1]))
  out$CRF9[what] = (0.706*(out$ilr1_fu[what] - ilr.m9[1]))
  out$Motor9[what] = (-0.047*(out$ilr1_fu[what] - ilr.m9[1]))
  out$Str9[what] = (0.047*(out$ilr1_fu[what] - ilr.m9[1]))
  
  # for changes in ilr3
  what = which(out$ilr == 3)
  out$FMI9[what] = (-0.019*(out$ilr1_fu[what] - ilr.m9[1]))
  out$CRF9[what] = (0.236*(out$ilr1_fu[what] - ilr.m9[1]))
  out$Motor9[what] = (-0.019*(out$ilr1_fu[what] - ilr.m9[1]))
  out$Str9[what] = (0.020*(out$ilr1_fu[what] - ilr.m9[1]))
  
  # for changes in ilr4
  what = which(out$ilr == 4)
  out$FMI9[what] = (0.038*(out$ilr1_fu[what] - ilr.m9[1]))
  out$CRF9[what] = (-0.217*(out$ilr1_fu[what] - ilr.m9[1]))
  out$Motor9[what] = (0.023*(out$ilr1_fu[what] - ilr.m9[1]))
  out$Str9[what] = (-0.024*(out$ilr1_fu[what] - ilr.m9[1]))
  
  # return
  return(out)
}