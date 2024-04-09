
.AUW_ENV <- NULL
.onLoad <- function(libname, pkgname) {
  .AUW_ENV <<- new.env()
  assign("CI10"           , "B2[0-4]|C[0-9][0-9]|D0[0-79]|D4[56]|D47[1345]|E1[0-4]|G590|G632|H280|H360|I0[89]|I1[0-5]|I2[0-3]|I3[4-9]|I6[0-3]|K70[23]|K717|K74|K761|L412|M142|N083|N18[3-5]|T82|Z21|Z95", envir = .AUW_ENV)
  assign("HIV"            , "B2[0-4]|Z21", envir = .AUW_ENV)
  assign("CANCER"         , "C0[0-9]|C1[0-9]|C2[0-6]|C3[0-4789]|C4[013-9]|C5[0-8]|C6[0-9]|C7[0-9]|C8[0-68]|C9[0-7]|D0[0-79]|D4[56]|D47[1345]", envir = .AUW_ENV)
  assign("GENERALCANCER"  , "C0[0-9]|C1[0-9]|C2[0-6]|C3[0-4789]|C4[0135-9]|C5[0-8]|C6[0-9]|C7[0124-9]|C8[0-68]|C9[0-7]|D4[56]|D47[1345]", envir = .AUW_ENV)
  assign("SKINCANCER"     , "C44", envir = .AUW_ENV)
  assign("PSEUDOCANCER"   , "C44|C73|D0[0-79]|D3[7-9]|D4[0-4]|D47[026-9]|D48", envir = .AUW_ENV)
  assign("BREASTCANCER"   , "C50", envir = .AUW_ENV)
  assign("SMALLCANCER"    , "C5[034]|C6[17]", envir = .AUW_ENV) # 소액암
  assign("UTERICANCER"    , "C5[3-5]", envir = .AUW_ENV)
  assign("PROSTATECANCER" , "C61", envir = .AUW_ENV)
  assign("BLADDERCANCER"  , "C67", envir = .AUW_ENV)
  assign("THYROIDCANCER"  , "C73", envir = .AUW_ENV)
  assign("CIS"            , "D0[0-79]", envir = .AUW_ENV) # Carcinoma in situ
  assign("BORDERLINETUMOR", "D3[7-9]|D4[0-4]|D47[026-9]|D48", envir = .AUW_ENV)
  assign("DIABETES"       , "E1[0-4]|G590|G632|H280|H360|M142|N083", envir = .AUW_ENV)
  assign("VALVE"          , "I0[89]|I3[4-9]|T82|Z95", envir = .AUW_ENV) # 심장판막
  assign("HYPERTENSION"   , "I1[0-5]", envir = .AUW_ENV)
  assign("ANGINA"         , "I20", envir = .AUW_ENV) # 협심증
  assign("STROKE"         , "I6[0-356]", envir = .AUW_ENV) # 뇌졸중
  assign("CIRRHOSIS"      , "K70[23]|K717|K74|K761", envir = .AUW_ENV) # 간경화
  assign("ACCIDENT"       , "[STV]|W[0-5]|W6[0-4]|W8[5-9]|W9[0-9]|X[0-24]|X5[15689]|X8[5-9]|X9|Y[0-2457]|Y3[0-46-9]|Y35[0-46-9]|Y8[0-4]", envir = .AUW_ENV)
}

