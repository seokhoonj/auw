
.AUW_ENV <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  assign(".ACCIDENT"        , "[STV]|W[0-5]|W6[0-4]|W8[5-9]|W9[0-9]|X[0-24]|X5[15689]|X8[5-9]|X9|Y[0-2457]|Y3[0-46-9]|Y35[0-46-9]|Y8[0-4]", envir = .AUW_ENV)
  assign(".ANGINA"          , "I20", envir = .AUW_ENV)
  assign(".BORDERLINE_TUMOR", "D3[7-9]|D4[0-4]|D47[026-9]|D48", envir = .AUW_ENV)
  assign(".CI10"            , "B2[0-4]|C[0-9][0-9]|D0[0-79]|D4[56]|D47[1345]|E1[0-4]|G590|G632|H280|H360|I0[89]|I1[0-5]|I2[0-3]|I3[4-9]|I6[0-3]|K70[23]|K717|K74|K761|L412|M142|N083|N18[3-5]|T82|Z21|Z95", envir = .AUW_ENV)
  assign(".CANCER"          , "C0[0-9]|C1[0-9]|C2[0-6]|C3[0-4789]|C4[013-9]|C5[0-8]|C6[0-9]|C7[0-9]|C8[0-68]|C9[0-7]|D0[0-79]|D4[56]|D47[1345]", envir = .AUW_ENV)
  assign(".CANCER_GENERAL"  , "C0[0-9]|C1[0-9]|C2[0-6]|C3[0-4789]|C4[0135-9]|C5[0-8]|C6[0-9]|C7[0124-9]|C8[0-68]|C9[0-7]|D4[56]|D47[1345]", envir = .AUW_ENV)
  assign(".CANCER_SKIN"     , "C44", envir = .AUW_ENV)
  assign(".CANCER_PSEUDO"   , "C44|C73|D0[0-79]|D3[7-9]|D4[0-4]|D47[026-9]|D48", envir = .AUW_ENV)
  assign(".CANCER_BREAST"   , "C50", envir = .AUW_ENV)
  assign(".CANCER_SMALL"    , "C5[034]|C6[17]", envir = .AUW_ENV)
  assign(".CANCER_UTERI"    , "C5[3-5]", envir = .AUW_ENV)
  assign(".CANCER_PROSTATE" , "C61", envir = .AUW_ENV)
  assign(".CANCER_BLADDER"  , "C67", envir = .AUW_ENV)
  assign(".CANCER_THYROID"  , "C73", envir = .AUW_ENV)
  assign(".CIRRHOSIS"       , "K70[23]|K717|K74|K761", envir = .AUW_ENV)
  assign(".CIS"             , "D0[0-79]", envir = .AUW_ENV) # Carcinoma in situ
  assign(".DIABETES"        , "E1[0-4]|G590|G632|H280|H360|M142|N083", envir = .AUW_ENV)
  assign(".HIV"             , "B2[0-4]|Z21", envir = .AUW_ENV)
  assign(".HYPERTENSION"    , "I1[0-5]", envir = .AUW_ENV)
  assign(".STROKE"          , "I6[0-356]", envir = .AUW_ENV)
  assign(".VALVE"           , "I0[89]|I3[4-9]|T82|Z95", envir = .AUW_ENV)
}

.onAttach <- function(libname, pkgname) {
  pkg_env <- as.environment(paste0("package:", pkgname))

  vars <- ls(.AUW_ENV, all.names = TRUE)

  for (var in vars) {
    assign(var, get(var, envir = .AUW_ENV, inherits = FALSE), envir = pkg_env)
  }
}

# 질병에 대한 할증 지수 개발 계산 적정성 여부 검사
# 어떤 방법을 써야 하는지 etc

