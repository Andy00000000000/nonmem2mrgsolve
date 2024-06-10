$PROB Mavoglurant PBPK (Wendling et al., 2016. DOI: 10.1208/s12248-015-9840-7) presented as an nlmixr2 example (https://nlmixr2.org/articles/mavoglurant.html)

$INPUT ID CMT EVID EVI2 MDV DV LNDV AMT TIME DOSE OCC RATE AGE SEX RACE WTKG HTM BLQ

$DATA ../../data/Mavoglurant_A2121_nmpk_edited.csv
    IGNORE=I
    IGNORE=(OCC.GT.1)  ; Match data subsets of nlmixr2 vignette
    IGNORE=(ID.GE.812) ; Match data subsets of nlmixr2 vignette
    IGNORE=(BLQ.EQ.1)  ; Match data subsets of nlmixr2 vignette

$SUBROUTINES ADVAN13 TRANS1 TOL=6 SUBROUTINES=D

$MODEL 
  COMP=(LU)                ; 01
  COMP=(HT)                ; 02
  COMP=(BR)                ; 03
  COMP=(MU)                ; 04
  COMP=(AD)                ; 05
  COMP=(SK)                ; 06
  COMP=(SP)                ; 07
  COMP=(PA)                ; 08
  COMP=(LI)                ; 09
  COMP=(ST)                ; 10
  COMP=(GU)                ; 11
  COMP=(BO)                ; 12
  COMP=(KI)                ; 13
  COMP=(AB)                ; 14
  COMP=(VB,DEFDOSE,DEFOBS) ; 15
  COMP=(RB)                ; 16

$PK
  ;--- Initial

  CALLFL=-2
  R15=RATE

  ;--- Estimated Parameters

  lCLint = THETA(1)
  MU_1 = lCLint
  CLint = EXP(MU_1+ETA(1))

  KbBR = EXP(THETA(2))
  KbMU = EXP(THETA(3))
  KbAD = EXP(THETA(4))
  KbBO = EXP(THETA(5))
  KbRB = EXP(THETA(6))

  ;--- Fixed Physiological Parameters

  CO  = (187.00*WTKG**0.81)*60.0/1000.0 ; Cardiac output (L/h) from White et al (1968) as presented by nlmixr2 vignette
  QHT = 4.0 *CO/100.0
  QBR = 12.0*CO/100.0
  QMU = 17.0*CO/100.0
  QAD = 5.0 *CO/100.0
  QSK = 5.0 *CO/100.0
  QSP = 3.0 *CO/100.0
  QPA = 1.0 *CO/100.0
  QLI = 25.5*CO/100.0
  QST = 1.0 *CO/100.0
  QGU = 14.0*CO/100.0
  QHA = QLI - (QSP + QPA + QST + QGU) ; Hepatic artery blood flow
  QBO = 5.0 *CO/100.0
  QKI = 19.0*CO/100.0
  QRB = CO - (QHT + QBR + QMU + QAD + QSK + QLI + QBO + QKI)
  QLU = QHT + QBR + QMU + QAD + QSK + QLI + QBO + QKI + QRB

  VLU = (0.76 *WTKG/100.0)/1.051
  VHT = (0.47 *WTKG/100.0)/1.030
  VBR = (2.00 *WTKG/100.0)/1.036
  VMU = (40.00*WTKG/100.0)/1.041
  VAD = (21.42*WTKG/100.0)/0.916
  VSK = (3.71 *WTKG/100.0)/1.116
  VSP = (0.26 *WTKG/100.0)/1.054
  VPA = (0.14 *WTKG/100.0)/1.045
  VLI = (2.57 *WTKG/100.0)/1.040
  VST = (0.21 *WTKG/100.0)/1.050
  VGU = (1.44 *WTKG/100.0)/1.043
  VBO = (14.29*WTKG/100.0)/1.990
  VKI = (0.44 *WTKG/100.0)/1.050
  VAB = (2.81 *WTKG/100.0)/1.040
  VVB = (5.62 *WTKG/100.0)/1.040
  VRB = (3.86 *WTKG/100.0)/1.040

  ;--- Fixed Drug-specific Parameters

  BP = 0.61    ; Blood:plasma partition coefficient
  fup = 0.028  ; Fraction unbound in plasma
  fub = fup/BP ; Fraction unbound in blood

  KbLU = exp(0.8334)
  KbHT = exp(1.1205)
  KbSK = exp(-.5238)
  KbSP = exp(0.3224)
  KbPA = exp(0.3224)
  KbLI = exp(1.7604)
  KbST = exp(0.3224)
  KbGU = exp(1.2026)
  KbKI = exp(1.3171)

  ;--- Finalize

  S15 = VVB*BP/1000.0

$DES

  DADT(1) = QLU*(A(15)/VVB - A(1)/KbLU/VLU)
  DADT(2) = QHT*(A(14)/VAB - A(2)/KbHT/VHT)
  DADT(3) = QBR*(A(14)/VAB - A(3)/KbBR/VBR)
  DADT(4) = QMU*(A(14)/VAB - A(4)/KbMU/VMU)
  DADT(5) = QAD*(A(14)/VAB - A(5)/KbAD/VAD)
  DADT(6) = QSK*(A(14)/VAB - A(6)/KbSK/VSK)
  DADT(7) = QSP*(A(14)/VAB - A(7)/KbSP/VSP)
  DADT(8) = QPA*(A(14)/VAB - A(8)/KbPA/VPA)
  DADT(9) = QHA*A(14)/VAB + QSP*A(7)/KbSP/VSP + QPA*A(8)/KbPA/VPA + QST*A(10)/KbST/VST + QGU*A(11)/KbGU/VGU - CLint*fub*A(9)/KbLI/VLI - QLI*A(9)/KbLI/VLI
  DADT(10) = QST*(A(14)/VAB - A(10)/KbST/VST)
  DADT(11) = QGU*(A(14)/VAB - A(11)/KbGU/VGU)
  DADT(12) = QBO*(A(14)/VAB - A(12)/KbBO/VBO)
  DADT(13) = QKI*(A(14)/VAB - A(13)/KbKI/VKI)
  DADT(14) = QLU*(A(1)/KbLU/VLU - A(14)/VAB)
  DADT(15) = QHT*A(2)/KbHT/VHT + QBR*A(3)/KbBR/VBR + QMU*A(4)/KbMU/VMU + QAD*A(5)/KbAD/VAD + QSK*A(6)/KbSK/VSK + QLI*A(9)/KbLI/VLI + QBO*A(12)/KbBO/VBO + QKI*A(13)/KbKI/VKI + QRB*A(16)/KbRB/VRB - QLU*A(15)/VVB
  DADT(16) = QRB*(A(14)/VAB - A(16)/KbRB/VRB)

  C15 = A(15)/S15

$THETA
  (7.6)   ; lCLint
  (1.1)   ; lKbBR
  (0.3)   ; lKbMU
  (2.0)   ; lKbAD
  (0.03)  ; lKbBO
  (0.3)   ; lKbRB

$OMEGA
(0, 4.00) ; IIV on CLint

$SIGMA
10.0      ; proportional residual variability
1.00      ; additive residual variability

$ERROR

  IPRED = A(15)/S15
  IRES=DV-IPRED
  W=SQRT(IPRED**2*SIGMA(1,1)+SIGMA(2,2))
  IWRES=IRES/W
  F_FLAG = 0
  Y = IPRED + IPRED*EPS(1) + EPS(2)

; $EST METHOD=IMP INTERACTION NOABORT PRINT=5 NOABORT AUTO=1 RANMETHOD=S2P CTYPE=3 NITER=5000

$EST METHOD=CONDITIONAL INTERACTION MAXEVAL=9999 PRINT=1 NOABORT NOTBT NOOBT NOSBT NSIG=2 SIGL=6

$COV UNCONDITIONAL

$TABLE ID EVID CMT DOSE C15 WTKG CLint
ETAS(1:LAST) NPDE CWRES IWRES IPRED NOPRINT ONEHEADER FILE=mod1.tbl
