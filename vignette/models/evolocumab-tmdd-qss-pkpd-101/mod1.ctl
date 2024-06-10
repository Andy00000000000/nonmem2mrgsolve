$PROB Simultaneous PKPD of the human anti-PCSK9 mAb evolocumab

$ABBR PROTECT

$INPUT ID TIME DV AMT ADDL II CMT MDV EVID DOSEMG HEALTHY

$DATA "../../data/NM_data_evoPKPD.csv" IGNORE=@

$SUBROUTINES ADVAN13 TRANS1 TOL=6

$MODEL
COMP = (DEPOT, DEFDOSE)            ; evolocumab SC depot
COMP = (TDA)                       ; total evolocumab amount in central
COMP = (TLC, NODOSE)               ; total ligand (PCSK9) concentration in central
COMP = (CLDL, NODOSE)              ; LDL cholesterol concentration

$PK

CALLFL=-2

;;;;; TMDD PK ;;;;;

TVKA = THETA(1)
KA = TVKA*EXP(ETA(1))

TVV = THETA(2)
V = TVV*EXP(ETA(2))

TVCL = THETA(3)
CL = TVCL*EXP(ETA(3))

TVKDEG = THETA(4)
KDEG = TVKDEG*EXP(ETA(4))

TVPCSK9_0 = THETA(5)*THETA(8)**(HEALTHY)
PCSK9_0 = TVPCSK9_0*EXP(ETA(5))

TVKSS = THETA(6)
KSS = TVKSS*EXP(ETA(6))

TVKINT = THETA(7)
KINT = TVKINT*EXP(ETA(7))

KEL = CL/V

KSYN = KDEG*PCSK9_0  ; zero-order PCSK9 production rate (nM/day)

F1 = THETA(9)

A_0(3) = PCSK9_0     ; change initial condition from default of zero

;;;;; PD ;;;;;

TVKOUT = THETA(10)
KOUT = TVKOUT*EXP(ETA(8))

TVLDLC_0 = THETA(11)
LDLC_0 = TVLDLC_0*EXP(ETA(9))

TVIMAX = THETA(12)
IMAX = TVIMAX*EXP(ETA(10))

TVIC50 = THETA(13)
IC50 = TVIC50*EXP(ETA(11))

INH0 = (IMAX*PCSK9_0)/(IC50+PCSK9_0)

KIN = KOUT*(1.0-INH0)*LDLC_0    ; zero-order LDL-C production rate (mg/dL/day)

A_0(4) = LDLC_0               ; change initial condition from default of zero

$DES

;;;;; TMDD PK ;;;;;

TDC = A(2)/V                                                         ; total evolocumab concentration (nM)
FDC = 0.5*((TDC-A(3)-KSS) + SQRT((TDC-A(3)-KSS)**2.0 + 4.0*KSS*TDC)) ; free evolocumab concentration (nM)
FLC = A(3) - (TDC - FDC)                                             ; free PCSK9 concentration (nM)

DADT(1) = -KA*A(1)                                            ; evolocumab SC depot (nmol)
DADT(2) =  KA*A(1) - KEL*FDC*V - (KINT*A(3)*FDC*V)/(KSS+FDC)  ; total evolocumab amount in central (nmol)
DADT(3) = KSYN - KDEG*A(3) - ((KINT-KDEG)*FDC*A(3))/(KSS+FDC) ; total PCSK9 concentration in central (nM)

;;;;; PD ;;;;;

INH = (IMAX*PCSK9_0)/(IC50+PCSK9_0)
IF (TIME.GT.0) INH = (IMAX*FLC)/(IC50+FLC)

DADT(4) = KIN - KOUT*(1.0-INH)*A(4)                           ; LDL-C concentration (mg/dL)

$ERROR

TDC2 = A(2)/V                                                            ; total evolocumab concentration (nM)
FDC2 = 0.5*((TDC2-A(3)-KSS) + SQRT((TDC2-A(3)-KSS)**2.0 + 4.0*KSS*TDC2)) ; free evolocumab concentration (nM)
FLC2 = A(3) - (TDC2 - FDC2)                                              ; free PCSK9 concentration (nM)

DRUG=0
IF(CMT.EQ.2)DRUG=1

LIG=0
IF(CMT.EQ.3)LIG=1

LDL=0
IF(CMT.EQ.4)LDL=1

IF(CMT.EQ.2)THEN
  IPRED = FDC2*144/1000	; convert nM to mcg/mL, approximate MW of evolocumab is 144 kDa
  IRES = DV-IPRED
  W = SQRT(IPRED**2*SIGMA(1,1)+SIGMA(2,2))
  IWRES = IRES/W
ENDIF

IF(CMT.EQ.3)THEN
  IPRED = FLC2
  IRES = DV-IPRED
  W = SQRT(IPRED**2*SIGMA(3,3)+SIGMA(4,4))
  IWRES = IRES/W
ENDIF

IF(CMT.EQ.4)THEN
  IPRED = F
  IRES = DV-IPRED
  W = SQRT(IPRED**2*SIGMA(5,5)+SIGMA(6,6))
  IWRES = IRES/W
ENDIF

Y1 = IPRED + IPRED*EPS(1) + EPS(2)
Y2 = IPRED + IPRED*EPS(3) + EPS(4)
Y3 = IPRED + IPRED*EPS(5) + EPS(6)

Y = Y1*DRUG + Y2*LIG + Y3*LDL

$THETA
0.2;        KA;              1/day ; first-order evolocumab absorption rate constant
3;          V;               L     ; evolocumab volume of distribution
0.3;        CL;              L/day ; evolocumab clearance
2;          KDEG;            1/day ; first-order PCSK9 degradation rate constant
5;          PCSK9_0;         nM    ; initial PCSK9 concentration
0.3;        KSS;             nM    ; steady-state constant
0.05;       KINT;            1/day ; first-order evolocumab-PCSK9 complex elimination rate constant
0.6;        PCSK9_0_HEALTHY; none  ; fold change in initial PCSK9 conc. for healthy vs. statin-treated
0.72 FIXED; F1;              none  ; evolocumab SC bioavailability
0.3;        KOUT;            1/day ; first-order elimination rate constant for LDL-C
100;        LDLC_0;          mg/dL ; baseline LDL-C concentration
1 FIXED;    IMAX;            none  ; maximal inhibition
1;          IC50;            nM    ; serum unbound PCSK9 concentration associated with half-maximal inhibition

$OMEGA BLOCK(1)
0.1;            KA

$OMEGA BLOCK(1)
0.03;           V

$OMEGA BLOCK(1)
0.1;            CL

$OMEGA BLOCK(1)
0 FIXED;        KDEG

$OMEGA BLOCK(1)
0.03;           PCSK9_0

$OMEGA BLOCK(1)
0.1;            KSS

$OMEGA BLOCK(1)
0 FIXED;        KINT

$OMEGA BLOCK(1)
0 FIXED;        KOUT

$OMEGA BLOCK(1)
0.04;           LDLC_0

$OMEGA BLOCK(1)
0 FIXED;        IMAX

$OMEGA BLOCK(1)
0.1;            IC50

$SIGMA
0.01    ; Evolocumab_Prop
0 FIXED ; Evolocumab_Add
0.01    ; PCSK9_Prop
0 FIXED ; PCSK9_Add
0.01    ; LDLC_Prop
20      ; LDLC_Add

$ESTIMATION METHOD=1 INTER NOHABORT MAXEVAL=9999 PRINT=5 NSIG=1 SIGL=3

$COVARIANCE PRINT=E MATRIX=S UNCONDITIONAL

;*...............80 Character limit............................................*
$TABLE ID TIME DV CMT MDV EVID IPRED CWRES IWRES ONEHEADER NOPRINT FILE=sdtab1
$TABLE ID KA V CL KDEG PCSK9_0 KSS KINT KOUT LDLC_0 IMAX IC50 ONEHEADER
       NOPRINT FILE=patab1 ; model parameters
$TABLE ID DOSEMG HEALTHY ONEHEADER NOPRINT FILE=catab1 ; categorical covariates
$TABLE ID ONEHEADER NOPRINT FILE=cotab1 ; continuous covariates