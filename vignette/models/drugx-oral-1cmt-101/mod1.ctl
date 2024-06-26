$PROB ev_no-delay_first-order_one-compartment_linear

$INPUT ID TIME EVID AMT DV RACEN WTKG Race=DROP

$DATA "../../data/F1003-Asian-PK-Study.csv" IGNORE=@

$SUBROUTINES ADVAN13 TRANS1 TOL=9 SUBROUTINES=D

$MODEL
COMP=(DEPOT,DEFDOSE)
COMP=(CENTRAL,DEFOBS)

$PK

TVKA = THETA(1)
KA = TVKA

TVCL = THETA(2)*(WTKG/71.0)**THETA(4)
CL = TVCL*EXP(ETA(1))

TVV = THETA(3)*(WTKG/71.0)**THETA(5)
V = TVV*EXP(ETA(2))
S2 = V

$DES

DADT(1) = -KA*A(1)
DADT(2) = KA*A(1) - (CL/V)*A(2)

$ERROR 

IPRED = F		
IRES = DV-IPRED   
W = SQRT(IPRED**2*SIGMA(1,1))
IWRES = IRES/W
Y = F + F*EPS(1)

$THETA
(0, 0.783) ; KA ; 1/hr ; Absorption Rate Constant
(0, 17.3)  ; CL ; L/hr ; Clearance
(0, 84.0)  ; V  ; L    ; Volume of Distribution
(1.83)     ; WTonCL
(1)        ; WTonV

$OMEGA BLOCK(2) 
0.0103 ; CL
0.00 0.111 ; V

$SIGMA 
0.0206 ; Proportional 

$ESTIMATION METHOD=1 INTER NOABORT MAXEVAL=9999 PRINT=5 NSIG=3 SIGL=9 NOTHETABOUNDTEST 
NOOMEGABOUNDTEST NOSIGMABOUNDTEST 

$COVARIANCE PRINT=E UNCONDITIONAL

$TABLE ID TIME DV MDV EVID IPRED CWRES IWRES ONEHEADER NOPRINT FILE=sdtab1
$TABLE ID KA CL V ETAS(1:LAST) ONEHEADER NOPRINT FILE=patab1 ; model parameters
$TABLE ID RACEN ONEHEADER NOPRINT FILE=catab1 ; categorical covariates
$TABLE ID WTKG ONEHEADER NOPRINT FILE=cotab1 ; continuous covariates