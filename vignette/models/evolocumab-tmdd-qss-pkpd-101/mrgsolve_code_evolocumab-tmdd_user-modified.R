code <- '
$PROB Simultaneous PKPD of the human anti-PCSK9 mAb evolocumab

$PARAM

HEALTHY=NA_real_

$THETA

0.227505 2.71313 0.276166 2.11422 5.88232 0.265911 0.0528221 0.58245 0.72 0.304556 113.171 1 1.40841

$CMT

DEPOT
TDA
TLC
CLDL

$MAIN

/* NOTE: There is no guarantee to the accuracy of the NONMEM to mrgsolve translator. It remains the responsibility of the user to validate the resulting mrgsolve code. */

/* NOTE: The nonmem2mrgsolve package remains in active development, please report bugs and feature requests to improve future versions. */
/* NOTE: If you find nonmem2mrgsolve helpful, please consider giving it a star at github.com/Andy00000000000/nonmem2mrgsolve. */

/* NOTE: The translator does not currently convert T or TIME to SOLVERTIME. */
/* NOTE: The translator does not currently convert MTIME() to self.mtime(). */

/* CALLFL=-2; */
double TVKA = THETA1;
double KA = TVKA*exp(ETA(1));
double TVV = THETA2;
double V = TVV*exp(ETA(2));
double TVCL = THETA3;
double CL = TVCL*exp(ETA(3));
double TVKDEG = THETA4;
double KDEG = TVKDEG*exp(ETA(4));
double TVPCSK9_0 = THETA5*pow(THETA8,(HEALTHY));
double PCSK9_0 = TVPCSK9_0*exp(ETA(5));
double TVKSS = THETA6;
double KSS = TVKSS*exp(ETA(6));
double TVKINT = THETA7;
double KINT = TVKINT*exp(ETA(7));
double KEL = CL/V;
double KSYN = KDEG*PCSK9_0;
F_DEPOT=THETA9;
TLC_0=PCSK9_0;
double TVKOUT = THETA10;
double KOUT = TVKOUT*exp(ETA(8));
double TVLDLC_0 = THETA11;
double LDLC_0 = TVLDLC_0*exp(ETA(9));
double TVIMAX = THETA12;
double IMAX = TVIMAX*exp(ETA(10));
double TVIC50 = THETA13;
double IC50 = TVIC50*exp(ETA(11));
double INH0 = (IMAX*PCSK9_0)/(IC50+PCSK9_0);
double KIN = KOUT*(1.0-INH0)*LDLC_0;
CLDL_0=LDLC_0;

$ODE

double TDC=TDA/V;
double FDC=0.5*((TDC-TLC-KSS) + sqrt(pow((TDC-TLC-KSS),2.0 )+ 4.0*KSS*TDC));
double FLC=TLC - (TDC - FDC);
dxdt_DEPOT=-KA*DEPOT;
dxdt_TDA=KA*DEPOT - KEL*FDC*V - (KINT*TLC*FDC*V)/(KSS+FDC);
dxdt_TLC=KSYN - KDEG*TLC - ((KINT-KDEG)*FDC*TLC)/(KSS+FDC);
double INH=(IMAX*PCSK9_0)/(IC50+PCSK9_0);
if(SOLVERTIME>0){
INH=(IMAX*FLC)/(IC50+FLC);
}
dxdt_CLDL=KIN - KOUT*(1.0-INH)*CLDL;

$OMEGA @block

0.0709136
0 0.0402987
0 0 0.11236
0 0 0 0
0 0 0 0 0.041995
0 0 0 0 0 0.0869048
0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0.0375891
0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0.127478

$CAPTURE

KA
V
CL
KDEG
PCSK9_0
KSS
KINT
KOUT
LDLC_0
IMAX
IC50
HEALTHY

'
