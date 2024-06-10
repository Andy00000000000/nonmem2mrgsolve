code <- '
$PROB Mavoglurant PBPK (Wendling et al., 2016. DOI: 10.1208/s12248-015-9840-7) presented as an nlmixr2 example (https://nlmixr2.org/articles/mavoglurant.html)

$PARAM

WTKG=NA_real_
INRATE=NA_real_

$THETA

7.2 2 0.45 2.3 -0.19 -10

$CMT

LU
HT
BR
MU
AD
SK
SP
PA
LI
ST
GU
BO
KI
AB
VB
RB

$MAIN

/* NOTE: There is no guarantee to the accuracy of the NONMEM to mrgsolve translator. It remains the responsibility of the user to validate the resulting mrgsolve code. */

/* NOTE: The nonmem2mrgsolve package remains in active development, please report bugs and feature requests to improve future versions. */
/* NOTE: If you find nonmem2mrgsolve helpful, please consider giving it a star at github.com/Andy00000000000/nonmem2mrgsolve. */

/* NOTE: The translator does not currently convert T or TIME to SOLVERTIME. */
/* NOTE: The translator does not currently convert MTIME() to self.mtime(). */

/* CALLFL=-2; */
R_VB=INRATE;
double lCLint = THETA1;
double MU_1 = lCLint;
double CLint = exp(MU_1+ETA(1));
double KbBR = exp(THETA2);
double KbMU = exp(THETA3);
double KbAD = exp(THETA4);
double KbBO = exp(THETA5);
double KbRB = exp(THETA6);
double CO  = (187.00*pow(WTKG,0.81))*60.0/1000.0;
double QHT = 4.0 *CO/100.0;
double QBR = 12.0*CO/100.0;
double QMU = 17.0*CO/100.0;
double QAD = 5.0 *CO/100.0;
double QSK = 5.0 *CO/100.0;
double QSP = 3.0 *CO/100.0;
double QPA = 1.0 *CO/100.0;
double QLI = 25.5*CO/100.0;
double QST = 1.0 *CO/100.0;
double QGU = 14.0*CO/100.0;
double QHA = QLI - (QSP + QPA + QST + QGU);
double QBO = 5.0 *CO/100.0;
double QKI = 19.0*CO/100.0;
double QRB = CO - (QHT + QBR + QMU + QAD + QSK + QLI + QBO + QKI);
double QLU = QHT + QBR + QMU + QAD + QSK + QLI + QBO + QKI + QRB;
double VLU = (0.76 *WTKG/100.0)/1.051;
double VHT = (0.47 *WTKG/100.0)/1.030;
double VBR = (2.00 *WTKG/100.0)/1.036;
double VMU = (40.00*WTKG/100.0)/1.041;
double VAD = (21.42*WTKG/100.0)/0.916;
double VSK = (3.71 *WTKG/100.0)/1.116;
double VSP = (0.26 *WTKG/100.0)/1.054;
double VPA = (0.14 *WTKG/100.0)/1.045;
double VLI = (2.57 *WTKG/100.0)/1.040;
double VST = (0.21 *WTKG/100.0)/1.050;
double VGU = (1.44 *WTKG/100.0)/1.043;
double VBO = (14.29*WTKG/100.0)/1.990;
double VKI = (0.44 *WTKG/100.0)/1.050;
double VAB = (2.81 *WTKG/100.0)/1.040;
double VVB = (5.62 *WTKG/100.0)/1.040;
double VRB = (3.86 *WTKG/100.0)/1.040;
double BP = 0.61;
double fup = 0.028;
double fub = fup/BP;
double KbLU = exp(0.8334);
double KbHT = exp(1.1205);
double KbSK = exp(-.5238);
double KbSP = exp(0.3224);
double KbPA = exp(0.3224);
double KbLI = exp(1.7604);
double KbST = exp(0.3224);
double KbGU = exp(1.2026);
double KbKI = exp(1.3171);
double S15 = VVB*BP/1000.0;

$ODE

dxdt_LU=QLU*(VB/VVB - LU/KbLU/VLU);
dxdt_HT=QHT*(AB/VAB - HT/KbHT/VHT);
dxdt_BR=QBR*(AB/VAB - BR/KbBR/VBR);
dxdt_MU=QMU*(AB/VAB - MU/KbMU/VMU);
dxdt_AD=QAD*(AB/VAB - AD/KbAD/VAD);
dxdt_SK=QSK*(AB/VAB - SK/KbSK/VSK);
dxdt_SP=QSP*(AB/VAB - SP/KbSP/VSP);
dxdt_PA=QPA*(AB/VAB - PA/KbPA/VPA);
dxdt_LI=QHA*AB/VAB + QSP*SP/KbSP/VSP + QPA*PA/KbPA/VPA + QST*ST/KbST/VST + QGU*GU/KbGU/VGU - CLint*fub*LI/KbLI/VLI - QLI*LI/KbLI/VLI;
dxdt_ST=QST*(AB/VAB - ST/KbST/VST);
dxdt_GU=QGU*(AB/VAB - GU/KbGU/VGU);
dxdt_BO=QBO*(AB/VAB - BO/KbBO/VBO);
dxdt_KI=QKI*(AB/VAB - KI/KbKI/VKI);
dxdt_AB=QLU*(LU/KbLU/VLU - AB/VAB);
dxdt_VB=QHT*HT/KbHT/VHT + QBR*BR/KbBR/VBR + QMU*MU/KbMU/VMU + QAD*AD/KbAD/VAD + QSK*SK/KbSK/VSK + QLI*LI/KbLI/VLI + QBO*BO/KbBO/VBO + QKI*KI/KbKI/VKI + QRB*RB/KbRB/VRB - QLU*VB/VVB;
dxdt_RB=QRB*(AB/VAB - RB/KbRB/VRB);
double C15=VB/S15;

$OMEGA @block

0.083

$CAPTURE

C15
WTKG
CLint

'
