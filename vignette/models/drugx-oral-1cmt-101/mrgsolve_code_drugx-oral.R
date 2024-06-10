code <- ' 
$PROB ev_no-delay_first-order_one-compartment_linear

$PARAM

WTKG=NA_real_

$THETA

0.782 17.3 85.8 1.85 1.58

$CMT

DEPOT
CENTRAL

$MAIN

/* NOTE: There is no guarantee to the accuracy of the NONMEM to mrgsolve translator. It remains the responsibility of the user to validate the resulting mrgsolve code. */

/* NOTE: The nonmem2mrgsolve package remains in active development, please report bugs and feature requests to improve future versions. */
/* NOTE: If you find nonmem2mrgsolve helpful, please consider giving it a star at github.com/Andy00000000000/nonmem2mrgsolve. */

/* NOTE: The translator does not currently convert T or TIME to SOLVERTIME. */
/* NOTE: The translator does not currently convert MTIME() to self.mtime(). */

double TVKA = THETA1;
double KA = TVKA;
double TVCL = THETA2*pow((WTKG/71.0),THETA4);
double CL = TVCL*exp(ETA(1));
double TVV = THETA3*pow((WTKG/71.0),THETA5);
double V = TVV*exp(ETA(2));
double S2 = V;

$ODE

dxdt_DEPOT=-KA*DEPOT;
dxdt_CENTRAL=KA*DEPOT - (CL/V)*CENTRAL;

$OMEGA @block

0.0105
0 0.00765

$CAPTURE

KA
CL
V
WTKG

'