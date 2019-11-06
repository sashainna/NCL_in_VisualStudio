0 12 0 -1 -1 LOADU/port.u
1 52 0 0 -1 MACHIN/pworks,0     $$ Declare default Postprocessor
2 9 0 1 -1 multax/on
3 35 0 2 -1 invis/sf1,sf4,sf5,sf7,sf8,sf6, sf39
4 53 0 3 -1 $$ invis/sf1,sf4,sf5,psf1,sf58,sf57,sf7,sf8,sf6, sf39
5 28 0 4 -1 $$ CV3   =SPLINE/PSF1,3,0.45
6 10 0 5 -1 remove/CV3
7 10 0 6 -1 remove/CV4
8 22 0 7 -1 CV3=SPLINE/PSF1,3,0.45
9 33 0 8 -1 CV4=SPLINE/OFFSET,CV3,ZS,1.000000
10 10 0 9 -1 remove/PL1
11 23 0 10 -1 PL1=PL/(PV/ON,CV3,0.21)
