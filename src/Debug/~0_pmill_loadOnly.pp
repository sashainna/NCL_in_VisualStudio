0 12 0 -1 -1 LOADU/port.u
1 25 0 0 -1 CV3   =SPLINE/PSF1,3,0.45
2 34 0 1 -1 $$ CV4   =SPLINE/OFFSET,CV3,ZS,1.0
3 30 0 2 -1 $$ PL1   =PL/(PV/ON,CV4,0.025)
4 28 0 3 -1 $$ PL1   =PL/(PV/ON,CV4,0.0)
5 32 0 4 -1 $$ PL2   =PL/(PV/ON,CV4,0.59538)
6 32 0 5 -1 $$ PL2   =PL/(PV/ON,CV4,0.53076)
7 30 0 6 -1 $$ PL2   =PL/(PV/ON,CV4,0.652)
8 28 0 7 -1 $$ PL2   =PL/(PV/ON,CV4,0.5)
9 32 0 8 -1 $$ PT1   =POINT/YSMALL,ENDPT,CV4
10 28 0 9 -1 $$ PL3   =PL/(PV/ON,CV4,0.9)
11 9 0 10 -1 $$ TH/0.1
12 29 0 11 -1 $$ CU    /0.35,0.175,0.35,-89
13 24 0 12 -1 $$ CV7=cv/intof,PSF1,pl2
14 19 0 13 -1 $$ OB    /PT1,X,Y,Z
15 27 0 14 -1 $$ PT2   =PT/X+0.25,Y-0.5,Z
16 21 0 15 -1 $$ GOTO  /X+3,Y-0.5,Z
17 11 0 16 -1 $$ GOTO/PT2
18 24 0 17 -1 $$ sft = sf/PSF1,PL1,PL2
19 33 0 18 -1 $$ sft = SURF/FILLET,PL1,PSF1,PL2
20 14 0 19 -1 $$ TA/THRU,PT2
21 53 0 20 -1 $$ PMILL/PSF1,CONTCT,PL1,PL2,STEP,0.25,LINEAR,OMIT,IN
22 9 0 21 -1 invis/sf1
23 9 0 22 -1 invis/sf4
24 9 0 23 -1 invis/sf5
25 13 0 24 -1 $$ invis/psf1
26 10 0 25 -1 invis/sf58
27 9 0 26 -1 invis/sf2
28 10 0 27 -1 invis/sf57
29 9 0 28 -1 invis/sf7
30 9 0 29 -1 invis/sf8
31 10 0 30 -1 invis/sf39
32 9 0 31 -1 invis/sf6
33 0 0 32 -1 ~
