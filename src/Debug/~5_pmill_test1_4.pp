0 12 0 -1 -1 LOADU/port.u
1 52 0 0 -1 MACHIN/pworks,0     $$ Declare default Postprocessor
2 9 0 1 -1 multax/on
3 5 0 2 -1 ca/on
4 35 0 3 -1 invis/sf1,sf4,sf5,sf7,sf8,sf6, sf39
5 22 0 4 -1 CV3=SPLINE/PSF1,3,0.50
6 26 0 5 -1 PT1=POINT/YSMALL,ENDPT,CV3
7 18 0 6 -1 CU/0.2,0.1,0.2,-89
8 12 0 7 -1 OB/PT1,X,Y,Z
9 24 0 8 -1 PT2=POINT/X+0.25,Y-0.5,Z
10 6 0 9 -1 th/0.1
11 8 0 10 -1 goto/PT2
12 26 0 11 -1 PT3=POINT/YLARGE,ENDPT,CV3
13 23 0 12 -1 PL1=PL/(PV/ON,CV3,0.03)
