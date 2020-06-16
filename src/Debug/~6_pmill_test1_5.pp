0 12 0 -1 -1 LOADU/port.u
1 52 0 0 -1 MACHIN/pworks,0     $$ Declare default Postprocessor
2 9 0 1 -1 multax/on
3 5 0 2 -1 ca/on
4 35 0 3 -1 invis/sf1,sf4,sf5,sf7,sf8,sf6, sf39
5 0 0 4 -1 ~
6 35 0 5 -1 PSRFspln_3_45   =SPLINE/PSF1,3,0.50
7 59 0 6 -1 PSRFspln_3_45_offstZS1  =SPLINE/OFFSET,PSRFspln_3_45,ZS,1.0
8 0 0 7 -1 ~
9 52 0 8 -1 %YstartPL   =PL/(PV/ON,PSRFspln_3_45_offstZS1,0.025)
10 60 0 9 -1 %ZstartShankClashLimit=PL/(PV/ON,PSRFspln_3_45_offstZS1,0.7)
11 0 0 10 -1 ~
12 42 0 11 -1 YstartPL   =PL/(PV/ON,PSRFspln_3_45,0.025)
13 50 0 12 -1 ZstartShankClashLimit=PL/(PV/ON,PSRFspln_3_45,0.7)
14 22 0 13 -1 CV3=SPLINE/PSF1,3,0.50
15 26 0 14 -1 PT1=POINT/YSMALL,ENDPT,CV3
16 18 0 15 -1 CU/0.2,0.1,0.2,-89
17 12 0 16 -1 OB/PT1,X,Y,Z
18 24 0 17 -1 PT2=POINT/X+0.25,Y-0.5,Z
19 6 0 18 -1 th/0.1
20 8 0 19 -1 goto/PT2
21 26 0 20 -1 PT3=POINT/YLARGE,ENDPT,CV3
