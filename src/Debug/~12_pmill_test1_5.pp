0 12 0 -1 -1 LOADU/port.u
1 52 0 0 -1 MACHIN/pworks,0     $$ Declare default Postprocessor
2 9 0 1 -1 multax/on
3 5 0 2 -1 ca/on
4 35 0 3 -1 invis/sf1,sf4,sf5,sf7,sf8,sf6, sf39
5 0 0 4 -1 ~
6 35 0 5 -1 PSRFspln_3_50   =SPLINE/PSF1,3,0.50
7 60 0 6 -1 %PSRFspln_3_50_offstZS1  =SPLINE/OFFSET,PSRFspln_3_50,ZS,1.0
8 0 0 7 -1 ~
9 52 0 8 -1 %YstartPL   =PL/(PV/ON,PSRFspln_3_45_offstZS1,0.025)
10 60 0 9 -1 %ZstartShankClashLimit=PL/(PV/ON,PSRFspln_3_45_offstZS1,0.7)
11 0 0 10 -1 ~
12 41 0 11 -1 startPL   =PL/(PV/ON,PSRFspln_3_50,0.025)
13 35 0 12 -1 endPL	=PL/(PV/ON,PSRFspln_3_50,0.7)
