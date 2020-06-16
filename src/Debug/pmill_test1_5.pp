LOADU/port.u
MACHIN/pworks,0     $$ Declare default Postprocessor
multax/on
ca/on
invis/sf1,sf4,sf5,sf7,sf8,sf6, sf39

PSRFspln_3_50   =SPLINE/PSF1,3,0.50
%PSRFspln_3_50_offstZS1  =SPLINE/OFFSET,PSRFspln_3_50,ZS,1.0

%YstartPL   =PL/(PV/ON,PSRFspln_3_45_offstZS1,0.025)
%ZstartShankClashLimit=PL/(PV/ON,PSRFspln_3_45_offstZS1,0.7)

startPL   =PL/(PV/ON,PSRFspln_3_50,0.025)
endPL	=PL/(PV/ON,PSRFspln_3_50,0.7)