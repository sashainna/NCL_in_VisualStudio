LOADU/port.u
multax/on
CV3   =SPLINE/PSF1,3,0.45
CV4   =SPLINE/OFFSET,CV3,ZS,1.0
PL1   =PL/(PV/ON,CV4,0.025)
PL2   =PL/(PV/ON,CV4,0.5)
PT1   =POINT/YSMALL,ENDPT,CV4
PL3   =PL/(PV/ON,CV4,0.9)
CU    /0.35,0.175,0.35,-89
OB    /PT1,X,Y,Z
PT2   =PT/X+0.25,Y-0.5,Z
invis/sf1,sf4,sf5,sf6,sf7,sf8,sf39,psf1,sf58,sf57
sequnc/1   $$ Define sequence number to segregate motion playback
*ti
th/0.1
GOTO  /X+3,Y-0.5,Z
GOTO/PT2
TA/THRU,PT2
PMILL/PSF1,CONTCT,PL1,PL2,STEP,0.2,HELIX,CCLW,OMIT,IN
*ti
*wi
*s/th
sequnc/2  $$ Define sequence number to segregate motion playback
*ti
PMILL/PSF1,CONTCT,PL2,PL3,STEP,0.2,HELIX,CLW,OMIT,IN
*ti

