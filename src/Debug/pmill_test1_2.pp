LOADU/port.u
MACHIN/pworks,0     $$ Declare default Postprocessor
multax/on
CV3   =SPLINE/PSF1,3,0.45
PL1   =PL/(PV/ON,CV3,0.025)
PL2   =PL/(PV/ON,CV3,0.7)
PT1   =POINT/YSMALL,ENDPT,CV3
OB    /PT1,X,Y,Z
PL3   =PL/(PV/ON,CV3,0.9)
PT2   =PT/X+0.25,Y-0.5,Z
$$
$$         Define Cutter and SHANK display
CU    /0.35,0.175,0.35,-89
cu    /disply,shank,.125,.125
OB    /cu,dia,cor,hgt      $$ Obtain cutter dimensions
$$
$$ draft /modify=PSF1,trans=40
$$ draft /modify=PSF1,trans=1
invis/sf1,sf4,sf5,sf6,sf7,sf8,sf39,psf1,sf58,sf57
sequnc/1   $$ Define sequence number to segregate motion playback
*ti
th/dia/4       $$ Set THICK to half the cutter diameter (dia/2)
GOTO  /X+3,Y-0.5,Z
GOTO/PT2
TA/THRU,PT2
PMILL/PSF1,CONTCT,PL1,PL2,STEP,0.1,LINEAR,OMIT,IN
*ti
*wi
*s/th
$$ Processing time for PMILL Command:
$$ PMILL/PSF1,CONTCT,PL1,PL2,STEP,0.1,LINEAR,OMIT,IN
$$
sequnc/2  $$ Define sequence number to segregate motion playback
*ti
PMILL/PSF1,CONTCT,PL3,PL2,STEP,0.1,LINEAR,OMIT,IN
*ti
$$ sequnc/3  $$ Define sequence number to segregate motion playback
$$ *ti
$$ PMILL/PSF1,CONTCT,PL2,PL4,STEP,0.1,LINEAR,OMIT,IN
$$ *ti
$$ Processing time for PMILL Command:
$$ PMILL/PSF1,CONTCT,PL2,PL3,STEP,0.1,LINEAR,OMIT,IN
$$
*pause
