LOADU/port.u
CV3   =SPLINE/PSF1,3,0.45
$$ CV4   =SPLINE/OFFSET,CV3,ZS,1.0
$$ PL1   =PL/(PV/ON,CV4,0.025)
$$ PL1   =PL/(PV/ON,CV4,0.0)
$$ PL2   =PL/(PV/ON,CV4,0.59538)
$$ PL2   =PL/(PV/ON,CV4,0.53076)
$$ PL2   =PL/(PV/ON,CV4,0.652)
$$ PL2   =PL/(PV/ON,CV4,0.5)
$$ PT1   =POINT/YSMALL,ENDPT,CV4
$$ PL3   =PL/(PV/ON,CV4,0.9)
TH/0.1
CU    /0.35,0.175,0.35,-89
$$ CV7=cv/intof,PSF1,pl2
$$ OB    /PT1,X,Y,Z
$$ PT2   =PT/X+0.25,Y-0.5,Z
$$ GOTO  /X+3,Y-0.5,Z
$$ GOTO/PT2
$$ sft = sf/PSF1,PL1,PL2
$$ sft = SURF/FILLET,PL1,PSF1,PL2
$$ TA/THRU,PT2
$$ PMILL/PSF1,CONTCT,PL1,PL2,STEP,0.25,LINEAR,OMIT,IN
invis/sf1
invis/sf4
invis/sf5
$$ invis/psf1
invis/sf58
invis/sf2
invis/sf57
invis/sf7
invis/sf8
invis/sf39
invis/sf6

