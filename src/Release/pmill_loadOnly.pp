LOADU/port.u
ca/on
$$ CV3   =SPLINE/PSF1,3,0.45
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
$$ invis/sf1,sf4,sf5,psf1,sf58,sf57,sf7,sf8,sf6, sf39
CV3   =SPLINE/PSF1,3,0.45
$$ CV4   =SPLINE/OFFSET,CV3,ZS,1.0
$$ PL1   =PL/(PV/ON,CV3,0.025)
$$ PL2   =PL/(PV/ON,CV3,0.5)

