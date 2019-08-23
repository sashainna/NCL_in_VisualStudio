loadu /SMILL_IgnoreInner.u
ca/on
$$
clrpl=pl/0,0,1,1.25
clr   =data/rp,godlta/clrpl
$$
$$
multax/on
$$
dia=.5
hdia=dia/2
cr=.05
hgt=1
th/0.1
cu    /dia,cr,hgt
$$
from  /(pt/pt4,0,0,.5)
invis /sf1
draft/fit,all
erase/motion
DRAFT /MODIFY=SF2,SF3,SF4,SF5,SF6,SF7,SF8,SF9,SF10,SF11,LAYER=10
$$
$$ **STOP
SMILL/LAYER=10,TO,LN1,PASS,40,RAPTO,0.5,RETRCT,0.625
