loadu/pencil_the_blue_10_202
$$
multax/on
cu    /.5,.25,2
disply/maxis
$$
$$ Boundary Curve
CVV(5326)=SPLINE/OUT,SFF(1911),0,0
draft/modify=cvv(5326),color=purple
$$
$$ Cut direction vectors
VEE(1)=ve/1,0,0
VEE(2)=ve/0,1,0
$$
$$ Start point
ptt(1)=pt/(pt/on,cvv(4084),.5),0,0,.2
ptt(2)=pt/(pt/on,cvv(4088),.5),0,0,.2
$$
ERASE/ALL
$$DISPLY/SFF(1911),vee(1),vee(2),CVV(5326),ptt(1)
SMILL /SFF(1911),ON,vee(1),BOUND,CVV(5326),HEIGHT,0.001,start,ptt(1),COMBIN,$
RAPTO ,.01,RETRCT,.02
$$
$$ ERASE/ALL
$$nxtstpt=pt/te
$$ disply/nxtstpt
$$rp,gt/ptt(2)
$$
$$ **STOP
$$SMILL /SFF(1911),ON,vee(2),BOUND,CVV(5326),HEIGHT,0.001,start,nxtstpt,COMBIN,$
$$RAPTO ,.01,RETRCT,.02

