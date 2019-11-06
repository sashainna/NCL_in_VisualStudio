PARTNO MULTIPLE CALL OUTPUT WHEN USING "NOW" PARAMETER IN NCL2018
%
vpx=ve/1,0,0
vpy=ve/0,1,0
vpz=ve/0,0,1
%
cu/.03125,0,.5625,0
ob/cu,dia,cor,hgt
%
clrpl=pl/0,0,1,3
gfd=25
dp1=.186+.05+(dia*.3)
%
Drl1=macro/hpv
prompt/hpv,"Hole Point Vector",now,pv
$$ prompt/hpv,"Hole Point Vector",pv
$$
ob/hpv,x,y,z,i,j,k
#p1=pt/x,y,z
rp,gt/(pt/projct,(pt/#p1,-.5,.5),clrpl)
cy/drill,dp1,gfd,ipm,.2
gt/hpv
cy/off
rp,gd/clrpl
termac
%
% Define "dp1" and point vectors for hole locations
%
defnam/pv,ppv,index
%
PT2      =POINT/-5.475,-0.024581
PT3      =POINT/-4.41728,-0.045346
PT4      =POINT/-3.618805,-0.086876
PT5      =POINT/-2.187773,-0.024581
PT6      =POINT/0.000254,-0.045346
PT7      =POINT/1.452026,-0.097258
PT8      =POINT/3.256371,0.027331
%
PPV(1)   =PNTVEC/PT2,VPZ
PPV(2)   =PNTVEC/PT3,VPZ
PPV(3)   =PNTVEC/PT4,VPZ
PPV(4)   =PNTVEC/PT5,VPZ
PPV(5)   =PNTVEC/PT6,VPZ
PPV(6)   =PNTVEC/PT7,VPZ
PPV(7)   =PNTVEC/PT8,VPZ
%
DRAFT    /VIEW=Front,PARAMS,CENTER=-0.655,-1.142,0.766,NORMAL=$
-0.516321,-0.590367,0.620386,YAXIS=0.09709,0.679393,0.727323,SCALE=1.0
%
%
% Example of errant output: The CALL statements below were output by
%                           selecting PPV(1) once, PPV(2) once and
%                           PPV(3) once.
%
% CALL     /DRL1,HPV=PPV(1)
% CALL     /DRL1,HPV=PPV(1)
% CALL     /DRL1,HPV=PPV(2)
% CALL     /DRL1,HPV=PPV(3)
% CALL     /DRL1,HPV=PPV(3)
%

