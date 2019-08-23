PARTNODOOR_RK_RW_REVX2_OP2  MARCH 7,2018
insert( P/N 1040-01202-20 REV. X2 )
$$ from model Door_Rk_Rw.igs  1/10/2018 (Rev. X2)
$$ machine #32  5-axis (BC) trunion
%**reset/stop
*set/indent,off
ca/on

machin/pworks,32
$$
$$ -------------- setup Modsys and bring in Unibase ----------------
ofz=.050   $$ fixture ended up at 2.950
ofz2=-1.18-1.5+5.5   $$ 5.5" riser on dif. machine
ubfn/Door_rk_rw_RevX2_geo.u
 mxm1=mx/transl,0,0,-ofz+ofz2
 modsys/mxm1
 get/all
ubfn/close,1
modsys/nm
p0=pt/0,0,0
$$ ------------- T O O L    L I S T --- &  S E T U P ---------------
PPRINT---TOOLIST---
$$         Endmills passes
$$ tool no.  1 = 2.000D x .06R FACEMILL (roughing faces) OP1 & 2
%$$ tool no.  2 = .750D X 0R X 2 FLT X 1.1" OOH (rough/finish) OP1
$$ tool no.  3 = .375D X0R X2 FLT X1.00" LBS (periph. rough/fin) OP1/2
$$ tool no.  4 = .1875D X 0R X 2 FLT X 1.00" LBS (periph. finish)OP1/2
$$ tool no.  5 = .0625D X 0R X 2 FLT x .12"min. FL x .270 min LBS
$$         (c'bores) x .550 OOH min. x .125D shank
$$ tool no.  6 = .500D X 0R X 2 FLT X 1.00" LBS (roughing) OP2
$$ tool no.  7 = .125D X 0R X 2 FLT X .25 LBS (finishing) OP2
$$ tool no.  8 = .25D x .0625R[.06] x 4[2]FLT x .950 LBS x 1.1OOH flnge
$$ tool no.  9 = .125D X .0625R BALL X 4[2]FLT X 1.0" LBS (finish)
$$          spot drills, drills, taps, reamers, deburr
$$ tool no. 10 = .125D x 90 DEG SPOT DRILL [.073] OP1 & OP2
$$ tool no. 11 = .125D X 1 FLT ENGRAVING TOOL - DEBURR OP1 & OP2
$$ tool no. 12 = .1875D X 4 FLT ENGRAVING TOOL - DEBURR OP1
$$ tool no. 13= .0781 DIA [5/64] DRILL X 1.564 LBS X 1.655 MIN OOH OP2
$$                    this is the long drill
$$ tool no. 14 = .089 DIA [#43] X .235 DEEP X 1.00 MIN. OOH OP2
$$ tool no. 15 = .0781 DIA (5/64) DRILL  [.558] (SHORT DRILL) OP2
$$ tool no. 16 = 2-56 ROLL FORMED TAP X [.287]   (2 plcs) OP2
$$ tool no. 17 = .098 DIA (#40) PRE-DRILL [.260] (.100 c'bore x1)
$$ tool no. 18 = .1495D X DRILL #25 [.245] (fixture holes) OP1
$$ tool no. 19 = 10-24 UNC TAP   (fixture holes)[.240] OP1
$$ tool no. 20 = .120 DIA DRILL  (fixture holes)[.240] OP1
$$ tool no. 21 = .126 DIA REAMER (fixture holes)[.200] OP1
$$ tool no. 23 = 4-40 UNC Tap x .627+tip LBS x .800 min OOH OP2
PPRINT ..............................
PPRINTN101= 2.000D X .06R FACEMILL
PPRINTN106= .500D X 0R X 2 FLT X 1.00" LBS
PPRINTN103= .375D X 0R X 2 FLT X 1.00" LBS
PPRINTN107= .125D X 0R X 2 FLT X .55 MIN. LBS X .625" MIN. OOH
PPRINTN206= .500D X 0R X 2 FLT X 1.00" LBS
PPRINTN203= .375D X 0R X 2 FLT X 1.00" LBS
PPRINTN104= .1875D X 0R X 2 FLT X 1.00" LBS
$$ S1 UP TO HERE
PPRINTN110= .125D X 90 DEG SPOT DRILL X.750 MIN OOH
PPRINTN115= .078 DIA [5/64] DRILL X .339 DEEP
PPRINTN116= 2-56 ROLL FORMED TAP X .397 DEEP X .450 MIN. LBS
PPRINTN114= .089 DIA [#43] X .235 DEEP X 1.00 MIN. OOH
PPRINTN113= .0781 DIA [5/64] DRILL X 1.564 LBS X 1.655 MIN OOH
PPRINTN123= 4-40 UNC Tap x .627+tip LBS x .800 min OOH
PPRINT LBS includes no diameter bigger than .125 shanks
PPRINTN105= .0625D X 0R X 4FLT X .550 MIN. LBS X .625" MIN OOH
PPRINTN105 .0625D-0RAD-.12 MIN.FL-.270 MIN.LBS-.550 OOH MIN.
PPRINT               X .125 SHANK
$$ S2 UP TO HERE
%PPRINTN111= .1875D X .06R CORNER ROUND X .060 MINOR DIA
PPRINTN111= .125D X 1FLT ENGRAVING TOOL -DEBURR
insert
pprint---SETUP-INFO---
pprintSET X0 Y0 Z0 AT CENTER OF ROTATION
pprintSET HINGED END TO THE +X DIRECTION
PPRINT
insert
pprint---PROGRAM-START---
pprint  5-AXIS-CODE  .

$$ ----------------------------  MACROS -----------------------------
$$includ/motion.inc
%includ/5axis_Trunion_AB_m25_tch.mac
%includ/5axis_Trunion_AB_m26_m30_tch.mac
includ/5axis_Trunion_BC_m22_m32_tch.mac
INCLUD/HOLDER.txt
includ/motion_les.inc

clearz=macro/Zzz=clrz,zzzl=zl
 $$ clear tool and create ckplz
 rp,gd/(pl/pa,(pl/p0,pe,(ve/ta)),zzzl,zzz)
 ckplz=pl/(pt/te),pe,(ve/ta)
termac

$$-------------------------- GEO ------------------------------------
call/basicm
draft/View=VVV1,PARAMS,CENTER=P0,NORMAL=vpz,YAxis=vpy
draft/format=single,name=VVV1,axis=on
disply/cv,60
**set/adispl,40,40,20
draft/fit,all
disply/waxis

%*stop
clrz=8+3  $$ 2.4" clearance
clrp1=pl/pa,(sf/stkln7,stkln5),zl,.5

draft/redraw,all

$$**stop
invis/layer,7,12,50,100
$$
$$ ------------- SETUP STOCK AND FIXTURES FOR IPV -------------------
$$
%fixcv1=spline/compos,fl17,lnn(24),lnn(27),fl31
%fixtur/load,51,"op1_fixture1.stk"
pte=pt/xs,endpt,stkln5
 mxr1=mx/yzrot,180
 mxr2=mx/transl,can(pte,1),can(pte,2),can(stkln5,3)
 mxr3=mx/mxr2,mxr1
stock/stl,1,inches,OP1_fini.stl
%stock/load,21,"stock_blank.stk"
stock/move,1,at,mxr3

$$   line 744
%*stop
$$ ------------------------------------------------------------------
$$ ------------------------- M O T I O N ----------------------------
cu/disply,shade,off
tlofps
tralst/on
**reset/autost
skip1=1
%@@@
%**skip/to,3097   $$ 631
skip1=0
$$*stop
erase/motion
$$ ------------------------ T O O L   # 1 ---------facemill---------
$$ tool #1 = 2.000 dia x .06R x 3 flute facemill
$$ face top of part
PPRINTLDT 2.00 DIA X .06 RAD FACEMILL
CALL/TCH,SEQ1=101,TLNO=1,IDIA=2.000,ILOC=1.0,IFLTH=.50,ICOR=.06,$
   IOOH=1.2,IHOLD=2,IRPM=8000,IFED=73,ISDIA=1.9
ob/cu,dia,cor,ht
hdia=dia/2
%cu/dia,cor,ht
$$$$$$$$$$$$$$$$$$$$$
   toler/.001
LINTOL /AXIS,0.00005
LINTOL /0.0005
PPTOL  /0.000005
$$$$$$$$$$$$$$$$$$$$$
pt3=pt/pttop,-3
ta/vpz
$$ --- top face ---
$$ .126.." stock on top now, leave .010
% bpl=pl/pa,(pl/lnn(248),pe,plx),zl,.010,(pt/pttop,-3)
 bpl=pl/pa,(pl/nln(112),pe,plx),zl,.010,pt3
 pte=pt/xs,endpt,stkln7
 ptli=pt/pte,-1.75,-.1
 ccl1=ln/(pt/pte,-.5),(pt/pte,lstk+.5)
 ccl2=ln/pa,ccl1,ys,wstk
 tmpsf=sf/ccl1,ccl2
 ccl3=ln/(pt/xs,endpt,ccl1),(pt/xs,endpt,ccl2)
 ccl4=ln/(pt/xl,endpt,ccl1),(pt/xl,endpt,ccl2)
 pl1=pl/ccl1;  pl2=pl/ccl2

 d1=dist/tmpsf,bpl
 stepz=.100
 intz=int(d1/stepz)+1
 stepz=d1/intz    $$ .063148
 stpovr=1.5

arcslp/fillet,0
 dc
  pthk=-stepz
  th/pthk,0
  rp,go/on,(pl/ptli,pa,plx),tmpsf,on,(pl/ptli,pa,ply)
   ptli=pt/te
  call/clearz
 rp,cut

do/10,i=1,intz,2
    pthk=-i*stepz
   dc
    th/pthk,0
    rp,go/on,(pl/ptli,pa,plx),tmpsf,on,(pl/ptli,pa,ply)
     ptli=pt/te
   rp,cut
  fr/fr1
%   fmill/tmpsf,height,0,start,ptli
  arcslp/fillet,stpovr/2
   rmill/tmpsf,on,ccl3,on,ccl4,on,pl1,on,pl2,2,1,stpovr,fr1,rp,fr1
   gd/hdia+.25,.1,0   $$ need .1 to get next rmill to work
    ptlo=pt/te
   if (i 'eq' intz) undo
 $$
    pthk=-(i+1)*stepz
   th/pthk,0
%   fmill/tmpsf,height,0,start,ptlo
   rmill/tmpsf,on,ccl4,on,ccl3,on,pl2,on,pl1,2,1,stpovr,fr1,rp,fr1
   gd/-hdia-.25,0,0
  arcslp/fillet,0
10:contin
remove/tmpsf

th/0
coolnt/off
spindl/off
call/clearz
sequnc/end




$$*stop
erase/motion

$$ ------------------------ T O O L   # 6 ---------roughing---------
$$ tool #6 = .500 dia x .03R x 3 flute endmill x 1.0" LBS
$$ pocket ID, and rough periphery
PPRINTLDT .500 DIA X 0 RAD X 1.00LBS ENDMILL
CALL/TCH,SEQ1=106,TLNO=6,IDIA=.500,ILOC=1.0,IFLTH=.750,ICOR=0,$
   IOOH=1.2,IHOLD=75,IRPM=9000,IFED=72,ISDIA=.480
ob/cu,dia,cor,ht
hdia=dia/2
%cu/dia,cor,ht
$$$$$$$$$$$$$$$$$$$$$
   toler/.001
LINTOL /AXIS,0.00005
LINTOL /0.0005
PPTOL  /0.000005
set/lincir,.002,all
$$$$$$$$$$$$$$$$$$$$$
 cvt(1)=cv/COMPOS,LNN(256),CVV(39),LNN(275),CII(166),lnn(359)
 cvt(2)=cv/compos,nln(116),cii(124),lnn(264),cvv(38),lnn(248)
 cvt(3)=cv/COMPOS,LNN(641),nci(263),lnn(556),cii(175),lnn(566),$
   cii(176),lnn(552),cvv(109)
 cvt(4)=cv/compos,lnn(608),nci(257),nln(507),cii(177),lnn(595),$
   nci(259),cii(178),nln(561),cii(179),lnn(602),cvv(111)
 cvt(5)=spline/compos,cvt(3),cvt(4)
 disply/cvt(3),100
 disply/cvt(4),100
 disply/cvt(2),200
 disply/cvt(1),200
 ptez=pt/cvt(3),1
 ptliz=pt/ptez,.35,-.35

 $$ remove excess around outside of tub first
 tplz=pl/pa,(pl/nln(112),pe,plx),zl,.010,pt3
 bplz=pl/pa,plz,zl,8.22278-.03
% cva(1)=spline/io,compos,layer=11,at,bpl2,cnum
 cva(1)=spline/PART,0,layer=11,at,bplz
 disply/cva(1),400
 visibl/stkln*
 ptez=pt/cva(1),0
 lilnz=ln/ptez,(pt/ptez,-.5,.5)
dc
 th/0,0,.25
 gt/(pt/ptez,1,1,1)
 srfvct/vnx,vny
 go/lilnz,bplz,stkln3
  pte2z=pt/te
 rp,gd/clrp1
rp,cut
rp,gt/pte2z

pthkz=0
dthkz=.175
fr/fr1
profil/off,cva(1),dthkz,left,start,nearpt,ptez,clw,arc,.25,$
   ps,bplz,pthkz,retrct,off,arc,.1,0,$
   fedrat,fr1,rp,fr1,fr1,fr1,fr1
profil/off,cva(1),.025,left,start,nearpt,ptez,clw,arc,.25,$
   ps,bplz,pthkz,retrct,off,arc,.1,0,$
   fedrat,fr1,rp,fr1,fr1,fr1,fr1
th/0
rp,gd/clrp1


$$ rough out pocket +.050, +.050
 tpl=pl/pa,(pl/lnn(248),pe,plx),zl,.010,pt3
 bpl=pl/lnn(296),pe,plx,(pt/ys,endpt,lnn(296))
draft/modify=tpl,color=ltblue
draft/modify=bpl,color=magnta
 d2=dist/tpl,bpl
 stepz=.175
 intz=int(d2/stepz)+1
 stepz=d2/intz  $$ .1467137

$$==================================================================$$
$$ macro to run "dntcut" pocket so actual pocket command will work
dumm=macro/
 pokmod/2,helix,tt,avoid,nowarn,.35,out,arc,island,arc,retrct,on,.25,$
    incr,stepz,depth,.05,.05,.02,cclw,out,down,arc,in,.05,trans,arc,$
    hdia,0.0,fr1,rp,fr1,fr1/3,fr1,fr1,fr1
 dc
  pocket/layer=11,plane,bpl,.05,tpl,part,.5,level,deep
  rp,gd/clrp1
 dc/nm
termac
$$==================================================================$$

pokm1=macro/
  pokmod/2,helix,tt,avoid,nowarn,.35,out,arc,island,arc,retrct,on,.25,$
    incr,stepz,depth,.05,.05,.02,cclw,out,down,arc,in,.05,trans,arc,$
    hdia,0.0,fr1,rp,fr1,fr1/3,fr1,fr1,fr1
termac

$$ ---
$$ move the following cuts +.050 in Z so don't gouge part
 mxt1=mx/transl,0,0,.05
 tracut/mxt1
draft/cutter,tracut=on

$$ 1st 3 levels  +.050 off +X periphery
 pthk=.050;dthk=.050

$$ ===== bad pocket =====
$$ comment this out if you want to see bad pocket      <<<<<<----TAD
$$call/dumm
$$ ======================

$$ good pocket
call/pokm1

pocket/layer=11,plane,bpl,.05,tpl,part,.5,level,deep

tracut/nm
$$ ---
th/0
rp,gd/clrp1
DRAFT/VIEW=VVV1,PARAMS,CENTER=0.662,0.248,8.517,NORMAL=$
-0.295872,-0.78466,0.544764,YAXIS=0.023944,0.564027,0.825409,SCALE=$
1.185427

f i n i
