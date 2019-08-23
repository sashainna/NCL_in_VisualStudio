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
$$includ/5axis_Trunion_BC_m22_m32_tch.mac
$$read/2,5axis_Trunion_BC_m22_m32_tch.mac
$$ Tool Change macros for 5-AXIS Trunion Machines
$$ Hurco #22, #27
$$ zbase = 1.400 above Z0 of rotation for machine #22 & #27
$$ Haas #32, #33
$$ table is Z-1.5, riser is 5.5 so Z4.0	for machine #33
$$ 1-30-18 added spindl/off option to tool change
LINTOL /AXIS,0.000005
LINTOL /0.000005
PPTOL  /0.000005
ffr=400

MULTAX/ON
INCLUD/HOLDER.txt
a=1

TCUTR=macro/n=1,tcdia,tcshnk,tccor=0,tcflt,tcsooh
$$ defines cutter and shank as curves
 tlnc1=ln/0,0,tcdia/2-tccor,0
 tlnc3=ln/tcdia/2,tccor,tcdia/2,tcflt-tccor
 tcic2=ci/yl,tlnc1,xs,tlnc3,ra,tccor
 tlnc5=ln/tcdia/2-tccor,tcflt,tcshnk/2,tcflt
 tcic4=ci/xs,tlnc3,ys,tlnc5,ra,tccor
 tlnc6=ln/tcshnk/2,tcflt,tcshnk/2,tcsooh-.25-.25
 tlnc8=ln/tcdia/2,tcsooh-.25,tcdia/2,tcsooh
 tlnc7=ln/(pt/yl,endpt,tlnc6),(pt/ys,endpt,tlnc8)
 tcvc(n)=cv/compos,tlnc1,tcic2,tlnc3,tcic4,tlnc5
 #dz=tcflt
 #mxmz=mx/transl,0,-#dz
 move/tlnc6,#mxmz
 move/tlnc7,#mxmz
 move/tlnc8,#mxmz
 tcvch(n)=cv/compos,tlnc6,tlnc7,tlnc8
 remove/tlnc*,tcic*
termac

Endr = Macro/edd=0,ptend=p0,brot=1,crot=1,flipx=0,flipy=0
$$ rotate axis to unload and/or setup next tool
 if (edd 'eq' 1) then
    tmpbuf = "G0 G90"
    coolnt/off
    spindl/off
 endif
 refsys/nm
 obtain/(ve/ta),#ddx,#ddy,#ddz
 #ccl1a=ln/(pt/te),(pt/(pt/te),#ddx,#ddy,#ddz)
 if (#ddy 'eq' 0) then
    if (#ddx 'gt' 0) then
      #angC=90
    else
      #angC=-90
    endif
    if (#ddx 'eq' 0) then
      if (brot'eq'1) 'and' (crot'eq'1) then
         jumpto/endd
      endif
      #angB=0
      #angC=0
      #mxB1=mx/xyrot,0
      #mxA1=mx/zxrot,0
      jumpto/end2
    endif
 elseif (#ddy 'lt' 0) then
    #angC=atan(#ddx/#ddy)
    #angC=#angC+180
 else
    #angC=atan(#ddx/#ddy)
 endif
 #mxB1=mx/xyrot,#angC
 #ccl1b=clone/#ccl1a,#mxB1
 obtain/#ccl1b,,,,#ddxx,#ddyy,#ddzz     $$ y,z only now
 if (#ddzz 'eq' 0) then
   if (brot'eq'1) 'and' (crot'eq'1) then
   jumpto/endd
   endif
   if (#ddyy 'ge' 1) #angB=90            $$sometimes 1.0000001
 else
   #angB=atan(#ddyy/#ddzz)
 endif
 #mxA1= mx/yzrot,#angB
end2:
 ccl1o=clone/#ccl1a,(mx/#mxA1,#mxB1)   $$ now at 0,0,1
 ptendo=clone/(pt/te),(mx/#mxA1,#mxB1)  $$ posted point of pt/te
 draft/modify=ptendo,ccl1o,color=blue
$$ now find new point
 obtain/ptend,#npx,#npy,#npz
 if ((#npx'eq'0)'and'(#npy'eq'0)'and'(#npz'eq'0)) then
   ptendb=pt/ptendo
 else
   ptendb=pt/ptend
 endif
 if (brot 'eq' 0) then
   ta/vpz
   if (crot 'ne' 1) then
     rotabl/axis,1,aa,crot,orient,ipm,0,next
     #mxB1b=mx/xyrot,crot
   else
     #mxB1b=mx/xyrot,#angC
   endif
   ptendn=clone/ptendb,(mx/invers,#mxB1b)
   rp,gt/ptendn
   jumpto/endd
 endif
 if (brot 'eq' 1) 'and' (crot 'eq' 1) then
   brot=#angB
   crot=#angC
   ta/#ddx,#ddy,#ddz
   ptendn=clone/ptendb,(mx/invers,(mx/#mxA1,#mxB1))
 else
   if (brot 'ne' 1) then
     rotabl/axis,2,aa,-brot,orient,ipm,0,next
     #mxA1b=mx/yzrot,brot
   else
     #mxA1b=mx/yzrot,#angB
   endif
   if (crot 'eq' 1) then
     #mxB1b=mx/xyrot,#angC
   else
     rotabl/axis,1,aa,-crot,orient,ipm,0,next
     #mxB1b=mx/xyrot,crot
   endif
   $$ new rotation
   ptendn=clone/ptendb,(mx/invers,(mx/#mxA1b,#mxB1b))
   ccl1n=clone/ccl1o,(mx/invers,(mx/#mxA1b,#mxB1b))
   obtain/ccl1n,,,,#ddxx,#ddyy,#ddzz
   if (#ddzz 'eq' -1) #ddzz=1
   if (flipx 'eq' 1) #ddxx=-#ddxx
   if (flipy 'eq' 1) #ddyy=-#ddyy
   ta/#ddxx,#ddyy,#ddzz
 endif
rp,gt/ptendn
endd:
if (edd 'eq' 1) then
insert@tmpbuf
endif
TERMAC

TCH=MACRO/SEQ1,TLNO,IDIA,ICOR=0,ILOC,IANG=0,ICUIM=0,IFLTH=0,        $
          ISDIA=0,IOOH,IHOLD=0,ICOL1=FLOOD,IRPM=500,ISDIR=CLW,      $
          IFED=10,ISFM=0,INTH1=0,ICHP=0,IWOFF=54,SPTH=-5,CLMMP1=ON, $
          CLMMP2=ON,convy=on
PROMPT/TCH,"Tool Change Macro"
PROMPT/SEQ1,6,0,"Sequence Number",1,999999
PROMPT/TLNO,4,0,"Tool Number",1,9999
PROMPT/IDIA,"Cutter Diameter",0,100
PROMPT/ICOR,"Cutter Corner Radius",0,50
PROMPT/ILOC,"Cutter Height",0,100
PROMPT/IANG,"Cutter Side Angle",-89,89
PROMPT/ICUIM,"Cutter Symbol or Curve Name"
PROMPT/IFLTH,"Cutter Flute Length",0,100
PROMPT/ISDIA,"Shank Diameter",0,100
PROMPT/IOOH,"Out Of Holder Distance",0,100
PROMPT/IHOLD,20,0,"Holder Name Or Number"
PROMPT/ICOL1,"Coolant Condition",FLOOD,ON,OFF,AIR,MIST
PROMPT/IRPM,5,0,"Spindle RPM",10,99999
PROMPT/ISDIR,"Spindle Rotation Direction",CLW,CCLW,OFF
PROMPT/IFED,"Feedrate",1,10000
PROMPT/ISFM,"Surfece Feed Per Minute",0,10000
PROMPT/INTH1,4,0,"Number of Teeth",0,100
PROMPT/ICHP,"Chip Load Per Tooth",0,100
PROMPT/IWOFF,2,0,"Work Offset",1,99
PROMPT/SPTH,2,3,"Z Distance From Home for Spindl/Coolant Activatn",-15,0
PROMPT/CLMMP1,"C AXIS CLAMP CONDITION",ON,OFF
PROMPT/CLMMP2,"B AXIS CLAMP CONDITION",ON,OFF
PROMPT/convy,"Chip Conveyor Condition",ON,OFF
SEQUNC/SEQ1
ztlno=tlno
zooh(tlno)=iooh
zflth(tlno)=iflth
If (IFLTH 'EQ' 0) IFLTH = ILOC
CUTTER/IDIA,ICOR,ILOC,IANG
OBTAIN/CUTTER,DIA,COR,HGT,CANG
HDIA=DIA/2
If (TYPE(ICUIM) 'EQ' 1 'OR' TYPE(ICUIM) 'EQ' 2) Then
 CUTTER/DISPLY,IDIA,ICOR,IFLTH,IANG
% CUTTER/DISPLY,IDIA,ICOR,ILOC,IANG
Else
 CUTTER/DISPLY,ICUIM
 IFLTH = CAN(PSEUDO,3)
 if (type(isdia) 'eq' 1 'or' type(isdia) 'eq' 2) then
  IF (ISDIA 'EQ' 0) ISDIA = CAN(PSEUDO,1)
 endif
Endif
#SHL = IOOH - IFLTH
if (type(isdia) 'eq' 1 'or' type(isdia) 'eq' 2) then
 CUTTER/DISPLY,SHANK,ISDIA,#SHL
 #shnkl=0
else
 cutter/disply,shank,isdia,holder
 #shnkl=can(shank,2)
endif
#HL1=0
If (TYPE(IHOLD) 'EQ' 1 'OR' TYPE(IHOLD) 'EQ' 2) Then
  If (IHOLD 'NE' 0) Then
    OBTAIN/HOLD(IHOLD),#X1,#Y1,#X2,#Y2,#X3,#Y3,#X4,#Y4,#X5,#Y5,$
        #X6,#Y6,#X7,#Y7
    if (#X7) then
      #HCV=CV/COMPOS,(LN/0,0,#X1,#Y1),(LN/#X1,#Y1,#X2,#Y2),$
                (LN/#X2,#Y2,#X3,#Y3),(LN/#X3,#Y3,#X4,#Y4), $
                (LN/#X4,#Y4,#X5,#Y5),(LN/#X5,#Y5,#X6,#Y6), $
                (LN/#X6,#Y6,#X7,#Y7)  $$,(LN/#X7,#Y7,0,#Y7)
    elseif (#X6) then
      #HCV=CV/COMPOS,(LN/0,0,#X1,#Y1),(LN/#X1,#Y1,#X2,#Y2),$
                (LN/#X2,#Y2,#X3,#Y3),(LN/#X3,#Y3,#X4,#Y4), $
                (LN/#X4,#Y4,#X5,#Y5),(LN/#X5,#Y5,#X6,#Y6)   $$, $
                $$(LN/#X6,#Y6,0,#Y6)
    elseif (#X5) then
      #HCV=CV/COMPOS,(LN/0,0,#X1,#Y1),(LN/#X1,#Y1,#X2,#Y2),$
                (LN/#X2,#Y2,#X3,#Y3),(LN/#X3,#Y3,#X4,#Y4), $
                (LN/#X4,#Y4,#X5,#Y5),(LN/#X5,#Y5,0,#Y5)
    elseif (#X4) then
      #HCV=CV/COMPOS,(LN/0,0,#X1,#Y1),(LN/#X1,#Y1,#X2,#Y2),$
                 (LN/#X2,#Y2,#X3,#Y3),(LN/#X3,#Y3,#X4,#Y4),$
                 (LN/#X4,#Y4,0,#Y4)
    elseif (#X3) then
      #HCV=CV/COMPOS,(LN/0,0,#X1,#Y1),(LN/#X1,#Y1,#X2,#Y2),$
                 (LN/#X2,#Y2,#X3,#Y3),(LN/#X3,#Y3,0,#Y3)
    else
      #HCV=CV/COMPOS,(LN/0,0,#X1,#Y1),(LN/#X1,#Y1,#X2,#Y2),$
                      (LN/#X2,#Y2,0,#Y2)
    endif
    if (#X7) then
      hcvv(ihold)=spline/#hcv
    elseif (#X6) then
      hcvv(ihold)=spline/#hcv
    else
      hcvv(ihold)=spline/#hcv
    endif
    CUTTER/DISPLY,HOLDER,hcvv(ihold)
    draft/modify=hcvv(ihold),layer=888
    invis/layer,888
    #HL1=CAN(HOLDER,2)
  Endif
Else
 CUTTER/DISPLY,HOLDER,IHOLD
 #HL1=CAN(HOLDER,2)
Endif
SL1(tlno) =IOOH + #HL1
SEQNO/SEQ1
LOADTL/TLNO,LENGTH,SL1(tlno)
ob/cu,cdia,crad,,csang
if (cdia 'ne' .0002) then
pprintPOSTED CUTTER DIA= @cdia, RADIUS= @crad, ANGLE= @csang
else
2ang=2*(90-csang)
FM1="%.2f"
TX1="POSTED CUTTER IS " & FORMAT(FM1,2ang)
%TX2=" DEG. SPOT DRILL OR DEBURR TOOL"
TX2=" DEG."
TX3=TX1 & TX2
PPRINT@TX3
endif
If (ISFM 'EQ' 0 'OR' INTH1 'EQ' 0 'OR' ICHP 'EQ' 0) Then
  RPMS=IRPM
  FD=IFED
  fr1=fd
Else
  CALL/FEDSP,ISSPD=ISFM,INTH=INTH1,ICHPL=ICHP
Endif
CLAMP/AXIS,1,CLMMP1
CLAMP/AXIS,2,CLMMP2
%IF (vocabf(isdir) 'eq' 72) THEN
%SPINDL/ISDIR
%ELSE
SPINDL/RPMS,ISDIR
%ENDIF
COOLNT/ICOL1
CUTCOM/ADJUST,IWOFF
FEDRAT/FD
TERMAC

clrend=macro/Zzz=11,zzzl=zl
$$ clear tool before calling tool change
coolnt/off
rp,gd/(pl/pa,(pl/p0,pe,(ve/ta)),zzzl,zzz)
termac

$$ add this macro to your "PP" motion file
%clearz=macro/Zzz=15,zzzl=zl
%$$ clear tool and create ckplz
%rp,gd/(pl/pa,(pl/p0,pe,(ve/ta)),zzzl,zzz)
%ckplz=pl/(pt/te),pe,(ve/ta)
%termac
$$INCLUD/HOLDER.txt
$$read/2,HOLDER.txt
$$
$$  Holder data file
$$ input format:  DATA/X1,Y1, X2,Y2, X3,Y3, X4,Y4, X5,Y5, X6,Y6, X7,Y7
$$    up to 7 points. A negative value in the X position means
$$     not using this point data.
$$ ~ dim. not measured thoroughly
HOLD(1)=DATA/0.5,0, 0.75,1.75, 1,1.75, 1,2
HOLD(2)=DATA/1,0, 1.5,2, -1,0, 0,0
HOLD(3)=DATA/0.75,0, 1,1.5, 1,2, -1,0
HOLD(11)=DATA/0.5,0, 0.75,1.75, 4,1.75, 4,4.05
HOLD(21)=DATA/0.5,0, 0.75,3.0, .75,5, 4,5, 4,7.3

$$*** 2.31" SHORTEST COLLET HOLDERS
HOLD(201)=DATA/.5625,0 ,.5625,1.06, .875,1.06, .875,1.685,$
               1.25,1.685, 1.25,2.31

$$ 5.92" long holder for 2" ISCAR INSERT facemill  x 3 flt
HOLD(2000)=DATA/.985,0, .985,5.3, 1.25,5.3, 1.25,5.92
$$ 4.00" long holder for 2" ISCAR INSERT facemill  x 3 flt
HOLD(2004)=DATA/.985,0, .985,3.3, 1.25,3.3, 1.25,3.92

$$ short SHRINKERS for .375,.5,.625,.75,1.0 em(3.5 deg. taper) 4pts
HOLD(37)=DATA/.4105,0, .5775,2.678, 1.375,2.678, 1.375,3.298
HOLD(50)=DATA/.5355,0, .7025,2.678, 1.375,2.678, 1.375,3.298
HOLD(62)=DATA/.5980,0, .7650,2.678, 1.375,2.678, 1.375,3.298
HOLD(75)=DATA/.6605,0, .8275,2.678, 1.375,2.678, 1.375,3.298
HOLD(100)=DATA/.865,0,  1.04,3.380, 1.375,3.380, 1.375,4.0

$$*** CAT40 ER16 HOLDER
HOLD(416)=DATA/.5625,0 ,.5625,3.5, .875,3.5, .875,4.125, 1.25,4.125, $
               1.25,4.75

$$ CAT 40  2.625" SHRINKERs (short) for .750,.500" endmills    "~"
HOLD(4450)=DATA/.5355,0, .7025,2.005, 1.25,2.005, 1.25,2.625
HOLD(4475)=DATA/.6605,0, .8275,2.005, 1.25,2.005, 1.25,2.625

$$ 4" SHRINKERS for .375,.5,.625,.75,1.0 em(3.5 deg. taper) 5pts
$$ .875 may be .8275 (no calipers)
HOLD(475)=DATA/.6605,0, .875,2.50, .875,3.375, 1.25,3.375, 1.25,4.0

$$ 6" SHRINKERS for .375,.5,.625,.75,1.0 em(3.5 deg. taper) 5pts
HOLD(637)=DATA/.4105,0, .5775,2.678, .5775,5.38, 1.375,5.38, 1.375,6.0
HOLD(650)=DATA/.5355,0, .7025,2.678, .7025,5.38, 1.375,5.38, 1.375,6.0
HOLD(662)=DATA/.598,0, .765,2.678, .765,5.38, 1.375,5.38, 1.375,6.0
HOLD(675)=DATA/.6605,0, .8275,2.678, .8275,5.38, 1.375,5.38, 1.375,6.0
HOLD(6100)=DATA/.7855,0, .9525,2.678, .9525,5.38, 1.375,5.38, 1.375,6.0

$$ 5.00" GAGE Holder  (Teledyne 630-213144-003r2 12/1/2017)
hold(500)=data/.3125,0, .5,.3, .5,3.75, .875,3.75, .875,4.375, $
    1.25,4.375, 1.25,5

$$ weld and shank (non shrinkers) for insert cutters
$$ holder for .625" insert cutter
HOLD(362)=DATA/.52,0, .875,.358, .875,1.6, 1.375,1.6, 1.375,2.220
$$ holder for .75" insert cutter
HOLD(375)=DATA/.58,0, .875,.295, .875,1.6, 1.375,1.6, 1.375,2.220


$$..... tapered collet holders (.69" Dia) .....
HOLD(509)=DATA/.21,0, .345,.25, .345,1, 1.25,1, 1.25,1.62
$$    2" LONGER HOLDER THAN 509
HOLD(510)=DATA/.21,0, .345,.25, .345,3, 1.25,3, 1.25,3.62
$$includ/motion_les.inc
$$read/2,motion_les.inc
$$ macros for programs
$$ last modified 9/16/2017
$$ ..........................................................
basicm=macro/
 lnx=ln/0,0,1,0
 lny=ln/0,0,0,1
 vpx=ve/1,0,0
 vpy=ve/0,1,0
 vpz=ve/0,0,1
 vnx=ve/-1,0,0
 vny=ve/0,-1,0
 vnz=ve/0,0,-1
 plx=pl/1,0,0,0
 ply=pl/0,1,0,0
 plz=pl/0,0,1,0
 p0=pt/0,0,0
termac

leadm=macro/psf=botpl1,pthkm=.02,dthkm=.05,lolnm=.1,rtk=0
 $$ leadout for unknown direction, rtk is right kerf (=1 for G42)
 dc
  th/pthkm,-hdia+dthkm,-hdia+lolnm
  psis/psf
  if (rtk 'eq' 1) then
     tr,gf/(ln/fwd),past,(ln/(pt/te),pe,(ln/fwd))
  else
     tl,gf/(ln/fwd),past,(ln/(pt/te),pe,(ln/fwd))
  endif
 cut
 th/0
termac

holdm=macro/n=1
% up to 7 pt holders allowed
 OBTAIN/HOLD(n),#X1,#Y1,#X2,#Y2,#X3,#Y3,#X4,#Y4,#X5,#Y5,#X6,#Y6,#X7,#Y7
 if (#X7) then
    HCV(n)=CV/COMPOS,(LN/0,0,#X1,#Y1),(LN/#X1,#Y1,#X2,#Y2),$
                (LN/#X2,#Y2,#X3,#Y3),(LN/#X3,#Y3,#X4,#Y4), $
                (LN/#X4,#Y4,#X5,#Y5),(LN/#X5,#Y5,#X6,#Y6), $
                (LN/#X6,#Y6,#X7,#Y7),(LN/#X7,#Y7,0,#Y7)
 elseif (#X6) then
    HCV(n)=CV/COMPOS,(LN/0,0,#X1,#Y1),(LN/#X1,#Y1,#X2,#Y2),$
                (LN/#X2,#Y2,#X3,#Y3),(LN/#X3,#Y3,#X4,#Y4), $
                (LN/#X4,#Y4,#X5,#Y5),(LN/#X5,#Y5,#X6,#Y6), $
                (LN/#X6,#Y6,0,#Y6)
 elseif (#X5) then
    HCV(n)=CV/COMPOS,(LN/0,0,#X1,#Y1),(LN/#X1,#Y1,#X2,#Y2),$
                (LN/#X2,#Y2,#X3,#Y3),(LN/#X3,#Y3,#X4,#Y4), $
                (LN/#X4,#Y4,#X5,#Y5),(LN/#X5,#Y5,0,#Y5)
 elseif (#X4) then
    HCV(n)=CV/COMPOS,(LN/0,0,#X1,#Y1),(LN/#X1,#Y1,#X2,#Y2),$
                 (LN/#X2,#Y2,#X3,#Y3),(LN/#X3,#Y3,#X4,#Y4),$
                 (LN/#X4,#Y4,0,#Y4)
 elseif (#X3) then
    HCV(n)=CV/COMPOS,(LN/0,0,#X1,#Y1),(LN/#X1,#Y1,#X2,#Y2),$
                   (LN/#X2,#Y2,#X3,#Y3),(LN/#X3,#Y3,0,#Y3)
 else

    HCV(n)=CV/COMPOS,(LN/0,0,#X1,#Y1),(LN/#X1,#Y1,#X2,#Y2),$
                   (LN/#X2,#Y2,0,#Y2)

 endif
termac


$$
$$ Macro for Drilling holes
$$ ipeck=(dia*.3)
DRIL  =MACRO/IPT,IMOD,IDEP=0,IFED=FD,IRAP=(.001*100),IPECK, $
             ITHRU=OFF,IRET=OFF,IRETT=ON
PROMPT/DRIL,"Cycle","Cycle Macro"
PROMPT/IPT,  "Select A Point, Point-vec Or Pattern"
PROMPT/IMOD, "Select Cycle Mode",DRILL,THRU,DEEP,BORE,REAM,TAP
PROMPT/IDEP, "Enter Depth Of Hole"
PROMPT/IFED, "Enter Desired Feedrate"
PROMPT/IRAP, "Enter Clearance Distance"
PROMPT/IPECK,"Enter Peck Distance"
PROMPT/ITHRU,"Thru Hole",ON,OFF
PROMPT/IRET, "RETRCT Condition G98/G99",ON,OFF
PROMPT/IRETT,"Retract to clearance plane at last hole",ON,OFF
OBTAIN/TOLER,TOL
$$
  IF (TYPE(IPT) 'EQ' 7 'OR' TYPE(IPT) 'EQ' 24 'OR' $
    TYPE(IPT) 'EQ' 25) THEN
    D1    =IDEP
    IF (VOCABF(ITHRU) 'EQ' VOCABF(ON)) THEN
$$
$$ Add drill point to depth for DRILL, THRU, or DEEP mode.
$$
      IF (VOCABF(IMOD) 'EQ' VOCABF(THRU)   'OR'  $
        VOCABF(IMOD) 'EQ' VOCABF(DEEP)   'OR'  $
        VOCABF(IMOD) 'EQ' VOCABF(DRILL)) THEN
        D1=IDEP+(.3*DIA)
      ELSE
$$
$$ Add TOL*50 to depth for REAM, or BORE mode.

        D1=IDEP+(TOL*50)
      ENDIF
    ENDIF
$$
    IF (TYPE(IPT) 'EQ' 24) THEN
      PP1=POINT/IPT,1
    ELSE
      PP1=POINT/IPT
    ENDIF
    DNTCUT
    GOTO  /PP1
    GODLTA/CLPL
    RAPID
    CUT
$$
    IF (VOCABF(IRET) 'EQ' VOCABF(ON)) THEN
      RETRCT/ON
    ELSE
      RETRCT/OFF
    ENDIF
$$
    CYCLE /IMOD,FEDTO,D1,RAPTO,IRAP,STEP,IPECK,IPM,IFED
    GOTO  /IPT
    CYCLE /OFF
$$
IF (VOCABF(IRETT) 'EQ' VOCABF(ON)) RAPID,GODLTA/CLpl
  ELSE
**WIN
$$
$$   Entity picked is not a Point, Pntvec or Patern.
$$   Press <ENTER> to exit macro and try again.
$$
**PAUSE
**WIN /CLOSE
**RESET/CALL
  ENDIF
$$
TERMAC


$$*********************************************************************
$$                          drill macro
$$*********************************************************************
remove/a
toolng=0   $$ change to 1 for posting "TOOLING" to machine
           $$ 0 for posting "TOOLING" to Vericut (drills holes)
drlmac=macro/a,b,c,d,e,hname,f=fr1                 $$ a=first th1(nn)
       nn=a                                         $$ b=last  th1(nn)
5:     if (toolng 'eq' 1) then
          ob/hname(nn),x1,y1,z1
insert X@X1 Y@Y1
          JUMPTO/Tooln1
       endif
       ppl1=pl/hname(nn),pa,plx                     $$ c=drill plane
       ppl2=pl/hname(nn),pe,c,ppl1                  $$ d=depth
       rp,gt/(pt/io,clpl,ppl1,ppl2)                $$ e=-1 bore / delay
       thick/.1,0                                  $$ e= 0 bore
       rp,go/on,ppl1,c,on,ppl2                     $$ e= 1 drill (pecks)
       thick/0                                     $$ f= feedrate
       if(e'eq'-1)jumpto/10 $$ bore with delay
       if(e'eq' 0)jumpto/15 $$ Bore
       if(e'gt' 0)jumpto/20 $$ pecks time
10:    gd/-(d+.1),fr1
       delay/4
       rp,gd/ (d+.1)
       jumpto/35
15:    gd/-(d+.1),f
       rp,gd/ (d+.1)
       jumpto/35
20:    dis =d/e
       dis1=dis
25:    gd/-(dis +.1),f
       rp,gd/ (dis1+.1)
       if(dis1-d'lt'  0)jumpto/30
       if(dis1-d'ge'  0)jumpto/35
30:    gd/-dis1,f
       dis1=dis1+dis
       jumpto/25
35:    rp,gd/clpl
Tooln1: nn=nn+1
       if(nn-b'le' 0)jumpto/5
       if(nn-b'gt' 0)jumpto/40
40:    termac
$$*********************************************************************
$$                   use this for drilling thru holes
$$ top=pl/1FLB(1)
$$ bot=pl/2fla(1)
$$ dp=dist/top,bot
$$
$$ call/drlmac,a= 1,b=3,c=top,d=dia*.3+dp+.1,e=0,hname=h098
$$*********************************************************************

$$ sample stock and fixture loading
%**stop
$$
$$ ------------- SETUP STOCK AND FIXTURES FOR IPV -------------------
$$ LOADING AND MOVING FIXTURES AND STOCKS
%ccv1=spline/compos,lnc1,lnc4,lnc2,lnc3
%ccv1m=clone/ccv1,mxm
%fixtur/load,51,"fix3_clamp.stk"
%mxr1=mx/yzrot,90
%mxr2=mx/transl,2.75,-4,8.3
%mxr3=mx/mxr2,mxr1
%mxr4=mx/mxm1,mxr3
%stock/stl,21,inches,shutter_op2_fini.stl
%stock/load,21,"op3_fixture3.stk"
%stock/move,21,at,mxt3

$$ .... TO CHANGE COLOR OF FIXTURE ....
%pprint IPV FIXTUR MODIFY 0 GREEN,-1,-1,-1,-1,51

$$ typical holders for little tools (2" longer usually)
$$HOLD(509)=DATA/.42/2,0, .69/2,.25, .69/2,1, 2.5/2,1, 2.5/2,1.62
$$ 2" LONGER HOLDER THAN 509
%HOLD(510)=DATA/.42/2,0, .69/2,.25, .69/2,1+2, 2.5/2,1+2, 2.5/2,1.62+2
%call/holdm,n=510
%invis/hcv(510)

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
call/dumm
$$ ======================

$$ good pocket
call/pokm1
pocket/layer=11,plane,bpl,.05,tpl,part,.5,level,deep
$$ pocket/layer=11,plane,bpl,.05,tpl,part,.5,level,deep
tracut/nm
$$ ---
th/0
rp,gd/clrp1
DRAFT/VIEW=VVV1,PARAMS,CENTER=0.662,0.248,8.517,NORMAL=$
-0.295872,-0.78466,0.544764,YAXIS=0.023944,0.564027,0.825409,SCALE=$
1.185427

f i n i
