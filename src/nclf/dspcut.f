
C*********************************************************************
C*    NAME         :  dspcut.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       dspcut.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:59
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dspcut
c*       plot cutter circles                      
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine dspcut 

      include 'com4a.com'
      include 'cutter.com'
      include 'wrksys.com'

      real*4 asc(320)
      equivalence (sc,asc)

      real*8 buf(6),cbuf(20)
      integer*4 idc(3),icfl(10),symkey(3)
      integer*2 i
      integer*2 i2v3, i2v4
      character*80 sym,ssym,hsym
      data i2v3 /3/, i2v4 /4/ 
c
c...Start new motion sequence
c
cc      call motbgn
c
c...Set the flag for display cutter
c

      ifl(130)=0
c 
c...Set the tool motion and tool axis values.
c
      do 10 i=1,6 
          buf(i)=sc(i)
   10 continue
c
c...Units millimeters
c...scale buffer down
c
      if (ifl(264).eq.1) then
          do 15,i=1,3
              buf(i) = buf(i) / 25.4
   15     continue
      endif
c
c...Apply working coordinate system matrix
c
      if (lwrk) then
          call conent(buf, wrkmx, i2v3)
          call conent(buf(4), wrkmx, i2v4)
          if (wrkscl.ne.1.0d0) then
              buf(4) = buf(4) / wrkscl
              buf(5) = buf(5) / wrkscl
              buf(6) = buf(6) / wrkscl
          endif
      endif
c
c...Apply tracut matrix
c
      if (ifl(73) .eq. 1 .and. itrafl .eq. 1) then
          call conent (buf,sc(41),i2v3)
          call conent (buf(4),sc(41),i2v4)
      endif
c 
c...Display the cutter
c 
      idc(1) = icutfl(1)
      if (idc(1) .eq. 0) idc(1) = 1
      idc(2) = icutfl(5)
      idc(3) = icutfl(6)
      call gdscut (cbuf,sym,ssym,hsym,symkey,icfl)
      call nclf_cutter_set(cbuf,idc,icutfl(2),cutsym(1),cutsym(2),
     1                     cutsym(3),cutkey)
      call pltmot (buf,buf,sc(7))
c
c...End of this motion sequence
c
cc      call motend
      return
      end
