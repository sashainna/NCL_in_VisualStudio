C*********************************************************************
C*    NAME         :  outmot.f
C*       CONTAINS:
C*    COPYRIGHT 1990 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       outmot.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:24
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine outmot
c*          Output motion point to screen or printfile and to cl file.
c*                                                               
C*    PARAMETERS   
C*       INPUT  : 
C*          tb        - tool end point, tool axis vector & tool fwd vector
C*          ds        - driver surface normal vector
C*          lnk       - print line count
C*          jptk      - point number
C*          isubcl    - cl file record subclass
C*       OUTPUT :  
C*          tdat      - clfile buffer
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine outmot (tdat, tb, ds, lnk, jptk, iclass, isubcl)

c      implicit undefined (a-z)
c      include 'com.com'
      include 'com8a.com'

      real*8 tdat(420), tb(24),ds(3)
      integer*2 lnk, jptk, iclass, isubcl

      integer*2 i, n, numitm, ntk1
      logical lv92
      integer*2 ntk,iptk
      equivalence (ntk,ifl(79)), (iptk,ifl(50))
c
      lv92 = sc(169) .lt. 9.2490

      ntk1 = ntk+1
      if (iclass.eq.5200) then
        n = 6
        max = 420
      else
        n = 3+ifl(82)*3
        max = 120
      endif
      if (ifl(104).eq.0) then
        do 10 i=1,n
10      tdat(ntk+i)=tb(i)
      else
        call modfy(tdat(ntk1), tb)
      endif
      if (iclass.eq.5200) then
        do 20 i=7,21
20      tdat(ntk+i) = tb(i)
c
c...If the normal to DS is a null vector use the normal vector from the motion array
c
        if (tdat(ntk+19).eq.0 .and. tdat(ntk+20).eq.0 .and.
     1    tdat(ntk+21).eq.0) then
          tdat(ntk+19) = ds(1)
          tdat(ntk+20) = ds(2)
          tdat(ntk+21) = ds(3)
        endif
        n = 21
      endif
      if (ifl(154).eq.1) call tdsply(lnk,tdat(ntk1),jptk)
      if (ifl(95).eq.1) goto 999
c
c... removed Chkpts condition (lcmm)
c
      if (ifl(246).gt.0 .and. lv92 )
     1    call savmot(tdat(ntk1),tb(7))
c
c...Output first point after AUTOST as single cl record
c
      if (ifl(90) .eq. 1 .and. iptk.eq.1) then
        numitm = 1
        if (ifl(42).eq.0) then
            call putcl5 (iclass,isubcl,numitm,tdat(1))
        else if (ifl(42) .eq. 1 .and. ifl(246) .ne. 0) then
            call putcl5 (iclass+100,isubcl,numitm,tdat(1))
        endif
      else
c
c...Output tdat array to the cl file if record is full
c
        ntk = ntk+n
        if (ntk.lt.max) goto 999
        numitm=ntk/n
        if (ifl(42).eq.0) then
            call putcl5 (iclass,isubcl,numitm,tdat(1))
        else if (ifl(42) .eq. 1 .and. ifl(246) .ne. 0) then
            call putcl5 (iclass+100,isubcl,numitm,tdat(1))
        endif
        ntk = 0
        isubcl = 6
      endif

999   return
      end
