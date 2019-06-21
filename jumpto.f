C*********************************************************************
C*    NAME         :  jumpto.f
C*       CONTAINS:
C*            jumpto   jmplab
C*    COPYRIGHT 1999 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       jumpto.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:13
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine jumpto
C*      this routine handles jumpto statements.
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
      subroutine jumpto

      include 'com8a.com'

      integer*2 ktv(4)
      equivalence (ktv,tv)
      if (nextyp.ne.5) then
          call error(22)
          go to 99999
      endif
      if (ifl(38).eq.0.and.ifl(45).eq.0) then
          call error(80)
          go to 99999
      endif
c
c...  set flag to parse label in jumpto statement with a : if lang= cadra
c
      if (ifl(374) .ne. 0) ifl(376)=1
      call parsit
      ifl(376)=0
      if (nextyp.ne.11) then
          ist=1
          call error(4)
          go to 99999
      endif
      if (ityp.eq.3) then
          call vstchk
          if (ist.eq.13) ityp=2
      endif
c
c...Jumpto the label
c
      if (ityp.eq.2.and.ist.eq.13) then
          call jmplab (1)
      else if (ist.ne.1) then
          call error(82)
          go to 99999
      else
          call error(9)
          go to 99999
      endif
c
99999 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine jmplab (kerr)
C*      this routine performs a jumpto a label.  'token' must have the
C*      label to jumpto and the 'tv' array should also be setup via
C*      the 'vstchk' routine.
C*    PARAMETERS   
C*       INPUT  : 
C*          kerr  =  0 Do not call the error routine.
C*       OUTPUT :  
C*          kerr  =  Non-zero when an error occurred and the error
C*                   routine was not called.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine jmplab (kerr)
c
      include 'com8a.com'
c
      real*8 savetv
      character*64 svtkn
c
      integer*4 i, lnum,svsub,ilin5,ilin6
      integer*2 ktv(4)
      equivalence (ktv,tv)
c
      integer*2 kerr,ie
c
      call getran(jb,ktv(1))
      call nclf_src_rec_to_line (jb4(ktv(2)),lnum)
      call nclf_src_rec_to_line (ifl4(5),ilin5)
      call nclf_src_rec_to_line (ifl4(6),ilin6)
      if (lnum.ne.0) then
          if (lnum-1.ge.ilin5.and.lnum-1.le.
     1        ilin6) go to 100
          savetv=tv
          svtkn=token2
          svsub=ivxsub
c
c...Find the JUMPTO label
c...in the source file
c...6/26/91  -  Bobby
c
          call labchk (ilin5,ilin6,svtkn,svsub,i)
          if (i .eq. 0) then
            ie = 111
            go to 9000
          endif
          tv     = savetv
          lnum = i
          inx    = 1
          ain(1) = ' '

100       continue
          call ifjump(lnum,ie)
C
C... Error jump into if-then-else not allowed
C
          if (ie.ne.0) then
            ie = 509
            goto 9000
          endif
          call putw2(nline)
          ifl(47)=1
          nline=lnum-ifl(123)
          call nclf_src_line_to_rec (nline,ifl4(4))
      else
          ie = 81
          go to 9000
      endif
      go to 99999
c
c...An error occurred
c
 9000 if (kerr .eq. 0) then
          kerr = ie
      else
          call error (ie)
      endif

99999 return
      end
