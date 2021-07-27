c **
C*********************************************************************
C*    NAME         :  lrpop.f
C*       CONTAINS:
C*    COPYRIGHT 1991 (c) Numerical Control Computer Sciences
C*     MODULE NAME AND RELEASE LEVEL 
C*       lrpop.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       10/27/16 , 13:46:33
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE: subroutine lrpop
C*
C*    PURPOSE
C*       To pop or remove an entry from the stack of data kept about 
C*       looping regions.                                           
C*                                                                    
C*       See LRPUSH.FOR for documentation on the format of the stack.
C*
C*    PARAMETERS   
C*       INPUT  : 
C*          none.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine lrpop

      include 'com4a.com'

      integer*2 maxent, stkrec
      character*64 ttok,tlab,msg
      integer*4 tsub,tkey,tipg,tiel
      integer*2 ttyp,tnwds
c
c **********************************************************************

c
c.....Removed for temporary variables in loops - ASF 12/31/13
c      curmac = ' '
      stkrec = ifl(66)
      maxent = ifl(174)

c          If no looping region record exists, report internal error.
      if (stkrec .eq. 0) then
          call error (415)
          goto 99999
      endif
 
c          Read stack ranfil record.
100   call getran (jbr, stkrec)
      if (maxent .gt. 1) then
          maxent = maxent - 1
          stkrec = jb(143)
          if (jbc(1:64) .ne. 'LOOPST' .and. jbc(65:128) .eq. ' ')
     1        curmac = jbc(1:64)
          goto 100

c              This record contains the entry.  Re-initialize the entry to
c              blanks and zeros then write it to the Ranfil.
      else
c
c.....Remove temporary variables created in loops - ASF 12/31/13
c
          call vxlfst
200       call vxlnxt(tlab,tsub,tkey,tnwds,ttyp,tipg,tiel)
          if (ttyp.gt.1) then
              ind = index(tlab,'#') - 1
              if (ind.gt.0.and.
     x            tlab(1:ind).eq.jbc(1:64)) then
                  if (tkey.gt.0) call dlgeom(tkey)
              endif
              goto 200
          endif
          jbc(1:64) = '        '
          jbc(65:128) = '        '
          jbc(129:192) = '        '
          jb4(49) = -1
          jb4(50) = -1
          jb4(51) = -1
          jb4(53) = 0
          jb4(54) = 0
          jb4(55) = 0
          jb(103) = 0
          jb(104) = 0
          call putran (jbr, stkrec)
c              Decrement the entry counter.
          ifl(174) = ifl(174) - 1
      endif
99999 return
      end
