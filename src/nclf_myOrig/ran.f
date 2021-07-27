C*********************************************************************
C*    NAME: ran.f
C*
C*    MODULE NAME AND RELEASE LEVEL
C*        ran.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:33
C*********************************************************************
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ranlst (nlin,tlin,mlin)
c*       This routine lists all keys stored in ranfile with
c*       associate labels, subscripts and geometry type.
c*
c*    PARAMETERS
c*      INPUT: nlin - 
c*             tlin -
c*             mlin -
C*********************************************************************

      subroutine ranlst (nlin,tlin,mlin)
c
      include 'com8a.com'

      integer*2 nlin,tlin,mlin 
      integer*4 ipg,iel,ial
      integer*2 ntyp
      integer*4 keyid,i4sub,kmax,kmin,ismin,ismax,kwds
      character*64 namin,namax
c
      kmin = 10000000
      kmax = -1
      ial  = 0
      if (.not.NXTEOS) then
         call parsit
         if (token2(1:3) .eq. 'ALL') ial = 1
      end if
      if (nlin .eq. 0) then
          nlin   = 15
          mlin   = 9
          tlin   = 15
          call opnwin()
      end if
c
      call vxlfst
 110  format ('key =',i6,'  ',a32,'(',i3,')','  typ ',i3,' nw = ',i5) 
 115  format ('key =',i6,'  ',a32,'(',i3,')') 
c
 100  call vxlnxt (token2, i4sub, keyid, kwds, ntyp, ipg, iel)
      if (ntyp .eq. 1) goto 8000
      if (ial .eq. 1) then
         write (cout,110) keyid,token2,i4sub,ntyp,kwds
         call wypstr (cout,80,nlin,tlin,mlin)
      end if
      if (kmin .gt. keyid) then
         kmin = keyid
         namin = token2
         ismin = i4sub
      end if
      if (kmax .lt. keyid) then
         kmax = keyid
         namax = token2
         ismax = i4sub
      end if
      goto 100
c
 8000 write (cout,115) kmin,namin,ismin
      call wypstr (cout,80,nlin,tlin,mlin)
      write (cout,115) kmax,namax,ismax
      call wypstr (cout,80,nlin,tlin,mlin)
      return
      end
