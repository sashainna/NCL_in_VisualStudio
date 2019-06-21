C*********************************************************************
C*    NAME         :  podpts.f
C*       CONTAINS:  podpts
C*    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*        podpts.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:26
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : podpts (iwhich) 
C*      parse PODPTS command & calculate pods.
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
      subroutine podpts (iwhich) 
c
      include 'com8a.com'
      include 'cmm.com'
c
      integer*2 iwhich
c
      integer*4 ns,np,len,strlen1,pinx(4), sub
      integer*2 i,k,ier,nsvx
      real*8 sf(25),pn(25),ppar(3),ph(25)
      character*64 cmname, podid(4)
      character*100 cmlin 
c
      do 57 k = 1,4
         do 55 i=1,64
            podid(k)(i:i) = ' '
   55    continue
   57 continue
c
      iwhich  = 0
      ifl(44) = 9
      cmname = PODMAC 
      ppar(1) = PODPAR(1)
      ppar(2) = PODPAR(2)
      ppar(3) = PODPAR(3)
c
c...Get Patern of pods
c
      nsvx   = INX
      call parsit
      if (ITYP .eq. 2 .and. IST .eq. 20) then
          INX = nsvx
          call pnlist (np,pn,ph,ier)
          if (ier .ne. 0) go to 9900
      else
          if (NPODPN .eq. 0) go to 9489
          np     = NPODPN
          do 110 i=1,np
             pn(i) = PODPID(i)
             ph(i) = PODHIT(i)
  110     continue
          INX = nsvx
      end if
c
c...Get part SFs (or PLs)  
c
      call sflst1 (ns,sf,ier)
      if (ier .gt. 1) go to 9900
      if (NXTEOS) then
          len = strlen1(PODVAR(1))
          if (len .eq. 0) go to 9490
          podid(1) = PODVAR(1)
          podid(2) = PODVAR(2)
          podid(3) = PODVAR(3)
          podid(4) = PODVAR(4)
          len = strlen1(PODMAC)
          if (len .gt. 0) cmlin = COMPOD
          go to 3000
      end if
c         
c...Get name of var to store generated Pod hights
c...or macro name
c
      call mclist (podid,pinx, cmname, sub,cmlin,ierr)
      if (ier .ne. 0) go to 9900
c
c...Process command
c
 3000 call gtpodh (np,pn,ph,ns,sf,podid,pinx, ppar)
      if (ifl(2) .ne. 0) go to 9000 
c
c...Process MACRO if specified
c
      if (cmname .ne. ' ') then
          NEXTYP = 5
          INX    = 1
          cin    = cmlin
          call callit
          if (ifl(2) .ne. 0) go to 9000 
c
c...Set flags to process TERMAC
c
          iwhich = 3
          ifl(310) = np
          cin = ' '
      end if
      go to 8000
c
c...Syntax errors 
c
 9000 if (ifl(2) .ne. 0) then
          call error(ifl(2))
          go to 8000
      end if
 9489 ier    = 489 
      go to 9900
c
 9490 ier    = 490 
c
 9900 call error (ier)
c
 8000 return
      end
