C*********************************************************************
C*    NAME         :  poddef.f
C*       CONTAINS:  poddef  pnlist  mclist
C*    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*        poddef.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:26
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : poddef 
C*      parse PODDEF command & set common defaults for pods.
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
      subroutine poddef 
c
      include 'com8a.com'
      include 'cmm.com'
c
      integer*4 np, pinx(4),sub
      integer*2 ier,i,k
c
      real*8 ppar(3)
      character*64 podid(4)
c
      character*64 cmname
      character*100 cmlin 
c
      do 57 k=1,4
         do 55 i=1,64
            podid(k)(i:i) = ' '
   55    continue
   57 continue
c
      ifl(44) = 9
c
c...Get Diameter, Hight & Tilt angle of pod
c
      do 110 i=1,3
          call parsit
          if (.not. SCALAR) go to 9007
          ppar(i) = TV
  110 continue
c
c...Get Patern of pods
c
      call pnlist (np,PODPID,PODHIT,ier)
      if (ier .ne. 0) go to 9900
      if (NXTEOS) go to 300
c         
c...Get name of var to store generated Pod hights
c...or macro name
c
      call mclist (podid, pinx, cmname, sub, cmlin,ier)
      if (ier .ne. 0) go to 9900
c
c...Store Pod definition in common block
c
      PODMAC = cmname
      PMACSUB = sub
      do 210 i=1,4
          PODVAR(i) = podid(i)
          PVARSUB(i) = pinx(i)
  210 continue     
      COMPOD = cmlin
  300 PODPAR(1) = ppar(1)
      PODPAR(2) = ppar(2)
      PODPAR(3) = ppar(3)
      NPODPN = np
      go to 8000
c
 9007 call error (7)
      go to 8000
c
 9900 call error (ier)
c
 8000 return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : pnlist (knp,pn,ph,ierr) 
C*      parses patern list for PODDEF command.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          knp     - number of paterns in list
C*          pn(knp) - array (R*8) of asws for paterns
C*          ph(knp) - array (R*8) of pod' height for patern of PTs. 
C*          ierr    - sytax error number if not 0.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine pnlist (knp,pn,ph,ierr)
c
      include 'com8a.com'
c
      integer*4 knp
      integer*2 ierr
      real*8 pn(*),ph(*)
c
      integer*4 ntyp 
      integer*2 nsvi
c
c...Get Patern of pods
c
      knp    = 0
      ierr   = 0
  200 nsvi   = INX
      call parsit
      if (ITYP .eq. 2 .and. IST .eq. 20) then
          knp  = knp + 1
          pn(knp)  = TV
c
c...Check type of PN & get pod hight if PN of points
c
          call gtpnnp (TV,npt,ntyp)
          if (ntyp .eq. 1) then
              call parsit
              if (ITYP .eq. 2 .and. IST .eq. 2 .or. ITYP .eq. 3 .or.
     -            ITYP .eq. 4) then
                  ph(knp) = TV
              else
                  go to 9004 
              end if
          end if
      else if (knp .eq. 0) then
          go to 9042
      else 
          INX   = nsvi
          go to 8000 
      end if
c
      if (NEXTYP .ne. 11) then
          if (knp .lt. 25) then
             go to 200
          else
             go to 9015
          end if
      end if
      go to 8000
c
c...Errors
c
 9001 ierr   = 1
      go to 8000
 9004 ierr   = 4
      go to 8000
 9015 ierr   = 15
      go to 8000
 9042 ierr   = 342
c
 8000 return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : mclist (varid,pinx, pmname,cmlin,ierr) 
C*      parses surface list for PODDEF command.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          varid(4) - array (c*64) of asws for macro dummy parameters 
C*          pmname   - macro name seen as c*64 
C*          cmlin    - text command line used to call macro.
C*          ierr     - syntax error number if not 0.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C***********************************************************************
c
      subroutine mclist (varid, pinx, pmname, sub, cmlin,ierr)
c
      include 'com8a.com'
c
      character*64 varid(*)
      integer*4 ierr,sub, pinx(*)
      character*(*) cmlin
c
      integer*2 i,j,k,ktv(4),irest(4)
      integer*2 iel,ipg,izer,nw
      integer*4 isvtk4
      character*64 cmname,pmname
      character*3 vnms(4) 
      character*40 label
c
      equivalence (TV,ktv),(rest,irest)
      equivalence (ktv(1),ipg), (ktv(2),iel)
c
      data vnms /'@K1','@K2','@K3','@K4'/
      data izer /0/ 
c
      ierr   = 0
      pmname = ' '
c         
c...Get name of var to store generated Pod hights
c...or macro name
c
      call parsit
      if (ITYP .ne. 1 .and. ITYP .ne. 2) go to 9004
      if (ITYP .eq. 2) go to 2000
c
c...MACRO
c
      if (IST .ne. 806) go to 9061 
      call parsit
      if (ITYP .ne. 2 .or. IST .ne. 11) go to 9054
      cmname = token2
      sub = ivxsub
      if (.not.NXTEOS) go to 9004
c
c......Check if macro is declared
c
      token2   = cmname
      call vstchk
      if (IST .ne. 11) go to 9054
      if (ktv(3) .lt. 3 .or. ktv(3) .gt. 4) go to 9023
      nw     = 20
c
c......Make call macro statement
c
      cmlin  = ' '
      i = strlen1 (cmname)
      cmlin(1:i)  = cmname(1:i)
      do 855 k=1,ktv(3) 
          cmlin(i+1:i+1) = ','
          call ncl_getmac_plabel(k, label, j)
          cin(i+2:) = label(1:j)
          nccin = i + 1 + j
          i      = i + j + 5
          cmlin(i-3:i) = '=' // vnms(k) 
c
c......Reserve temporary variables to use in macro
c
          ivxsub = 0
          TOKEN2(1:6) = vnms(k)
          call vstchk
          if (IST.eq.14) goto 840
          savid2 = TOKEN2
          isvsub = ivxsub
          ifl(9) = ifl(11)
          ifl(10) = ifl(12)
          IDST = 14
          REST = 0.
          irest(3) = 32767
          irest(4) = 14
          call vstore
  840     varid(k) = token2
          pinx(k) = ivxsub
  855 continue
      pmname = cmname
      go to 8000
c
c...List of permanent variables (numpt,PT(,PV))
c          
 2000 k      = 0 
 2100 if (ITYP .ne. 2) go to 9082
      if (IST .ne. 1) go to 20
c
c...unknown - check if reserved
c
      isvtk4 = 0
      if (ivxsub.ne.0) then
         isvtk4 = ivxsub
         ivxsub = 0
         call vstchk
         if (IST.eq.14) goto 50
      endif
      SAVID2 = TOKEN2
      isvsub = 0
      ifl(9)  = ifl(11)
      ifl(10) = ifl(12)
      IDST = 14
      REST = 0.
      irest(3) = 32736  
      irest(4) = 14
      call vstore
      ivxsub = isvtk4
      go to 80
c
   20 if (.not.(ITYP.eq.2.and.ivxsub.ne.0)) go to 60
c
c...known and subscripted - check subscript
c
      isvtk4 = ivxsub
      ivxsub = 0
      call vstchk
      if (ITYP.ne.2.and.IST.ne.14) go to 9088
c
c...subscript out of range
c
   50 if (isvtk4 .gt. 1000000) go to 9085
      ivxsub = isvtk4
      go to 80
c
c...identifier has not been reserved
c  
   60 if (IST.ne.14) go to 9088
c
c...sub'd var - start at sub 1
c
      ivxsub = 0
   80 k     = k + 1
      varid(k) = token2
      if (.not.NXTEOS) then
          if (k .lt. 4) then
              call parsit
              go to 2100
          else
              go to 9004
          end if
      end if
      if (k .lt. 3) goto 9082
      go to 8000
c
 9004 ierr   = 4
      go to 8000
c
 9023 ierr   = 423
      go to 8000
c
 9054 ierr   = 54
      go to 8000
c
 9061 ierr   = 61
      go to 8000
c
 9082 ierr   = 282
      go to 8000
c
 9085 ierr   = 85
      go to 8000
c
 9088 ierr   = 88
c
 8000 return
      end

