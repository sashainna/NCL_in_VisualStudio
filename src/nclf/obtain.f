C*********************************************************************
C*    NAME         :  obtain.f
C*       CONTAINS:
C*           obtain  obpdcs  obattr  obdsrf
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       obtain.f , 25.5
C*    DATE AND TIME OF LAST  MODIFICATION
C*       11/22/17 , 11:08:30
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine obtain
c*       purpose of subroutine: store the canonical form of geometry into
c*       individual scalar variables.                                  
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
       subroutine obtain

      include 'com8a.com'
      include 'const.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld,sub, ipg,iel
      integer*2 istold
      character*64 label

      real*8 val(20),primdata(16)
      integer*2 ktv(4)
      equivalence(ktv,tv)
      integer*4 nclkey,ic,ival(2),nc, strlen1,enum
      integer*2 nwds, ietype, nw
      logical trflg, lds
      integer*2 POCKET,UNITS,MAXANG,CONTCT,ATTRIB
      parameter (POCKET=22)
      parameter (UNITS=841)
      parameter (MAXANG=741,CONTCT=856)
      parameter (ATTRIB=879)
      integer*2 IPOINT,IVECTOR,ILINE,IPLANE,ICIRCLE,ISURFACE,IMATRIX
      INTEGER*2 IPNTVEC,IDATA
      parameter (IPOINT=603,IVECTOR=604,ILINE=605,IPLANE=606)
      parameter (ICIRCLE=607,ISURFACE=609,IMATRIX=610,IPNTVEC=613)
      parameter (IDATA=858)
      integer*2 iist,indx,isavnx,ierr
      logical*2 saveos
c      
       iist =0 
c
c...Added geom type for CADRA
c
       if (ifl(374).ne.0.and.nextyp.eq.9) then
         call parser
         call parser
         if (ityp.eq.1)then
           if (ist.eq.IDATA) then
             iist = DATAST
           else if (ist.eq.IPOINT.or.ist.eq.IVECTOR.or.ist.eq.ILINE
     1      .or.ist.eq.IPLANE.or.ist.eq.ICIRCLE.or.ist.eq.ISURFACE
     2      .or.ist.eq.IMATRIX.or.ist.eq.IPNTVEC)then
             call vctoid(ist,iist,indx)
           else
             isvinx=inx
             call error(1)
             go to 99999
           endif
         endif
       endif

       if (nextyp.ne.5) then
           isvinx=inx
           call error(22)
           go to 99999
       endif
       ifl(44)=9
       ldtflg = .true.
       ldtext = .true.
       isavnx = inx
       call parsit
       ldtflg = .false.
       ldtext = .false.
       lds    = .false.
       if (ityp.eq.2 .and. ist.eq.POCKET) then
         call pokobt
         goto 99999
       endif
c
c...OBTAIN/PS,DS,CS
c
      if (ityp .eq. 1 .and. ist .eq. 728 .or. ist .eq. 729 .or.
     1    ist .eq. 753) then
        call obpdcs
        go to 99999
      endif
c                  must be pt,ve,ln,pl,ci,mx,data,data
       IF (ITYP.EQ.1.OR.(ITYP.EQ.2.AND.((IST.GT.2.AND.IST.LT.8
     1       .or. ist.eq.9 .or. ist .eq. 30 .or. ist .eq. 1)
     2       .OR.IST.EQ.10 .or. ist .eq. 21 .or. ist.eq.DATAST .or.
     3       ist.eq.VSOLID).or.
     4        (ityp.eq.2.and.(ist.eq.2.or.ist.eq.1)
     5           .or.(ityp.eq.5.and.ist.eq.9)))) THEN
C                  VALID VOCAB WORDS ARE CUTTER, THICK, TOLER,
C                  MAXDP, NUMPTS, FEDRAT.  EPM  5-26-89
           IF (ITYP.EQ.1) THEN
               KTV(3) = 1
               IF (IST.EQ.1009) THEN
C                                         ****  FEDRAT
                   VAL(1) = SC(123)
               ELSE IF (IST.EQ.737) THEN
C                                         ****  NUMPTS
                   VAL(1) = IFL(91)
               ELSE IF (IST.EQ.736) THEN
C                                         ****  MAXDP
                   VAL(1) = SC(54)
               ELSE IF (IST.EQ.731) THEN
C                                         ****  TOLER
                   KTV(3) = 4
                   VAL(1) = SC(27)
                   VAL(2) = SC(168)
                   VAL(3) = SC(91)
                   VAL(4) = dacos(SC(92))*RADIAN
c
c...CUTTER
c
               ELSE IF (IST.EQ.716) THEN
                   KTV(3) = 6
c
c......Pre-9.6
c
                   if (sc(169) .lt. 9.549) then
                       DO 50,I=1,KTV(3)
50                         VAL(I) = SC(27+I)
c
c......Post-9.6
c
                   else
                       call obcutr (val,ic)
                   endif
c
c...PSEUDO
c
               ELSE IF (IST.EQ.762) THEN
                   ktv(3) = 6
                   call obcuds (val,ic)
c
c...SHANK
c
               ELSE IF (IST.EQ.192) THEN
                   ktv(3) = 5
                   call obhold (val,1)
c
c...HOLDER
c
               ELSE IF (IST.EQ.157) THEN
                   ktv(3) = 5
                   call obhold (val,2)
c
c...PSEUDO
c
               ELSE IF (IST.EQ.762) THEN
                   ktv(3) = 6
                   call obcuds (val,ic)
C                                         ****  THICK
               ELSE IF (IST.EQ.717) THEN
                   ktv(3) = 7
                   val(1) = sc(23)
                   val(2) = sc(24)
                   val(3) = sc(132)
                   val(4) = sc(177)
                   val(5) = sc(178)
                   val(6) = sc(179)
                   val(7) = sc(180)
C
C...UNITS
C
               ELSE IF (IST.EQ.UNITS) THEN
                   VAL (1) = IFL(264)+1
c
c...MAXANG
c
               else if (ist.eq.MAXANG) then
                   val(1) = sc(80)
c
c...CONTCT
c
               else if (ist.eq.CONTCT) then
                 if (lcntct) then
                   val(1) = 1.0d0
                 else
                   val(1) = -1.0d0
                 endif
c
c...ATTRIB
c
               else if (ist.eq.ATTRIB) then
                 call obattr (val,ktv(3),ierr)
                 if (ierr .ne. 0) go to 99999
c
c...COLOR
c
               ELSE IF (IST.EQ.900) THEN
                  call parsit
                  nc  = strlen1(token2)
                  call ncl_obclr(token2, nc, ivxsub, val(1), val(2), 
     1                      val(3), ierr)
                  if (ierr .ne. 0) then
                      CALL ERROR(ierr)
                      go to 99999
                  endif
                  KTV(3) = 3
c
c...Unrecognized word
c
               ELSE
                   CALL ERROR(372)
                   GO TO 99999
               ENDIF
           ELSE
           if (iist.ne.0.and.ifl(374).ne.0.and.iist.ne.ist)then
             call error(429)
             goto 99999
           endif
c
c....Check for SYMBOL
c
      
           if (ityp .eq. 2 .and. ist .eq. 1) then
             call ub_symbol_name(token2,ivxsub,nclkey,val,i)
             if (i .eq. 1) ist = 31
             if (i .eq. 2) ist = 32
           endif
c
c...tool end pt and tool axis vector for CADRA
c
           if ((ityp.eq.2.and.(ist.eq.2.or.ist.eq.1))
     1           .or.(ityp.eq.5.and.ist.eq.9))then
             inx = isavnx
             do 54 i=1,6
               val(i) = sc(i)
   54        continue
             ktv(3) = 6
             goto 60
           endif
           if (ist.eq.DATAST) then
             lds = .true.
             call gtdesc (tv, nclkey, nwds, ietype)
c
c...Surace primitive
c
           else if (ist.eq.9) then
             call gtdesc (tv, nclkey, nwds, ietype)
             call gtprimt(nclkey,ifl(72),ietype,primdata)
             if (ietype.le.2) then
               call error(477)
               go to 99999
             end if
             do 55 i =1,12
  55         val(i) = primdata(i) 
             if (ietype.eq.6) val(7) = val(7) * RADIAN
             ktv(3) = 9
c
c...Annotation
c
           else if (ist .eq. VANOTE) then
             call gtdesc (tv,nclkey,nwds,ietype)
             call uaf_obtain_notes(nclkey,ival(1),ival(2),val(3),val(6),
     1                             val(9))
             val(1) = ival(1)
             val(2) = ival(2)
             ktv(3) = 11
c
c...Symbol
c
           else if (ist .eq. 32) then
             call ub_symbol_name(token2,ivxsub,nclkey,val,i)
             if (i .ne. 2) then
               call error (189)
               go to 99999
             endif
             ktv(3) = 3
c
c...Solid
c
           else if (ist .eq. VSOLID) then
             call gtdesc (tv,nclkey,nwds,ietype)
             call nclf_get_solid(nclkey,ival(1),val(2),ival(2))
             val(1) = ival(1)
             ktv(3) = ival(2) + 1
c
c...Standard geometry
c
           else
              trflg = .true.
              call gtentt(tv, trflg, nclkey, ietype, val(1))
              lds = .false.
           endif
c                  now loop thru the scalar variables
c                  and store the canonical item into each
c                  one.  two commas in a row signify a void
c                  scalar variable.
           ENDIF
   60      ifl(44)=0
c
           nw = ktv(3)
           do 100 i=1,nw
               if (nextyp.ne.11) then
                   ldtflg = .true.
                   ldtext = .true.
                   call parsit
                   ldtflg = .false.
                   ldtext = .false.
                   if (ityp.eq.2) then
                       idst = ist
                       savid2=token2
                       isvsub = ivxsub
                       keyold = keyhld
                       istold = ist
                       ifl(9)=ifl(11)
                       ifl(10)=ifl(12)
                       if (lds) then
   80                    call dtgetv (nclkey, i, rest, ivoc, 
     x                               nwds,label, sub)
                         if (ivoc.eq.2) then
                           token2 = label
                           ivxsub = sub
                           call vstchk
                           if (ist.ne.2) then
                              if (idst.eq.1) idst = ist
                              if (idst.ne.ist) goto 9089
                              call entcpy
                           else if ((ityp.eq.2).and.(ist.eq.DATAST)) 
     1                                  then                  
                              call vxchk (token2, ivxsub,nclkey,
     1                              ipg,iel,nwds, ist)
                              goto 80
                           else       
                              rest = tv
                              idst = 2
                           endif
                           if (ifl(2).gt.0) goto 9999
                         else if (ivoc.eq.24) then
c
c...added for handle text value
c
                           nc = strlen1(label)
                           length = nc
                           call rmquot (label,length)
                           token2 = label
                           ivxsub = sub
                           ityp = 9
                           tokstr = label
                           length = nrndex(label,' ')
                           lstrng = .true.
                           saveos = nxteos
                           nxteos = .true.
                           call dotext
                           nxteos = saveos
                         else
                           if (ist.ne.1.and.ist.ne.2) goto 9089
                           idst = 2
                           ldtflg = ivoc .eq. 1
                         endif
                       else
                         if (ist.ne.1.and.ist.ne.2) goto 9089
                         idst = 2
                         rest = val(i)
                       endif
                       if (.not. defwf) call vstore
                       defwf = .false.
                       call parsit
                   else if (ityp.ne.5.or.ist.ne.9) then
c                          not two commas in a row
                       call error(87)
                       go to 99999
                   endif
               endif
100        continue
           if (nextyp.ne.11 .and. .not.lds) then
               call error(4)
               go to 99999
           endif
       else
           call error(189)
           go to 99999
       endif
99999  return
c                             error - Ident currently has diff't type
9089   ist = 0
       call error(89)
       return
c                             error from entcpy
9999   ist = 0
       call error(ifl(2))
       return
       end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine obpdcs
c*       Stores the name of the PS, DS, or CS in the provided text
c*       variable.
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
      subroutine obpdcs
c
      include 'com8a.com'
      include 'const.com'
c
      integer*2 nwds,ietype,nc
      integer*4 nclkey,lstkey,nc1,strlen1
c
      character*96 label
c
c...Obtain/PS
c
      ldtext = .true.
      if (ist .eq. 728) then
          call gtdesc (sc(35),nclkey,nwds,ietype)
c
c...Obtain/DS
c
      else if (ist .eq. 729) then
          nclkey = ifl4(15)
c
c...Obtain/CS
c
      else
          nclkey = ifl4(16)
      endif
c
c...Get the label of the geometry
c
      if (nclkey .ne. 0) then
          call ncl_getlab (nclkey,96,label)
          nc1 = strlen1(label)
          nc = nc1
      endif
      if (nclkey .eq. 0) then
          label = ' '
          nc = 0
      endif
c
c...Get the text variable to store the data in
c
      call parsit
      if (ityp .ne. 2 .or. (ist .ne. 1 .and. ist .ne. 24)) go to 9000
      lstkey = 0
      if (ist .eq. 24) call gtdesc (tv,lstkey,nwds,ietype)
		savid2 = token2
		isvsub = ivxsub
c
c...Store the text label
c
      call ncl_pttext (label,nc,lstkey,nclkey)
      idst = TEXTVAR
      call ptdsc3 (nclkey,1,idst,rest)
      call vstore
c
c...End of routine
c
 8000 ldtext = .false.
      return
c
c...Text variable expected
c
 9000 call error (82)
      go to 8000
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine obattr (gval,knwds,kerr)
c*       Retrieves the geometry attributes of an entity.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*
C*       OUTPUT :  
C*          gval    = (1) = Color, (2) = Layer, (3) = Line Style,
C*                    (4) = Line Weight.
C*          knwds   = Number of parameters returned in 'gval' (4).
C*          kerr    = Non-zero if an error occurred.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine obattr (gval,knwds,kerr)
c
      include 'com8a.com'
      include 'const.com'
c
      integer*2 knwds,kerr
c
      real*8 gval(*)
c
      integer*2 nwds,ietype,color,pen,linwgt,lintyp
      integer*4 nclkey
c
c...Get the geometry entity
c
      kerr = 0
      call parsit
      if (ityp .ne. 2 .or. ist .eq. 1 .or. ist .eq. 2 .or.
     1    (ist .ge. 11 .and. ist .le. 17) .or. ist .eq. 19 .or.
     2    ist .eq. 22 .or. (ist .ge. 24 .and. ist .le. 32)) go to 9000
      call gtdesc (tv,nclkey,nwds,ietype)
c
c...Get entity's attributes
c
      call umf_get_attrib(nclkey,color,pen,lintyp,linwgt,layer)
      gval(1) = color
      gval(2) = layer
      gval(3) = lintyp
      gval(4) = linwgt
      knwds = 4
c
c...End of routine
c
 8000 return
c
c...Invalid geometry entity
c
 9000 call error (244)
      kerr = 1
      go to 8000
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine obdsrf (dskey,pskey,cskey)
c*       Retrieves the keys of the Drive, Part, and Check surfaces.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*
C*       OUTPUT :  
C*          dskey   = Drive surface key.
C*          pskey   = Part surface key.
C*          cskey   = Check surface key.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine obdsrf (dskey,pskey,cskey)
c
      include 'com8a.com'
      include 'const.com'
c
      integer*4 dskey,pskey,cskey
c
      integer*2 nwds,ietype
c
      dskey = ifl4(15)
      if (psmult) then
        pskey = ifl4(17)
      else
        call gtdesc (sc(35),pskey,nwds,ietype)
      endif
      cskey = ifl4(16)
c
      return
      end
