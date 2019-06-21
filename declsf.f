C*********************************************************************
C*    NAME         :  declsf.for
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       declsf.for , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:32
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine declsf
C*      this routine parses surf declarations. the valid syntax
C*      combinations are:
C*          1. (name =) surf/<fit,>cv1,cv2,thru,cv5,cv6....
C*not implemented2. (name =) surf/pl1,pl2,cv1,cv2,cv5.....
C*          3. (name =) surf/sf1,sf2,thru,sf4
C*          4. (name =) surf/xl,pl1,zl,pl2,radius,1,pt1,pt2
C*          5. (name =) surf/quilt,9309,9310,......
C*          6.
C*          7. (name =) surf/offset,sf1,zlarge,.250
C*          8. (name =) surf/fillet,sf1,sf2,...
C*          9. (name =) surf/select,mshsf1,stpat,ndpat
C*         10. (name =) surf/IN,sf1,cv1,...
C*         11. (name =) surf/OUT,sf1
C*         12. (name =) surf/sf1
C*         13. (name =) surf/revolv,cv,pt,ve,sang[,eang]
C*      the following represents the info passed to geogn2 in syscom:
C*             sc(10)             !       sc(11) thru sc(nn)
C*--------------------------------!----------------------------------------
C* type  subtype  numwds  notused !   sc(11)       sc(12)       sc(13)
C* ----  -------  ------  ------- !   ------       ------       ------
C*  607     1   num elms  1 if fit!   sc(11)-sc(nn)=elements (max=50)
C*  607     2   num elmnts  *     !   pl1 loc      pl2 loc     elements
C*  607     3   num elmnts  *     !   sc(11)-sc(nn)=elements
C*  607     4       *       *     !   mod num      pl1 loc    mod num etc.
C*  607     5   num elmnts  *     !   integers representing patches of quilt
C*  607     7       *       *     !   sf name      modifier     distance
C*  607     8       *       *     !   sf1 loc      sf2 loc      nrpt loc ...
C*  607     9   strt ptch end ptch!   sf1 loc      not used     not used
C*  607    10       *       *     !   sf1 loc      cv1 loc
C*  607    11       *       *     !   sf1 loc
C*  607    12       *       *     !   sf1 loc
C*  607    13   num elms    *     !     cv           pt         ve
C**************************************************************************
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
       subroutine declsf
      include 'com8a.com'
 
       parameter (maxpn=20)
       parameter (maxcv=50)
 
       real*8 jbuff(35),e(20)
       integer*2 ie(80)
       integer*4 nclkey
       equivalence (e,ie)
       character*6 token6,sc11ch
       character*1 aout(50)
       character*(MAX_PATH) fname,fname2
       equivalence (token2,token6)
 
c
c...Changed for RS6000 . Paul
c...11/19/91
c
c      integer*2 isvtkn(4),itkn(4),roclun,ierror,isc12(4)
c
       integer*2 ierror,isc12(4)
       integer*4 roclun
c
 
       equivalence (aout,cout)
       equivalence (sc(11),leq3),(leq3,sc11ch),(sc(12),isc12)
       logical leq3
       logical brafnd,lmesh
      real*4 u1,v1,u2,v2,uv(4)
      equivalence(sc(19),uv),(u1,uv(1)),(v1,uv(2)),(u2,uv(3)),(v2,uv(4))
      logical lflg
      integer*2 primtyp,errfl
      integer*2 fillet, select, IN, OUT, REDEF, DISK, COMBIN
      parameter (fillet=402)
      parameter (select=1074)
      parameter (IN=652)
      parameter (OUT=653)
      parameter (REDEF=840)
      parameter (DISK=1097)
      parameter (COMBIN=1071)
 
       if (ityp.eq.1.and.ist.eq.COMBIN) then
         isc10(2) = 10
         sc(11) = 0.
         sc(19) = 0.
         itype = 0
         call parsit
         if (ityp.eq.2.and.ist.eq.SURF) then
           itype = 6
           call gtdesc (tv, nclkey, nwds, itype)
           call sftype (nclkey, isftyp)
           if (isftyp.eq.0.or.isftyp.eq.27.or.isftyp.eq.28) then
             call error (321)
             goto 99999
           endif
           call nclf_genpocket_netsf(nclkey,errfl)
           if (errfl.ne.0) then
             call error(errfl)
             goto 99999
           endif
           itype = 9
           call ptdesc(nclkey,itype,tv)
           sc(11) = tv
           isc10(2) = 14
         else
           call error(186)
           goto 99999
         endif
         goto 88888
       endif
 
c                                             quilt or mesh
       if (ityp.eq.1.and.(ist.eq.837.or.ist.eq.839)) then
c                                   test if this ncl has surf package
           call sfpkg (brafnd)
           if (.not.brafnd) then
             call error (278)
             goto 99999
           endif
cccccccccccc
c          its a rockwell quilt surface or md or northrop mesh surface
c                - get the file name that contains the patches
cccccccccccc
           lmesh=(ist.eq.839)
           roclun=4
           fname=' '
           brafnd=.false.
           isvinx=inx
           i=index(cimage(1:nccimg),',')
           if (i.eq.0)i=inx
8          i=i+1
           if (cimage(i:i).eq.' '.and.i.lt.72)goto 8
c
c...No filename given
c...Bobby  -  12/13/96
c
           if (i .ge. 72) then
               call error(271)
               go to 99999
           endif
 
           j=index(cimage(i:nccimg),',')
           if (j.eq.0)j=72-i
           fname=cimage(i:i+j-2)
           i=index(fname,']')
           j=index(fname(i+1:),'.')
           if (j.eq.0) then
             j=index(fname,' ')
             fname(i+j:)='.msh'
           endif
 
c         call flname to get the expanded file name. kathy
c
           call flname(5, fname, fname2)
           do 10 i=1,60
             if (ain(inx).eq.',') goto 11
             inx=inx+1
10         continue
 
11         continue
           inx=inx+1
            call flopen(roclun, fname2, 'OLD', 'SEQUENTIAL',
     x          'UNFORMATTED', 80,
     x          'NULL', ierror)
           if (ierror.ne.0) then
                if (debug)then
                    write(cout,1717)ierror
1717                format (' ierror=',i4)
                    call putmsg (cout,80,18,0)
                endif
 
                if (ierror.eq.29) then
c                         file not found
                     call error(270)
                else
                     call error(271)
                endif
                go to 99999
           endif
 
           if (.not.lmesh) then
c                                  rockwell quilt - get patches
             isc10(2)=5
 
             do 18,i=1,12
               call parsit
               if (scalar) then
                    sc(10+i)=itv
               else
                    call error (53)
                    close (roclun)
                    go to 99999
               endif
               if (nextyp.eq.11) then
                   isc10(3)=i
                   go to 88888
               endif
18           continue
             call error (15)
             close (roclun)
             go to 99999
           endif
c                            mcdonnell-douglas or northrop mesh
           isc10(2)=6
           isc10(3)=0
           isc10(4)=0
 
           if (nextyp.eq.11) then
             sc11ch=' '
             goto 88888
           endif
 
           call parsit
c                            get name of surface
           if (ityp.ne.1.and.ityp.ne.2.and.ityp.ne.3) then
c...
             call error(87)
             close (roclun)
             goto 99999
           endif
           if (ityp.eq.2.and.ist.eq.2) then
             write(sc11ch,1010) itv
1010         format(i6)
             j=0
             do 30 i=1,6
               if (sc11ch(i:i).ne.' ') then
                 j=j+1
                 sc11ch(j:j)=sc11ch(i:i)
               endif
30           continue
             if (j.ne.6) sc11ch(j+1:)=' '
           else
             sc11ch=token6
           endif
c                            get optional start and end patch numbers
           if (nextyp.ne.11) then
             call parsit
             if (ityp.ne.3.or.itv.lt.0)then
               call error(224)
               close (roclun)
               goto 99999
             endif
             isc10(3)=itv
             if (nextyp.eq.11) then
               isc10(4)=itv
             else
               call parsit
               if (ityp.ne.3.or.itv.lt.0) then
                 call error(224)
                 close (roclun)
                 goto 99999
               endif
               if (nextyp.ne.11) then
                 call error(4)
                 close (roclun)
                 goto 99999
               endif
               isc10(4)=itv
             endif
           endif
           goto 88888
       endif
c
c...sf2=sf/sf1
c
       if (ityp.eq.2.and.ist.eq.9 .and. nextyp .eq. 11) then
         call gtdesc (tv, nclkey, nwds, itype)
         call sftype (nclkey, isftyp)
         sc(11)=tv
         isc10(2)=12
         if (isftyp .eq. 27) isc10(2)=3
         isc10(3)=1
         goto 88888
       endif
c                                         super surface
       if (ityp.eq.2.and.ist.eq.9) then
         j=11
         k=0
         l=ifl(4)+1
         do 50 i=1,40
           call gtdesc (tv, nclkey, nwds, itype)
           call isitwf (nclkey, iwf)
           call sftype (nclkey, isftyp)
           if (isftyp.eq.25 .or. isftyp.eq.27) then
             call error(321)
             goto 99999
           endif
 
           if (j.lt.23) then
             sc(j)=tv
             j=j+1
           else
             k=k+1
             if (k.gt.35) then
               call putran(jbuff,l)
               l=l+1
               k=1
             endif
             jbuff(k)=tv
           endif
           if (nextyp.eq.11.or.(nextyp.eq.7.and.ifl(111).ne.0)) then
             isc10(2)=3
             isc10(3)=i
             if (j.gt.22) call putran(jbuff,l)
             go to 88888
           endif
           idtype = 9
           call parsit
           if (err) goto 99999
           if (ityp.ne.2.or.ist.ne.9) then
             call error(186)
             goto 99999
           endif
50       continue
         call error(15)
         goto 99999
       endif
 
       if (ityp.eq.1.and.ist.eq.REDEF) then
c                                              **** Trimmed surface
         isc10(2) = 10
         sc(11) = 0.
         sc(19) = 0.
         itype = 0
         call parsit
         if (ityp.eq.2.and.ist.eq.SURF.or.ist.eq.PLANE) then
           sc(11) = tv
           itype = 6
           if (ist.eq.SURF) then
             call gtdesc (tv, nclkey, nwds, itype)
             call sftype (nclkey, isftyp)
             if (isftyp.eq.0.or.isftyp.eq.27.or.isftyp.eq.28) then
               call error (321)
               goto 99999
             endif
             u1 = .5
             v1 = .5
             iret = 9
             call parsuv(iret, lflg, u1, v1)
             if (iret.gt.0) then
               call error(iret)
               goto 99999
             endif
           endif
           call parsit
         endif
         sc(12) = 0.
         i = 0
         if (.not. vocab .or. ist.ne.OUT .and.
     x      (itype.ne.9 .or. ist.ne.IN)) then
           call error(478)
           goto 99999
         endif
         if (ist.eq.OUT) then
           call parsit
           if (.not. geom .and. (ist.ne.CURVE.or.ist.ne.CIRCLE)) then
             call error(21)
             goto 99999
           endif
           sc(12) = tv
           i = 1
           isc10(3) = 1
           if (nxteos) goto 88880
           call parsit
         endif
         if (.not. vocab .and. ist.ne.IN) then
           call error(479)
           goto 99999
         endif
         idtype = CURVE
         call parsit
         do while (geom .and. (ist.eq.CURVE.or.ist.eq.CIRCLE))
           i = i+1
           call gtdesc (tv, nclkey, nwds, itype)
           call ncl_put_key (nclkey)
           idtype = CURVE
           call parsit
         enddo
         if (ityp.ne.7) then
           call error(21)
           goto 99999
         endif
         isc10(3) = i
         go to 88880
       endif
       if (ityp.eq.1.and.ist.eq.OUT) then
c                                              **** Extract trimmed surface
         isc10(2) = 11
         i = 0
         call parsit
         if (ityp .eq. 2 .and. (ist.eq.SURF .or. ist.eq.VSOLID)) then
             call gtdesc (tv, nclkey, nwds, itype)
             if (ist .eq. SURF) then
                 call sftype (nclkey, i)
             else
                 call nclf_get_solid (nclkey,istyp,params,nparams)
                 if (istyp .eq. 10) i = 27
             endif
             if (i .eq. 27) then
                 sc(11) = tv
                  asn = tv
                  isc10(1) = 607
                  isc10(2) = 15
                 call prsout (nclkey,2)
                 go to 88880
             endif
         else
           call error(186)
           goto 99999
         endif
         if (i.ne.99) then
           call error (321)
           goto 99999
         endif
         sc(11) = tv
         go to 88880
       endif
 
       if (ityp.eq.1.and.ist.eq.861) then
c                                              **** revolve
           isc10(2) = 13
c
c......Get curve to revolve
c
           call parsit
           if (ityp .ne. 2 .or. (ist.ne.POINT .and. ist.ne.PNTVEC
     x         .and.ist.ne.5 .and. ist.ne.7 .and. ist.ne.8)) then
               call error (21)
               goto 99999
           endif
           sc(11) = tv
c
c.....Get radii
c
           call parsit
           if (scalar .and. tv.gt.1.d-4) then
             isc10(3) = 2
             sc(12) = tv
             if (.not. nxteos) then
               call parsit
               if (scalar .and. tv.gt.1.d-4) then
                 isc10(3) = 3
                 sc(13) = tv
                 if (.not.nxteos) call parsit
               endif
             endif
             goto 88888
c
c.....Get axis point for revolution
c
           else if (ityp .ne. 2 .or. (ist.ne.3 .and. ist.ne.21 .and.
     x                                              ist.ne.LINE)) then
              call error (20)
              goto 99999
           endif
           sc(12) = tv
           if (ist.eq.LINE .or. ist .eq. 21) then
              sc(13) = 0.0
           else
c
c.....Get axis vector (could be PV)
c
             call parsit
             if (ityp .ne. 2 .or. (ist .ne. 4 .and. ist .ne. 21)) then
                 call error (11)
                 goto 99999
             endif
             sc(13) = tv
           endif
c
c......Get starting angle
c
           if (nxteos) then
             sc(14) = 0.
             sc(15) = 360.
           else
             call parsit
             if (.not. scalar) then
               call error (3)
               goto 99999
             endif
             sc(14) = tv
c
c......Get ending angle
c
             call parsit
             if (.not. scalar) then
                call error (3)
                goto 99999
             endif
             sc(15) = tv
           endif
c
c......End of statement
c
           if (.not. nxteos) then
              call error (4)
              goto 99999
           else
              goto 88880
           endif
       endif
 
       if (ityp.eq.1 .and. ist.eq.DISK) then
c                                              **** disk surface
           isc10(2) = 13
c
c......Get curve to revolve
c
           call parsit
           if (ityp .ne. 2 .or. (ist.ne.POINT .and. ist.ne.PNTVEC .and.
     x                        ist.ne.LINE .and. ist.ne.CIRCLE)) then
               call error (192)
               goto 99999
           endif
           sc(11) = tv
           isc10(3) = 1
           if (ist .ne. CIRCLE) then
             call parsit
c
c.....Get radius
c
             if (scalar) then
               isc10(4) = 1
             else if (ityp .eq. 2 .and. (ist.eq.3 .or. ist.eq.21)) then
               isc10(4) = 2
             else
               call error (20)
               goto 99999
             endif
             sc(12) = tv
           endif
           goto 88888
       endif
 
       if (ityp.eq.1.and.ist.eq.666) then
c                                              **** offset
           isc10(2) = 7
           call parsit
           if (ityp.eq.2.and.ist.eq.9) then
               call gtdesc (tv, nclkey, nwds, itype)
               call sftype (nclkey, i)
               if (i.eq.0) then
                 call error (321)
                 goto 99999
               endif
               sc(11) = tv
           else
               call error(186)
               go to 99999
           endif
           call parsit
           if (ityp.eq.1.and.ist.gt.637.and.ist.lt.644) then
               sc(12)=0
               isc12(4) = ist
           else if (geom .and. (ist.eq.VECTOR .or. ist.eq.PNTVEC)) then
               sc(12) = tv
           else
               call error(18)
               go to 99999
           endif
           call parsit
           if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.
     1         ityp.eq.4) then
               sc(13) = tv
               go to 88880
           else
               call error(7)
               go to 99999
           endif
       endif
 
       if (ityp.eq.1.and.ist.eq.834) then
c                                              **** fit
           isc10(4)=1
           idtype = 8
           call parsit
           if (err) go to 99999
       else
           isc10(4)=0
       endif
       if (ityp.eq.1) go to 2000
       k=0
       j=11
       l=ifl(4)+1
       items=0
       do 100 i=1,50
c
c...vp 12-apr-93 PV added as a slope (vector only)
c
           if ((ityp.eq.3.and.tv.eq.0).or.(ityp.eq.2.and.
     1         (ist.gt.2.and.ist.lt.9.and.ist.ne.6 .or.
     2          ist.eq.21))) then
               items=items+1
               if (isc10(4).eq.0) then
                 if (items.gt.maxpn*2) then
                   call error(93)
                   go to 99999
                 endif
               else
                 if (items.gt.maxcv) then
                   call error(93)
                   go to 99999
                 endif
               endif
               if (isc10(4).eq.1) then
c                         ******  if its fit
                   if (.not.(ityp.eq.2..and.(ist.eq.5.or.ist.eq.7
     1               .or.ist.eq.8))) then
c                        ***** must be an line, curve or circle
                     call error(201)
                     go to 99999
                   endif
               endif
               if (ityp.eq.2 .and. ist.eq.8) then
                 call gtdesc (tv, nclkey, nwds, itype)
                 call isitwf (nclkey, itype)
                 if (itype.eq.1) then
                   call error(383)
                   goto 99999
                 endif
               endif
               if (j.lt.23) then
                   sc(j)=tv
                   j=j+1
               else
                   k=k+1
                   if (k.gt.35) then
                       call putran(jbuff,l)
                       l=l+1
                       k=1
                   endif
                   jbuff(k)=tv
               endif
           else if (ityp.eq.7.or.(ifl(111).ne.0.and.ityp.eq.5.and.
     1            ist.eq.7)) then
               if (i.lt.2) then
                   call error(21)
                   go to 99999
               else
                   isc10(2)=1
                   isc10(3)=i-1
                   if (j.gt.22) call putran(jbuff,l)
                   go to 88888
               endif
           else if (items.eq.1 .and. scalar .and. tv.gt.1.d-4) then
               isc10(2) = 13
               isc10(3) = 2
               if (.not. nxteos) then
                 call parsit
                 if (scalar .and. tv.gt.1.d-4) then
                   isc10(3) = 3
                   if (.not.nxteos) call parsit
                 endif
               endif
               goto 88888
           else
               call error(65)
               go to 99999
           endif
           idtype = 8
           call parsit
           if (err) go to 99999
100    continue
       isc10(2)=1
       isc10(3)=i-1
       if (j.gt.22) call putran(jbuff,l)
       go to 88888
2000   if (ist.ge.638.and.ist.le.643) then
c
c... xl
c
           isc10(2)=4
           isc10(3)=0
           sc(11)=ist
           call parsit
           if (ityp.eq.2.and.(ist.eq.PLANE.or.ist.eq.SURF)) then
c
c... xl,pl1
c
               if (ist.eq.SURF) then
                 call gtdesc(tv,nclkey,nwds,ietype)
                 call ncl_get_sf_primtyp(nclkey,primtyp)
                 if (primtyp.ne.3) then
                   call error(19)
                   go to 99999
                 endif
               endif
               sc(12)=tv
               call parsit
               if (ityp.eq.1.and.(ist.ge.638.and.ist.le.643)) then
c
c... xl,pl1,zs
c
                   sc(13)=ist
                   call parsit
                   if (ityp.eq.2.and.(ist.eq.PLANE.or.ist.eq.SURF)) then
c
c... xl,pl1,zs,pl2
c
                       if (ist.eq.9) then
                         call gtdesc(tv,nclkey,nwds,ietype)
                         call ncl_get_sf_primtyp(nclkey,primtyp)
                         if (primtyp.ne.3) then
                           call error(19)
                           go to 99999
                         endif
                       endif
                       sc(14)=tv
                       call parsit
                       if (ityp.eq.1.and.ist.eq.23) then
c
c... xl,pl1,zs,pl2,radius
c
                           call parsit
                           if (ityp.eq.3.or.ityp.eq.4.or.
     *                         (ityp.eq.2.and.ist.eq.2)) then
c
c... xl,pl1,zs,pl2,radius,num
c
                               sc(15)=tv
                               sc(16)=tv
                               if (nextyp.ne.11) then
                                   call parsit
                                   if (ityp.eq.3.or.ityp.eq.4.or.
     *                                (ityp.eq.2.and.ist.eq.2)) then
                                       isc10(3)=1
                                       sc(16)=tv
                                       call parsit
                                   endif
c
c...vp 12-apr-93 PV added as a point only
c
                                   if (ityp.eq.2.and.(ist.eq.POINT
     *                                 .or. ist.eq.PLANE
     *                                 .or. ist.eq.PNTVEC)) then
                                       sc(17)=tv
                                       call parsit
                                   else
                                       call error(249)
                                       go to 99999
                                   endif
                                   if (ityp.eq.2.and.(ist.eq.POINT
     *                                 .or. ist.eq.PLANE
     *                                 .or. ist.eq.PNTVEC)) then
                                       sc(18)=tv
                                   else
                                       call error(249)
                                       go to 99999
                                   endif
                               else
c
c... aak 18-may-1998: give error if no more arguments
c
                                   call parsit
                                   call error(249)
                                   go to 99999
                               endif
                           else
                               call error(7)
                               go to 99999
                           endif
                       else
                           call error(197)
                           go to 99999
                       endif
                   else
                       call error(19)
                       go to 99999
                   endif
               else
                   call error(18)
                   go to 99999
               endif
           else
               call error(19)
               go to 99999
           endif
       ELSE IF (IST.EQ.FILLET) THEN
         ISC10(2)=8
         ISC10(3)=0
         CALL PARSIT
         IF (ITYP.NE.2.OR.(IST.NE.6.AND.IST.NE.9)) THEN
           CALL ERROR(326)
           GOTO 99999
         ENDIF
         u1 = .5
         v1 = .5
         u2 = .5
         v2 = .5
         IF (IST.EQ.9) THEN
           SC(11) = TV
           iret = 9
           call parsuv(iret, lflg, u1, v1)
           if (iret.gt.0) then
             call error(iret)
             goto 99999
           endif
           CALL PARSIT
           IF (ITYP.NE.2.OR.(IST.NE.6.AND.IST.NE.9)) THEN
             CALL ERROR(326)
             GOTO 99999
           ENDIF
           SC(12) = TV
           if (ist.eq.9) then
             iret = 9
             call parsuv(iret, lflg, u2, v2)
             if (iret.gt.0) then
               call error(iret)
               goto 99999
             endif
           endif
         ELSE
           SC(12) = TV
           CALL PARSIT
           IF (ITYP.NE.2.OR.IST.NE.9) THEN
             CALL ERROR(186)
             GOTO 99999
           ENDIF
           SC(11) = TV
           iret = 9
           call parsuv(iret, lflg, u1, v1)
           if (iret.gt.0) then
             call error(iret)
             goto 99999
           endif
         ENDIF
         CALL PARSIT
c
c...vp 12-apr-93 PV added as a point only
c
         IF (ITYP.NE.2 .OR. IST.NE.3 .and. ist.ne.21) THEN
           CALL ERROR(20)
           GOTO 99999
         ENDIF
         SC(13) = TV
         CALL PARSIT
         IF (ITYP.EQ.2.AND.IST.EQ.2.OR.ITYP.EQ.3.OR.ITYP.EQ.4) THEN
           SC(14) = 0.
           SC(15) = TV
           SC(16) = TV
           CALL PARSIT
           IF (ITYP.EQ.2.AND.IST.EQ.2.OR.ITYP.EQ.3.OR.ITYP.EQ.4) THEN
             ISC10(3)=1
             SC(16) = TV
             CALL PARSIT
           ENDIF
         ELSE IF (ITYP.EQ.2.AND.IST.EQ.8) THEN
           SC(14) = TV
           CALL PARSIT
         ELSE
           CALL ERROR(7)
           GOTO 99999
         ENDIF
         SC(17) = 0.
         IF (ITYP.EQ.7) GOTO 88888
         IF (ITYP.NE.2.OR.(IST.NE.3.AND.IST.NE.8.AND.IST.NE.20)) THEN
           CALL ERROR(12)
           GOTO 99999
         ENDIF
         SC(17) = TV
         IF (IST.EQ.3) THEN
           CALL PARSIT
           IF (ITYP.NE.2.AND.IST.NE.3) THEN
             CALL ERROR(20)
             GOTO 99999
           ENDIF
           SC(18) = TV
         ENDIF
       ELSE IF (ITYP.EQ.1 .AND. IST.EQ.SELECT) THEN
         ISC10(2)=9
         ISC10(3)=0
         ISC10(4)=0
         CALL PARSIT
         IF (ITYP.NE.2.OR.IST.NE.9) THEN
           CALL ERROR(186)
           GOTO 99999
         ENDIF
         call gtdesc(tv, nclkey, nwds, itype)
         call sftype (nclkey, isftyp)
         if (isftyp.ne.26) then
           call error(321)
           goto 99999
         endif
         sc(11) = tv
c                            get optional start and end patch numbers
         if (nextyp.eq.11.or.(nextyp.eq.7.and.ifl(111).ne.0)) goto 88888
         call parsit
         if (.not. scalar .or.itv.lt.0)then
           call error(224)
           goto 99999
         endif
         isc10(3)=itv
         isc10(4)=itv
         if (nextyp.eq.11.or.(nextyp.eq.7.and.ifl(111).ne.0)) goto 88888
         call parsit
         if (.not. scalar .or.itv.lt.0)then
           call error(224)
           goto 99999
         endif
         isc10(4)=itv
         goto 88880
       else
           call error(12)
           go to 99999
       endif
88880  if (.not.nxteos) then
         call error(4)
         go to 99999
       endif
88888  isc10(1)=607
99999  return
       end
