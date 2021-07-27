C*********************************************************************
C*    NAME         :  remove.f
C*       CONTAINS:
C*    COPYRIGHT 1986 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       remove.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:10:37
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine remov
c*       parse and process the remove statement.  
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
c
      subroutine remov

      include 'com4a.com'
      include 'comdb.com'

      real*8 vs(35),origin(3)
      character*8 cvs(35)
      real*8 asw(36)
      integer*2 ksw(144)
      real*8 asn
      integer*4 ikey, kps, ipg, iel
      character*64 wname
      character*1 awname(64)
      integer*4 nclkey, i4sub, kwds
      integer*2 ietype, nwds
      integer*2 gtype(50),idum, ginc,jindex
      integer*2 ietyp(4)
      logical all,genric,lgeo
c
      equivalence (asn,ikey)
      equivalence (asn,ietyp)
      equivalence (vs, cvs, jb)
      equivalence (asw,ksw)
      equivalence (wname,awname)

      integer*2 POCKET,DATAV
      parameter (POCKET=22)
      parameter (DATAV =858)
c
c
c        loop through statement to ensure all syntax is correct
c        before any removing of entities is done
c
      if (nextyp.ne.5) then
          isvinx=inx
          call error(22)
          go to 99999
      endif
c
      ldtflg = .true.
      isvix = inx
      ifl(44)=9
c      wild = .false.
      all = .false.
      genric = .false.
      ginc = 0
  100 idtype = 0
      wdtype = 1
      call parsit
c
      if (err .and.
     x    (.not.(ifl(2).eq.88 .or.
     x           ifl(2).eq.85 .or.
     x           ifl(2).eq.61))) then
          call error(ifl(2))
          go to 99999
      else
          err=.false.
          ifl(2)=0
      endif
c
c        ok if all
c        added on 7/19/88 by kathy
c
      if (ityp .eq. 1 .and. ist .eq. 816) then
         all = .true.
      else if (ityp.eq. 1) then 
         ginc = ginc + 1
         if (ginc.gt.50) ginc = 50
         call vctoid (ist,gtype(ginc),idum)
         if (ist.eq.DATAV) gtype(ginc) = DATAST
         if (ist.eq.877) gtype(ginc) = VSYMBOL
         if (gtype(ginc).eq.0) then
           call error (244)
           go to 99999
         endif
         genric = .true.
      else if (ityp.eq. 2 .and. (geom .or. ist .eq. 1 .or. ist .eq. 2
     x         .or.ist.eq.POCKET.or.ist.eq.DATAST
     x         .or.(ist.eq.TEXTVAR.and.ltxtsb))) then 
c
           call vstchk
           if (ist .eq. 1)  then
               call ub_symbol_name(token2,ivxsub,nclkey,origin,i)
           endif
c
c...if it is wild card parser word, if could return any type
c...because we didn't checked when parser
c
      else if ((wildwd).or.(filtgeo).or.(ityp .eq. 10)) then
          goto 400
c
c        bad if none of the above
c
      else if (ityp .ne. 7) then 
          call error (244)
          go to 99999
      endif
c
c      get next token unless we're at the end of the statement
c
  400 if (nextyp .ne. 11) goto 100
c 
c       statement is syntactically correct, now parse again and do the
c       removal operations
c
      inx= isvix
      call gtdesc (sc(35), kps, nwds, ietype)
  500 idtype = 0
      wdtype = 1
      call parsit
      if (err .and.
     x    (.not.(ifl(2).eq.88 .or.
     x           ifl(2).eq.85 .or.
     x           ifl(2).eq.61))) then
          call error(ifl(2))
          go to 99999
      else
          err=.false.
          ifl(2)=0
      endif
c
c        remove specifically named geometric entity
c
      if (ityp .eq. 2) then
         call gtdesc (tv, nclkey, nwds, ietype)
         if  (geom .or. ist .eq. 2 .or. ist.eq.DATAST
     x             .or. ist .eq. TEXTVAR) then
c correct tv content for scalars
            if (ist.eq.2) then
               call vxchk(token2, ivxsub, nclkey, ipg, iel, 
     x              kwds, ietype)
            endif
            call ifidge (ietype,lgeo)
            if (lgeo .and. (.not. wildwd .or. ietype.ne.matrix) .or.
     x          ietype.eq.2 .or.
     x          ietype.eq.DATAST .or. ietype.eq.TEXTVAR) then
               if (nclkey.eq.kps) call rstps (ietype)
               call dlgeom (nclkey)
            else if (ietype.eq.POCKET) then
               call pokdel(nclkey)
            endif
c
c.....if there is no error for delete, we need adjust 'nextnum' if it is in wildcard parse
c
            if (.not. err) call setwildmin()
c
c...Delete symbol
c
         else if (ityp .eq. 2 .and. ist .eq. 1) then
            call ub_symbol_name(token2,ivxsub,nclkey,origin,i)
            if (i .eq. 1) call ub_symbol_delete (nclkey)
            if (i .eq. 2) call dlgeom (nclkey)
c
c.....if there is no error for delete, we need adjust 'nextnum' if it is in wildcard parse
c
            if (.not. err) call setwildmin()
c
c...Delete pocket
c
         else if (ist.eq.POCKET) then
            call pokdel (nclkey)
            if (.not. err) call setwildmin()
         endif
      else if (ityp .eq. 1 .and. ist .eq. 877) then
          nclkey = 0
          call ub_symbol_delete (nclkey)
          if (.not. err) call setwildmin()
      else if (ityp .eq. 1 .and. genric) then
          call vxlfst
200       continue
          call vxlnxt (token2, i4sub, nclkey, kwds, ietype, ipg, iel)
          if (ietype .eq. 1) goto 210
          if (jindex(gtype, ietype, ginc) .ne. 0) then 
            if (ietype.eq.POCKET) then
              call pokdel (nclkey)
            else
              if (nclkey.eq.kps) call rstps (ietype)
              call dlgeom (nclkey)
            endif
          endif
          goto 200
210       continue
c
c        remove all defined geometry if token is 'all'
c        added on 7/19/88 by kathy
c
      else if (ityp .eq. 1 .and. all) then
c
          call vxlfst
10        continue
          call vxlnxt (token2, i4sub, nclkey, kwds, ietype, ipg, iel)
          if (ietype .eq. 1) goto 111
            call ifidge (ietype,lgeo)
            if (lgeo .or. ietype .eq. 2 .or. ietype.eq.DATAST .or.
     x          ietype.eq.TEXTVAR) then
               if (nclkey.eq.kps) call rstps (ietype)
               call dlgeom (nclkey)
            else if (ietype.eq.POCKET) then
               call pokdel (nclkey)
            endif
90          continue
            goto 10
111       continue
c
      endif
c
c        go get next token unless we're at the end of the statement
      if (nextyp .ne. 11) goto 500
c
99999 continue
      ldtflg = .false.
      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine pokdel (nclkey)
c*       Remove a pocket list.
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey      - Pointer to pocket list.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine pokdel (nclkey)

      include 'com.com'

      integer*4 nclkey

      integer*2 irslt
      integer*2 ietype

      integer*2 POCKET
      parameter (POCKET=22)

      call pkldel (nclkey)
      ietype = POCKET
      call vxdlk  (nclkey, ietype, irslt)

99999 return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine rstps (ietype)
c*       Reset the part surface to a plane when it is removed.
C*    PARAMETERS   
C*       INPUT  : 
C*          ietype    - Type of part surface.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : Part surface set to Z-plane if it was a surface.
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine rstps (ietype)

      include 'com.com'
      include 'mocom.com'

      integer*2 ietype

c          motion common equivalences
      integer*2 jd(450)
      equivalence (d,jd)

      if (ietype.eq.SURF) then
        jd(1) = PLANE
        d(3)  = 0.
        d(4)  = 0.
        d(5)  = 1.
        d(6)  = 0.
      endif
      sc(35)  = 0.
      sc(144) = 0.

99999 return
      end
