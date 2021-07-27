C*********************************************************************
C*    NAME         :  declsy.f
C*       CONTAINS:
C*             declsy  placsy  decomp
C*
C*    COPYRIGHT 2000 (c) Numerical Control Computer Sceinces Inc.
C*    All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       declsy.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       12/01/15 , 08:19:24
C********************************************************************/
c
c***********************************************************************
c
c   SUBROUTINE:  declsy
c
c   FUNCTION:  This routine defines a symbol using the command
c              sym1=SYMBOL/[AT,pt,][LENGTH,tl,] geo1,geo2, ... ,geon
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c

      subroutine declsy
c
      include 'com8a.com'
      include 'wrksys.com'
c
      integer*2 MAXENT
      parameter (MAXENT=500)
c
      integer*2 ietype,nwds,nent,ierr
      integer*4 nclkey,keys(500),isub
c
      logical trflg
c
      real*8 pt(6), tlen
c
      character*64 sym
c
      integer*2 ATV, LENV
      parameter (ATV=189)
      parameter (LENV=9)
c
c...Initialize routine
c
      call curnam (sym,isub)
      trflg = .false.
      pt(1) = 0.
      pt(2) = 0.
      pt(3) = 0.
      tlen = 0.
      call conref (pt,POINT)
c
c...SYMBOL/AT,pt
c
      if (ityp .eq. 1 .and. ist .eq. ATV) then
          call parsit
          if (ityp .eq. 2 .and. (ist .eq. 3 .or. ist .eq. 21)) then
              call gtentt (tv, trflg, nclkey, ietype, pt)
c
c......Metric conversion
c
              if (ifl(264) .eq. 1) then
                  pt(1) = pt(1) / 25.4
                  pt(2) = pt(2) / 25.4
                  pt(3) = pt(3) / 25.4
              endif
          else
              go to 9000
          endif
c
c......Parse next token
c
          call parsit
      endif
          
      if (ityp .eq. 1 .and. ist .eq. LENV) then
          call parsit
          if (ityp.eq.3.or.ityp.eq.4.or.
     1             (ityp.eq.2.and.ist.eq.2)) then
              if (ifl(264) .eq. 1) then
                  tlen = tv/25.4
              else
                  tlen = tv
              endif
          else
              go to 9000
          endif
c
c......Parse next token
c
          call parsit
      endif          
c
c...Transform point
c......Modsys
c
      if (lwrk) call conent (pt,wrkmx,POINT)
c
c......Refsys
c
      if (ifl(72) .eq. 1) call conent (pt,sc(56),POINT)
c
c...SYMBOL/geo-list
c
      nent   = 0
  100 if (ityp .eq. 7) go to 1000
c
c...Check for valid geometry type
c
      if (ityp .ne. 2) go to 9100
      if (ist .ne. 3 .and. ist .ne. 5 .and. ist .ne. 7 .and.
     1    ist .ne. 8 .and. ist .ne. 9 .and. ist .ne. 33) go to 9100
c
c...Store key of entity
c
      nent   = nent   + 1
      if (nent .gt. MAXENT) go to 9200
      call gtdesc (tv,keys(nent),nwds,ietype)
      idtype = 0
      call parsit
      if (err) go to 8000
      go to 100
c
c...Define symbol
c
 1000 if (nent .eq. 0) go to 9300
      if (tlen.eq.0.) then
         call ub_simple_symbol (sym,isub,keys,nent,pt,ierr)
      else
         call ubf_create_symbol (sym,isub,keys,nent,pt,tlen,ierr)
      endif
      if (ierr .ne. 0) go to 9400
      defwf = .true.
c
c...End of routine
c
      defwf  = .true.
      ifl(262) = 1
      ifl(329) = 1
 8000 return
c
c...Point expected
c
 9000 ifl(2) = 20
      err = .true.
      go to 8000
c
c...Valid geometry type expected
c
 9100 ifl(2) = 1
      err = .true.
      go to 8000
c
c...Too many entities
c
 9200 ifl(2) = 15
      err = .true.
      go to 8000
c
c...No enties given
c
 9300 ifl(2) = 1
      err = .true.
      go to 8000
c
c...Could not create symbol
c
 9400 ifl(2) = 12
      err = .true.
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  placsy
c
c   FUNCTION:  This routine places a symbol using the command
c              PLACE/sym1,AT,pt, [,SCALE,s] [,ROTATE,r]
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c

      subroutine placsy
c
      include 'com8a.com'
      include 'wrksys.com'
c
      integer*4 nclkey,isub,ipsub
      integer*2 len,ietype,ierr,ifl44
c
      logical trflg,idid
c
      real*8 pt(6),scal,rot
c
      character*256 txt
      character*64 sym,inst
      character*256 tmpsym, symlib
c
      integer*2 ATV,SCALE,ROTATE
      parameter (ATV=189)
      parameter (SCALE=25)
      parameter (ROTATE=1066)
c
c...Initialize routine
c
      call curnam (inst,ipsub)
      trflg = .true.
      ierr  = 0
      ifl44 = ifl(44)
      ifl(44) = 9
      pt(1) = 0.
      pt(2) = 0.
      pt(3) = 0.
      scal  = 1.
      rot   = 0.
      symlib = 'symlib'
c
c...PLACE/sym1
c
cc      ldtext = .true.
cc      call parsit
cc      ldtext = .false.
      if (ityp .eq. 7) go to 9000
      if (lstrng) then
        len = 0
        call gttext(txt,len)
        sym = txt(1:len)
        tmpsym = txt(1:len)
      else
        sym = token2
        isub = ivxsub
        tmpsym = token2
      endif
c
c...PLACE/symlib,sym1
c
      call svpars
      ldtext = .true.
      call parsit
      ldtext = .false.
      if (ityp .ne. 1) then
c...          symlib = sym
          symlib = tmpsym
          if (lstrng) then
            len = 0
            call gttext(txt,len)
            sym = txt
          else
            sym = token2
            isub = ivxsub
          endif
      else
          call rtpars
      endif
c
c...Get next parameter list
c
      idid   = .false.
      if (nextyp .eq. 11) go to 9000
  100 call parsit
c
c...PLACE/AT,pt
c
      if (ityp .eq. 1 .and. ist .eq. ATV) then
          call parsit
          if (ityp .eq. 2 .and. (ist .eq. 3 .or. ist .eq. 21)) then
              call gtentt (tv, trflg, nclkey, ietype, pt)
c
c......Metric conversion
c
              if (ifl(264) .eq. 1) then
                  pt(1) = pt(1) / 25.4
                  pt(2) = pt(2) / 25.4
                  pt(3) = pt(3) / 25.4
              endif
c
c......Modsys
c
              if (lwrk) call conent (pt,wrkmx,POINT)
c
c......Refsys
c
              if (ifl(72) .eq. 1) call conent (pt,sc(56),POINT)
              idid   = .true.
          else
              go to 9100
          endif
c
c...PLACE/SCALE,s
c
      else if (ityp .eq. 1 .and. ist .eq. SCALE) then
          call parsit
          if (scalar) then
              scal   = tv
          else
              go to 9200
          endif
c
c...PLACE/ROTATE,r
c
      else if (ityp .eq. 1 .and. ist .eq. ROTATE) then
          call parsit
          if (scalar) then
              rot    = tv
          else
              go to 9200
          endif
c
c...Invalid command
c
      else
          go to 9300
      endif
c
c...Get next parameter
c
      if (nextyp .ne. 11) go to 100
c
c...Place symbol
c
      if (.not. idid) go to 9100
      call ub_simple_place (sym,isub,inst,ipsub,symlib,pt,scal,rot,ierr)
      if (ierr .ne. 0) go to 9400
      savid2 = inst
      isvsub = ipsub
      defwf = .true.
c
c...End of routine
c
 8000 ifl(44) = ifl44
      return
c
c...Premature end of command
c
 9000 call error (71)
      err   = .true.
      go to 8000
c
c...Point expected
c
 9100 call error (20)
      err   = .true.
      go to 8000
c
c...Scalar expected
c
 9200 call error (7)
      err   = .true.
      go to 8000
c
c...Invalid parameter
c
 9300 call error (61)
      err   = .true.
      go to 8000
c
c...Could not place symbol
c
 9400 token2 = sym
      call error (9)
      err   = .true.
      go to 8000
c
c...Subscripted entity not allowed
c
 9500 token2 = sym
      call error (86)
      err   = .true.
      go to 8000
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : decomp 
C*      Processes the DECOMP command, which decomposes a Symbol.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine decomp
c
      include 'com.com'
c
      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld, keytmp
      integer*2 istold
      integer*2 i,ierr,iret,parslb,ind,flg
      integer*4 nclkey,isub,ixsub,nent,ssub,oldkey,tent
      logical*2 lplus,lall
c
      real*8 origin(3)
c
      character*64 label,sym,stok
      character*256 txt
      flg = 3
      ind = 0
      tent = 0
      lplus = .false.
      lall = .false.
c
c...Setup starting label for decomposed entities
c
      isub  = isvsub
      label = savid2
      if (isub .lt. 1) then
          iret = parslb(label,isub)
          isub = -iret
      endif
      if (isub .eq. 0) isub = 1
c
c...Get symbol to decompose
c
10    call parsit
      if (ityp .eq. 7 .and. ind .eq. 0) go to 9000
      if (ityp .eq. 1 .and. ist .eq. 816) then
          if (ind .eq. 0) then
              lall = .true.
              goto 20
          else
              goto 9200
          endif
      else if (ityp .eq. 1 .and. 
     x          (ist .eq. 19 .or. ist .eq. 15)) then
          goto 9200
      endif
      ind = ind + 1
      if (lstrng) then
          len = 0
          call gttext(txt,len)
          sym = txt(1:len)
          ixsub = 0
      else
          sym = token2
          ixsub = ivxsub
      endif
      call ub_symbol_name (sym,ixsub,nclkey,origin,i)
      if ((i .ne. 2 .and. ind .eq. 1) .or.
     x    (i .eq. 1)) go to 9000
c
c...Get option scalar to receive # of entities
c
      isca = 0
      if (i .eq. 0) then
          stok = token2
          ssub = ivxsub
          isca = 1
          call vstchk
          keytmp = keyhld
          goto 30
c
c...Decompose symbol
c
      else
          call ubf_explode_syminstance (nclkey,label,isub,nent,flg,ierr)
          if (ierr .ne. 0) go to 9100
          tent = tent + nent
          if (isub .lt. 0) then
              isub = isub - nent
          else
              isub = isub + nent
          endif
          if (nextyp .ne. 11) goto 10
      endif
      goto 30
c
c...Decompose all instances
c
20    if (nextyp .ne. 11) then
          call parsit
          if (lstrng) then
              len = 0
              call gttext(txt,len)
              sym = txt(1:len)
              ixsub = 0
          else
              sym = token2
              ixsub = ivxsub
          endif
          call ub_symbol_name (sym,ixsub,nclkey,origin,i)
          if ((ityp .ne. 2 .or. (ist .ne. 1 .and. ist .ne. 2)) .or.
     x        i .ne. 0) goto 9200
          stok = token2
          ssub = ivxsub
          call vstchk
          keytmp = keyhld
          isca = 1
      endif
      flg = 4
      call ubf_explode_syminstance (nclkey,label,isub,nent,flg,ierr)
      if (ierr .ne. 0) go to 9100
      tent = nent
c
c...Store number of entities scalar
c
30    if (isca .eq. 1) then
          if (nextyp .ne. 11) then
              call parsit
              if (ityp .eq. 1 .and. ist .eq. 19) then
                  lplus = .true.
              else if (ityp .ne. 1 .or. ist .ne. 15) then
                  goto 9200
              endif
          endif
          idst = 2
          savid2 = stok
          isvsub = ssub
          keyold = keytmp
          if (lplus) then
              call ubf_get_numexplod(tent)
              rest = tent
          else
              call ubf_reset_numexplod
              rest = tent
              call ubf_set_numexplod(tent)
          endif
          call vstore
      endif
c
c...Disable driver calls of geognx, vstore, & dspent
c
      defwf  = .true.
      ifl(262) = 1
      ifl(329) = 1
c
c...End of routine
c
 8000 return
c
c...Symbol instance expected
c
 9000 call error (9)
      go to 8000
c
c...Error decomposing symbol
c
 9100 call error (ierr)
      go to 8000
c
c...Scalar expected
c
 9200 call error (282)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  prtsym (symnam, sub, refsys, flag)
c
c   FUNCTION:  This routine prints the data of a symbol.
c
c   INPUT:  
c           labstr   C*n   D1  -  Label of symbol. 
c
c           sub      C*n   D1  -  subscript string.
c
c           cref     C*n   D1  -  Refsys string.
c           flag:    1: symbol  2: instant
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine prtsym (symnam, sub, isub, cref, flag)
c
      include 'com8a.com'
c
      character*(*) symnam,cref, sub
c
      integer*4 flag, isub
      integer*4 strlen1,nc,nc1,nc2
      real*8 origin(3)
      integer*4 nclkey
      character*80 labstr
      character*10 atype
      character*11 numstr(3)
      character*64 sym
c
      call ub_symbol_name(symnam, isub, nclkey, origin, i)
      nc = strlen1(symnam)
      if (nc .gt. 64) nc = 64
      write (labstr, 1071) symnam(1:nc), sub
1071  format (a, a8)
      nc = strlen1(labstr)       
      do 120 j=nc+1,14,1
          labstr(j:j) = ' '
  120 continue
      nc = strlen1(labstr)
      if (flag .eq. 1) then
          atype='symbol'
      else if (flag .eq. 2) then
          atype='instance'
      endif
      call ul_format_data11(origin(1), numstr(1))
      call ul_format_data11(origin(2), numstr(2))
      call ul_format_data11(origin(3), numstr(3))
      if (flag .eq. 1) then
          if (nc.le.14) then
              write (cout,20) labstr(1:14), atype, cref,
     1                    (numstr(i),i=1,3)
          else
              call prtge1 (labstr)          
              write (cout,21) atype,cref,
     1                    (numstr(i),i=1,3)
          endif
          call prtge1 (cout)
      else
          call ub_get_instance_master(nclkey, sym)
          if (nc.le.14) then
              write (cout,30) labstr(1:14), atype, cref, sym
          else
              call prtge1 (labstr)          
              write (cout,31) atype,cref,sym
          endif
          call prtge1 (cout)
          write (cout,32) (numstr(i),i=1,3)
          call prtge1 (cout)
      endif
   20 format (a14,1x,a10,1x,a5,1x,a11,1x,a11,1x,a11)
   21 format (15x,a10,1x,a5,1x,a11,1x,a11,1x,a11)
   30 format (a14,1x,a10,1x,a5,1x,a40)
   31 format (15x,a10,1x,a5,1x,a40)
   32 format (32x,a11,1x,a11,1x,a11)
c
c...End of routine
c
 8000 return
      end
