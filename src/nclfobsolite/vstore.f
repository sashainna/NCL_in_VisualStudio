C*********************************************************************
C*    NAME         :  vstore.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       vstore.f , 25.2
C*    DATE AND TIME OF LAST MODIFICATION
C*       08/17/15 , 17:40:10
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine vstore
c*       this routine adds vname & asn (saveid &  
c*       rest) to vst.  vst sto location is in ifl (9/10)              
c*                                                                   
c*     vname may or may not be in vst.  the legality of re-defs is   
c*     outside the vstchk/vstore process.  
c*
C*    PARAMETERS   
C*       INPUT  : 
c*         saveid2 - must contain the name of the variable to be stored.  
c*         ifl(9) - must contain the record number in the ranfile where  
c*                  the variable is to be stored.                        
c*         ifl(10) - must contain the element number in the above        
c*                  ranfile record where the variable is to be stored.   
c*         idst - must contain the subtype of the variable to be stored. 
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine vstore

      include 'com8a.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld, tmp1, tmp2
      integer*2 istold
      integer*2 ksn(4)
      integer*4 nkey2
      equivalence (rest, ksn, nkey2)
      real*8 temp
      integer*4 nclkey, keytmp,isav
      integer*2 itmp(4), ieold, nwds, ietype
      equivalence (temp, nclkey, itmp), (itmp(4),ieold)
      real*8 asw(36)
      integer*2 ksw(144),itemp
      equivalence (asw,ksw)
      integer*4 isub
      logical ldel
c
c...  Handle macros & macro parameters
c
      if (idst.eq.11 .or. idst.eq.12) then
        nclkey = 0
c
c... if lang = cadra , store macro param with a _ in the end
c
        if (ifl(374) .ne. 0 .and. idst.eq.12) then
          ipos = 1
  100     if (savid2(ipos:ipos).eq.' '.or.ipos.eq.7) then
            savid2(ipos:ipos) = '_'
            token2=savid2
            goto 200
          else
            ipos = ipos + 1
            goto 100
          endif
  200     continue
        endif
        tmp1 = ksn(1)
        tmp2 = ksn(2)
        call vxstor (savid2, isvsub, nclkey, tmp1, tmp2,
     x               ksn(3), ksn(4), ifl(299))
        goto 99999
      endif
C
C      call vxchk  (savid2, isvsub, nclkey, jpg, jel, nwds, ietype)

      nclkey = keyold

      if (idst .eq. 2) then
c
c...   Save scalars in unibase except when loading unibase 
c
        if (ifl(299).eq.0) then
          token2=savid2
          isav = isvsub
c  if its a macro parameter set to a scalar value, blank out the name so the
c  name wont be saved in the unibase and a subsequent load of the unibase will
c  not create a scalar with the same name as the macro parameter.
          if (ist.eq.12) then
            savid2 = ' '
            isvsub = 0
          endif
          keytmp = nclkey
          call ptgeo(idst, rest, keytmp)
          nkey2 = keytmp
          ksn(3) = 1
          ksn(4) = 2
          savid2=token2
          isvsub = isav
        endif
      endif

      if (savid2(1:3).eq.'@UN') goto 99999
c
c... if lang = cadra , store labels with a : in the end
c
      if(ifl(374) .ne. 0 .and. idst.eq.13) then
        ipos = 1
 1100   if (savid2(ipos:ipos).eq.' '.or.ipos.eq.7) then
          savid2(ipos:ipos) = ':'
          token2=savid2
          goto 1200
        else
          ipos = ipos + 1
          goto 1100
        endif
 1200 continue
      endif
c
c...   Save in NCL_name_list
c
      ldel = .false.
      if (istold.gt.1) then
        if ((idst.ge.2.and.idst.lt.11)
     1      .or.idst.eq.18.or.idst.eq.20.or.idst.eq.21.or. idst .eq. 30
     2      .or.idst.eq.33)
     3          then
          if (nclkey.ne.0 .and. nclkey.ne.nkey2 .and. idst.ne.17) then
            if (idst.ne.2.and.ifl(299).ne.1) call upattr(nclkey,nkey2)
            ldel = .true.
            call dlgeom (nclkey)
            call gtdesc(sc(35), keytmp, nwds, ietype)
            if (nclkey.eq.keytmp) sc(35) = 0.
          endif
        endif
        if (nkey2.eq.0 .or. nkey2 .ne. nclkey) then
          itemp = 0
          if (.not.ldel) call vxdel (savid2, isvsub, itemp)
          tmp1 = ifl(9)
          tmp2 = ifl(10)
          call vxstor (savid2, isvsub, nkey2, tmp1, tmp2,
     x                 ksn(3), ksn(4), ifl(299))
        endif
      else
        tmp1 = ifl(9)
        tmp2 = ifl(10)
        call vxstor (savid2, isvsub, nkey2, tmp1, tmp2,
     x               ksn(3), ksn(4), ifl(299))
      endif
c
c             If this was a subscripted variable, check to see if
c             the subscript was the highest one used.
c
      if (isvsub.ne.0) then
        isub = 0
        call vxnupd (savid2, isub, isvsub)
      endif

99999 return
      end
