C*********************************************************************
C*    NAME         :  genpts.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       genpts.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:05
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine genpts
C*      set up for the saving of points and vectors created during motion.
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
c   ifl(246) has value assigned according the following syntaxes:
c
c     0   -  GENPTS/NOMORE
c     1   -  GENPTS/[POINT,]N,PTNAM
c     2   -  GENPTS/[POINT,]N,PTNAM,,TVNAM
c     3   -  GENPTS/[POINT,]N,PTNAM,FWDNAM
c     4   -  GENPTS/[POINT,]N,PTNAM,FWDNAM,TVNAM
c     7   -  GENPTS/PNTVEC,2,N,PVNAM
c     8   -  GENPTS/PNTVEC,[1,]N,PVNAM
c     9   -  GENPTS/PNTVEC,2,N,PVNAM,FWDNAM
c    10   -  GENPTS/PNTVEC,[1,]N,PVNAM,TVNAM
c
C********************************************************************/
C
      subroutine genpts
c
      include 'com8a.com'
      include 'gpts.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld,isvtk4
      integer*2 istold
c
      integer*2 igen,iminit,npts,i,ier,m
c
      integer*4 mtyp,nfwd
      integer*2 ktv(4),irest(4)
      equivalence (igen,ifl(246)),(iminit,ifl(247)),(npts,ifl(250))
      equivalence (tv,ktv),(rest,irest)
c
c...Create points for GENPTS/NEXT
c
      if (ifl(372) .eq. 1) call strgpt (igprec,imotp,gptend)
c
c...Initialize routine
c
      igen = 0
      ifl(372) = 0
      if (nextyp.ne.5) then
         isvinx = inx
         ier = 22
         go to 8888
      endif
      ifl(44) = 9
c
c...Get first parameter
c......check if NOMORE
c
      call parsit
      if (ityp.eq.1.and.ist.eq.53.and.nextyp.eq.11) goto 99999
c
      mtyp   = 0
      nfwd   = 0
      msf    = 0
      m      = 4
c
c......PNTVEC
c
      if (ityp .eq. 1) then
          if (ist .eq. 613) then
              mtyp   = 6
              nfwd   = 2
              m      = 3
              call parsit
              if (ITYP .eq. 3 .or. ITYP .eq. 4) then
                  if (ITV .eq. 1 .or. ITV .eq. 2) then
                      nfwd = 3 - ITV
                  else
                      ier = 61
                      go to 8888
                  endif
                  call parsit
              endif
c
c......POINT
c
          else if (ist .eq. 603) then
              call parsit
          endif
      endif
      mtyp   = mtyp + nfwd
c
c......PS
c
      if (ityp .eq. 1) then
          if (ist .eq. 728) then
              msf    = 1
c
c......DS 
c
          else if (ist .eq. 729) then
              msf    = 2
c
c......ARC 
c
          else if (ist .eq. 182) then
              msf    = 3
c
c......Not TE
c......Unrecognized minor word
c
          else if (ist .ne. 650) then
              ier  = 61
              go to 8888
          endif
          call parsit
      endif
c
c...Get name of var to store # of pts generated
c
      if (ityp.eq.2.and.(ist.eq.1.or.ist.eq.2)) then
c
c...Scalar (or unknown - make it a scalar)
c
          idst = 2
          savid2 = token2
          isvsub = ivxsub
          ifl(9)  = ifl(11)
          ifl(10) = ifl(12)
          rest = 0.
          keyold = keyhld
          istold = ist
          call vstore
c
c...Simple variable expected
c
      else
          ier = 282
          go to 8888
      endif
      sc135 = token2
      inx135 = ivxsub
c
      itgen  = 1
      npts   = 0 
      iminit = 1
      ix     = 0
c
c...Get names of arrays to store pts, tvs & fwds
c
      do 100 i=1,m
         call parsit
         if (ityp.eq.5.and.ist.eq.9.and.i.ne.1) goto 100
c
c......GENPTS/NEXT,NOW
c
         if (ityp .eq. 1) then
             if (ist .eq. 161) then
                 ifl(372) = 0
                 if (nextyp .eq. 11) go to 900
                 go to 100
             else if (ist .eq. 162) then
                 ifl(372) = 1
                 call ncl_setptr(imotp,igprec)
                 call conv8_8 (sc(1),gptend,9)
                 if (nextyp .eq. 11) go to 900
                 go to 100
             else
                 ier = 436
                 go to 8888
             endif
         else if (ityp.ne.2) then
c
c...identifier expected
c
             ier = 87
             go to 8888
         endif
         if (.not.(ist.eq.1)) go to 20
c
c...unknown - check if reserved
c
         isvtk4 = 1
         if (ivxsub.ne.0) then
             isvtk4 = ivxsub
             ivxsub = 0
             call vstchk
             if (ist.eq.14) goto 50
         endif
         savid2 = token2
         isvsub = ivxsub
         ivxsub = 0
         ifl(9)  = ifl(11)
         ifl(10) = ifl(12)
         idst = 14
         rest = 0.
         irest(3) = 32767
         irest(4) = 14
         call vstore
         ivxsub = isvtk4
         go to 80
20       if (.not.(ityp.eq.2.and.ivxsub.ne.0)) go to 60
c
c...known and subscripted - check subscript
c
         isvtk4 = ivxsub
         ivxsub = 0
         call vstchk
         if (ityp.ne.2.and.ist.ne.14) then
c
c...identifier has not been reserved
c
             ier = 88
             go to 8888
         endif
50       if (isvtk4.gt.10000000) then
c
c...subscript out of range
c
             ier = 85
             go to 8888
         endif
         ivxsub = isvtk4
         go to 80
c
c...identifier has not been reserved
c  
60       if (ist.ne.14) then
             ier = 88
             go to 8888
         end if
c
c...sub'd var - start at sub 1
c
         ivxsub = 1
         go to 80
80       if (i.eq.1) then
             sc136 = token2
             inx136 = ivxsub
         else if (i.eq.2) then
             itgen = 3
             sc138 = token2
             inx138 = ivxsub
         else
             itgen = itgen + 1
             sc137 = token2
             inx137 = ivxsub
         endif
         if (nextyp.eq.11) goto 900
100   continue
c
      if (nextyp.ne.11) then
         ier = 4
         go to 8888
      endif
c
900   if (mtyp .ne. 0) itgen = mtyp - 1 + itgen
      igen   = itgen
      ifl(384) = msf
      goto 99999
c
c...errors
c
 8888 call error (ier)
c
99999 return
      end
