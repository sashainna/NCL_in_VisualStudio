c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       strwf2.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:46
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1988 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **  PROGRAM NAME: strwf2                                            **
C **                                                                  **
C **  PURPOSE OF PROGRAM:                                             **
C          Store a wireframe entity in the ranfile.
C          The arguments passed in are used and other
C          required variables are set up.
C **********************************************************************
C **********************************************************************
 
      subroutine strwf2(nclkey, ietype, nam, isub)
 
      include 'com4a.com'
 
      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      integer*4 nclkey, isub
      integer*2 ietype
      character*64 nam
      integer*4 jtp,subsv
      integer*2 numwds(10),irest(4),ktv(4)
      equivalence (rest,irest), (tv,ktv)
c
      data numwds /0,1,3,3,6,4,11,1,1,12/
c
      token2 = nam
      ivxsub = isub
      call vstchk
c
c...vp 24-feb-97 add check for subsripted name to create
c...reserved variable name record in "run file". The existance of
c...record is not important (Ian 12.1.2.99 Book of Revelation) but
c...subscripted entities can not be referenced until record is created
c...forced by other means.
c
      if (ityp .eq. 2 .and. ist .eq. 1 .and. isub .ne.0) then
          subsv   = ivxsub
          ivxsub = 0
          call vstchk
          if (ityp.eq.2 .and. ist .eq. 14) then
c...yurong              if (subsv .le. ktv(3)) then
              if (subsv .le. 1000000) then
                 ivxsub = subsv
                 call vstchk 
              endif
          else
c
c...do automatic reserv
c
              savid2 = token2
              isvsub = ivxsub
              ifl(9) = ifl(11)
              ifl(10) = ifl(12)
              idst = 14
              rest = 0.
              irest(3) = 32767
              irest(4) = 14
              call vstore
              ivxsub = subsv
              call vstchk
          end if
      end if
c
c...end of fix
c
      keyold = keyhld
      istold = ist
      ifl(9)=ifl(11)
      ifl(10)=ifl(12)
      if (nam(1:3) .ne. '@UN') then
          savid2 = token2
          isvsub = isub
      endif
      rest = 0.
      idst = ietype

      call ptdesc(nclkey,idst,rest)
      if (idst.lt.11) then
        irest(3) = numwds(idst)
      else if (idst .eq. 20) then
        call gtpnnp (nclkey,irest(3),jtp)
      else if (idst .eq. 21) then
        irest(3) = 6 
      else if (idst .eq. DATAST) then
        call dtgtnw (nclkey,irest(3))
      else
        irest(3) = 0
      endif
      call vstore
 
99    return
      end
