C********************************************************************/
C*    NAME         :  sequnc.f
C*       CONTAINS:
C*					sequnc
C*    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       sequnc.f , 25.1
C*     DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:40
C********************************************************************/
C
c***********************************************************************
c
c   SUBROUTINE:  sequnc
c
c   FUNCTION:  Parses the SEQUNC command.
c
c    PARAMETERS
c       INPUT  :
c          iflg         - 0: parsit gets the label;
c                         1: the label is provided
c          seqlab       - sequence label
c       OUTPUT : none
c    RETURNS      : none
c    SIDE EFFECTS : none
c    WARNINGS     : none
c
c***********************************************************************
c
      subroutine sequnc(iflg,seqlab)
c
      include 'com8a.com'
      include 'cutter.com'
c
      integer*2 iflg
      character*(*) seqlab

      integer*2 icfl(10),itemp1(4),icyc(12),ncsym(3)
      integer*4 i,jcfl(10),nc,strlen1,ix,inum
c
      real*8 rcl(100),trmx(12),dbuf(20)
c
      character*80 sym(3)
      character*792 acl
c
      equivalence (rcl(1),acl), (rcl(26),ncsym), (rcl(28),icfl)
      equivalence (rcl(31),itemp1), (rcl(47),icyc)
c
      data trmx /1,0,0,0, 0,1,0,0, 0,0,1,0/
c
c...Get SEQUNC label
c
      idtype = -1
      if (iflg .eq. 0) then
          call parsit
      else
          ityp = 2
          ist  = 1 
          token2 = seqlab
          length = strlen1(token2)
      endif
c
c...SEQUNC/END
c
      if (token2(1:3) .eq. 'END' .and. length .eq. 3) then
          call putcl(7200,2,1,rcl)
          call seqend (0)
          go to 8000
      endif
c
c...Allow for numeric labels
c
      if (((ityp .eq. 2 .and. ist .eq. 2) .or. ityp .eq. 3 .or.
     1     ityp .eq. 4) .and. tv .gt. 0. .and. tv .le. 32767.) then
          inum   = tv
          call aptisn (inum,token2,length)
      endif
c
c...Store SEQUNC/label
c
      acl(1:20) = token2
      call seqsto(0,token2,length)
      length = length - 1
c
c...Store current cl point
c
      do 100 i=1,6,1
          rcl(i+3) = sc(i)
  100 continue
      if (ifl(73) .eq. 1) then
          call conent(rcl(4),sc(41),3)
          call conent(rcl(7),sc(41),4)
      endif
c
c...Store cutter statements
c
      call cutget (rcl(10),dbuf,jcfl,sym(1),sym(2),sym(3))
      do 200 i=1,10,1
          icfl(i) = jcfl(i)
          rcl(i+15) = dbuf(i)
          rcl(i+59) = dbuf(i+10)
  200 continue
c
c...Store cutter symbols
c
      ix = 553
      do 300 i=1,3,1
          nc = strlen1(sym(i))
          ncsym(i) = nc
          if (nc .gt. 0) then
              acl(ix:ix+nc-1) = sym(i)
              ix = ix + nc
          endif
  300 continue
c
c...Store MULTAX
c
      itemp1(2) = ifl(82)
c
c...Store RAPID, FEDRAT, & SPINDL
c
      call rpchk (itemp1(1))
      itemp1(3) = FITYP
      itemp1(4) = CSPDIR
      rcl(32) = CFEED(1)
      rcl(45) = CFEED(2)
      rcl(46) = CRPM
c
c...Store TRACUT
c
      if (ifl(73) .eq. 1) then
          do 500 i=1,12,1
              rcl(32+i) = sc(40+i)
  500     continue
      else
          do 550 i=1,12,1
              rcl(32+i) = trmx(i)
  550     continue
      endif
c
c...Store cycle
c
      do 600 i=1,10,1
          icyc(i) = ICIPRM(i)
          rcl(49+i) = RCIPRM(i)
  600 continue
      icyc(11) = CYCRET
c
c...Save cl record
c
      call putcl (7200,1,78,rcl)
c
c...End of routine
c
 8000 return
      end
