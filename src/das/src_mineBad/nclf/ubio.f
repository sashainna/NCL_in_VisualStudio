c*****************************************************
c**
C*     MODULE NAME AND RELEASE LEVEL
C*       ubio.f , 25.1
C*     DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:49
c*********************************************************************
c
c***********************************************************************
c
c   FILE NAME:  ubio
c   CONTAINS:
c               ubio    ubgetu  ubgetd  ubputu  ubnfnd  ubimsg  ubnstr
c               
c
c   copyright (c) 1996 NCCS.
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  ubio (kdir)
c
c   FUNCTION:  This routine performs GET and PUT operations on both
c              NCL Data Bases (DBFN) and Unibase (UBFN).
c
c   INPUT:  kdir    I*2  D1  - 1 = GET.  2 = PUT.
c
c   OUTPUT: none.
c
c***********************************************************************
c

      subroutine ubio (kdir)
c
      include 'com8a.com'
      include 'comdb.com'
c
      integer*2 ii,prm,wlen,nfnd,iren,nlay,endthru,iflsav
      integer*4 layer(50), i4sub, i4bsub,iary(20),offset
c
      logical errok,copied,first,found,lg,iug,idg,iup,idp
c
      real*8 rary(10),rsav
c
      character*64 hldsv
      character*64 tok2sv
      character*64 ubname,rfname
c
      integer*2 DATAV
      parameter (DATAV=858)
c
c...Initialize routine
c
      errcom = ' '
      ifl(44) = 9
      first  = .true.
      msglin = 15
      ldtflg = .true.
      ldtext = .true.
      endthru = 0
      nfnd = 0
      offset = 0
c
c...Set Mode flags
c
      iug    = ifl(316) .eq. 2 .and. kdir .eq. 1
      iup    = ifl(316) .eq. 2 .and. kdir .eq. 2
      idg    = ifl(316) .eq. 1 .and. kdir .eq. 1
      idp    = ifl(316) .eq. 1 .and. kdir .eq. 2
      if (iug .or. idg) ifl(354) = 1
c...'put' should act as 'get' when parse it
c...yurong
      if (iup .or. idp) ifl(354) = 1
c
c...Output I/O message when in
c...Batch or VT modes
c
      if (ifl(35) .eq. 0 .or. ifl(35) .eq. 2) then
          call ersw3 (15,1)
          if (ifl(154).eq.0) then
              if (iug) then
                  cout = '     retrieving item(s) from Unibase'
              else if (idg) then
                  cout = '     retrieving item(s) from Database'
              else if (iup) then
                  cout = '     storing item(s) to Unibase'
              else
                  cout = '     storing item(s) to Database'
              endif
              call putmsg (cout,36,15,0)
          endif
      endif
c
c...GET/RENAME
c
      iren   = 0
      nlay   = 0
      if (iug) then
          call svpars
          wdtype = -1
          call parsit
          if (ityp .eq. 1 .and. ist .eq. 869) then
             iren   = 1
          else
             call rtpars
          endif
      endif
c
c...GET,PUT/LAYER
c...Perform all parsing here
c
      if (iug .or. iup) then
          call svpars
          wdtype = -1
		  call parsit
          if (ityp .eq. 1 .and. ist .eq. 902) then
              nlay    = 0
              if (nextyp .eq. 11) go to 9217
c
c......Get next parameter
c
   20         idtype = -1
              wdtype = -1
              call parsit
c
c......THRU
c
              if (ityp .eq. 1 .and. ist .eq. 152) then
                  if (nlay .eq. 0 .or. nextyp .eq. 11) go to 9217
                  wdtype = -1
                  call parsit
                  if (ityp .eq. 3 .or.
     1                (ityp .eq. 2 .and. ist .eq. 2)) then
                      i = itv
                      if (i .lt. layer(nlay)) go to 9114
                      if (layer(nlay) .gt. 0)
     1                    layer(nlay) = layer(nlay) * (0-1)
                  else
                      go to 9217
                  endif
              else if (ityp .eq. 1 .and. ist .eq. 666) then
                  offset    = 0
                  if (nextyp .eq. 11) go to 9217
c
c......Get next parameter
c
                  call parsit
c
c......Store offser number
c
                  if (ityp .eq. 3 .or.
     1                (ityp .eq. 2 .and. ist .eq. 2)) then
                      offset = itv
                  else
                      go to 9217
                  endif
                  prm   = -10
                  go to 500              
              endif
c
c......Store layer number
c
              if (ityp .eq. 3 .or.
     1            (ityp .eq. 2 .and. ist .eq. 2)) then
                  if (itv .lt. 0) go to 9217
                  nlay = nlay + 1
                  layer(nlay) = itv
                  if (nextyp .ne. 11) go to 20
              else
                  go to 9217
              endif
              prm   = -10
              go to 500
              
          else
              call rtpars
          endif
      endif
c
c...Get next item to GET/PUT
c
   50 prm    = 1000
      i4sub  = 0
      i4bsub = 0
      idtype = 0
      wdtype = -1
      call parsit
      if (endthru.eq.1) then
          errok  = .false.
          ubname = token2
          i4bsub = ivxsub
          prm    = -3
          rfname = ubname
          i4sub = i4bsub
          found  = .false.
          endthru = 0
          goto 500
      endif
      if (ityp .eq. 7) go to 8000
c
c...Errors allowed are ...
c......Identifier has not been reserved
c......Subscript out of range
c......Invalid syntax format
c......Identifier not previously defined
c......Parser error on token '*'
c
      if (err .and.
     1    ((ifl(2) .eq. 88 .or. ifl(2) .eq. 85 .or.
     2      ifl(2) .eq. 61))) then
          err    = .false.
          ifl(2) = 0
cc      else if (err .and. ityp .eq. 2 .and. ist .eq. 1) then
cc          err    = .false.
cc          ifl(2) = 0
c
c...If an error occured during an inclusive subscript
c...then assume the end of this inclusive subscript
c...Bobby  -  2/17/00
c
      else if (err .and. ifl(325) .ne. 0) then
          err   = .false.
          ifl(2) = 0
          ifl(325) = 0
          go to 50
      else if (err) then
          go to 8000
      endif
      found  = .false.
      errok  = .true.
      call vctoid (ist,ii,ix)
      call ifidge (ii,lg)
c
c...---/ALL
c
      if (ityp .eq. 1 .and. ist .eq. 816) then
          prm    = 0
c
c...Check for an offset for layer setting when using GET
c
          if (iug .and. nextyp.eq.9) then
              call parsit
              if (ityp.eq.1 .and. ist.eq.666) then
                  if (nextyp.eq.9) then
                      call parsit
                      if ((ityp.eq.2 .and. ist.eq.2) .or. ityp.eq.3 
     x                    .or. ityp.eq.4) then
                          offset = itv
                      else
                          goto 9224
                      endif
                  else
                      goto 9224
                  endif
              else
                  goto 9224
              endif
          endif
c
c...Generic geometry name
c...POINT, LINE, CIRCLE, etc.
c
      else  if (ityp .eq. 1 .and. (ist .gt. 601 .and. ist .lt. 611 .or.
     1          ist .eq. 613 .or. ist .eq. 636 .or. ist.eq.DATAV .or.
     2          ist .eq. 123 .or. ist .eq. 616 .or. ist.eq.877 .or.
     3          ist .eq. 878)) then
c
c......POINT-3; VECTOR-4; LINE-5; PLANE-6; CIRCLE-7; CURVE-8; SURF-9
c......SHAPE-2; PATERN-36 POINT-VECTOR-13; SYMBOL-31; PLACE-32
c
          if (nextyp .eq. 4) go to 150
          prm    = ii
          if (ist .eq. DATAV) prm = DATAST
          ubname = token2 
          i4bsub = 0
          rfname = ubname
c
c...---/DRAFT
c
      else if (ityp.eq.1 .and. ist.eq.1059) then
          prm = 34
          ubname = token2 
          i4bsub = 0
          rfname = ubname
      endif
c
c...Wild card (*) type name
c
  150 if (ityp .eq. 10) then
          ubname = token2
          i4bsub = ivxsub
          rfname = ubname
          wlen   = length
          prm    = -2
c
c...this parser should not called by V97 and V97+ because we get XX* one time
c...but V96 and V96- will get 'XX' then '*', so they need call parser twice.
c...          call parser
c
c......Reset error flag set by invalid type syntax
c......(PT*)
c
          ifl(2)=0
c
c...End of command
c
      else if (ityp .eq. 7) then
          if (first) then
              call error (110)
          endif
          go to 8000
c
c...---/name1,name2,etc.
c
      else if (prm .eq. 1000) then
          errok  = .false.
          ubname = token2
          i4bsub = ivxsub
          prm    = -3
c
c......If not end of command
c......Go check for "name1,AS,name2"
c
          if ((nextyp.eq.11) .or. (ifl(325).ne.0)) then
              rfname = ubname
              i4sub = ivxsub
c
c...---/name1,AS,name2
c
          else
              rsav   = tv
              iflsav = ifl(325)
              call pthsav (iary,rary,tok2sv,hldsv)
              idtype = 0
              wdtype = -1
              call parsit
              if (ityp .eq. 1 .and. ist .eq. 832) then
c
c......End of command syntax error
c
                  if (nextyp.eq.11) then
                      call error (244)
                      go to 8000
                  endif
c
c......Get AS geometry name (name2)
c
                  idtype = 0
                  wdtype = -1
                  call parsit
                  if (err .and.
     1                ((ifl(2) .eq. 88 .or. ifl(2) .eq. 85 .or.
     2                  ifl(2) .eq. 61))) then
                      err    = .false.
                      ifl(2) = 0
                  else if (err) then
                      go to 8000
                  endif
                  tv     = rsav
                  ist    = iary(2)
                  rfname = token2
                  i4sub = ivxsub
              else
                  rfname = ubname
                  i4sub = i4bsub
c
c...if end of input, because we reset back, so in  n THRU m claud, we haven't
c....executable the last one yet, so remember here and execute it later
c...Yurong
c
                  if ((ityp .eq. 7).and.(iflsav.eq.(-1))) then
                      endthru = 1
                  endif
                  call pthrst (iary,rary,tok2sv,hldsv)
              endif
          endif
      endif
c
c...Perform Data/Unibase I/O
c...GET/Unibase
c 
  500 if (iug) then
          call ubgetu (ubname,i4bsub,rfname,i4sub,prm,found,errok,
     1                 copied,msglin,iren,nlay,layer,offset)
          if (ifl(325).ne.0) then
             ifl(2)=0
             err   = .false.
          endif
c
c......End inclusive THRU after 20 not found
c
          if (found) then
              nfnd = 0
          else if (ifl(325) .ne. 0) then
              if (nfnd .eq. 20) then
                  ifl(325) = 0
                  nfnd = 0
              else
                  nfnd = nfnd + 1
              endif
          endif
c
c...GET/Database
c 
      else if (idg) then
          call ubgetd (ubname,i4bsub,rfname,i4sub,prm,wlen,found,errok,
     1                 msglin,copied)
          if (ifl(325).ne.0) then
             ifl(2)=0
             err   = .false.
          endif
c
c...PUT/Unibase
c 
      else if (iup .or. idp) then
          call ubputu (ubname,i4bsub,rfname,i4sub,prm,wlen,found,msglin,
     1                 nlay,layer)
      endif
c
c...Parse next parameter
c...If not at end of command and no user interrupt
c
      if (nextyp .ne. 11 .and. ifl(86) .ne. 1) go to 50
c
c...End of routine
c
 8000 ifl(86) = 0
      ifl(354) = 0
      ldtflg = .false.
      ldtext = .false.
      return
c
c...End of statement expected
c
 9004 ifl(2) = 004
      go to 8000  
c
c...Invalid THRU clause
c
 9114 ifl(2) = 114
      go to 8000
c
c...Integer expected
c
 9217 ifl(2) = 217
      go to 8000
c
c...Positive integer expected
c
 9224 ifl(2) = 224
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ubgetu (ubname,ubsub,rfname,rfsub,prm,found,errok,
c                        copied,mlin,iren,nlay,layer)
c
c   FUNCTION:  This routine performs GET operations on Unibases (UBFN).
c
c   INPUT:  ubname   C*n  D1  - Name of entity stored in external
c                               data base.
c
c           ubsub    I*2  D1  - 'ubname's subscript.
c
c           rfname   C*n  D1  - New name of entity retrieved.
c
c           rfsub    I*2  D1  - 'rfname's subscript.
c
c           prm      I*2  D1  - Type of GET to perform.  -3 = Specific
c                               name of entity (LN1), -2 = Wildcard
c                               specification (LNA*), 0 = GET/ALL,
c                               positive = Geometry type (PT,LN,etc.).
c
c           errok    L*2  D1  - Should be set to .TRUE. when multiple
c                               entries are being retrieved.  Used for
c                               error message display only.
c
c           mlin     I*2  D1  - Line count for window output.
c
c           iren     I*2  D1  - 1 = Rename geometry as it is imported.
c
c           nlay     I*2  D1  - >0 when retrieving by Layers.
c
c           layer    I*4  D50 - Layer numbers to retrieve when nlay > 0.
c
c   OUTPUT: found    L*2  D1  - Returns .TRUE. if the entity was found
c                               in the data base.
c
c           copied   L*2  D1  - Returns .TRUE. if the requested entity
c                               was transfered.
c
c***********************************************************************
c
      subroutine ubgetu (ubname,ubsub,rfname,rfsub,prm,found,errok,
     1                   copied,mlin,iren,nlay,layer,offset)
c
      include 'com8a.com'
      include 'comdb.com'
c
      integer*2 prm,mlin,iren,nlay
      integer*4 ubsub,rfsub,layer(50),offset,isav
c
      logical found,errok,copied
c
      character*64 ubname,rfname
c
      integer*2 svityp,svist,i2fnd,i2erok,i2cpd
c
      character*64 svtok2
c
c...Save parsing parameters
c
      svtok2  = token2
      svityp = ityp
      svist  = ist
      isav = ivxsub
c
c...Read from secondary Unibase
c
      ifl(299) = 1
      i2cpd  = iren
      call ubact(2)
      call ubfind (prm,ubname,ubsub,rfname,rfsub,i2fnd,i2erok,i2cpd,
     1             nlay,layer, offset)
      call ubact(1)
      ifl(299) = 0
c
c...Set found flags
c
      found  = i2fnd  .eq. 1
      errok  = i2erok .eq. 1
      copied = i2cpd  .eq. 1
c
c...Restore parsing parameters
c
      token2  = svtok2
      ivxsub = isav
      ityp   = svityp 
      ist    = svist
c
c...Check for user interrupt
c
      call ckintr (ifl(86),ifl(35))
      if (ifl(86) .eq. 1) then
          call error (-243)
          go to 8000
      endif
c
c...Entity not found
c
      if (.not. found) then
          ist   = 2
          if ((ifl(325).eq.0).and.(rfsub.le.1000000)) then
              call ubnfnd (rfname,rfsub)
          endif
          go to 8000
c
c...Write out stored message
c
      else
          if (copied) then
              if (ifl(154) .eq. 1) 
     1            call ubimsg (rfname,rfsub,ubname,ubsub,
     2                         ' retrieved as ',mlin)
          else
              call ubnstr (rfname,rfsub,mlin,errok)
          endif
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ubgetd (ubname,ubsub,ifname,rfsub,prm,wlen,found,errok,
c                        copied,mlin)
c
c   FUNCTION:  This routine performs GET operations on NCL Data Bases
c              (DBFN).
c
c   INPUT:  ubname   C*n  D1  - Name of entity stored in external
c                               data base.
c
c           ubsub    I*2  D1  - 'ubname's subscript.
c
c           ifname   C*n  D1  - New name of entity retrieved.
c
c           rfsub    I*2  D1  - 'rfname's subscript.
c
c           prm      I*2  D1  - Type of GET to perform.  -3 = Specific
c                               name of entity (LN1), -2 = Wildcard
c                               specification (LNA*), 0 = GET/ALL,
c                               positive = Geometry type (PT,LN,etc.).
c
c           wlen     I*2  D1  - Length of 'ubname' string.
c
c           errok    L*2  D1  - Should be set to .TRUE. when multiple
c                               entries are being retrieved.  Used for
c                               error message display only.
c
c           mlin     I*2  D1  - Line count for window output.
c
c   OUTPUT: found    L*2  D1  - Returns .TRUE. if the entity was found
c                               in the data base.
c
c           copied   L*2  D1  - Returns .TRUE. if the requested entity
c                               was transfered.
c
c***********************************************************************
c
c...note: I haven't change character to 64 from 6 because I think the data file have that
c...format
c
      subroutine ubgetd (ubname,ubsub,ifname,rfsub,prm,wlen,found,errok,
     1                   copied,mlin)
c
      include 'com8a.com'
      include 'comdb.com'
c
      integer*2 prm,wlen
      integer*4 ubsub,rfsub,isav,len1,len2,strlen1,match
c
      logical found,errok,copied
c
      character*64 ubname,ifname
c
      integer*2 svityp,svist,dbitem,dbrec,dbrech,assrec,subnm(4),
     1          iadvst(144),idbvst(144),rsubnm(4)
c
      logical dbrecl
c
      real*8 rdbvst(36)
c
      character*1 atname(6),awname(6)
      character*6 tname,wname
      character*8 dbvst(36),dbname,rfname
      character*64 svtok,dbname2,rfname2
c
      equivalence (idbvst,rdbvst,dbvst)
      equivalence (tname,atname), (wname,awname)
      equivalence (dbname,subnm), (rfname,rsubnm)
c
c...Initialize routine
c
      wname  = ubname(1:6)
      dbname = ubname(1:8)
      subnm(4) = ubsub
      rfname = ifname(1:8)
      rsubnm(4) = rfsub
      assrec = 0
      dbrech = 1
c
c...Prepare for Wildcard (*,ALL) GET
c
      if (prm .lt. 0 .and. prm .ne. -2) go to 2400
          
      dbrec  = 1
      dbitem = 0
      dbrecl = .false.
c
c......Read a variable symbol table record from
c......the Database
c
 2150 call getdb (rdbvst,dbrec)
      if (dbrech .eq. 9) dbrecl = .true.
c
c......Have all names on record been handled?
c
 2200 if (idbvst(141) .le. dbitem) then
c
c.........Is there a continuation record?
c
          if (idbvst(143) .ne. 0) then
                 dbrec  =idbvst(143)
                  dbitem = 0
                  go to 2150
c
c.........Has the last Database vst record been read?
c.........If so nothing matches
c
              else if (dbrecl) then
                  go to 8000
c
c.........Otherwise go get next index record
c
              else
                  dbrec  = dbrech + 2
                  dbrech = dbrec
                  dbitem = 0
                  go to 2150
              endif
c
c......Get the next name from the Database
c
          else
              dbitem = dbitem + 1
              if (dbvst(dbitem) .eq. ' ') goto 2200
c
c.........Wildcard given
c
              if (prm .eq. -2) then
                  tname=dbvst(dbitem)
c
c...wildcard string could be '*', '*str', 'str1*str2', '*str1*str2*...*str'
c...just call 'chkwstr' to see if token2 match the wild card string
c
                 len1 = strlen1 (tname)
                 len2 = wlen
                 call chkwstr(tname, len1, wname, len2, match)
                 if (match.eq.0)  go to 2200
c
c.........Standard name given
c
              else if (prm .gt. 0) then
                  if (assrec .ne. dbrec+1) then
                      assrec = dbrec  + 1
                      call getdb (iadvst,assrec)
                  endif
                  if (prm .eq. iadvst(dbitem*4)) go to 2300
                  go to 2200
              endif
          endif
c
c...Found a match
c...Go get it
c
 2300     dbname = dbvst(dbitem)
          if (dbname .eq. '        ') goto 2200
          rfname = dbname
c
c...Get entity from Database
c
 2400 if (ifl(64).ne.0) then
          cdbio  = ifl(64)
          svtok  = token2
          isav = ivxsub
          svityp = ityp
          svist  = ist
          call dbfind (dbname,rfname,found,errok,copied)
          token2  = svtok
          ivxsub = isav
          ityp   = svityp
          ist    = svist
c
c...Check for user interrupt
c
          call ckintr (ifl(86),ifl(35))
          if (ifl(86) .eq. 1) then
              call error (-243)
              go to 8000
          endif
c
c...Entity not found
c
          rfname2 = rfname
          dbname2 = ubname
          if (.not. found) then
              ist   = 2
              if (prm .eq. 0 .or. prm .eq. -2 .or.
     1            ubsub .eq. xffff) go to 2200
              call ubnfnd (rfname2, ubsub)
              go to 8000
c
c...Write out stored message
c
          else
              if (copied) then
                  if (ifl(154) .eq. 1) then
                      call ubimsg (rfname2,rfsub,dbname2,ubsub,
     2                             ' retrieved as ',mlin)
                  endif
              else
                  call ubnstr (rfname2,rfsub,mlin,errok)
              endif
          endif
          if (prm .ge. 0 .or. prm .eq. -2) goto 2200
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ubputu (ubname,ubsub,ifname,rfsub,prm,wlen,found,mlin,
c                        nlay,layer)
c
c   FUNCTION:  This routine performs PUT operations on both NCL Data
c              Bases (DBFN) and Unibases (UBFN).
c
c   INPUT:  ubname   C*n  D1  - Name of entity stored in current
c                               Unibase.
c
c           ubsub    I*2  D1  - 'ubname's subscript.
c
c           ifname   C*n  D1  - New name of to store in external
c                               data base.
c
c           rfsub    I*2  D1  - 'ifname's subscript.
c
c           prm      I*2  D1  - Type of PUT to perform.  -3 = Specific
c                               name of entity (LN1), -2 = Wildcard
c                               specification (LNA*), 0 = PUT/ALL,
c                               positive = Geometry type (PT,LN,etc.).
c
c           wlen     I*2  D1  - Length of 'ubname' string.
c
c           mlin     I*2  D1  - Line count for window output.
c
c           nlay     I*2  D1  - >0 when storing by Layers.
c
c           layer    I*4  D50 - Layer numbers to store when nlay > 0.
c
c   OUTPUT: found    L*2  D1  - Returns .TRUE. if the entity was found
c                               in the data base.
c
c***********************************************************************
c
      subroutine ubputu (ubname,ubsub,ifname,rfsub,prm,wlen,found,mlin,
     1                   nlay,layer)
c
      include 'com8a.com'
      include 'comdb.com'
c
      integer*2 prm,mlin,wlen,nlay
      integer*4 ubsub,rfsub,layer(50),len1,len2,strlen1,match
c
      logical found
c
      character*64 ubname,ifname
c
      integer*2 svityp,svist,i
      integer*2 inlay,color,pen,linwgt,lintyp,idtok(4),irtok(4)
      integer*4 isub, nclkey,dsubnm, subnm,isvtok,ipg,iel,oldkey
      integer*4 nc
      logical first
c
      real*8 asw, stv
c
      character*1 awname(64)
      character*64 wname
      character*64 dbname,rfname
      character*8 dbname8,rfname8
      character*64 svtok
      character*72 tmpstr
c
      equivalence (wname,awname)
      equivalence (dbname8,idtok), (rfname8,irtok)
c
c...Make sure Database is open for write
c
      if (ifl(316) .eq. 1) then
          if (wdbio.eq.0) then
              ifl(2) = 98
              go to 8000
          endif
      else if (ifl(316) .ne. 2) then
          ifl(2) = 450
          go to 8000
      else
          ifl(299) = 1
      endif
c
c...Initialize routine
c
      oldkey = 0
      first= .true.
      cdbio  = wdbio
      errcom = ' '
      ifl(44) = 9
      dbname = ubname         
	dsubnm = ubsub

      rfname = ifname
      subnm = rfsub
      wname  = ubname
c
c...Get entity to store
c.....Symbols are not stored in the variable list and so must be
c.....searched for directly
c
200   if ((prm.lt.0 .and. prm.ne.-2 .and. prm.ne.-10) .or.
     x     prm.eq.31 .or.prm.eq.32) then
          if (ityp .eq. 2 .and. ist .eq. 2) then
              isub   = ubsub
              call vxchk (dbname,isub,nclkey,ipg,iel,kwds,ietype)
          else
              if (prm.eq.31 .or. prm.eq.32) then
                  call ncl_ubget_symbol(token2,prm,oldkey)
                  ubname = token2
                  if (prm.eq.-1) goto 8000
              endif
              call ub_symbol_name (ubname,ivxsub,nclkey,origin,i)
              isub = ivxsub
              oldkey = nclkey
              kwds = 1
              if (i.eq.0) then
                  call gtdesc (tv,nclkey,nwds,ietype)
                  kwds = nwds
              else if (i.eq.1) then
                  ietype = 31
              else
                  ietype = 32
              endif
          endif
          if (prm.ne.31.and.prm.ne.32) then
              goto 240
          else
              goto 230
          endif
c
c.....Look for draft entities
c
      else if (prm.eq.34) then
          call ncl_ubget_draftent(token2,prm,oldkey,ivxsub)
          ubname = token2
          if (prm.eq.-1) goto 8000
          isub = ivxsub
          nclkey = oldkey
          kwds = 1
          ietype = 34
          goto 230
      endif
c
c...Prepare for wildcard PUT
c
210   if (first) call vxlfst
      first = .false.
c
c......Get next entity.
c
  220 call vxlnxt (token2, isub, nclkey, kwds, ietype, ipg, iel)
      if (ietype .eq. 1) go to 8000
      if ((ietype .ge. 11 .and. ietype .le. 17) .or. ietype .eq. 19 .or.
     1    nclkey .eq. 0) go to 220
c
c......Test if name is of unnamed nested generated geometry
c
      if (token2(1:1).eq.'@') go to 220
c
c...wildcard string could be '*', '*str', 'str1*str2', '*str1*str2*...*str'
c...just call 'chkwstr' to see if token2 match the wild card string
c
      if (prm .eq. -2) then
          if (isub.gt.0) then
              nc = strlen1(token2)
              write (tmpstr, 1010) token2, isub
 1010         format (a<nc>,'(',i6,')')
              len1 = strlen1 (tmpstr)
         else
              len1 = strlen1 (token2)
              tmpstr = token2
          endif
          len2 = wlen
          call chkwstr(tmpstr, len1, wname, len2, match)
          if (match.eq.0)  go to 220
      else if (prm .gt. 0) then
          if (prm .eq. ietype) go to 230
          go to 220
c
c......PUT/LAYER
c
      else if (prm .eq. -10) then
          call umf_get_attrib (nclkey,color,pen,lintyp,linwgt,inlay)
          do 227 i=1,nlay,1
              if (layer(i) .lt. 0) then
                  if (inlay .ge. (layer(i) * (0-1)) .and.
     1                inlay .le. layer(i+1)) go to 230
              else
                  if (inlay .eq. layer(i)) go to 230
              endif
  227     continue
          go to 220
      endif
  230 rfname = token2
      subnm = isub
      dsubnm = isub
      dbname = rfname
c
c...Get ready for data transfer
c
  240 svtok  = token2
      isvtok = isub
      svityp = ityp
      svist  = ist
      stv    = tv
c
c...Put entity into Database
c
      if (ifl(316) .eq. 1) then
        nwds = kwds
        call ptdsc3 (nclkey, nwds, ietype, asw)
        rfname8(1:6) = rfname(1:6)
        dbname8(1:6) = dbname(1:6)
        idtok(4) = isub
        irtok(4) = isub
        call dbstor (rfname8,dbname8,asw,found)
c
c...Put entity into Unibase
c
      else if (ifl(316) .eq. 2) then
        call ubstor (rfname,subnm,dbname,dsubnm,nclkey,ietype,found)
      endif
c
c...Restore saved settings
c
      token2 = svtok(1:64)
      ivxsub = isvtok
      ityp   = svityp
      ist    = svist
      tv     = stv
c
c...Check for user interrupt
c
      call ckintr (ifl(86),ifl(35))
      if (ifl(86) .eq. 1) then
          call error (-243)
          go to 8000
      endif
c
c...Entity not found
c
      if (.not. found) then
          if (prm .eq. 0 .or. prm .eq. -2 .or. subnm .eq. 1000000)
     1        go to 220
          call ubnfnd (dbname,dsubnm)
          go to 8000
c
c...Write out stored message
c
      else
          call ubimsg (rfname,subnm,dbname,dsubnm,
     1                 ' stored as ',mlin)
      endif
c
c...Go to 200 if PUT is for multiple symbols or symbol instances
c
      if (prm.eq.31.or.prm.eq.32.or.prm.eq.34) goto 200
      if (prm .ge. 0 .or. prm .eq. -2 .or. prm .eq. -10) goto 220
c
c...End of routine
c
 8000 ifl(299) = 0
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ubnfnd (rfname,isub)
c
c   FUNCTION:  This routine displays an error message when an entity
c              is not found for GET and PUT operations.
c
c   INPUT:  rfname   C*n  D1  - Name of entity not found.
c
c           isub     I*2  D1  - 'rfname's subscript.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine ubnfnd (rfname,isub)
c
      include 'com8a.com'
c
      integer*4 isub
c
      character*64 rfname
      integer*4 nc, strlen1
c
c...If no subscript just show name
c
      nc = strlen1(rfname)
      if (isub .eq. 0) then
          write (errcom,1010) rfname
 1010     format (' name not found:',a<nc>)
c
c...Write name plus subscript
c
      else
          write (errcom,1020) rfname,isub
 1020     format (' name not found: ',a<nc>,'(',i6,')')
      endif
c
c...Output error message
c
      ist    = 2
      call error(9)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ubimsg (ubname,ibsub,rfname,isub,ctxt,mlin)
c
c   FUNCTION:  This routine outputs the 'retrieved' and 'stored'
c              messages for GET and PUT operations.
c
c   INPUT:  ubname   C*n  D1  - Name of entity retrieved from data base.
c
c           ibsub    I*4  D1  - 'ubname's subscript.
c
c           rfname   C*n  D1  - New name of entity stored.
c
c           isub     I*4  D1  - 'ifname's subscript.
c
c           ctxt     C*n  D1  - 'retrieved/stored as' text.
c
c           mlin     I*2  D1  - Line count for window output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine ubimsg (ubname,ibsub,rfname,isub,ctxt,mlin)
c
      include 'com8a.com'
c
      integer*2 mlin
      integer*4 ibsub,isub
c
      character*(*) ubname,rfname,ctxt
c
      character*80 namedb,namerf
      integer*4 nc, nc2, strlen1
c
c...If no subscript just show name
c
      nc = strlen1(ubname)
      if (ibsub .eq. 0) then
          write (namedb,1022) ubname
 1022     format (a<nc>,8x)
c
c...Write out name and subscript
c
      else
          write (namedb,1024) ubname,ibsub
 1024     format (a<nc>,'(',i6,')')
      endif
c
c...Write out AS name
c
      nc = strlen1(rfname)
      if (isub .eq. 0) then
          write (namerf,1022) rfname
c
c...Write out AS name and subscript
c
      else
          write (namerf,1024) rfname,isub
      endif
      nc = strlen1(namerf)
      nc2 = strlen1(namedb)
      write (cout,1030) namerf,ctxt,namedb
 1030 format (5x,a<nc>,a,a<nc2>)
      nc = strlen1(cout)
      i = nc
      call putmsg (cout,i,mlin,0)
      mlin   = mlin   + 1
      if (mlin .gt. 24) mlin = 15
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ubnstr (rfname,isub,mlin,errok)
c
c   FUNCTION:  This routine outputs an 'entity not stored' message for
c              GET and PUT operations.
c
c   INPUT:  rfname   C*n  D1  - Name of entity which was not stored.
c
c           isub     I*2  D1  - 'ubname's subscript.
c
c           mlin     I*2  D1  - Line count for window output.
c
c           errok    I*2  D1  - .TRUE. = output name of entity not
c                               stored.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine ubnstr (rfname,isub,mlin,errok)
c
      include 'com8a.com'
c
      integer*2 mlin
      integer*4 isub
c
      logical errok
c
      character*64 rfname
c
      character*80 namerf
      integer*4 nc, strlen1
c
c...Generate a warning and open scrolling window
c
      call error (-384)
      if (ifl(35) .eq. 0 .or. (ifl(35) .eq. 2 .and. ifl(350) .eq. 1))
     1    call opnwin()
c
c...If no subscript just show name
c
      if (isub .eq. 0) then
          write (namerf,1010) rfname
 1010     format (a64)
c
c...Write name plus subscript
c
      else
          nc = strlen1 (rfname)
          write (namerf,1020) rfname,isub
 1020     format (a<nc>,'(',i6,')')
      endif
c
c...Create message
c
      nc = strlen1 (namerf)
      write (cout,1050) namerf
 1050 format (5x,a<nc>,' already defined - not stored ********')
c
c...Single item not stored - report name
c
      if (.not.errok) then
          if (.not.err) then
              nc = strlen1 (namerf)
              write (errcom,1070) namerf
 1070         format (' item in error= ',a<nc>)
              call error (8)
          endif
          go to 8000
      endif
c
c...Output error message
c
      nc = strlen1(cout)
      call putmsg (cout,i,mlin,0)
      mlin = mlin + 1
      if (mlin .gt. 24) mlin = 15
c
c...End of routine
c
 8000 return
      end
