C*********************************************************************
C*    NAME         :  prtgeo.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       prtgeo.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:29
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine prtgeo
c*       this routine prints the canonical form   
c*       of all geometry types during a batch run for the 'print/'     
c*       statement                                                     
c*
c*      this subroutine should be re-written and broken into several separate
c*      subroutines to make it easier to follow the logic and process large
c*      amounts of variable symbols more efficiently.  the subroutines that
c*      this one should be broken into should be:
c*
c*        prtgeo - main driver subroutine for print geometry statements
c*        pgcomp - compare a current vst name entry to a specific sort array
c*                  entry and return a higher, equal or lower indication
c*        pgins  - insert the current vst name entry at a specific spot in
c*                  the sort array.
c*        pgprnt - print the canonical form of all entries in the sort array
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
      subroutine prtgeo

      include 'com8a.com'
      include 'const.com'

c (*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)
c
c   note: the arrays srtnam and srtasw may be enlarged or reduced to adjust
c         the amount of storage this routine uses.  they are the arrays that
c         geometry names are held in to sort them.  a smaller array will
c         cause more possible read passes through the ranfil while a larger
c         array will cause fewer passes but use more storage space.  the
c         variable 'max' must always have a value of two less than the array
c         size of the srtnam array.  the srtnam and srtasw must always be
c         the same size as each other.
c
c (*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)
      integer*2 max
      character*64 srtnam(202)
      integer*4 ssrtsub(202), mxtext
      character*8 srtasw(202)
      integer*2 isrtas(144)
      real*8 rrtasw(202)
      equivalence (isrtas,rrtasw,srtasw)

      real*8      r8buf(16)
      integer*2   i,j,k,ii,jj,kk,imove,ptype,hinum,srtsub,cllet,prtmax
      integer*2   idum,istart
      integer*4   cjbnum,srtnum,ipnt
c
c...sub value should allow five digits
c...(change character*6 to character*7)
c...Yurong
c
      character*6 atype
      character*8 sub
      character*80 labstr
      character*10 lprim
      character*5 refsys
      real*8      rbuff(6)
      integer*2   nwds,ietype
      integer*4   ipg,iel,nclkey, isub
      logical     over,full,found,first,noalph,nomac, scalr
      logical     trflg, lgeo, all, sym_start
c
      character*64 ctemp,jblet,srlet
      character*11 numstr(4)
      character*1 csrtnm(64),cjblet(64),srtlet(64)
      equivalence (ctemp,csrtnm),(jblet,cjblet),(srlet,srtlet)
c 
      integer*2   pg,el,nw,type,pgeq(4)
      character*8 pgelnw
      real*8      paelnw
      real*8      scvalu
      equivalence (pgelnw,paelnw,scvalu,pgeq)
      equivalence (pg,pgeq(1)),(pgeq(2),el),(pgeq(3),nw)
      equivalence (pgeq(4),type)
c
      character*64 jbmask
      integer*4   cjbsub,jbmask33
      character*1 atjb
      equivalence (jbmask,atjb)
c
      character*5 ccn,ssn
      character*1 cnum(5),snum(5)
      equivalence (ccn,cnum),(ssn,snum)
c
      character*4 cpntp(2)
      data cpntp /'pt #','pv #'/
c
      integer*4 nc1,nc2, strlen1

      trflg=.true.
      ldtext = .true.
      ifl(44)=9
      call parser
      if ((ityp .eq.1 .and.
     x    (ist.eq.662 .or.
     x     ist .eq.663 .or.
     x     ist .eq.71 .or.
     x     ist .eq.72 ))) then

c                 its a print/large, small, on,in or off,in
          if (ist.eq.663) then
c                                    print/small
              ifl(154)=0
          else if (ist.eq.662) then
c                                    print/large
              ifl(154)=1
          else if (ist.eq.71) then
              call parser
              if (ist.eq.652) then
c                                    print/on,in
                  ifl(218)=0
              else
                  call error(379)
              endif
          else if (ist.eq.72) then
              call parser
              if (ist.eq.652) then
c                                    print/off,in
                  ifl(218)=1
              else
                  call error(379)
              endif
          endif
          if (nextyp.ne.11) call error(4)
          go to 99999
      endif
c

c...Added check for NCL-VT mode
c...Paul  -  10/3/91
c...Old version was:
c   if (ifl(35).eq.0) go to 99999
c
cccc  if (ifl(35).eq.0 .or. ifl(35) .eq. 2) go to 99999
c
c...Added check for NCL501+ mode
c...Paul  -  02/11/92
c...Old version was:
c   if (ifl(35).eq.0 .or. ifl(35) .eq. 2) go to 99999
c
      if (ifl(35).eq.0.or.(ifl(35).eq.2.and.ifl(350).eq.0)) goto 99999

      if (ityp.eq.3.and.itv.eq.0.and.nextyp.eq.11) then
c              it's a print/0 (form feed).  set prline to max.  epm 1-30-86
           prline = ifl(151)
           go to 99999
      endif

      first=.true.
      refsys=' '
      if (ifl(72).eq.1) refsys='rs/on'

c            check for geometry type or 'all'
c                                                'all' = 816
c       set a logical flage = all for 'all' ptype = 20 is now
c       used for patern  on: 3/31/88 by: kathy
c
      all = .false.
      if (ityp .eq. 1) then
          if (ist .eq. 816) then
              all = .true.
          else if (ist .eq. 601) then
              ptype = 2
          else
              call vctoid (ist,ptype,idum)
          end if
      else
          call error (203)
          go to 99999
      endif

c            check for 'full' option - future option
      full=.false.
c      if (nextyp.ne.11) then
c          call parsit
c                                                *** temp look for 'all'
c          if (ityp.eq.1 .and. ist.eq.816) then
c              full=.true.
              if (nextyp.ne.11) then
                  call error (4)
                  go to 99999
              endif
c          else
c              call error (204)
c              go to 99999
c          endif
c      endif

c            set up sort parameters
      max=200
      hinum=0
      do 110 i=1,max+1
          srtnam(i)=' '
          ssrtsub(i) = 0
          do 110 j=1,4
              isrtas(j)=0
110       continue

100   continue
      over=.false.
      sym_start = .false.
      call vxlfst
c            read the ranfil variable symbol table records
150   call vxlnxt (token2, isub, nclkey, nwds, ietype, ipg, iel)
      if (ietype.eq.1) then
          sym_start = .true.
c
c...include symbol too
c
          call symfst
          call symnxt (token2, isub, nclkey, ietype)
          if (ietype.eq.1) then
             goto 600
          endif
      endif

c *****************************************************************************
c            get names and sort if right geometry type
c *****************************************************************************
          ivxsub = isub
          jbmask = token2
          jbmask33 = ivxsub
c
c....remove the macro prefix
c
          i = strlen1 (jbmask)
          call rmlab_prefix(jbmask, jbmask, i)
c
c...fill the cout with space after token
c
          do 165 j=i+1,64
             jbmask(j:j) = ' '
165       continue
c            test if name is a dummy macro call entry or a nested
c            geometry name - ignore it if so
          nomac=.false.
          if (jbmask33.ne.1000000 .and. atjb.ne.'@' ) nomac=.true.

c              check if this is a scalar
          call ifidge (ietype,lgeo)

c            test if this entry is the type to be printed
          if (nomac .and.
     x         (ietype .eq. ptype .or.
     x         (all  .and. (ietype .eq. 2 .or. ietype.eq.TEXTVAR
     x         .or. lgeo)))) then

              found=.false.
c                  if this is the first name put it in the first table entry
              if (.not.(hinum.eq.0 .and. srtnam(1)(1:64).eq.'      '))
     x             goto 2099
                  j=1
                  srtnam(j)=jbmask
                  ssrtsub(j) = jbmask33
                  call ptdsc3 (nclkey,nwds,ietype,rrtasw(j))
                  if (hinum.le.max) hinum=hinum+1
                  go to 499
c
2099          continue

c                   separate current entry into alphabetic numeric and
c                    optionally subscript parts
              ctemp=jbmask

c                  alphabetic part
              noalph=.true.
              jblet = ' '
              nc1 = strlen1(ctemp)
              do 155 k=nc1,1,-1
                  if (csrtnm(k).ge.'A' .and. csrtnm(k).le.'Z') then
                      cjblet(k)=csrtnm(k)
c                          cllet = current name last letter position
c                                  used to determine numeric portion
c                                  of name
                      if (noalph) cllet=k
                      noalph=.false.
                  else if (.not. noalph) then
                      cjblet(k)=csrtnm(k)
                  else
                      cjblet(k)=' '
                  endif
155           continue

c                  numeric part
              kk=0
              ccn = ' '
              do 160 k=cllet+1,nc1
                  if (csrtnm(k).ge.'0' .and.
     1                csrtnm(k).le.'9') then
                      kk=kk+1
                      cnum(kk)=csrtnm(k)
                  endif
160           continue
              call chr2int (cnum(1),kk,cjbnum)
c
c       If there is no numeric field set the value to -1. kathy
c
              if (kk .eq. 0 .and. cjbnum .eq.0) cjbnum=-1

c                 get subscript value if any
              cjbsub=0
              if (jbmask33.gt.0) cjbsub=jbmask33

c ****************************************************************************
c                  if name is alphabetically lower shift all existing
c                  names down one and insert new name
c ****************************************************************************
              do 400 j=1,hinum

c                   separate sort table entry into alphabetic and numeric parts
                  ctemp=srtnam(j)
                  srlet = ' '
                  nc1 = strlen1(ctemp)

c                     alphabetic part
                  noalph=.true.
                  do 170 k=nc1,1,-1
                      if (csrtnm(k).ge.'A' .and. csrtnm(k).le.'Z') then
                          srtlet(k)=csrtnm(k)
                          if (noalph) cllet=k
                          noalph=.false.
                      else if (.not. noalph) then
                          srtlet(k)=csrtnm(k)
                      else
                          srtlet(k)=' '
                      endif
170               continue

c                     numeric part
                  kk=0
                  ssn = ' '
                  do 180 k=cllet+1,nc1
                      if (csrtnm(k).ge.'0' .and.
     1                    csrtnm(k).le.'9')then
                          kk=kk+1
                          snum(kk)=csrtnm(k)
                      endif
180               continue
                  call chr2int (snum(1),kk,srtnum)
c
c       If there is no numeric field set the value to -1. kathy
c
                  if (kk .eq. 0 .and. srtnum .eq.0) srtnum=-1

c                     pre-test for alphabetic-numeric sequence
c                      this test keeps the order from being done in a strict
c                      collating sequence.  for example the three entrees:
c                          pt1, pt10 and pt2
c                      will be printed in the order:
c                          pt1, pt2 and pt10
c                      instead of the order:
c                          pt1, pt10 and pt2

c                     test alphabetic parts
                  if (srlet.eq.jblet) then

c                         if alphabetic parts are equal test numeric parts
                      if (cjbnum.lt.srtnum) go to 190
                      if (cjbnum.gt.srtnum) go to 400
                      if (cjbnum.eq.srtnum .and. ccn.eq.ssn) then

c                              if numeric parts are equal test subscripts
                          srtsub=ssrtsub(j)
                          if (cjbsub.lt.srtsub) go to 190
                          if (cjbsub.gt.srtsub) go to 400
c                             this should only happen when a subscripted
c                             variable is the first on the array on a
c                             non-first pass
                          if (cjbsub.eq.srtsub) go to 500
                      endif
                  endif

                  if (.not.(jbmask.lt.srtnam(j))) go to 3099

c                            don't insert before first position if this
c                            is not the first pass because the first entry
c                            is the lowest allowable entry on non-first
c                            passes
190                   if (.not.first .and. j.eq.1) go to 500

c                         shift entities down one position
                      do 200 imove=hinum,j,-1
                          srtnam(imove+1)=srtnam(imove)
	                    ssrtsub(imove+1) = ssrtsub(imove)
                          srtasw(imove+1)=srtasw(imove)
200                   continue

c                         add new entry in vacated spot
300                   srtnam(j)=jbmask
	                ssrtsub(j) = jbmask33
                      call ptdsc3 (nclkey,nwds,ietype,rrtasw(j))
                      if (hinum.le.max) hinum=hinum+1
                      go to 499
3099              continue
400           continue

c                  if not found to be lower than any existing names put it at
c                  the bottom of the stack if there is room
              if (hinum.le.max) then
                  hinum=hinum+1
                  srtnam(hinum)=jbmask
                  ssrtsub(hinum) = jbmask33
                  call ptdsc3 (nclkey,nwds,ietype,rrtasw(hinum))
              endif
          endif

c             test if the sort array has been overflowed
499       if (hinum.gt.max) over=.true.
500   continue
c *****************************************************************************

c         go get next entity
      if (.not. sym_start) then
          go to 150
      else
          call symnxt (token2, isub, nclkey, ietype)
          if (ietype.eq.1) then
             goto 600
          endif
      endif
c *****************************************************************************
c *****************************************************************************

c            print canonical form of sorted geometry names
600   prtmax=hinum-1
      if (.not.over) prtmax=hinum
      do 700 i=1,prtmax
          pgelnw=srtasw(i)
c             identify type of geometry for printed output
          scalr = .false.
          if (type.eq.2) then
              atype='scalar'
              scalr = .true.
          else if (type.eq.TEXTVAR) then
              atype='text'
          else
              call idtopr (type,atype)
          endif

c             check for subscript and put it in sub if found
          jbmask=srtnam(i)
          jbmask33 = ssrtsub(i)
c...should allow 6 digit  999999
c...in sub value (change i5 to i6)
c...Yurong
          sub='        '
          if (jbmask33.gt.0) write (sub,1070) jbmask33
1070      format ('(',i6,')')
          nc1    = strlen1(srtnam(i)(1:64))
          if (nc1 .gt. 64) nc1 = 64
          write (labstr, 1071) srtnam(i)(1:nc1),sub
1071      format (a, a8)
          nc2 = strlen1(labstr)       
          do 120 j=nc2+1,14,1
             labstr(j:j) = ' '
  120     continue
          nc2 = strlen1(labstr)
          call ifidge (type,lgeo)
cjingrong
c          if (lgeo .and. type .ne. 8 .and. type .ne. 9 .or.
c     x        (type .eq. 8 .or. type .eq. 9) .and. full) then
          if (lgeo .and. type .ne. 8 .or. scalr .or.
     x        type .eq. 8 .and. full) then

c c c c c  warning: additional code must be put in to handle large amounts
c c c c c           of canonical data for curves and surfaces
c
c                     get canonical data for entity
                  if (type .ne. 20 .and. type .ne. VSOLID .and.
     1                    type .ne. VANOTE) then 
                      call gtentt(scvalu,trflg,nclkey,type,r8buf(1))
                  endif
c 
c......Get the length of the entity's label
c......Added support for long label names (VX)
c......Bobby  -  11/3/97
c              
              if (scalr) then
c                      print scalar value
                  call ul_format_data11(r8buf(1), numstr(1))
                  if (nc2 .le. 14) then
                      write (cout, 1010) labstr(1:14),atype, numstr(1)
                      call prtge1 (cout)
                  else
c
c...if label+subcript is longer than 14 chars, use the whole line
c
                      call prtge1 (labstr)
                      write (cout,1012) atype, numstr(1)
                      call prtge1 (cout)
                  endif
c
c...should allow 6 digit
c...in sub value (change a7 to a8)
c...Yurong
c
1010              format (a, 1x, a6, 5x, 5x, 1x, a11)
1012              format (15x, a6, 5x, 5x, 1x, a11)
                  istart = 4
                  go to 700
              endif

c                 print first three pieces of data for points, vectors & lines
              if (type.eq.3 .or. type.eq.4 .or. type.eq.5 .or.
     1            type .eq. 20 .or. type .eq. 21) then
c
                  call ul_format_data11(r8buf(1), numstr(1))
                  call ul_format_data11(r8buf(2), numstr(2))
                  call ul_format_data11(r8buf(3), numstr(3))
                  if (type .ne. 20) then
                       if (nc2 .le. 14) then
                           write (cout,1020) labstr(1:14),atype,
     1                          refsys, (numstr(k),k=1,3)
                           call prtge1 (cout)
                       else
c
c...if label+subcript is longer than 14 chars, use the whole line
c
                           call prtge1 (labstr)
                           write (cout,1019) atype,refsys,
     1                          (numstr(k),k=1,3)
                           call prtge1 (cout)
                       endif
1019                   format (15x,a6,5x,a5, 1x,a11, 1x, a11, 1x, a11)
1020                   format (a,1x,a6,5x,a5,1x,a11, 1x, a11, 1x, a11)
1011                   format (15x,a6,5x,5a,1x)
1021                   format (a,1x,a6,5x,a5,1x)
1022                   format (a4,i4,3x,a11, 1x, a11, 1x, a11)
1023                   format (32x,a4,i4,3x,a11, 1x, a11, 1x, a11)
                       istart=4
                  else if (type .eq. 20) then
                       call gtpnnp(paelnw,nw,ipnt)
                       do 655 ii = 1,nw
                          call gpnptt(rbuff,paelnw,ii,trflg)
                          call ul_format_data11(rbuff(1), numstr(1))
                          call ul_format_data11(rbuff(2), numstr(2))
                          call ul_format_data11(rbuff(3), numstr(3))
                          if (ii .eq. 1) then
                              if (nc2 .le. 14) then
                                  write (cout,1021) labstr(1:14),
     1                                 atype,refsys
                                  write (cout(34:),1022) cpntp(ipnt),ii,
     1                                    (numstr(k),k=1,3)
                                  call prtge1 (cout)
                              else
                                  call prtge1 (labstr)
                                  write (cout,1011) atype,refsys
                                  write (cout(34:),1022)
     1                                      cpntp(ipnt),ii,
     2                                      (numstr(k),k=1,3)
                                 call prtge1 (cout)
                              endif
                          else
                             write (cout,1023) cpntp(ipnt),ii,
     1                                    (numstr(k),k=1,3)
                             call prtge1 (cout)
                          endif
                          if (ipnt .eq. 2) then
                             call ul_format_data11(rbuff(4), numstr(1))
                             call ul_format_data11(rbuff(5), numstr(2))
                             call ul_format_data11(rbuff(6), numstr(3))
                             write (cout,1030) (numstr(k),k=1,3)
                             call prtge1 (cout)
                          end if
655                    continue
                  endif
cjingrong
              else if (type.eq.9) then            
                    call gtprimt (nclkey,ifl(72),ietype,r8buf)
                    if (ietype.eq.0 .or. ietype .eq. 1 .or.
     1                  ietype .eq. 7) then
                       call sftype (nclkey,ietype)
                       nw    = 0
                       if (ietype .eq. 91) then
                           lprim = 'surf'
                       else if (ietype .eq. 26) then
                           lprim = 'mesh-sf'
                       else if (ietype .eq. 25) then
                           lprim = 'quilt-sf'
                       else if (ietype .eq. 27) then
                           lprim = 'net-sf'
                       else if (ietype .eq. 28) then
                           lprim = 'eval-sf'
                       else if (ietype .eq. 29) then
                           lprim = 'nsurf'
                       else if (ietype .eq. 99) then
                           lprim = 'trim-sf'
                       else if (ietype .eq. 100) then
                           lprim = 'revolve-sf'
                       endif
                    else if (ietype.eq.2) then
                       lprim = 'ruled-sf'
                       nw    = 0
                    else if (ietype.eq.3) then
                       lprim = 'plane-sf'
                       nw    = 4
                    else if (ietype.eq.4) then
                       lprim = 'sphere-sf'
                       nw    = 4
                    else if (ietype.eq.5) then
                       lprim = 'cylindr-sf'
                       nw    = 8
                    else if (ietype.eq.6) then
                       lprim = 'cone-sf'
                       nw    = 9
                       r8buf(7) = r8buf(7) * RADIAN
                    else if (ietype.eq.7) then
                       lprim = 'revolve-sf'
                       nw    = 0
                    end if
                    atype = 'surf'
                    nc = nw
                    if (nc.gt.4) nc = 4
                    do 1111 k=1, nc
                        call ul_format_data11(r8buf(k), numstr(k))
1111                continue
                    if (nc2 .le. 14) then
                        write (cout,1027) labstr(1:14), lprim,
     1                          refsys,(numstr(k),k=1,nc)
                    else
                        call prtge1 (labstr)
                        write (cout,1028) lprim,
     1                          refsys,(numstr(k),k=1,nc)
                    endif
                    call prtge1 (cout)
1027                format (a,1x,a10,1x,a5,1x,a11,1x,a11,1x,a11,1x,a11)
1028                format (15x,a10,1x,a5,1x,a11,1x,a11,1x,a11,1x,a11)
                    istart=5
c
c...Annotation
c
              else if (type .eq. VANOTE) then
                  call gtdesc (pgelnw,nclkey,nwds,ietype)
                  call prinan (nclkey,labstr,nc2,refsys)
c
c...Solids
c
              else if (type .eq. VSOLID) then
                  call gtdesc (pgelnw,nclkey,nwds,ietype)
                  call prinso (nclkey,labstr,nc2,refsys)
              else if (type.eq.VSYMBOL) then
                  call prtsym (srtnam(i),sub, ssrtsub(i), refsys, 1)
              else if (type.eq.VPLACE) then
                  call prtsym (srtnam(i),sub, ssrtsub(i), refsys, 2)
             else
c                     print first four pieces of data for all other data types
                    do 1112 k=1, 4
                        call ul_format_data11(r8buf(k), numstr(k))
1112                continue
                  if (nc2 .le. 14) then
                      write (cout,1025) labstr(1:14),atype,refsys,
     1                                  (numstr(k),k=1,4)
                      call prtge1 (cout)
                  else
                      call prtge1 (labstr)
                      write (cout,1026) atype,
     1                                  refsys,(numstr(k),k=1,4)
                      call prtge1 (cout)
                  endif
1025              format (a,1x,a6,5x,a5,1x,a11,1x,a11,1x,a11,1x,a11)
1026              format (15x,a6,5x,a5,1x,a11,1x,a11,1x,a11,1x,a11)
                  istart=5
              endif
c                 print all remaining pieces of data four per line
              if (type .ne. 20 .and. type .ne. VSOLID .and.
     1                type .ne. VANOTE) then      
                  do 650 j=istart,nw,4
                     jj=j+3
                     if (jj.gt.nw) jj=nw
                     do 1113 k=j, jj
                        call ul_format_data11(r8buf(k), 
     1                          numstr(k - j + 1))
1113                 continue                  
                     nc = jj - j + 1
                     write (cout,1030) (numstr(k),k=1,nc)
1030                 format (32x,a11,1x,a11,1x,a11,1x,a11)
1031                 format (23x,a4,i4,1x,a11,1x,a11,1x,a11,1x,a11)
                     call prtge1 (cout)
650               continue
              endif
          else if (type.eq.TEXTVAR) then
              if (nc2 .le. 14) then
                  write (cout,1032) labstr(1:14), atype
              else
                  call prtge1 (labstr)
                  write (cout,1033) atype
              endif
1032          format (a, 1x, a6, 5x, 6x)
1033          format (15x, a6, 5x, 6x)
              call gtdesc(pgelnw,nclkey,nwds,ietype)
              cout(33:33) = '"'
              jj = 100
              call ncl_gttext(nclkey,cout(34:),jj)
              mxtext = 46
              if (ifl(106).gt.80) then
                  mxtext = ifl(106)-34
              endif
              is = ifl(387)
              if (is .eq. 0) is = ifl(106)
              if (is .lt. 80) is = 80
              if (jj .gt. mxtext) then
                  cout(is:is) = '*'
              endif          
              jj = jj+34
              cout(jj:jj) = '"'
              cout(is+1:)=' '
              call prtge1 (cout)
          else
c                 print name and type only for curves, surfaces & shapes
              if (nc2 .le. 14) then
                  write (cout,1040) labstr(1:14), atype,refsys
                  call prtge1 (cout)
              else
                  call prtge1 (labstr)
                  write (cout,1041) atype,refsys
                  call prtge1 (cout)
              endif
1040          format (a,1x,a6,1x,a5)
1041          format (15x,a6,1x,a5)
          endif

700   continue
c *****************************************************************************
c *****************************************************************************
c         check if all geometry has been sorted
      if (over) then
      do 800 i=1,max
          srtnam(i)=' '
          ssrtsub(i)= 0
          do 800 j=1,4
              isrtas(j)=0
800       continue

c             put overflow entry in position one of sort array
          srtnam(1)=srtnam(max+1)
          ssrtsub(1)=ssrtsub(max+1)
          srtasw(1)=srtasw(max+1)
          hinum=1
          first=.false.
          go to 100
      endif
99999 continue
      ldtext = .false.
      return
      end
c
      subroutine prtge1 (cbuf)
c
      include 'com.com'
      character*(*) cbuf
c
      integer*2 nc2,ie
      integer*4 nc,strlen1
c
      nc    = strlen1(cbuf)
      nc2   = nc
      ie = ifl(106)
      if (ie .lt. 80) ie = 80
      if (nc2 .gt. ie) then
          call putprt (cbuf,ie,1)
          call putprt (cbuf(ie+1:nc2),nc2-ie,1)
      else
          call putprt (cbuf,nc2,1)
      endif
      return
      end



