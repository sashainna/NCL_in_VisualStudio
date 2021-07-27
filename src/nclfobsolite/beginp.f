C*********************************************************************
C*    NAME         :  beginp.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       beginp.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:38
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine beginp (irslt,it,isx)
C*       This subroutine scans the input line for the begining of a nested
C*       geometry item.  The valid beginning for a nested geometry item is a
C*       '(' followed by a vocabulary word specifying a geometry type, or a
C*       '(' followed by an identifier and '='.  For example:
C*  
C*              (line/
C*              (ln1=line/
C*              (a=1.23      % nested scalar assignment
C*  
C*       are both valid beginings for a nested geometry item.  If a valid
C*       begining for a nested geometry item is found, the argument irslt is
C*       set to a value of 1, otherwise a value of 0 is returned.
C*       The argument 'it' is set to 1 if the nested item is un-named, and
C*       set to 2 if its a named geo item, and set to 3 if its a scalar 
C*       assignment.  The argument 'isx' points to the '(' note that this 
C*       routine only searches the input line from inx to the end.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          irslt: 
C*          it: 
C*          isx: 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
 
      subroutine beginp (irslt,it,isx)

      include 'com8a.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      integer*2 irslt,it
      integer*2 nump,isx,is,nix,idum
      character*2 cynam 
c     character*6 types(10)
      logical lgeo
      integer*2 ipscnt
      equivalence (ipscnt,ifl(305))
c
c     DATA TYPES/'@SH   ','@PT   ','@VE   ','@LN   ','@PL   ','@CI   ',
c    1           '@CV   ','@SF   ','@MX   ','@PN   '/

      nump=0
      is=inx
c
c...Increased the input buffer size
c...Bobby  -  3/1/94
c
      do 100 i=inx,ifl(369)
      if (ain(i).eq.'(') then
          inx=i+1
          ifl(44)=0
          ldtext = .true.
          idtype = -1
          filtype = -1
          call parsit
          ldtext = .false.
          idum   = ist
          if (ityp .eq. 1) call vctoid (ist,idum,nix)
          call ifidge (idum,lgeo)
c
c...we added FILTER(geom_type, other_filt_attr)
c...so we need allow geo_type, such as POINT followed by a ',' or ')'
c
          if (ityp .eq. 1 .and. lgeo .and.
     x            ((nextyp.eq. 9).or.(nextyp.eq. 9))) then
              nump=nump+1
              goto 100
          endif
c
c...if (nextyp .eq. 7), then if will like "(string)", then
c...its a '(' that is not opening a nest
c      
	    if ((ityp .eq. 1 .and. lgeo .or. 
     1        ityp .eq. 2 .and. nextyp .eq. 1) .and.
     2        (nextyp .ne. 7) ) then
c                  its a geometry type or a variable symbol followed by '='
              if (ityp.eq.1) then
c                  its a geometry type (ln,pt,etc)

c                      Skip a slash "/" after the geometry type token
                  if (nextyp .eq. 5) then
                      inx = inx+1
                  else
                      call error(22)
                      go to 99999
                  endif

c                    save the first unnamed item for callit.  epm 10-3-84
                  if (ifl(192).eq.0) ifl(192)=isvinx
                  call vctoid (ist,idst,nix)
c
c..... Use @CV for ssplines and splines.
c
                  if (ist.eq.619 .or. ist.eq.628) nix = 117
                  isc10(1) = ist
c                    build and save the name
                  call idtonm (idst,cynam)
                  savid2 = '@' // cynam
c           if PSIS statement, use different name prefix
                  if (ipscnt.gt.0.and.(idst.eq.6.or.idst.eq.9)) then
                      savid2='@PS'
                      itemp=ipscnt
                  else
                      itemp = ifl(nix) + 1
                  endif
c
c......Allow for up to 99 nested definitions
c......Bobby  -  9/29/97
c
c                           set it to 1 so getnst can wrap around numbers epm 2-8-89
                  if (itemp.gt.99) itemp=1
c
c......Allow for up to 99 nested definitions
c......Bobby  -  9/29/97
c
c                  write(asave(4),10)itemp
                  write(savid2(4:5),10)itemp
10                format(i2.2)
                  ivxsub=0
                  isvsub=0
                  it=1
                  token2=savid2
                  call vstchk
                  keyold = keyhld
                  istold = ist
                  ifl(9)=ifl(11)
                  ifl(10)=ifl(12)
              else
c                    its a variable symbol followed by '='
                  if (ifl(41) .eq. 0 .and. ist .gt. 2) then
                      if (token2(1:1) .ne. '@') then
                          call error(8)
                          go to 99999
                      endif
                  endif
                  if (ist .lt. 2 .or. .not.lgeo .and. ist.gt.10) then
c                 if (ist.lt.2 .or. ist.gt.10.and.ist.ne.18
c    1                   .and.ist.ne.20 .and. ist.ne.21) then
                      if (ist.ne.1) then
                          call error(1)
                          go to 99999
                      endif
                  endif
                  isvsub = ivxsub
                  savid2=token2
                  keyold = keyhld
                  istold = ist
                  ifl(9)=ifl(11)
                  ifl(10)=ifl(12)
                  idst=ist
                  it=2
c                    skip over the '='
                  inx=inx+1
                  ifl(44)=0
                  call parsit
                  idum   = ist
                  if (ityp .eq. 1) call vctoid (ist,idum,nix)
                  call ifidge (idum,lgeo)
                  if (ityp .eq. 1 .and. lgeo) then
c                 if (ityp.eq.1.and.ist.gt.601.and.ist.lt.611
c    1                   .or.ist.eq.636 .or. ist.eq.613) then
c                        its a geometry type
                      if (nextyp.ne.5) then
                          call error(22)
                          go to 99999
                      else
                          inx=inx+1
                      endif
                      if (idum .ne. idst .and. idst.ne.1) then
                          call error(89)
                          go to 99999
                      endif
                      idst = idum
                      isc10(1) = ist
                  else if (ityp.eq.3.or.ityp.eq.4) then
c                             its a nested scalar assignment
                      it=3
                  else
                      call error(61)
                      go to 99999
                  endif
              endif
              irslt=1
              isx=i
              go to 99999
          else
c                its a '(' that is not opening a nest
              nump=nump+1
          endif
      else if (ain(i).eq.')') then
          if (nump.eq.0) then
c                 its an ending paren
              irslt=0
              inx=is
              go to 99999
          else
              nump=nump-1
          endif
      else if (ain(i).eq.' ') then
c              its the end of the statement
          go to 200
      endif
100   continue
c           scanned the whole line and found no '(' that started a nest
200   irslt=0
      inx=is

99999 continue
      return
      end
