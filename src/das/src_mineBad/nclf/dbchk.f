c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       dbchk.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:49
c**
c*****************************************************
c**
c** copyright (c) 1981,1982 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: dbchk                                          **
c **                                                                  **
c **  purpose of subroutine: to check the variable symbol table of a  **
c **    data base file for the existence of the symbol passed in      **
c **    'token'                                                       **
c **                                                                  **
c **  input: variable name to be checked in common variable 'token'   **
c **  output: ist - subtype of variable                               **
c **          tv - token value (contents of 'b' record for variable)  **
c **                                                                  **
c **  if the variable name is found, the page and element numbers of  **
c **    where it is in the dbfile are returned in the calling         **
c **    parameter variables ipg and iel.                              **
c **  if the variable name is not found, the values returned in ipg   **
c **    and iel point to the next available spot in the dbfile and    **
c **    value of tv has no meaning and ist is set to 1.               **
c **                                                                  **
c **********************************************************************
c **********************************************************************
 
      subroutine dbchk (ipg,iel)
 
      include 'com8a.com'
      include 'comdb.com'
 
      integer*2 numnam,ksn(4),jpg,ijb8(144)
      integer*4 iword
      real*8 atoken,asn,jb8(36)
      equivalence (ksn,asn),(ijb8,jb8)
      equivalence (token2,atoken,iword)
 
c          set for unknown identifier
      ksn(4)=1
c          calculate the page number the token should be found in
c          from the first 4 characters of the token using the
c          formula:
c            page=remainder of calculation (iword-iword/5*5)*2+1
      ipg=(iword-iword/5*5)*2+1
100   call getdb (jb8,ipg)
c          if no entries on this page, exit
      if (ijb8(141).gt.0) go to 200
      iel=1
      ist=1
      goto 600
c          check token against names in db record
200   numnam=ijb8(141)
      do 300 i=1,numnam
          if (atoken.ne.jb8(i)) go to 300
c          match, get data from associated page
          iel=i
          jpg=ipg+1
          call getdb (jb8,jpg)
          asn=jb8(i)
          go to 500
300   continue
c          no match.  if page is not full set iel to next open spot
      iel=ijb8(141)+1
      ist=1
      if (numnam.lt.35) goto 600
c          page full and no match so check for continuation page
      if (ijb8(143).eq.0) go to 400
      ipg=ijb8(143)
      go to 100
 
c          page is full but not continued so get next 2 page pair
c          initialize 2 pages and update header record
400   ijb8(143)=wdbnpg
      ist=1
      call putdb (jb8,ipg)
c          initialize continuation pages
      do 450 i=1,143
450       ijb8(i)=0
      ipg=wdbnpg
      call putdb (jb8,ipg)
      jpg=ipg+1
      call putdb (jb8,jpg)
      iel=1
      wdbnpg=wdbnpg+2
      go to 600
c          put data from associated record in tv.
c          set ist from last 2 bytes of data.
500   ist=ksn(4)-ksn(4)/16*16
      if (ksn(4) .eq. 20) then
        ist =20 
      endif 
      if (ist.eq.0) ist=2
      tv=asn
 
600   return
      end
