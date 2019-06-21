C*********************************************************************
C*    NAME         :  vocsup.f
C*       CONTAINS:
C*    COPYRIGHT 1991 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       vocsup.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:54
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine vctoid (ivoc,iist,indx) 
C*      this routine converts vocabulary value (geometry types) to 
C*      corresponding ncl geometry type id number. 
C*      To add new entity change itn, ixt & ivn data statement at the first
C*      unused index.
C*    PARAMETERS   
C*       INPUT  : 
C*          ivoc   I*2  D1  -  vocabulary word value 
C*       OUTPUT :  
C*          iist   I*2  D1  -  ncl geometry item id 
C*          indx   I*2  D1  -  index in ifl() common block
C*                             associate with item
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine vctoid (ivoc,iist,indx) 
c
      integer*2 ivoc,iist,indx
c
      integer*2 ixt(20),itn(20),ivn(20)
c
      data ivn /603, 604, 605, 606, 607, 608, 609, 610, 602, 636,
     -          613, 614, 628, 402, 619, 615, 616, 877, 878, 123/
      data ixt /112, 113, 114, 115, 116, 117, 118, 119, 143, 277,
     -          309,  19,  18, 333, 339, 143, 373, 380, 381, 382/
      data itn /  3,   4,   5,   6,   7,   8,   9,  10,  18,  20,
     -           21,   9,   8,   7,   8,  18,  30,  31,  32,  33/
c
      do 220 i=1,20
          if (ivn(i) .ne. 0) then
              if (ivoc .eq. ivn(i)) go to 600
          else
              go to 500
          end if
  220 continue
  500 iist   = 0
      return
c
  600 iist   = itn(i)
      indx   = ixt(i)
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine idtovc (ivoc,iist,indx) 
C*      this routine converts ncl geometry type id to vocabulary value. 
C*      To add new entity change itn, ixt & ivn data statement at the first
C*      unused index.
C*    PARAMETERS   
C*       INPUT  : 
C*          iist  I*2   D1  -  ncl internal geometry type id. 
C*       OUTPUT :  
C*          ivoc  I*2   D1  -  vocabulary value corresponding to id
C*          indx  I*2   D1  -  ifl index used with associate geometry 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine idtovc (ivoc,iist,indx) 
c
      integer*2 ivoc,iist,indx
c
      integer*2 ixt(20),itn(20),ivn(20)
c
      data ivn /603, 604, 605, 606, 607, 608, 609, 610, 602, 636,
     -          613, 614, 628, 619, 616, 877, 123, 3*0/
      data ixt /112, 113, 114, 115, 116, 117, 118, 119, 143, 277,
     -          309,  19,  18, 339, 373, 380, 382, 3*999/
      data itn /  3,   4,   5,   6,   7,   8,   9,  10,  18,  20,
     -           21,   9,   8,   8,  30,  31,  33, 3*0/
c
      do 220 i=1,20
          if (itn(i) .ne. 0) then
              if (iist .eq. itn(i)) go to 600
          else
              go to 500
          end if
  220 continue
  500 ivoc   = 0
      return
c
  600 ivoc   = ivn(i)
      indx   = ixt(i)
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ifidge (iist,lgeo) 
C*      This routine checks if ncl id number is valid geometry type. 
C*    PARAMETERS   
C*       INPUT  : 
C*          iist 
C*       OUTPUT :  
C*          lgeo 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine ifidge (iist,lgeo) 
c
      integer*2 iist
      logical  lgeo
c
c...Would not work with NCL502 V9.0 on SGI
c...when logical assignment instead of IF ???
c...Bobby  -  9/3/97
c
      lgeo = .false.
      if (iist .eq. 18 .or. (iist .gt. 19 .and. iist .lt. 23) .or.
     1    (iist .gt. 2 .and. iist .lt. 11) .or.
     1    (iist .ge. 30 .and. iist .le. 33))
     2        lgeo = .true.
c
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine idtonm (iist,cnam) 
C*      This routine converts ncl id number to geometry type name. 
C*      To add new entity change itn & ctn data statement at the first
C*      unused index.
C*    PARAMETERS   
C*       INPUT  : 
C*          iist   I*2  D1  - ncl geometry id number 
C*       OUTPUT :  
C*          cnam   C*2  D1  - ncl geometry default name 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine idtonm (iist,cnam) 
c
      integer*2 iist
      character*2 cnam
c
      integer*2 i,itn(20)
      character*2 ctn(20)
c
      data itn /  3,   4,   5,   6,   7,   8,   9,  10,  18,  20,
     -           21,  30,  31,  33, 6*0/
      data ctn /'PT','VE','LN','PL','CI','CV','SF','MX','SH','PN',
     -          'PV','AN','SY','SO',6*'  '/
c
      do 220 i=1,20
          if (itn(i) .gt. 0) then
              if (iist .eq. itn(i)) go to 600
          else
              go to 500
          end if
  220 continue
c
  500 cnam   = '  '
      return
c
c...valid name
c
  600 cnam   = ctn(i)
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine idtopr (iist,cnam) 
C*      This routine converts ncl id number to geometry type name for
C*      print file.
C*      To add new entity change itn & ctn data statement at the first
C*      unused index.
C*    PARAMETERS   
C*       INPUT  : 
C*          iist   I*2  D1  - ncl geometry id number 
C*       OUTPUT :  
C*          cnam   C*6  D1  - ncl geometry default name 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine idtopr (iist,cnam) 
c
      integer*2 iist
      character*6 cnam
c
      integer*2 i,itn(20)
      character*6 ctn(20)
c
      data itn /  3,       4,       5,       6,       7,       8,
     -            9,      10,      18,      20,      21,      30,
     -           31,      32,      33,
     -          5*0/
      data ctn /'point ','vector','line  ','plane ','circle','curve ',
     -          'surf  ','matrix','shape ','patern','pntvec','anote ',
     -          'symbol','instnc','solid ',
     -        5*'      '/
c
      do 220 i=1,20
          if (itn(i) .gt. 0) then
              if (iist .eq. itn(i)) go to 600
          else
              go to 500
          end if
  220 continue
c
  500 cnam   = '  '
      return
c
c...valid name
c
  600 cnam   = ctn(i)
      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nmtoid (iist,cnam) 
C*      This routine converts geometry type name to ncl id number. 
C*      To add new entity change itn & ctn data statement at the first
C*      unused index.
C*    PARAMETERS   
C*       INPUT  : 
C*          cnam   C*64  D1  - ncl geometry default name 
C*       OUTPUT :  
C*          iist   I*2  D1  - ncl geometry id number 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine nmtoid (iist,cnam64) 
c
      integer*2 iist
      character*64 cnam64
      character*2 cnam
c
      integer*2 i,itn(20)
      character*2 ctn(20)
      integer*4 len, strlen1
c
      data itn /  3,   4,   5,   6,   7,   8,   9,  10,  18,  20,
     -           21,  30,  31,  7*0/
      data ctn /'PT','VE','LN','PL','CI','CV','SF','MX','SH','PN',
     -          'PV','AN','SY','SO', 6*'  '/
c

      iist   = 0 
      len = strlen1(cnam64)
      cnam = cnam64(1:2)
      if (len.gt.2) return
      do 220 i=1,20
          if (ctn(i) .ne. '  ') then
              if (cnam .eq. ctn(i)) go to 600
          else
              go to 500
          end if
  220 continue
c
  500 iist   = 0 
      return
c
c...valid name
c
  600 iist   = itn(i)
      return
      end
