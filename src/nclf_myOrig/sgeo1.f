C*********************************************************************
C*    NAME         :  sgeo1.f
C*       CONTAINS:
C*			sgeo1
C*			parse_expr
C*			dist_expr
C*			isvocwd
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       sgeo1.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:42
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine sgeo1 (j)
c*       this routine displays the canonical form 
c*       of all geometry types except curves and surfaces              
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
      subroutine sgeo1 (j)

      include 'com8a.com'

c          j = line number for sgeo to start clearing screen from
      integer*2 i, j
      integer*4 len1, len2, len3
      character*80 scal
      character*65 descript
      character*21 classnm
      real*8 val

c                                                     *** scalar
      if (ist.eq.2) then
          write(cout,900) tv
c  900     format (8x, 'value:', f18.7)
  900     format (8x, 'value:', f23.7)
          call putmsg(cout,80,18,0)
c
c...added class and description
c
          call expnm2 (token2, ivxsub, i, scal)
          len1 = i
          call ncl_getscalar(scal, len1, classnm, len2, 
     1            descript, len3, val)

          if (len3.ne.0) then
              do 30 i=len3+1, 64, 1
                  descript (i:i) = ' '
   30         continue
c
c...here cut the decription to 58 chars in order to displayed in one line
c
              nc = ifl(387) - 1
              if (nc .lt. 0) nc = ifl(106)
              nc = nc - 22
              if (nc.gt.64) nc = 64
              write(cout,700) descript(1:nc)
  700         format (8x, 'Description: ', a) 
              call putmsg(cout,80,21,0)
          endif

          if (len2.ne.0) then
              if (classnm(1:7).ne.'Default') then 
                  do 20 i=len2+1, 20, 1
                      classnm (i:i) = ' '
   20             continue
                  write(cout,500) classnm
  500             format (8x, 'Class: ', a20)
                  call putmsg(cout,80,19,0)
              endif
          endif
          cout=' '
          call putmsg (cout,80,16,0)
      else if (ist.eq.3.or.ist.eq.4) then
c                                                     *** point/vector
          write (cout,1020)
1020      format (23x,'x',18x,'y',18x,'z')
          call putmsg (cout,80,17,0)
          write(cout,1025) sc(10),sc(11),sc(12)
1025      format (14x,3(f18.7,1x))
          call putmsg(cout,80,18,0)
      else if (ist.eq.5 .or. ist .eq. 21) then

c                                                     *** line
          write (cout,1020)
          call putmsg (cout,80,17,0)
          write(cout,1030) sc(10),sc(11),sc(12)
1030      format ('start point:  ',3(f18.7,1x))
          call putmsg(cout,80,18,0)
          write(cout,1040) sc(13),sc(14),sc(15)
1040      format ('delta values: ',3(f18.7,1x))
          call putmsg(cout,80,19,0)
          j=20
      else if (ist.eq.6) then

c                                                     *** plane
          write (cout,1050)
1050      format (10x,'x',18x,'y',18x,'z',16x,'offset')
          call putmsg (cout,80,17,0)
          write(cout,1070) sc(10),sc(11),sc(12),sc(13)
1070      format(1x,4(f18.7,1x))
          call putmsg(cout,80,18,0)
      else if (ist.eq.10) then

c                                                     *** matrix
          write (cout,1080)
1080      format (15x,'x(1)',15x,'y(1)',15x,'z(1)',11x,
     1            'translation')
          call putmsg (cout,80,17,0)
          write(cout,1090) sc(10),sc(11),sc(12),sc(13)
1090      format ('x(2):',4(f18.7,1x))
          call putmsg(cout,80,18,0)
          write(cout,1100) sc(14),sc(15),sc(16),sc(17)
1100      format ('y(2):',4(f18.7,1x))
          call putmsg(cout,80,19,0)
          write(cout,1110) sc(18),sc(19),sc(20),sc(21)
1110      format ('z(2):',4(f18.7,1x))
          call putmsg(cout,80,20,0)
          j=21

c                                                     *** circle
          else if (ist.eq.7) then
              write (cout,1020)
              call putmsg (cout,80,17,0)
              write(cout,851)sc(10),sc(11),sc(12)
851           format('center:       ',f18.7,1x,f18.7,1x,f18.7)
              call putmsg(cout,80,18,0)
              write(cout,852)sc(13),sc(14),sc(15)
852           format('axis:         ',f18.7,1x,f18.7,1x,f18.7)
              call putmsg(cout,80,19,0)
              write(cout,853)sc(16)
853           format('radius:       ',f18.7)
              call putmsg(cout,80,20,0)
              write(cout,854)sc(17),sc(18),sc(19),sc(20)
854           format('limit plane:  ',4(f14.7,1x))
              call putmsg(cout,80,21,0)
              j=22
      endif

      return
      end



C*********************************************************************
C*    E_SUBROUTINE     : parse_expr (value_str, value, stat)
c*       this routine pass the value string to see if it is valid scalar
c*       
C*    PARAMETERS   
C*       INPUT  : 
C*          value string to be check: 
C*       OUTPUT :  
C*          value: the expression value
C*			stat: -1: not valid expression
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine parse_expr (value_str, len, value, stat)

      include 'com8a.com'

      integer*4 stat,i,j,tmpnci
      character*(MAX_LEN) value_str, ucin, tmpcin
      integer*2     tmpinx, iflsv111, iflsv2, iflsv44
      real*8  value
      integer*4 len

      tmpcin = cin
      tmpnci = nccin
      tmpinx = inx
      iflsv111 = ifl(111)
      iflsv2 = ifl(2)
      iflsv44 = ifl(44)

      inx = 1
c
c... we have to remove the spaces in between because we use parse routine
c... to calculator, if there are spaces, it only pass to token before spaces
c
      j = 1
      do 40 i=1, len, 1 
         if (value_str(i:i).ne.' ') then
              cin(j:j) = value_str(i:i)
              j = j + 1
         endif
   40 continue
      nccin = j - 1
cc      do 50 i=len+1, 256, 1 
cc           cin(j:j) = ' '
cc   50 continue

      ifl(111) = 0
      ifl(2) = 0 
c
c...we eval express, so we set ifl(44) = 0
c
      ifl(44) = 0
      err = .false. 
      SIMCOM = .true.
      call convrt(cin,ucin,nccin)
      cin = ucin(1:nccin)
      call parsit
      SIMCOM = .false.
      if ((err).or. ((nextyp .ne. 11).and.(nextyp.ne.0))) then
          stat = -1
      else
c
c.....if it pass the identifier, it must be scalar
c
          if ((ityp.eq.2) .and. (ist.eq.2)) then
              stat = 0
c...number
          else if ((ityp.eq.3) .or. (ityp.eq.4)) then
              stat = 0
          else
              stat = -1
          endif
      endif
      cin = tmpcin
      nccin = tmpnci
      inx = tmpinx
      ifl(111) = iflsv111
      ifl(2) = iflsv2
      ifl(44) =  iflsv44
      value = tv

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : dist_expr (value_str, value, stat)
c*       this routine pass the dist value string and get the distant
c*       
C*    PARAMETERS   
C*       INPUT  : 
C*          value_str: (geo1, geo2) or (curve): 
C*       OUTPUT :  
C*          value: the dist value
C*			stat: -1: not valid dist
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine dist_expr (value_str, value, stat)

      include 'com8a.com'

      integer*4 stat,i,j
      character*(MAX_LEN) value_str, ucin, tmpcin
      integer*2     tmpinx, iflsv111, iflsv2, iflsv44
      real*8  value
      integer*4 len, strlen1,tmpnci

      tmpcin = cin
      tmpnci = nccin
      tmpinx = inx
      iflsv111 = ifl(111)
      iflsv2 = ifl(2)
      iflsv44 = ifl(44)

      inx = 1
c
c... we have to remove the spaces in between because we use parse routine
c... to calculator, if there are spaces, it only pass to token before spaces
c
      j = 1
      len = strlen1(value_str)
      do 40 i=1, len, 1 
         if (value_str(i:i).ne.' ') then
              cin(j:j) = value_str(i:i)
              j = j + 1
         endif
   40 continue
      nccin = j - 1

      ifl(111) = 0
      ifl(2) = 0 
      err = .false. 
      SIMCOM = .true.
      call convrt(cin,ucin,nccin)
      cin = ucin
c                  ifl(44)=9 sets the parser to ignore commas
      ifl(44)=9
      call dist
      SIMCOM = .false.
      if (err) then
          stat = -1
      else
          stat = 0
      endif
      cin = tmpcin
      nccin = tmpnci
      inx = tmpinx
      ifl(111) = iflsv111
      ifl(2) = iflsv2
      ifl(44) =  iflsv44
      value = tv

      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine isvocwd(str, flag)
c*       check contents in variable 'str' for a 
c*       match with vocabulary words                                    
C*    PARAMETERS   
C*       INPUT  : 
c*          str - contains the character string to be matched for  
c*                  against the vocabulary tables.                   
C*       OUTPUT :  
c*          flag - set to a value of 1 if found otherwise set to 0. 
c*                                                                   
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine isvocwd(str, len, flag)

      include 'com8a.com'
      include 'vaxvoc.com'
      integer*2 cur,inc

      character*6 token6

      integer*4 flag,len,strlen1
      character*256 str
      integer*4 i

      flag = 0

      do 50 i=1, len, 1 
           token6(i:i) = str(i:i)
   50 continue
      do 60 i=len+1, 6, 1 
           token6(i:i) = ' '
   60 continue
c
c...search vocabulary common
c
      cur=512
      inc=256
c.....
c.....In old version (with cur=256) inly 9 iter. was necessary.
c.....Now we need 10. Paul 08/10/92
c.....
      do 100,i=1,10
          len = strlen1 (token6)
          if (token6.lt.vvcnam(cur)) then
              cur=cur-inc
          else if (token6.gt.vvcnam(cur)) then
              cur=cur+inc
          else if (token6.eq.vvcnam(cur)) then
              flag=1
              go to 99999
          endif
          inc=inc/2
100   continue
99999 continue

      return
      end
