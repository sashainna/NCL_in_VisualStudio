c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
C*        gettrm.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:08
c*****************************************************
C**
C** COPYRIGHT (C) 1981, 1982, 1985 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **  PROGRAM NAME: GETTRM                                            **
C **                                                                  **
C **  PURPOSE OF PROGRAM: GETS INPUT FROM TERMINAL AND/OR DATA TABLET **
C **                                                                  **
C **  *WARNING*  This routine has been changed to return 80 characters**
C **             instead of 72, because the loop marker 'X' in column **
C **             80 was not overwritten when getting the next line    **
C **             ter from the keyboard.   Bobby  -  6/27/91           **
C **********************************************************************
C **********************************************************************
 
      subroutine gettrm (in,knc)
 
      include 'com4a.com'
      include 'dtcom.com'
 
      character*1  in(MAX_LEN)
      character*(MAX_LEN+30) inp
      integer*4 knc, kmxc, klin, kcol, strlen1, tmpx, tmpy
      integer*2 tmp2
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c             this code needs to be updated to handle implied check
c             surface motion errors to display the proper line pointed
c             to by ifl(216).    epm   3-14-88
c
      if (ifl(215).eq.4) then
          ifl(215)=0  
      endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c
 2000 if (ifl(35) .eq. 2 .and. ifl(350) .le. 1) then
        if(ifl(141) .eq. 0) then 
          if(ifl(37) .ne. 2) then
             call getsrc (inp,knc,nline-1)
          else
             inp = ' '
             knc = 0
          endif
        else
          inp = cimage(1:nccimg)
          knc = ifl(141)
          ifl(141) = 0
        endif
        if (ifl(322).eq.1) then
          call vx_gettrm (nline, inp, in)
        else
          kmxc = MAX_LEN
          klin = 1
          kcol = 9
          call gvtlin(inp,knc,kmxc,klin,kcol)
          if (knc .eq. -3) goto 1000
          do  347 i =2,4,1
             klin = i
             kcol = 1
             call plot(klin,kcol)
             call clreol
347       continue

          do 345 i = 1,knc,1
          in(i) = inp(i:i)
345       continue
        endif
      endif
c
      if (ifl(35).ne.2 .or. (ifl(35).eq.0 .and. ifl(350).eq.2)) then
          call getlin (ifl(141), in)
          knc = ifl(141)
          ifl(141)=0
      endif

99999 return

1000  tmpy = 2   
      tmpx = 1
      tmp2 = ifl(253)
      ifl(253) = 0
      call plot(tmpy,tmpx)
      call clreos
      ifl(303) = 2
      call statln
      call ssrc(stat)
      ifl(253) = tmp2
      inp = cimage(1:nccimg)
      call domode

      write(cout,1010)nline
 1010 format (' ',i6,':  ')
      call putmsg(cout,8,1,0)

      go to 2000

      end
