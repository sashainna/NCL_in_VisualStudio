c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       uerr.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:50
c**
c*****************************************************
C**
C** COPYRIGHT (C) 1988 MILLS DATA SYSTEMS CORPORATION
C**
C **********************************************************************
C **********************************************************************
C **  PROGRAM NAME: uerror                                            **
C **                                                                  **
C **  PURPOSE OF PROGRAM:                                             **
C          Generate a warning if geometry label is used previously, 
C          and CANON is set of OFF and AUTO NAME is OFF too.
C **********************************************************************
C **********************************************************************
 
      subroutine uerror(label, isub)
 
      include 'com.com'
 
      character*64 label
      integer*4  isub,num,nc,strlen1
      character*20 char

c                     generat an error and open scrolling window. kathy
	  call error(-384)
c  
c...Added check for NCL-VT mode  
c...Paul  -  10/3/91  
c...Old version was:  
c   if (ifl(35).eq.0) then
c
      nc = strlen1 (label)
      if (ifl(35).eq.0 .or. ifl(35) .eq. 2) then
           if (isub .eq.0) then
              write (cout,10) label
10            format (5x,a<nc>,6x,'already defined -',
     x        ' not loaded *******')
           else
              call itoc (isub,char,num)
              write (cout,20) label,char
20            format (5x,a<nc>,'(',a<num>,')',6x, 'already defined -',
     x        ' not loaded *******')
           endif
           nc = strlen1(cout)
           call putmsg (cout, nc,15,0)  
      endif
 
 
99    return
      end
C **********************************************************************
C **********************************************************************
C **  PROGRAM NAME: uerror1                                           **
C **                                                                  **
C **  PURPOSE OF PROGRAM:                                             **
C          Generate a warning if geometry label is used previously, 
C          when generating fillets.
C **********************************************************************
C **********************************************************************
 
      subroutine uerror1(iern, label, isub)
 
      include 'com.com'
 
      character*64 label
      integer*2  i, isub, iern 
c
      integer*4  len, strlen1 
c
      character*5 ctemp
      character*1 ctemp1(5)
      equivalence (ctemp,ctemp1)
 
      len = strlen1(label)    
      ERRCOM(1:) = label
      if (isub.gt.0) then
          ERRCOM(len+1:len+1) = '('
          len = len + 2
          write(ctemp,1000) isub
1000      format(i5)
          do 120 i=1,5
             if (ctemp1(i).ne.' ') then
                ERRCOM(len:len)=ctemp1(i)
                len=len+1
             endif
120       continue
          ERRCOM(len:len) = ')'
      endif
      call error(iern)
c  
 
99    return
      end
C **********************************************************************
C **********************************************************************
C **  PROGRAM NAME: wintst                                            **
C **                                                                  **
C **  PURPOSE OF PROGRAM:                                             **
C          Test if scrolling window can be opened and if so test if
C          it is already opened, then open it.
C **********************************************************************
C **********************************************************************
 
      subroutine wintst
 
      include 'com.com'
c
      integer*2 status
 
      if (ifl(35).eq.0 .or. ifl(35) .eq. 2) then
          call winopn (status)
          if (status .eq. 0) call opnwin
      end if
      return
      end
