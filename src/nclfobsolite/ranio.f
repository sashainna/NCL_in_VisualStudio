C*********************************************************************
C*    NAME         :  ranio.f
C*       CONTAINS:
C*                   getran
C*                   putran
C*                   ranopn
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ranio.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:34
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getran (jbuff,ipg)
c*       checks if virtual ranfil is in effect and   
c*       moves data to/from the virtual ranfil area in virtual region  
c*       if record number is less than 197                             
c*                                                                   
C*    PARAMETERS   
C*       INPUT  : 
c*          jbuff - integer2 buffer that record is     
c*                  read into                          
c*          ipg - integer2 value of the record number  
c*                to be read                           
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine getran (jbuff,ipg)

      include 'com8a.com'
      include 'nclrd.com'

      integer*2 jbuff(143),ipg
      integer*2 njbuff(144)
      integer*4 inc
      real*8 r8buff(36)
      character*8 c8buff(36)
      equivalence (r8buff, c8buff, njbuff)

c **********************************************************************
c     entry putran (jbuff,ipg) was here
c **********************************************************************
      jpg=ipg
c          move record from/to virtual ranfil in memory
      if (ipg.gt.1960) then
          jpg=ipg-1960
          go to 200
      endif
      inc=ipg
      inc=(inc-1)*144
      do 100 i=1,143
          jbuff(i)=iwndw(inc+i)
100   continue

      goto 99999

c              do physical i/o to disk
200   continue
      read (ranlun,rec=jpg) jbuff

99999 return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine putran (jbuff,ipg)
c*                                                                   
C*    PARAMETERS   
C*       INPUT  : 
c*          jbuff - integer2 buffer that record is     
c*                  read into                          
c*          ipg - integer2 value of the record number  
c*                to be read                           
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c **********************************************************************
c **********************************************************************
      subroutine putran (jbuff,ipg)

      include 'com8a.com'
      include 'nclrd.com'

      integer*2 jbuff(143),ipg
      integer*2 njbuff(144)
      integer*4 inc
      real*8 r8buff(36)
      character*8 c8buff(36)
      equivalence (r8buff, c8buff, njbuff)

      jpg=ipg
c          move record from/to virtual ranfil in memory
      if (ipg.gt.1960) then
          jpg=ipg-1960
          go to 200
      endif
      inc=ipg
      inc=(inc-1)*144
      do 100 i=1,143
          iwndw(inc+i)=jbuff(i)
100   continue

      goto 99999

c              do physical i/o to disk
200   continue
      if (ifl(71) .eq. 0) then
          call ranopn (ierr)
          if (ierr .ne. 0) then
              write (cout,120)
  120         format ('Could not open ranfil.rf file.')
              nc = 30
              call ferrmsg (cout,nc)
              quit = .true.
              go to 99999
          endif
          ifl(71) = 1
      endif
      write (ranlun,rec=jpg) jbuff

99999 return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ranopn
c*       Opens ranfil file named ranfil.rfx where
c*       x is from 1 to 99.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          ierr   = 1 = Could not open ranfile.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine ranopn (ierr)
 
      include 'com8a.com'
 
      integer*2 ierr
c
      character*40 temp
      integer numf, ipos, ioerr
      character*(MAX_PATH) ldir
      integer*4 nc,mxc
      character*(MAX_PATH) fname
 
      temp = 'ranfil.rf'
      numf = 1
      ierr = 0
      ipos = index (temp, ' ')
      if (numf .le.9) then
          write (temp(ipos:),1010) numf
1010      format (i1)
      else
          write (temp(ipos:),1020) numf
1020      format (i2)
      endif
      mxc = MAX_PATH
      call getdef(ldir,nc,mxc)
  100 fname = ldir(1:nc) // temp(1:)

      call flopen (ranlun, fname, 'NEW','DIRECT',
     x            'UNFORMATTED', 288, 'NULL', ioerr)
c
      if (ioerr .ne. 0 .and. numf .lt. 99) then
          numf = numf +1
          if (numf .le.9) then
              write (temp(ipos:),1010) numf
          else
              write (temp(ipos:),1020) numf
          endif
          goto 100
      endif
 
      if (ioerr .ne. 0) ierr = 1
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine rancls
c*       Closes the ranfil if it has been opened.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
      subroutine rancls
 
      include 'com8a.com'
 
      if (ifl(71) .eq. 1) close (unit=ranlun,status='delete')
      ifl(71) = 0
 
      return
      end
