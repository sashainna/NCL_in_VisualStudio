C*********************************************************************
C*    NAME         :  savepp.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       savepp.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:38
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine savepp (name,nci,whr)
c*        writes a sequential part program file from the partpgm.ncl  
c*        file.                                                         
C*    PARAMETERS   
C*       INPUT  : 
c*        name = the name of the sequential file the part program    
c*               is to be saved as.  if name is blank the original   
c*               part program name is used.                          
c*
c*        nci  = Number of chars in 'name'.
c*
c*        whr  = 1 = This routine is called from *EDT.
C*       OUTPUT :  
C*          none
C*    RETURNS      : 0 = success, 1 = failure
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      function savepp (name,nci,whr)

      include 'com4a.com'

      integer*4 savepp,nci
      character*(*) name
      character*(MAX_LEN) buf
      character*1 lbuf(MAX_LEN),lfbuf(MAX_PATH)
      character*(MAX_PATH) fbuf,pfile
c      equivalence (buf,lbuf)

      integer*2 dlen, ioerr, whr, edt,tmp,irtyp
      integer*4 irec,nc,tpltlun
      data edt /1/
c
c...Initialize routine
c
      savepp = 0
      ioerr = 0
      fbuf = name(1:nci)
      if (fbuf .eq. ' ') fbuf = ppfnam
      tmp = MAX_PATH - 1
      i = min (dlen(fbuf, MAX_PATH), tmp)  + 1
      lfbuf(i)='{'
      call flname(5, fbuf, pfile)
      tpltlun = pltlun
c 
c...Open output file
c
      if (ifl(35) .eq. 1) then
          call flopnw(tpltlun, pfile, 'SEQUENTIAL', 'FORMATTED', 
     x                MAX_LEN, 'NULL', ioerr)
      else
c          open (unit = 7, file = pfile)
          call flopnm(tpltlun, pfile, 'SEQUENTIAL', 'FORMATTED', 
     x                MAX_LEN, 'NULL', ioerr)
      endif
      if(ioerr .ne. 0) goto 88888
c
c...Loop through the source file
c
      
      irec = 0
20    call getsrc(buf,nc,irec,1)
      if (srceof) goto 50
c
c......Don't save INCLUDEd files
c
      irtyp = IRCTYP - (IRCTYP/10*10)
      if (irtyp .eq. 1 .or. irtyp .eq. 3) go to 45
c
c......Store current line if *EDT
c
      if (whr .eq. edt .and. irec+1 .eq. nline) then
          buf = '<_N_>' // buf(1:nc)
          nc = nc + 5
c
c......Executing loop
c
      else if (whr .eq. edt .and. IRCTYP .ge. 20 .and. IRCTYP .lt. 30)
     1         then
          buf = '<_X_>' // buf(1:nc)
          nc = nc + 5
c
c......Executing Macro
c
      else if (whr .eq. edt .and. IRCTYP .ge. 30 .and. IRCTYP .lt. 40)
     1         then
          buf = '<_C_>' // buf(1:nc)
          nc = nc + 5
      endif
c
c......Write out line
c
 40    write (tpltlun, 1010) buf(1:nc)
 1010  format (a)
      
45    irec = irec + 1
      go to 20
c
c...Close output file
c
50    close (unit = tpltlun)

      go to 99999
c
c...Error saving file
c
88888 if (ioerr .eq. -30) then
         savepp = -30
      else
         savepp = 1
      endif
c
c...End of routine
c
99999 return
      end
