c
c***********************************************************************
c
c   FILE NAME: batchlic.f
c   CONTAINS:
c        mfbatchlic(batfile, flen, err)
c        batchlic(batfile, flen, err)
c        remspc (cstr1,cstr2,knc)
c        putbuf (krec, buf, flag)
c        clrrec()
c        mfopndat(msg, err)
c        setmotif()
c        setbatch()
c        setbatch()
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        batchlic.f , 23.1
c      DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:06
c
c***********************************************************************
c***********************************************************************
c  Subroutine : mfbatchlic(batfile, flen, err)
c
c  Purpose  : run batch file of nccs_license
C
c  Inputs   : batfile: batch file name
c             flen: len of batch file name
c
c  Outputs  : err: no error when run batch.
c
c  Returns  : none
c***********************************************************************
      subroutine mfbatchlic(batfile, flen, err)
c
      include 'menu.inc'
c
      byte batfile(MAX_PATH)
      integer*4 flen, err
      byte temp(MAX_PATH)
      character*(MAX_PATH) file
      integer*4 i
      equivalence (temp, file)

      do 10 i = 1, flen
   10     temp(i) = batfile(i)
      call reset_batch()
      call batchlic(file, flen, err)
      return
      end
c
c***************************************************************************
c  Subroutine : batchlic(batfile, flen, err)
c
c  Purpose  : run batch file of nccs_license
C
c  Inputs   : batfile: batch file name
c          flen: len of batch file name
c
c  Outputs  : err: no error when run batch.
c
c  Returns  : none
c****************************************************************************/
      subroutine batchlic(batfile, flen, err)
c
      include 'menu.inc'
      integer*4 flen, err
c
      character*(MAX_PATH) batfile
c
      byte bbuf(MAX_PATH)
      character*(MAX_PATH) cbuf
      equivalence (bbuf,cbuf)
      character*132 TEMPBUF(10,10000)
      byte btempbuf(132,10000,10)
      equivalence (tempbuf,btempbuf)
      character*132 title
      byte btitle(132)
      equivalence (title,btitle)
      character*200 dspmsg
      byte bdspmsg(200)
      equivalence (dspmsg,bdspmsg)
      character*80 msg, addbuf, repbuf, noaddbuf
      byte bmsg(80)
      equivalence (msg,bmsg)
      integer*4 i,j, krec, bufnum, addnc, repnc,noadd,noaddnc,tnc
      integer*4 strlen1, mlen, tlen, flag
      character*(MAX_PATH) cdev
      character*(MAX_FILE) cfil,cext
      character*1 el
      character*2 nl
      byte elb
      byte nlb(2)
      equivalence (elb, el)
      equivalence (nlb, nl)
c
      data elb /0/
      data nlb /13, 10/
c
c...init buf with empty space
c
      do 50 i = 1, 10000
          do 30 j = 1, 10
              TEMPBUF (j, i) = ' '
   30     continue
   50 continue
C WNT-SUN-SGI-IBM-HPX-VAX-START
      cbuf(1:flen) = batfile(1:flen)
      call fbreak (batfile,cdev,cfil,cext)
      call touppr (cext,cext)
c
c...Convert binary file to text file
c
      if (cext(1:4) .eq. '.LIC') then
          call get_licbuf(bbuf, flen, btempbuf, bufnum, err)
      else
          call get_batbuf(bbuf, flen, btempbuf, bufnum, err)
      endif
C WNT-SUN-SGI-IBM-HPX-VAX-END
      if (err.ne.0) goto 8000

      BATCHFILE = 1
      tnc = 0

      do 300 i = 1, bufnum
c
c...store record and added in
c
          krec = 1
          do 200 j=1,10,1
c
c...Move data to FRMBUF array, removing leading spaces.
c
             call rmleadsp(TEMPBUF(j, i), 132, FRMBUF(krec))
             krec = krec + 1
  200     continue
          call addrec (msg, err)
          if (err.ne.0) then
C WNT-START
              mlen = strlen1(msg)
              flag = 0
              call add1dispmsg(msg, mlen, flag)
C WNT-END
C SUN-SGI-IBM-HPX-VAX-START
C             mlen = strlen1(msg)
C             tlen = 20
C             title = 'NCCS_LICENSE Message' // char(0)
C             call lic_mfmessag(bmsg, mlen, btitle, tlen, BATCHFILE)
C SUN-SGI-IBM-HPX-VAX-END
              goto 400
          endif
  300 continue
  400 if (addnum.ne.0) then
          call itoc(addnum, addbuf, addnc, 1)
          dspmsg = addbuf(1:addnc)//' records added'//nl
          tnc = addnc+16
      endif
      if (repnum.ne.0) then
          call itoc(repnum, repbuf, repnc,1)
          dspmsg = dspmsg(1:tnc) // repbuf(1:repnc)
     1                   // ' records replaced'  // nl
          tnc = tnc + repnc + 19
      endif
      noadd = bufnum - addnum - repnum
      if (noadd .ne. 0) then
          call itoc(noadd, noaddbuf, noaddnc,1)
          dspmsg = dspmsg(1:tnc) // repbuf(1:noaddnc)
     1        //' records not added or replaced' // el
          tnc = tnc + noaddnc + 32
      endif
      title = 'NCCS_LICENSE Message'// el
C WNT-START
      flag = 0
      if (noadd.ne.bufnum) then
          call add1dispmsg(dspmsg, tnc, flag)
      else
          dspmsg = 'No record added or replaced'
          tnc = 27
          call add1dispmsg(dspmsg, tnc, flag)
      endif
C WNT-END
C SUN-SGI-IBM-HPX-VAX-START
C     tlen = 20
C     if (noadd.ne.bufnum) then
C         dspmsg = dspmsg(1:tnc) // el
C         mlen = tnc
C     else
C         dspmsg = 'No record added or replaced' // el
C         mlen = 27
C     endif
C     call lic_mfmessag(bdspmsg, mlen, btitle, tlen, BATCHFILE)
C SUN-SGI-IBM-HPX-VAX-END
 8000 BATCHFILE = 0
      return
      end
c***********************************************************************
c
c   SUBROUTINE:  remspc (cstr1,cstr2,knc)
c
c   FUNCTION:  This routine removes all of the spaces in a character
c              string, except for characters within quotes.
c
c   INPUT:  cstr1   C*n  D1  -  Original character string.
c
c   OUTPUT: cstr2   C*n  D1  -  Character string minus spaces.
c
c           knc     I*4  D1  -  Number of characters in 'cstr2'.
c
c***********************************************************************
c
      subroutine  rmleadsp(bufin,len, bufout)

      character*(*) bufin, bufout
      integer*4 len, num, i, len1
      num = 1
      do while(bufin(num:num).eq. ' ')
         num = num+1
      end do
      len1 = 1
      do 200 i=num, len, 1
         bufout(len1:len1) = bufin(i:i)
         len1=len1+1
  200 continue
      do 400 i=len1,len,1
         bufout(i:i) = ' '
  400 continue
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  putbuf (krec, buf, flag)
c
c   FUNCTION:  This routine  put a string in the global buffer
c
c   INPUT:  krec  : record number
c             bbuf:    string to save
c           flag: 1: store for add&delete
c                  2: store for search
c
c   OUTPUT:
c
c
c***********************************************************************
c
      subroutine  putbuf (krec, bbuf, flag)

      include 'menu.inc'
      byte bbuf(132)
      byte temp(132)
      character*132 buf
      integer*4 krec, flag
      integer*4 i
      equivalence (temp, buf)

      i = 1
      do while (i .ne. 133)
          temp(i) = bbuf(i)
          i = i+1
      end do
      call rmleadsp(buf, 132, buf)
c
c....convert to upper case
c
      call touppr(buf, buf)
      if (flag .eq. 1) then
c
c...save for add & delete
c
         FRMBUF(krec) =   buf
      else
c
c...save for search
c
         FRMBUF(krec+20) =   buf
      endif

      return
      end
c***********************************************************************
c
c   SUBROUTINE:  clrrec()
c
c   FUNCTION:  This routine resets
c              the search pointer to the beginning of the file.
c
c   INPUT:
c
c   OUTPUT:
c
c
c***********************************************************************
c
      subroutine clrrec()
      include 'menu.inc'
      integer*4 i
c
c...Clear form data
c
      do 100 i=1,10,1
          FRMBUF(i) = ' '
          SAPNC(i) = 0
  100 continue

c
c...Initialize search pointers
c
      SMREC(1) = IERRDS(2)
      SMREC(2) = IERRDS(2)
      SMREC(3) = 0
      SMREC(4) = 0
      return
      end


c***********************************************************************
c
c   SUBROUTINE:  mfopndat(msg, err)
c
c   FUNCTION:  This routine  put a string in the global buffer
c
c   INPUT:
c
c
c   OUTPUT:
c
c
c***********************************************************************
c
      subroutine mfopndat(bmsg, err)
c
      byte bmsg(200)
      byte temp(200)
      character*200 cmsg
      integer*4 err, i
      equivalence (temp, cmsg)

      i = 1
      do while (i .ne. 200)
          temp(i) = bmsg(i)
          i = i+1
      end do
      call opndat(cmsg, err)
      i = 1
      do while (i .ne. 200)
          bmsg(i) = temp(i)
          i = i+1
      end do

      return
      end
c***********************************************************************
c
c   SUBROUTINE:  setmotif()
c
c   FUNCTION:  This routine set MOTIF = 1
c
c   INPUT:
c
c   OUTPUT:
c
c
c***********************************************************************
c
      subroutine setmotif()
      include 'menu.inc'
c
      MOTIF = 1
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  setbatch()
c
c   FUNCTION:  This routine set BATCHFILE = 1
c
c   INPUT:
c
c   OUTPUT:
c
c
c***********************************************************************
c
      subroutine setbatch()
c
      include 'menu.inc'
c
      BATCHFILE = 1
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  setbatch()
c
c   FUNCTION:  This routine set BATCHFILE = 1
c
c   INPUT:
c
c   OUTPUT:
c
c
c***********************************************************************
c
      subroutine reset_batch()

      include 'menu.inc'
c
      BATCHFILE = 1
      addnum = 0
      repnum = 0
      return
      end

