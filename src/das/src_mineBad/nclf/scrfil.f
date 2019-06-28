c
c***********************************************************************
c
c   FILE NAME: scrfil.f
c   CONTAINS:
c               scrini  scrfin  scrall  scrdea  scrlod  scrsto  scrrd
c               scrpt
c
c     MODULE NAME AND RELEASE LEVEL
c        scrfil.f , 25.1
c     DATE AND TIME OF LAST MODIFICATION
c        04/29/15 , 15:10:39
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  scrini
c
c   FUNCTION:  This routine initializes the scratch file handler and
c              should be called whenever a new part is started.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine scrini
c
      include 'com8a.com'
      include 'scratch.com'
      common/rrs1/ idf1, idr1, msz1, dwa1, dwc1, dws1
      integer*4 idf1, idr1, msz1, dwa1, dwc1, dws1
c
c...Assign logical unit numbers
c...for scratch files
c
      idf1 = 1

      ISCRFL(1) = iscrln(1) * (-1)
      ISCRFL(2) = iscrln(2) * (-1)
      ISCRFL(3) = iscrln(3) * (-1)
      ISCRFL(4) = iscrln(4) * (-1)
c
c...Assign local scratch file limits
c
      ISCRNM(1) = 4
      ISCRNM(2) = 4
      ISCRNM(3) = 128
c
c...End of routine
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  scrfin
c
c   FUNCTION:  This routine deallocates and closes all of the scratch
c              files currently allocated.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine scrfin
c
      include 'scratch.com'
c
      integer*4 i
c
c...Close all open scratch files
c
      do 100 i=1,ISCRNM(1),1
          call scrdea (i)
  100 continue
c
c...End of routine
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  scrall (kfilp,kerr)
c
c   FUNCTION:  This routine allocates and opens a scratch file for
c              general usage.
c
c   INPUT:  none.
c
c   OUTPUT: kfilp   I*4  D1    -  Index of scratch file allocated.  Used
c                                 for input to 'scrlod' & 'scrsto'.
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
      subroutine scrall (kfilp,kerr)

      include 'scratch.com'

      integer*4 kfilp,kerr
      integer*4 i
c
c...Find next scratch file to allocate
c
      kerr   = 0
      do 100 i=1,ISCRNM(1),1
          if (ISCRFL(i) .lt. 0) go to 150
  100 continue
      kerr = 1
      go to 8000
c
c...Open and allocate scratch file
c
  150 ISCRFL(i) = ISCRFL(i) * (-1)

      kfilp  = i
c     write (filn,10) kfilp
c  10 format ('nclscr',i1,'.tmp')
c     lscun = iscrfl(kfilp)
c     lscrl = ISCRNM(3) * 4
c     call flopen (lscun,filn,'SCRATCH','DIRECT',
c    1             'UNFORMATTED',lscrl,'ZERO',ler)
      call scropn (kfilp,ISCRNM(3),kerr)
      if (kerr .ne. 0) go to 8000
c
c...Initialize file
c
      ISCREF(kfilp) = 0
      do 200 i=1,ISCRNM(2),1
          ISCREC(1,i,kfilp) = 0
          ISCREC(2,i,kfilp) = 0
          ISCRCH(i,kfilp) = 0
  200 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  scrdea (kfilp)
c
c   FUNCTION:  This routine deallocates and closes a single general
c              usage scratch file.
c
c   INPUT:  kfilp   I*4  D1    -  Index of scratch file to deallocate.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine scrdea (kfilp)
 
      include 'scratch.com'
 
      integer*4 kfilp
c
c...Close & deallocate scratch file
c
      if (ISCRFL(kfilp) .gt. 0) then
c         close (ISCRFL(kfilp),status='DELETE')
          call scrfre(kfilp)
          ISCRFL(kfilp) = ISCRFL(kfilp) * (-1)
      endif
c
c...End of routine
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  scrlod (kfilp,kdat,kst,kcnt,kbyt,kerr)
c
c   FUNCTION:  This routine loads the requested portion of a general
c              usage scratch file.  The starting position and number of
c              values requested must be a multiple of 4 bytes.
c
c   INPUT:  kfilp   I*4  D1    -  Index of scratch file to load data
c                                 from.
c
c           kst     I*4  D1    -  Starting location within scratch file
c                                 of data.
c
c           kcnt    I*4  D1    -  Number of array items to load from
c                                 scratch file.
c
c           kbyt    I*4  D1    -  Specifies the number of bytes per
c                                 array item.  For example, specify 4
c                                 for I*4 arrays or 8 for R*8 arrays.
c
c   OUTPUT: kdat    t*n  Dn    -  Array to receive data.  Can be either
c                                 integer or real.
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
      subroutine scrlod (kfilp,kdat,kst,kcnt,kbyt,kerr)
 
      include 'scratch.com'
 
      integer*4 kfilp,kdat(*),kst,kcnt,kbyt,kerr
 
      integer*4 nwrd,irec,ipt,ipta,i
c
c...Calculate starting record # &
c...array pointer for data
c
      kerr   = 0
      call scrpt (kst,kcnt,kbyt,irec,ipt,nwrd,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Read 1st record
c
      call scrrd (kfilp,irec,ipta,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Load requested data 
c
      do 100 i=1,nwrd,1
c
c......Data overlaps multiple records
c......Read in next record
c
          if (ipt .gt. ISCRNM(3)) then
              irec   = irec   + 1
              ipt    = 3
              call scrrd (kfilp,irec,ipta,kerr)
              if (kerr .ne. 0) go to 8000
          endif
c
c......Transfer data
c
          kdat(i) = ISCREC(ipt,ipta,kfilp)
          ipt    = ipt    + 1
  100 continue
c
c...End of routine
c
 8000 kerr = kerr
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  scrsto (kfilp,kdat,kst,kcnt,kbyt,kerr)
c
c   FUNCTION:  This routine stores the input data into the requested
c              portion of a general usage scratch file.  The starting
c              position and number of values to store must be a multiple
c              of 4 bytes.
c
c   INPUT:  kfilp   I*4  D1    -  Index of scratch file to store data
c                                 in.
c
c           kdat    t*n  Dn    -  Array containing data to store data.
c                                 Can be either integer or real.
c
c           kst     I*4  D1    -  Starting location within scratch file
c                                 of data.
c
c           kcnt    I*4  D1    -  Number of array items to store in
c                                 scratch file.
c
c           kbyt    I*4  D1    -  Specifies the number of bytes per
c                                 array item.  For example, specify 4
c                                 for I*4 arrays or 8 for R*8 arrays.
c
c   OUTPUT: kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
      subroutine scrsto (kfilp,kdat,kst,kcnt,kbyt,kerr)
 
      include 'scratch.com'
 
      integer*4 kfilp,kdat(*),kst,kcnt,kbyt,kerr
 
      integer*4 nwrd,irec,ipt,ipta,i
c
c...Calculate starting record # &
c...array pointer for data
c
      kerr   = 0
      call scrpt (kst,kcnt,kbyt,irec,ipt,nwrd,kerr)
      if (kerr .ne. 0) return
c
c...Read 1st record
c
      call scrrd (kfilp,irec,ipta,kerr)
      if (kerr .ne. 0) return
      ISCRCH(ipta,kfilp) = 1
c
c...Load requested data 
c
      do 100 i=1,nwrd,1
c
c......Data overlaps multiple records
c......Read in next record
c
          if (ipt .gt. ISCRNM(3)) then
              irec   = irec   + 1
              ipt    = 3
              call scrrd (kfilp,irec,ipta,kerr)
              if (kerr .ne. 0) return
              ISCRCH(ipta,kfilp) = 1
          endif
c
c......Transfer data
c
          ISCREC(ipt,ipta,kfilp) = kdat(i)
          ipt    = ipt    + 1
  100 continue
c
c...End of routine
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  scrrd (kfilp,krec,kpt,kerr)
c
c   FUNCTION:  This routine loads a specific scratch file record into
c              memory.  It should not be called straight from the
c              general public, but rather is used by the 'scrlod' and
c              'scrsto' routines.
c
c   INPUT:  kfilp   I*4  D1    -  Index of scratch file to load data
c                                 from.
c
c           krec    I*4  D1    -  Record number to read.
c
c   OUTPUT: kpt     I*4  D1    -  Pointer within 'ISCREC(n,kpt,kfilp)'
c                                 for memory storage array that con-
c                                 tains the loaded record.
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
      subroutine scrrd (kfilp,krec,kpt,kerr)
c
      include 'scratch.com'
c
      integer*4 kfilp,krec,kpt,kerr
c
      integer*4 i
c
c... debug values
c
c      integer*4 isze
c      parameter (isze = 128*4*4)
c      integer*4 buf(isze)
c      integer*4 lusc, lrsc
c      equivalence (buf(1),iscrec(1,1,1))

c
c...Determine if record is
c...currently in memory
c
      do 100 i=1,ISCRNM(2),1
          if (krec .eq. ISCREC(1,i,kfilp)) then
              kpt    = i
              ISCREC(2,kpt,kfilp) = ISCREC(2,kpt,kfilp) + 1
              return
          else if (ISCREC(1,i,kfilp) .eq. 0) then
              kpt    = i
              go to 500
          endif
  100 continue
c
c...Current record is not located
c...in memory and all records are
c...currently in use
c
c......Determine which record to use
c
      kpt    = 1
      do 200 i=1,ISCRNM(2),1
          if (ISCREC(2,i,kfilp) .lt. ISCREC(2,kpt,kfilp)) kpt = i
  200 continue
c
c......Write out this record
c
      if (ISCRCH(kpt,kfilp) .eq. 1) then
c
c... debug values lusc,lrsc,j,buf
c
c         lusc = iscrfl(kfilp)
c         lrsc = iscrec(1,kpt,kfilp)
c         j = iscrnm(3)*(kpt-1)+iscrnm(3)*iscrnm(2)*(kfilp-1)+1
c        write (unit=iscrfl(kfilp), rec=iscrec(1,kpt,kfilp),
c    x      iostat=kerr, err=9000)
c    x         (iscrec(j,kpt,kfilp),j=1,iscrnm(3))
         call scrput (kfilp,iscrec(1,kpt,kfilp),kerr)
         if (kerr .gt. 0) return
      endif
c
c......Requested record does not exist
c......So create it
c
  500 if (krec .gt. ISCREF(kfilp)) then
          do 600 i=1,ISCRNM(3),1
              ISCREC(i,kpt,kfilp) = 0
  600     continue
c
c.........Create intermediate records
c
          if (ISCREF(kfilp)+1 .lt. krec) then
              do 650 i=ISCREF(kfilp)+1,krec-1,1
                  ISCREC(1,kpt,kfilp) = i
c
c... debug values lusc,lrsc,j,buf
c
c                 lusc = iscrfl(kfilp)
c                 lrsc = iscrec(1,kpt,kfilp)
c                 j = iscrnm(3)*(kpt-1)+
c     x                  iscrnm(3)*iscrnm(2)*(kfilp-1)+1
c                write (unit=iscrfl(kfilp), rec=iscrec(1,kpt,kfilp),
c    x              iostat=kerr, err=9000)
c    x                 (iscrec(j,kpt,kfilp),j=1,iscrnm(3))
                 call scrput (iscrec(1,kpt,kfilp),kerr)
                 if (kerr .gt. 0) return
  650         continue
          endif
          ISCREC(1,kpt,kfilp) = krec
          ISCREC(2,kpt,kfilp) = 1
          ISCRCH(kpt,kfilp) = 1
          ISCREF(kfilp) = krec
c
c......Read in data
c
      else
c
c... debug values lusc,lrsc,j,buf
c
c                 lusc = iscrfl(kfilp)
c                 lrsc = krec
c                 j = iscrnm(3)*(kpt-1)+
c     x                  iscrnm(3)*iscrnm(2)*(kfilp-1)+1
c                read (unit=iscrfl(kfilp), rec=krec, iostat=kerr,
c    x              err=9000) (iscrec(j,kpt,kfilp),j=1,iscrnm(3))
                 call scrget (kfilp,krec,iscrec(1,kpt,kfilp),kerr)
                 if (kerr .gt. 0) return
          ISCREC(2,kpt,kfilp) = ISCREC(2,kpt,kfilp) + 1
          ISCRCH(kpt,kfilp) = 0
      endif
c
c...End of routine
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  scrpt (kst,kcnt,kbyt,krec,kpt,kwrd,kerr)
c
c   FUNCTION:  This routine calculates the following data based on the
c              number of bytes per item, starting location for data and
c              number of items to transfer.
c
c                    1) Record within scratch file that contains data.
c                    2) Starting location of data within record.
c                    3) Number of 4 byte items to transfer.
c
c              It should not be called straight from the general public,
c              but rather is used by the 'scrlod' and 'scrsto' routines.
c
c   INPUT:  kst     I*4  D1    -  Starting location of data within
c                                 scratch file.
c
c           kcnt    I*4  D1    -  Number of array items to store in
c                                 scratch file.
c
c           kbyt    I*4  D1    -  Specifies the number of bytes per
c                                 array item.  For example, specify 4
c                                 for I*4 arrays or 8 for R*8 arrays.
c
c   OUTPUT: krec    I*4  D1    -  Record within scratch file that con-
c                                 tains data.
c
c           kpt     I*4  D1    -  Starting location of data within
c                                 record.
c
c           kwrd    I*4  D1    -  Number of 4 byte items to transfer.
c
c           kerr    I*4  D1    -  Returns non-zero when an error occur-
c                                 red.
c
c***********************************************************************
c
      subroutine scrpt (kst,kcnt,kbyt,krec,kpt,kwrd,kerr)
c
      include 'scratch.com'
c
      integer*4 kst,kbyt,kcnt,krec,kpt,kwrd,kerr
c
      integer*4 inc,ist,inum
c
      real*4 rn1,rn2
c
c...Number of bytes must be multiple of 4
c
      kerr   = 0
c
c...Added inum for RS6000 port
c...Bobby  -  12/10/91
c
      inum   = 4
      inc    = mod(kbyt*kcnt,inum)
      if (kbyt .eq. 0 .or. inc .ne. 0) go to 9000
      inc    = mod(kbyt*kst,inum)
      if (inc .ne. 0) go to 9000
c
c...Calculate number of B*4's to xfer
c
      kwrd   = kbyt*kcnt/inum
c
c...Put starting pointer in B*4 base
c
      rn1    = kbyt
      rn2    = kst - 1
      ist    = rn2*rn1/4. + 1
c
c...Calculate record and B*4 within record
c...to start xfer
c
      inc    = ISCRNM(3) - 2
      krec   = (ist-1)/inc + 1
      kpt    = ist - (krec-1)*inc + 2
c
c...End of routine
c
 8000 return
c
c...Byte count must be multiple of 4
c
 9000 kerr   = 1
      go to 8000
      end
