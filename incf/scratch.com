c
c     MODULE NAME AND RELEASE LEVEL
c        scratch.com , 25.1
c     DATE AND TIME OF LAST MODIFICATION
c        04/29/15 , 15:07:33
c
c...ISCRNM(1) = Number of scratch files that can be opened at one time.
c...ISCRNM(2) = Number of records per scratch file that are resident in
c...            memory.
c...ISCRNM(3) = Record length in I*4's for scratch files.
c...
c...ISCRFL(*1)       = Logical unit numbers for scratch files.
c...ISCREF(*1)       = Last record used for each scratch file.
c...ISCREC(*3,*2,*1) = Scratch file record data stored in memory.
c...ISCRCH(*2,*1)    = 1 - Scratch file memory record has changed, it
c...                   needs to be written out.
c...
c...LSCRID(*1) = Identifying string of routine that allocated scratch
c...             file.  Used mainly for debug purposes.
c
      common /scrch / ISCRFL, ISCRNM, ISCREF, ISCREC, ISCRCH
c
      integer*4 ISCRFL(4),ISCRNM(3),ISCREF(4),ISCREC(128,4,4),
     1 ISCRCH(4,4)
c
      common /lscrch/ LSCRID
c
      character*8 LSCRID(4)
