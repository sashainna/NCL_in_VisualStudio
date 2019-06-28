c
c***********************************************************************
c
c   FILE NAME: licmisc.f
c   CONTAINS:
c               autini
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        licmisc.f , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:08
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  autini
c
c   FUNCTION:  This routine initializes the Software License data base
c              manager.
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine autini
c
      include 'menu.inc'
c
      integer*4 i
c
C VAX-START
C     LDEVIC = 'NCCS$LICENSE'
C VAX-END
C SUN-SGI-CIM-IBM-HPX-DOS-WNT-START
      LDEVIC = 'NCCS_LICENSE'
C SUN-SGI-CIM-IBM-HPX-DOS-WNT-END
c
c...Initialize form
c
      NLEVL  = 2
      FRMREC = 9
      IPRLIN = 22
c
      IPRMDS(1) = 4
      IPRMDS(2) = 6
      IPRMDS(3) = 8
      IPRMDS(4) = 10
      IPRMDS(5) = 12
      IPRMDS(6) = 14
      IPRMDS(7) = 16
      IPRMDS(8) = 18
      IPRMDS(9) = 20
c
      FRMST(1) = 10
      FRMST(2) = 11
      FRMST(3) = 11
      FRMST(4) = 10
      FRMST(5) = 18
      FRMST(6) = 22
      FRMST(7) = 10
      FRMST(8) = 12
      FRMST(9) = 11
c
      FRMEN(1) = 59
      FRMEN(2) = 30
      FRMEN(3) = 30
      FRMEN(4) = 69
      FRMEN(5) = 19
      FRMEN(6) = 32
      FRMEN(7) = 20
      FRMEN(8) = 23
      FRMEN(9) = 29
c
c...Initialize selection fields
c
      do 400 i=1,9,1
          MLEVL(i) = 0
          FRMBUF(i+20) = ' '
          SAPNC(i+20) = 0
  400 continue
c
c...Initialze prompts
c
      SAPRM(1) = 'Data base file does not exist.  Do you want to ' //
     1           'create it?'
      SAPRM(2) = 'Record already exists.  Do you want to replace it?'
      SAPRM(3) = 'Are you sure you want to delete this record?'
      SAPRM(4) = 'Are you sure you want to purge the data base?'
      SAPRM(5) = 'Hit return to continue.'
c
c...Initialize messages
c
      SAERR(1) = '*ERROR*  Invalid company name.'
      SAERR(2) = '*ERROR*  Invalid hardware device.'
      SAERR(3) = '*ERROR*  Invalid software name.'
      SAERR(4) = '*ERROR*  Invalid option list.'
      SAERR(5) = '*ERROR*  Invalid number of users.'
      SAERR(6) = '*ERROR*  Invalid termination date.'
      SAERR(7) = '*ERROR*  Invalid version date.'
      SAERR(8) = '*ERROR*  Invalid system ID.'
      SAERR(9) = '*ERROR*  Invalid password.'
      SAERR(21) = 'Record was not added to data base.'
      SAERR(22) = 'New company has been added.'
      SAERR(23) = 'New software product has been added.'
      SAERR(24) = 'Software product was replaced.'
      SAERR(25) = 'No match found.'
      SAERR(26) = 'Record successfully deleted.'
      SAERR(27) = 'Company successfully deleted.'
      SAERR(28) = 'Data base successfully purged.'
c
c...End of routine
c
 8000 return
      end
c***********************************************************************
c
c   SUBROUTINE:  na_autini
c
c   FUNCTION:  This routine initializes the Customer License data base
c              manager.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine na_autini
c
      include 'menu.inc'
c
      integer*4 i
c
c...Initialize form
c
      NLEVL  = 1
      FRMREC = 8
      IPRLIN = 22
c
      IPRMDS(1) = 4
      IPRMDS(2) = 6
      IPRMDS(3) = 8
      IPRMDS(4) = 10
      IPRMDS(5) = 12
      IPRMDS(6) = 14
      IPRMDS(7) = 16
      IPRMDS(8) = 18
      IPRMDS(9) = 20
      IPRMDS(10) = 4
c
      FRMST(1) = 10
      FRMST(2) = 11
      FRMST(3) = 11
      FRMST(4) = 10
      FRMST(5) = 18
      FRMST(6) = 22
      FRMST(7) = 10
      FRMST(8) = 12
      FRMST(9) = 11
      FRMST(10) = 64
c
      FRMEN(1) = 49
      FRMEN(2) = 30
      FRMEN(3) = 30
      FRMEN(4) = 69
      FRMEN(5) = 19
      FRMEN(6) = 32
      FRMEN(7) = 20
      FRMEN(8) = 23
      FRMEN(9) = 29
      FRMEN(10) = 71
c
c...Initialize selection fields
c
      do 400 i=1,10,1
          MLEVL(i) = 0
          FRMBUF(i+20) = ' '
          SAPNC(i+20) = 0
  400 continue
c
c...Initialze prompts
c
      SAPRM(1) = 'Data base file does not exist.  Do you want to ' //
     1           'create it?'
      SAPRM(2) = 'Record already exists.  Do you want to replace it?'
      SAPRM(3) = 'Are you sure you want to delete this record?'
      SAPRM(4) = 'Are you sure you want to purge the data base?'
      SAPRM(5) = 'Hit return to continue.'
c
c...Initialzie messages
c
      SAERR(1) = '*ERROR*  Invalid company name.'
      SAERR(2) = '*ERROR*  Invalid hardware device.'
      SAERR(3) = '*ERROR*  Invalid software name.'
      SAERR(4) = '*ERROR*  Invalid option list.'
      SAERR(5) = '*ERROR*  Invalid number of users.'
      SAERR(6) = '*ERROR*  Invalid termination date.'
      SAERR(7) = '*ERROR*  Invalid version date.'
      SAERR(8) = '*ERROR*  Invalid system ID.'
      SAERR(9) = '*ERROR*  Invalid password.'
      SAERR(10) = '*ERROR*  Invalid license number.'
      SAERR(21) = 'Record was not added to data base.'
      SAERR(22) = 'New company has been added.'
      SAERR(23) = 'New software product has been added.'
      SAERR(24) = 'Software product was replaced.'
      SAERR(25) = 'No match found.'
      SAERR(26) = 'Record successfully deleted.'
      SAERR(27) = 'Company successfully deleted.'
      SAERR(28) = 'Data base successfully purged.'
      SAERR(29) = 'Record successfully printed.'
c
c...End of routine
c
 8000 return
      end
c
