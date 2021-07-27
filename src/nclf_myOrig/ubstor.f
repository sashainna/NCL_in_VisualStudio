c***********************************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       ubstor.f , 25.1
c**     DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:50
c**
c***********************************************************************
c **********************************************************************
c  SUBROUTINE: ubstor(ubname,rfname,stored)
c
c  FUNCTION: put selected entity into unibase.    
c
c  INPUT:  ubname  C8   - label & subscript to put in unibase, different
c                         if 'as' is specified,
c
c          rfname  C8   - current referance label & subscript of entity 
c
c  OUTPUT: stored  L    - .true. if stored successfuly in inibase.
c                                                                
c **********************************************************************

      subroutine ubstor (ubname, usub, rfname, rsub, nclkey,
     x           ietyp,stored)

      include 'com8a.com'
      include 'comdb.com'

      integer*4    usub, rsub, nclkey
c      integer*2    rsubnm(33),usubnm(33)
      integer*2    ietyp,i,ierr,DRAFTV
      character*64 rfname,ubname
      logical      stored
      parameter (DRAFTV = 34)
c
c
c...  check if valid entity type
c
      if (ietyp.lt.2 .or. ietyp .gt.10 .and. ietyp .ne. 20 .and. 
     x    ietyp .ne. PNTVEC .and. ietyp .ne. SHAPE .and.
     x    ietyp .ne. DATAST .and. ietyp .ne. TEXTVAR .and.
     x    ietyp .ne. VSOLID .and. ietyp .ne. VANOTE .and.
     x    ietyp .ne. VSYMBOL .and. ietyp .ne. VPLACE .and.
     x    ietyp .ne. DRAFTV)
     x       go to 8888

      call isitag(nclkey,i)
      if (i .eq. 1) goto 8888
c
c...Store with new name
c
      call ubput (ubname,usub,nclkey,ietyp,ierr)
      call ubact(1)
      if (ierr .ne. 0) goto 8888
      stored = .true.
      goto 99999
c
c...entity ignored
c
 8888 stored = .false.
c
99999 return
      end
