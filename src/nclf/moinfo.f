C*********************************************************************
C*    NAME         :  moinfo.f
C*       CONTAINS:
C*					moinfo (pskey,pscond,dscond)
C*					moinfs
C*					moinfr
C*             asfrst
C*             asfset
C*					cpyifl (ist1,ist2,nc)
C*					cpysc (ist1,ist2,nc)
C*					rstifl (ist1,ist2,nc)
C*					rstsc (ist1,ist2,nc)
C*
C*    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*        moinfo.f , 25.4
C*    DATE AND TIME OF LAST  MODIFICATION
C*        10/27/16 , 13:49:27
C*********************************************************************
C
c
c***********************************************************************
c
c   SUBROUTINE:  moinfo (pskey, pscond, dscond)
c
c   FUNCTION:  Returns the part surface key, part surface condition,
c              and drive surface condition.
c
c   INPUT:  none
c
c   OUTPUT: pskey   I*4  D1  -  Part surface Unibase key id.
c
c           pscond  I*4  D1  -  Part surface condition 0 = TLOFPS,
c                               1 = TLONPS.
c
c           dscond  I*4  D1  -  Drive surface condition -1 = TLLFT,
c                               0 = TLON, 1 = TLRGT.
c
c***********************************************************************
c
      subroutine moinfo (pskey,pscond,dscond)
 
      include 'com4a.com'
c
      integer*4 pskey,pscond,dscond
c
      integer*2 nwds,ietype
c
c...Return motion parameters
c
      call gtdesc (sc(35),pskey,nwds,ietype)
      pscond = ifl(342)
      dscond = ifl(219)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ifatops(autops)
c
c   FUNCTION:  Returns AUTOPS flag
c
c   INPUT:  none
c
c   OUTPUT: autops   I*4  D1  -  AUTOPS flag
c
c***********************************************************************
c
      subroutine ifatops(autops)
 
      include 'mocom.com'
c
      integer*4 autops
c
c...Return AUTOPS flag
c
      if (fautops.eq..true.) then
          autops = 1
      else
          autops = 0
      endif
c
c...End of routine
c
 8000 return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  getdtbl6(d6)
c
c   FUNCTION:  Returns first 6 value of 'd' table
c
c   INPUT:  none
c
c   OUTPUT: d6   array of  real*8  -  first 6 value of 'd' table
c
c***********************************************************************
c
      subroutine getdtbl6(d6)
 
      include 'mocom.com'
c
      real*8 d6(6)
c
c...Return d value
c
      d6(1) = d(1)
      d6(2) = d(2)
      d6(3) = d(3)
      d6(4) = d(4)
      d6(5) = d(5)
      d6(6) = d(6)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  moinfs ()
c
c   FUNCTION:  Saves the current motion information for test running
c              a motion statement (i.e. CONTOUR->PREVIEW).
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine moinfs
c
      include 'com.com'
      include 'mocom.com'
      include 'mosave.com'
      include 'fillet.com'
c
      integer*4 ibrec(2),irec(2)
c
c...Save ifl's
c
      call cpyifl (21,1,4)
      call cpyifl (91,5,1)
      call cpyifl (213,6,5)
      call cpyifl (219,11,1)
      call cpyifl (246,12,2)
      call cpyifl (276,14,1)
      call cpyifl (327,15,1)
      call cpyifl (331,16,3)
      call cpyifl (337,19,6)
      call cpyifl (347,25,3)
      call cpyifl (352,28,1)
c
c...Save sc's
c
      call cpysc (1,1,9)
      call cpysc (23,10,13)
      call cpysc (53,23,2)
      call cpysc (80,25,13)
      call cpysc (123,38,3)
      call cpysc (141,41,3)
      call cpysc (153,44,2)
      call cpysc (162,46,1)
      call cpysc (167,47,2)
      call cpysc (171,49,3)
      call cpysc (177,52,4)
      call cpysc (195,56,1)
c
c...Save clfile pointer
c
      call ncl_setptr(imotp,i4stsv)
c
c...Save ARCSLP/FILLET pointers
c...and variables
c
      call ncl_setptr (isrec,i4rec)
      call ncl_setptr (isrec2,i4rec2)
      call ncl_setptr (isrec3,i4rec3)
      do 75 i=1,6,1
          odptsv(i) = oldpts(i)
   75 continue
      do 100 i=1,10,1
          i4fsav(i) = filflg(i)
  100 continue
      do 200 i=1,42,1
          sppsav(i) = sptsav(i)
  200 continue
cc      ifl(347) = 0
c
c...Turn off GENPTS
c
      ifl(246) = 0
c
c...Use temporary clfile for preview motion
c
      if (ifl(347) .ne. 0) then
          call ncl_setptr (isrec,irec)
      else
          call ncl_setptr (imotp,irec)
      endif
      call ncl_set_temp_clfile (irec,ibrec,imotp)
c
c...Set ARCSLP/FILLET pointers
c...to temporary clfile
c
cc      if (ifl(347) .ne. 0) then
          call ncl_setptr (ibrec,isrec2)
          call ncl_setptr (ibrec,isrec)
          call ncl_setptr (imotp,isrec3)
cc      endif
      sautops = fautops 
c
c...Let everyone know this is a
c...Preview type motion
c
      ifl(216) = 1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  moinfr (keepfl)
c
c   FUNCTION:  Restores the current motion information after test running
c              a motion statement (i.e. CONTOUR->PREVIEW).
c
c   INPUT : keepfl  I*4  D1  -  0 = Preview motion was not accepted.
c                                   Reset clfile and all motion data.
c
c                               1 = Preview motion was accepted, only
c                                   reset clfile.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine moinfr (keepfl)
c
      include 'com.com'
      include 'mocom.com'
      include 'mosave.com'
      include 'fillet.com'
c
      integer*2 iclf
      integer*4 keepfl
      data iclf /0/
c
      integer*4 i40(2)
c      data i40 /0/


      call ncl_zroptr(i40)
c
c.....Reset only if motion was not accepted - ASF 1/29/14
c
      if (keepfl .eq. 0) then
c
c...Save ifl's
c
          call rstifl (21,1,4)
          call rstifl (91,5,1)
          call rstifl (213,6,5)
          call rstifl (219,11,1)
          call rstifl (246,12,2)
          call rstifl (276,14,1)
          call rstifl (327,15,1)
          call rstifl (331,16,3)
          call rstifl (337,19,6)
          call rstifl (347,25,3)
          call rstifl (352,28,1)
c
c...Save sc's
c
          call rstsc (1,1,9)
          call rstsc (23,10,13)
          call rstsc (53,23,2)
          call rstsc (80,25,13)
          call rstsc (123,38,3)
          call rstsc (141,41,3)
          call rstsc (153,44,2)
          call rstsc (162,46,1)
          call rstsc (167,47,2)
          call rstsc (171,49,3)
          call rstsc (177,52,4)
          call rstsc (195,56,1)
c
c...Delete clfile records
c
cc      call cldel (iclf,i4stsv,i40,imotp)
          call ncl_setptr(i4stsv,imotp)
c
c...Restore ARCSLP/FILLET pointers
c
          call ncl_setptr (i4rec,isrec)
          call ncl_setptr (i4rec2,isrec2)
          call ncl_setptr (i4rec3,isrec3)
c
cc      call ncl_eqlptr(isrec,i4stsv,iflg)
cc      if (iflg .eq. 1) call ncl_setptr (imotp,isrec)
cc      call ncl_eqlptr(isrec2,i4stsv,iflg)
cc      if (iflg .eq. 1) call ncl_setptr (imotp,isrec2)
cc      call ncl_eqlptr(isrec3,i4stsv,iflg)
cc      if (iflg .eq. 1) call ncl_setptr (imotp,isrec3)
c
          do 75 i=1,6,1
              oldpts(i) = odptsv(i)
   75     continue
          do 100 i=1,10,1
              filflg(i) = i4fsav(i)
  100     continue
          do 200 i=1,42,1
              sptsav(i) = sppsav(i)
  200     continue
      endif
      fautops = sautops 
c
c...Reset Preview motion
c
      call ncl_reset_temp_clfile(keepfl)
      call ncl_reset_temp_motptr ()
      ifl(216) = 0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  asfrst ()
c
c   FUNCTION:  Outputs an ARCSLP/FILLET interrupt record to the clfile
c              if it is currently enabled.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine asfrst
c
      include 'com.com'
c
      real*8 rdat
c
c...Output ARCSLP/FILLET interrupt record
c
      if (ifl(347) .ne. 0) call putcl (13000,1,1,rdat)
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  asfset ()
c
c   FUNCTION:  Outputs an ARCSLP/FILLET resume record to the clfile
c              if it is currently enabled.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine asfset
c
      include 'com.com'
c
      real*8 rdat
c
c...Output ARCSLP/FILLET interrupt record
c
      if (ifl(347) .ne. 0) call putcl (13000,2,1,rdat)
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cpyifl (ist1,ist2,nc)
c
c   FUNCTION:  Saves the requested 'ifl' flags in the 'iflsv' array.
c
c   INPUT:  ist1    I*2  D1  -  Starting index of 'ifl' array to save.
c
c           ist2    I*2  D1  -  Starting index of 'iflsv' array to save
c                               the ifl's into.
c
c           nc      I*2  D1  -  Number of ifl's to save off.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine cpyifl (ist1,ist2,nc)
c
      include 'com.com'
      include 'mosave.com'
c
      integer*2 ist1,ist2,nc
c
c...Save ifl numbers
c
      do 100 i=0,nc-1,1
          iflsv(ist2+i) = ifl(ist1+i)
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cpysc (ist1,ist2,nc)
c
c   FUNCTION:  Saves the requested 'sc' variables in the 'scsv' array.
c
c   INPUT:  ist1    I*2  D1  -  Starting index of 'sc' array to save.
c
c           ist2    I*2  D1  -  Starting index of 'scsv' array to save
c                               the sc's into.
c
c           nc      I*2  D1  -  Number of sc's to save off.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine cpysc (ist1,ist2,nc)
c
      include 'com.com'
      include 'mosave.com'
c
      integer*2 ist1,ist2,nc
c
c...Save ifl numbers
c
      do 100 i=0,nc-1,1
          scsv(ist2+i) = sc(ist1+i)
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rstifl (ist1,ist2,nc)
c
c   FUNCTION:  Restores the requested 'ifl' flags from the 'iflsv' array.
c
c   INPUT:  ist1    I*2  D1  -  Starting index of 'ifl' array to restore.
c
c           ist2    I*2  D1  -  Starting index of 'iflsv' array to restore
c                               the ifl's from.
c
c           nc      I*2  D1  -  Number of ifl's to restore.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine rstifl (ist1,ist2,nc)
c
      include 'com.com'
      include 'mosave.com'
c
      integer*2 ist1,ist2,nc
c
c...Save ifl numbers
c
      do 100 i=0,nc-1,1
          ifl(ist1+i) = iflsv(ist2+i)
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rstsc (ist1,ist2,nc)
c
c   FUNCTION:  Restores the requested 'sc' variables from the 'scsv' array.
c
c   INPUT:  ist1    I*2  D1  -  Starting index of 'sc' array to restore.
c
c           ist2    I*2  D1  -  Starting index of 'scsv' array to restore
c                               the sc's from.
c
c           nc      I*2  D1  -  Number of sc's to restore.
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine rstsc (ist1,ist2,nc)
c
      include 'com.com'
      include 'mosave.com'
c
      integer*2 ist1,ist2,nc
c
c...Save ifl numbers
c
      do 100 i=0,nc-1,1
          sc(ist1+i) = scsv(ist2+i)
  100 continue
c
c...End of routine
c
 8000 return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  wrttpos_cl()
c
c   FUNCTION: write current tool position as from point into cl file
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
c
      subroutine wrttpos_cl()
c
      include 'com.com'
c
      real*8 gtp(6)
c
      gtp(1) = sc(1)
      gtp(2) = sc(2)
      gtp(3) = sc(3)
      call putcl (5000,3,1,gtp)
c
      return
      end
