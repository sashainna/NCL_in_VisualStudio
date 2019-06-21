C*********************************************************************
C*    NAME         :  index.f
C*       CONTAINS:
C*          indx    traout
C*
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*        index.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:12
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine indx
C*       handle the index command.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine indx

      include 'com8a.com'
      include 'mocom.com'

      integer*2 ktv(4)
      real*8 ridxvl(2)
      equivalence (ridxvl(1),id4vl1),(ridxvl(2),id4vl2),(ktv,tv)
      integer*4 idx,id4vl1(2),id4vl2(2),mdx
      character*64 xtoken
      equivalence (xtoken,token2)
      logical tru,fals
      integer*2 iflg

      data mdx /99999./

      tru=.true.
      fals=.false.
      if (nextyp.ne.5) then
          isvinx=inx
          call error(22)
          go to 99999
      endif
      ifl(44)=9
      call parsit
c          look for index number
      if ((ityp.eq.2.and.ist.eq.2).or.
     1    (ityp.eq.3.and.itv.gt.0)) then
          idx=itv
          if (ityp.eq.2) idx=tv
          if (idx.gt.mdx) then
              call error (151)
              go to 99999
          endif
          if (nextyp.ne.11) then
c                  look for ',nomore'
              if (nextyp.eq.9) then
                  call parsit
                  write (xtoken,2010) idx
2010              format ('ix',i6.6)
                  if (ityp.eq.1.and.ist.eq.53) then
                      call vstchk
                      if (ist.eq.15.) then
c                              add ending pointers to vst entry
                          ipg=ktv(1)
                          iel=ktv(2)
                          call getent (ridxvl,2,ipg,iel,0)
c
c...Store the ending sequence number
c...Bobby  -  12/1/92
c
c                          if (id4val(2) .eq. 0) then
c                              id4val(2) = i4stat(2)
                          call ncl_tstptr(id4vl2,iflg)
                          if (iflg .eq. 0) then
                              call ncl_setptr(imotp,id4vl2)
                              call putent (ridxvl,2,ipg,iel,tru,0)
c
c......This Index has already
c......been terminated
c
                          else
                              call error(153)
                              go to 99999
                          endif
                      else
                          call error (146)
                          go to 99999
                      endif
                  else
                      call error (147)
                      go to 99999
                  endif
              else
                  call error (57)
                  go to 99999
              endif
          else
c                  write to vst as an index subtype
              write (xtoken,2010) idx
              call vstchk
c
c   added a test for ist.eq.15 to reprocess index#'s. kathy
c
              if (ist.eq.1 .or. ist .eq. 15) then
                  ifl(9)=ifl(11)
                  ifl(10)=ifl(12)
c
c...Store Index in Ranfile
c
c                  id4val(1) = i4stat(2)
c                  id4val(2) = 0
                  call ncl_setptr(imotp,id4vl1)
                  call ncl_zroptr(id4vl2)
                  call putent (ridxvl,2,ipg,iel,fals,0)
c
c...Store Index number in variable name list
c
                  ktv(1)=ipg
                  ktv(2)=iel
                  ktv(3)=2
                  ktv(4)=15
                  rest=tv
                  idst=15
                  savid2=token2
                  isvsub = ivxsub
                  call vstore
c
c...Output TRACUT matrix to clfile
c...so COPYed motion is displayed correctly
c
                  call traout
c
c...Feed rate should NOT be output
c...with INDEX command
c...Bobby  -  4/21/97
c
              else
                  call error (150)
                  go to 99999
              endif
c
          endif
      else
          call error (7)
          go to 99999
      endif

99999 return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine traout
C*       handle the index command.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine traout
c
      include 'com.com'
c
      integer*2 icl,isubcl,inum
c
c...Initialize routine
c
      icl    = 7000
c
c...Output TRACUT matrix
c
      if (ifl(73) .eq. 1) then
          isubcl = 1
          inum   = 13
c
c...Output TRACUT/NOMORE
c
      else
          isubcl = 2
          inum   = 1
      endif
c
c...Output TRACUT record to clfile
c
      call putcl (icl,isubcl,inum,sc(41))
c
c...End of routine
c
 8000 return
      end
