C*********************************************************************
C*    NAME         :  copy2.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       copy2.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:44
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine copy2 (copdat,mxtr,copymx,numcpy)
C*   do the reading and writing of the clfile 
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
      subroutine copy2 (copdat,mxtr,copymx,numcpy)
c
      include 'com8a.com'
      include 'mocom.com'
c
      integer*2 class,subcl,offset,svtc,tracut,isv129,npt
      integer*2 cpynum,itmcnt,numitm,pltitm,iclf,iflg,iflg1,jerr
      integer*4 ien,reched(6)
      integer*4 iclad1(2),iclad2(2)
      integer*4 clen(2),clpg(2),svpg(2),isv2(2),iept(2)
c
      logical mxtr,first,frstco,bfirst
c
      real*8 copymx(16),claddr(2),endpos(12),seq,trinv(12)
      real*8 cldata(422),copdat(2)
      real*8 endmx,endmy,endmz,lastmx,lastmy,lastmz
c
      equivalence (tracut,ifl(73))
      equivalence (iclad1,claddr(1)),(iclad2,claddr(2))
      equivalence (sc(171),lastmx), (sc(172),lastmy), (sc(173),lastmz)
c
c...Don't do anything on empty index
c
      claddr(1)=copdat(1)
      claddr(2)=copdat(2)
      call ncl_eqlptr(iclad1,iclad2,iflg)
      if (iflg.eq.1) go to 99999
c
c...Initialize variables
c
      iclf = 0
      svtc=tracut
      tracut=0
      isv129=ifl(129)
c                  end of online plotting variables
      bfirst=.true.
      frstco=.true.
      first=.true.
      iline=25
      cpynum=1
      itmcnt=1
      isv1=istat(1)
      call ncl_setptr(imotp,isv2)
      call ncl_zroptr(iept)
      imot = 0
      rpfron = .false.
c
c...Save last XYZ position
c
      endmx  = sc(1)
      endmy  = sc(2)
      endmz  = sc(3)
c
c...Set flag to force plotm to call motbgn
c...on first output
c
cc      ifl(270) = -1

      if (svtc.eq.1) then

c              invert tracut matrix if tracut is in effect
          sfx=sc(41)**2+sc(45)**2+sc(49)**2
          sfy=sc(42)**2+sc(46)**2+sc(50)**2
          sfz=sc(43)**2+sc(47)**2+sc(51)**2
          sm=1.d-4
          if(sfx.gt.sm.and.sfy.gt.sm.and.sfz.gt.sm) then

c                  do rotation params
              do 100 i=1,3
                  j=4*i-3
                  trinv(i)=sc(j+40)/sfx
                  trinv(i+4)=sc(j+41)/sfy
100               trinv(i+8)=sc(j+42)/sfz

c                  translate origin
              do 200 j=4,12,4
                  i=j-3
                  trinv(j)=-trinv(i)*sc(44)-trinv(i+1)*sc(48)
     x                     -trinv(i+2)*sc(52)
200           continue
          else

c                  error. mx does not invert
              ifl(2)=255
              goto 99999
          endif
      endif
c
c...Read data from the clfile and
c...write it out after possible translation
c
      call ncl_setptr(iclad1, clpg)
      call ncl_setptr(iclad2, clen)
      call ncl_setptr(imotp, svpg)
  500 call clread (iclf,clpg,reched,cldata,jerr)
      class  = reched(3)
      subcl  = reched(4)
      numitm = reched(5)
      if (class.eq.2000 .and. subcl.eq.5) rpfron = .true.
c
c......Translate through matrix if circle and
c......translation requested in the copy
c
      if (class.eq.3000 .and. mxtr) then
          iadd=0
          if (ifl(144).eq.1) iadd=4
          call conent (cldata(2+iadd),copymx,3)
          call conent (cldata(5+iadd),copymx,4)
c
c.........Modify radius of circle
c.........by factoring vector to unity and
c.........radius by inverse of same
c
         cldata(9+iadd)=cldata(9+iadd)*
     x                  dsqrt(copymx(1)**2+copymx(2)**2+copymx(3)**2)
      endif
c
c......Translate through matrix if motion and
c......translation requested in the copy
c
      if (class .eq. 5000 .or. class .eq. 5100 .or. class .eq. 2500 .or.
     1    class .eq. 5200 .or. class .eq. 5210 .or. class .eq. 5300)
     2       then
          if (class .eq. 5000 .or. class .eq. 5200) imot = 1
          offset=3
          npt = offset
          if (class .ge. 5200) then
              offset = 21
              npt = 6
          else if (ifl(82).eq.1) then
              offset=6
              npt = 6
          endif
          do 700 i=1,numitm,offset
              if (mxtr) then
                  call conent(cldata(i),copymx,3)
                  if (npt.eq.6) call conent(cldata(i+3),copymx,4)
              endif
c
c.........Move last known position to holding area
c.........unless this is the first move
c.........then get it from sc table
c
              if (.not.frstco) then
                  do 697 j=1,npt
                      endpos(j)=endpos(j+6)
  697             continue
              else
                  frstco=.false.
                  do 698 j=1,6
                      endpos(j)=sc(j)
  698             continue
              endif
c
c.........Move current position to holding area
c.........(used to calculate forward direction at end of copy)
c
              do 699 j=1,npt
                  endpos(j+6)=cldata(i+j-1)
  699         continue
  700     continue
          pltitm = numitm
          numitm = numitm / offset
      else
          numitm = numitm + 1
      endif
c
c......Save cl record
c
      call putcl (class,subcl,numitm,cldata)
c
c......Write out x,y,z,i,j,k values
c
      if (motdfl.and.(class.eq.5000.or.class.eq.2500) .or.
     1    class .eq. 5200) then
          do 705 j=1,pltitm,offset
              if (iline.gt.24) then
                  iline=16
                  write (cout,1060) cpynum
 1060             format (' copy',i4,'   x',11x,'y',11x,'z',11x,'i',11x,
     1                     'j',11x,'k')
                  if (ifl(35) .eq. 0 .or. ifl(35) .eq. 2 .or.
     1                (ifl(35).eq.1 .and. bfirst)) then
                      call putmsg (cout,80,15,0)
                      bfirst=.false.
                      if (ifl(35).eq.0 .or. ifl(35) .eq. 2)
     1                    call ersw3 (16,1)
                  endif
              endif
c
c.........If tracut in effect, run data through inverse
c.........before printing and plotting it
c
              ien    = j      + npt - 1
              if (svtc .eq. 1) then
                  call conent (cldata(j),trinv(1),3)
                  if (npt.eq.6) call conent (cldata(j+3),trinv(1),4)
              endif
              write (cout,1040) itmcnt,(cldata(i),i=j,ien)
 1040         format (i4,3f12.4,3f12.6)
c
c.........Print x,y,z,i,j,k position
c
              if (ifl(154).eq.1.or.(ifl(154).eq.0.and.iline.eq.16)) then
                  call putmsg (cout,80,iline,0)
                  iline=iline+1
                  itmcnt=itmcnt+1
              endif
c
c.........See if user aborted copy operation
c
              call ckintr (ifl(86),ifl(35))
              if (ifl(86) .eq. 1) then
c
c............Delete clfile records
c
                  call ncl_eqlptr(isv2,imotp,iflg)
                  if (iflg .eq. 0) call cldel (iclf,isv2,iept,imotp)
c
c............Output error message
c
                  err    = .true.
                  ifl(2) = 149
                  call error (149)
c
c............Erase motion display
c............for this copy
c
cc                  call moters
                  lastmx = endmx
                  lastmy = endmy
                  lastmz = endmz
                  go to 99977
              endif

705       continue
      endif
c
c.........Plot motion
c
cc      if (ifl(100).eq.1) then
cc          if (class.eq.5000 .or. class.eq.2500) then
cc              if (clpg .eq. clen) ifl(130) = 0
cc              tracut = svtc
cc              do 710 i=1,pltitm,offset
cc                  if (ifl(82).eq.0) then
cc                      do 706 j=1,3
cc                          buf(j)=cldata(i+j-1)
cc                          buf(j+3)=sc(j+3)
cc  706                 continue
cc                      call plotm (buf,.false.)
cc                  else
cc                      call plotm (cldata(i),.false.)
cc                  endif
cc710           continue
cc              tracut=0
cc          endif
cc      endif
790   if (first) then
          first=.false.
      endif
      if (class.eq.5000 .or. class.eq.2500 .or. class .eq. 5200)
     1        rpfron = .false.
c
c......If not at end of copy area
c......then go get next record
c
      call ncl_eqlptr(clpg,clen,iflg)
      call ncl_tstptr(clpg,iflg1)
      if (iflg.eq.0 .and. iflg1.ne.0) go to 500
c
c...End of copy
c......Check if all copies are done
c
      numcpy = numcpy-1
      if (numcpy .lt. 1) go to 99977
c
c......More copies to do
c
      cpynum = cpynum + 1
      iline  = 25
      itmcnt = 1
c
c.........Point to beginning clfile record
c.........of last set of copied cl file data
c
      call ncl_setptr(svpg, clpg)
      call ncl_setptr(imotp, clen)
      call ncl_setptr(imotp, svpg)

      ifl(129) = isv129
      bfirst = .true.
      go to 500
c
c...No more copies
c
99977 continue
      tracut = svtc
c
c...Reset TRACUT matrix
c...for motion display
c
      call traout
c
c......Close motion segment
c
cc      call motend
      if (ifl(2).ne.0) go to 99999
c
c......Print the last point if print/small
c
      if (imot .eq. 1) then
          if (ifl(154).eq.0.and.motdfl) call putmsg(cout,80,iline,0)
c
c......Put ending tool position in sc(1-9)
c
          do 99988 i=1,npt
              sc(i)=endpos(i+6)
99988     continue
c
c......If tracut in effect, run data through inverse
c......before printing and plotting it
c
          if (svtc.eq.1) then
              call conent (sc(1),trinv(1),3)
c
c.........Only do the tlaxis vector if multax is on
c
              if (ifl(82).eq.1) call conent (sc(4),trinv(1),4)
          endif
          sc(7)=endpos(7)-endpos(1)
          sc(8)=endpos(8)-endpos(2)
          sc(9)=endpos(9)-endpos(3)
c
c......Unitize the forward direction
c
          seq=dsqrt(sc(7)**2+sc(8)**2+sc(9)**2)
          if (seq.gt..00001) then
              sc(7)=sc(7)/seq
              sc(8)=sc(8)/seq
              sc(9)=sc(9)/seq
          endif
      endif
99999 return
      end
