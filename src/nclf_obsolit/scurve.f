C*********************************************************************
C*    NAME         :  scurve.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
c*     MODULE NAME AND RELEASE LEVEL
c*       scurve.f , 25.1
c*    DATE AND TIME OF LAST MODIFICATION
c*       04/29/15 , 15:10:39
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine scurve (iline)
c*       this routine displays the canonical form of a curve in window 3
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
 
      subroutine scurve (iline)

      include 'com8a.com'
c      implicit undefined (a-z) 
c      include '../incf/com.com'

      integer*2 maxpt, mxcvhd
      parameter (maxpt=50)
      parameter (mxcvhd=(2+(maxpt+1)/2))

      integer*4 kmxc, kcol, klin , strlen1


      integer*2 nwds,ietype
      integer*4 nclkey
      integer*2 iline
      integer*2 iwf,i,i2,i3,numseg,ipnt,iclosd
      real*8 crvhed(mxcvhd)
      real*4 r4cvhd(mxcvhd*2),r4sc(40)
      equivalence (crvhed,r4cvhd),(r4sc,sc(10))

      call gtdesc(tv, nclkey, nwds, ietype)
      call isitwf(nclkey,iwf)
      if (iwf.eq.1) then
        call wfstrg(nclkey,cout)
      else 
        call gtcvhd(nclkey, crvhed)
        numseg=r4cvhd(1)
        r4cvhd(1)=0.
        iline=18
        write (cout,1010) numseg
1010    format (16x,'number of segments: ',i4)
      endif
      call gtclsd(nclkey,0,iclosd)
      nc = strlen1(cout)
40    if (iclosd.eq.1) then
        cout(nc+1:) = ',   Closed'
      else
        cout(nc+1:) = ',   Open'
      endif
      nc = strlen1(cout)
      i = nc
      call putmsg(cout,i,17,0)
      if (iwf.eq.1) goto 99999
      do 100 i=1,numseg,3
          i2=i+1
          i3=i+2
          if ((i+2).le.numseg) then
              write(cout,1023) i,r4cvhd(i),i2,r4cvhd(i2),i3,r4cvhd(i3)
1023          format (3(' segment(',i2,')=',f10.6,4x))
          else if ((i+1).le.numseg) then
              write (cout,1022) i,r4cvhd(i),i2,r4cvhd(i2)
1022          format (' segment(',i2,')=',f10.6,4x,
     1                ' segment(',i2,')=',f10.6)
          else
              write (cout,1021) i,r4cvhd(i)
1021          format (' segment(',i2,')=',f10.6)
          endif
          nc = strlen1(cout)
          j = nc
          call putmsg(cout,j,iline,0)
100       iline=iline+1
      do 5000 ipnt=1,numseg+1
c
c...Added check for NCL-VT mode
c...Paul  -  10/3/91
c...Old version was:
c   if (ifl(35).eq.0) then
c
c...VX
c
          if (ifl(35).eq.0 .or. (ifl(35) .eq. 2 .and. ifl(322) .ne. 1))
     1           then

              write (cout,1030)
1030          format ('enter c to continue display of point values,',
     1                ' or q to quit showing curve data')
c
c...Added check for NCL-VT mode
c...Paul  -  11/06/91
c...Old version was:
c   call nclpmt (cout, cin)
c
          if (ifl(35) .eq. 2) then
             call putmsg(cout,78,3,2)
             nccin = 0
             kmxc =1
             kcol = 9
             klin = 1
             call gvtlin(cin,nccin,kmxc,klin,kcol)
          else
             nccout = strlen1(cout)
             call nclpmt (cout, nccout, cin, nccin)
          endif

              if (ain(1).eq.'q'.or.ain(1).eq.'Q'.or.ifl(256).eq.1) then
                  call clswin()
                  go to 99999
              endif
          else
              cout=' '
              call putmsg (cout,1,2,0)
          endif
          call gtcseg (nclkey, ipnt, sc(10))
          write (cout,1040)
1040      format (22x,'x',17x,'y',17x,'z')
          nc = strlen1(cout)
          j = nc
          call putmsg (cout,j,17,0)
          write (cout,1050) ipnt,sc(10),sc(11),sc(12)
1050      format ('point',i3,5x,3(f14.7,4x))
          nc = strlen1(cout)
          j = nc
          call putmsg (cout,j,18,0)
          write (cout,1060) r4sc(7),r4sc(8),r4sc(9)
1060      format ('deltas',7x,3(f14.7,4x))
          nc = strlen1(cout)
          j = nc
          call putmsg (cout,j,19,0)
          write (cout,1070) r4sc(10)
1070      format ('du/ds(0)=',f14.6)
          nc = strlen1(cout)
          j = nc
          call putmsg (cout,j,20,0)
          write (cout,1080) r4sc(11)
1080      format ('du/ds(1)=',f14.6)
          nc = strlen1(cout)
          j = nc
          call putmsg (cout,j,21,0)
          write (cout,1090) r4sc(12)
1090      format ('rho=     ',f14.6)
          nc = strlen1(cout)
          j = nc
          call putmsg (cout,j,22,0)
          iline=23
          if (ipnt.eq.numseg) go to 99999
5000  continue
99999 cout=' '
      nc = strlen1(cout)
      j = nc
      call putmsg (cout,j,2,0)

      return
      end
