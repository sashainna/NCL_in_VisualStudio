c*****************************************************************
c*    NAME:       dstrge.f
C*      CONTAINS:  fortran routines to display point, vector,
C*                 pointvector, line and matrix.
C*       dstrge (dtype, nclkey, dbuf)
C*       dstlin (gpt,gvec) 
C*       dstptv (gpt,gvec)
C*       medpot (gpt,gvec,gpos)
C*       dstmat (gmat,nclkey)
C*
c*      MODULE NAME AND RELEASE LEVEL
c*       dstrge.f , 25.2
c*      DATE AND TIME OF LAST MODIFICATION
c*       08/17/15 , 17:44:19
c*******************************************************************
C*******************************************************************
C*    E_SUBROUTINE     : subroutine dstrge(dtype, nclkey, dbuf)
C*       stroke point, vector, line
C*    PARAMETERS   
C*       INPUT  : 
c*          dtype - integer*2
c*                  type of entity to display
C*          dbuf  - real*8 array
c*                  data buffer containing canonical data of entity
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine dstrge (dtype, nclkey, dbuf, tfmat)

c  dtype   type of display
c
c   3       disply/pt   - not used
c   4       disply/ve
c   5       disply/ln
c   10      disply/mx
c   21      disply/pv
c
      include 'com8a.com'

      real*8 dbuf(16), axis(3), cross(3), pt(3)
      integer*2 dtype
      integer*4 nclkey
      real*8 tfmat(12)
c                                              **** display/point
c                                       ****         not used
      if (dtype.eq.3) then
            ifl(128)=0
            cross(1)=dbuf(1)+.05/sc(106)
            cross(2)=dbuf(2)
            cross(3)=dbuf(3)
            call gmova3( cross(1), cross(2), cross(3))
            ifl(128)=1
c                    draw starting point of cross hairs x-line
            cross(1)=dbuf(1)-.05/sc(106)
            call glina3(cross(1), cross(2), cross(3)) 
            ifl(128)=0
            cross(1)=dbuf(1)
            cross(2)=dbuf(2)+.05/sc(106)
            call glina3 (cross(1), cross(2), cross(3))
            ifl(128)=1
c                    draw starting point of cross hairs y-line
            cross(2)=dbuf(2)-.05/sc(106)
            call gmova3 (cross(1), cross(2), cross(3))
            ifl(128)=0
            cross(2)=dbuf(2)
            cross(3)=dbuf(3)+.05/sc(106)
            call glina3 (cross(1), cross(2), cross(3))
            ifl(128)=1
c                    draw starting point of cross hairs z-line
            cross(3)=dbuf(3)-.05/sc(106)
            call gmova3 (cross(1), cross(2), cross(3))
            ifl(128)=0
            call gdraw
            call drwlab(dbuf(1), dbuf(2), dbuf(3), nclkey)

c                                              **** display/vector
      else if (dtype .eq. 4) then
          axis(1) = 0.
          axis(2) = 0.
          axis(3) = 0.
c
c...vp3.12.93 added arrowhead to vector using PV logic to displey
c
c
c.....added tranformation 
c.....Yurong
c
           call dstptv (axis, dbuf, tfmat)
c
c...vp3.12.93  added PV entity                 **** display/pntvec
c
      else if (dtype .eq. 21) then
c
c....add transformation but not complete test
c....if has problem, please pass in unit matrix
c...Yurong
c
          call dstptv (dbuf(1),dbuf(4), tfmat)
c                                              **** display/line
      else if (dtype .eq. 5) then
          call medpot (dbuf,dbuf(4),cross)
c
c...added matrix
c...not test, if something wrong
c...please put unit matrix
c...Yurong
c
          call dstlin (dbuf,dbuf(4), tfmat)
          call gdraw
          pt(1) = cross(1)
          pt(2) = cross(2)
          pt(3) = cross(3)
          call cctmtf(pt, tfmat, pt)
          call drwlab(pt(1), pt(2), pt(3), nclkey)

      else if (dtype.eq.6) then
c                                              **** display/plane
          ifl(2)=-202
c                 
c...display/MX
c
      else if (dtype .eq. 10) then
c
c.....add tranformation 
c....Yurong
c
          call dstmat (dbuf,nclkey,tfmat,dbuf,0)
      endif

      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dstlin(gpt,gvec, tfmat)
C*           Display line.
C*    PARAMETERS   
C*       INPUT  : 
C*           gtp   - starting point coordinates
C*           gvec  - delta (vector coordinates)
C*
C*       OUTPUT : none
C*
C*********************************************************************
      subroutine dstlin (gpt,gvec, tfmat)
c
      real*8 gpt(3),gvec(3)
      real*8 pend(3)
      real*8 tfmat(12)
      real*8 pt(3)
c
c...Draw line to the end of PointVector
c
c
c....add transformation
c....Yurong
c
      pt(1) = gpt(1)
      pt(2) = gpt(2)
      pt(3) = gpt(3)
      call cctmtf(pt, tfmat, pt)

      call gmova3 (pt(1), pt(2), pt(3))
      pend(1) = gpt(1) + gvec(1)
      pend(2) = gpt(2) + gvec(2)
      pend(3) = gpt(3) + gvec(3)
      pt(1) = pend(1)
      pt(2) = pend(2)
      pt(3) = pend(3)
      call cctmtf(pt, tfmat, pt)

      call glina3 (pt(1), pt(2), pt(3))
c
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dstptv (gpt,gvec)
C*       display pointvector, this routine was designed to display PV
C*       3.12.93 by vadim p.
C*    PARAMETERS   
C*       INPUT  : 
C*          gpt   - real*8 array D3
c*                  X,Y,Z coordiantes of start point. 
C*          gvec  - real*8 array D3
c*                  I,J,K's of vector. 
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine dstptv (gpt,gvec, tfmat)
c
      include 'com.com'
c
      real*8 gpt(3),gvec(3)
      real*8 tfmat(12)
      real*8 pt1(3), pt2(3)
c
      real*8 pend(3),hedl,hed(3),pth(3),vd,angs(2),alp,f_mag,vec(3)
c
      integer*4 i,j
c
      hedl   = .125
c
c...Adjust length of unit metric vector
c...So that it is 1in instead of 1mm
c
      vd = f_mag(gvec) * 25.4
      if (ifl(264) .eq. 1 .and. vd  .ge. .999 .and. vd .le. 1.001) then
        call vctmsc (gvec,vec,25.4d0)
      else
        call vctovc (gvec,vec)
      endif

c
c...Draw line to the end of PointVector
c
      pt1(1) = gpt(1)
      pt1(2) = gpt(2)
      pt1(3) = gpt(3)
      call cctmtf(pt1, tfmat, pt1)
      call gmova3 (pt1(1), pt1(2), pt1(3))
      pend(1) = gpt(1) + vec(1)
      pend(2) = gpt(2) + vec(2)
      pend(3) = gpt(3) + vec(3)
      pt2(1) = pend(1)
      pt2(2) = pend(2)
      pt2(3) = pend(3)
      call cctmtf(pt2, tfmat, pt2)

      call glina3 (pt2(1), pt2(2), pt2(3))
c
c...Get vector's polar coordinates
c
      call getang (angs(1),angs(2),vec)
c
c...Get PV length and check arrowhead size
c
      vd     = dsqrt (vec(1)**2+vec(2)**2+vec(3)**2)
      if (vd .lt. 2.*hedl) hedl = .5 * vd 
c
c...PV is now on Z axis,
c...the first tail faces X axis
c
      hed(1) = 0.4 * hedl
      hed(2) = 0.
      hed(3) = vd - hedl 
      alp    = 120.0
      j      = 3
      do 290 i=1,3,1
c
c...Adjust tail back to original vector direction
c
          pth(1) = hed(1)
          pth(2) = hed(2)
          pth(3) = hed(3)
          call ptadjr (pth,pth,angs)
c
c...Add start point and draw tail
c
          pth(1) = gpt(1) + pth(1)
          pth(2) = gpt(2) + pth(2)
          pth(3) = gpt(3) + pth(3)
      pt1(1) = pth(1)
      pt1(2) = pth(2)
      pt1(3) = pth(3)
      call cctmtf(pt1, tfmat, pt1)
      call glina3 (pt1(1), pt1(2), pt1(3))
      call gmova3 (pt2(1), pt2(2), pt2(3))
c
c...Rotate tail 120 degree  
c
          if (i .ne. 3) call vecadj (hed,hed,alp,j)
  290 continue
c
c...Finish draw 
c
      call gdraw
c
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine medpot (gpt,gvec,gpos)
C*        Calculate coordinates of the midle point on line/vector.
C*       INPUT  : 
C*          gpt   - real*8 array D3 (coordiantes of start point). 
C*          gvec  - real*8 array D3 (I,J,K's of vector). 
C*       OUTPUT :  
C*          gpos  - real*8 array D3 (coordinates of the midle point).
C*
C*********************************************************************
      subroutine medpot (gpt,gvec,gpos)
c
      real*8 gpt(3),gvec(3),gpos(3)
c
      gpos(1) = gpt(1) + .5 * gvec(1)
      gpos(2) = gpt(2) + .5 * gvec(2)
      gpos(3) = gpt(3) + .5 * gvec(3)
c
      return
      end
c
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dstmat (gmat,nclkey,tfmat,labpos,kflag)
C*        Display matrix.
C*       INPUT  : 
C*          gmat    - Real*8 array D16 (mx data). 
C*          nclkey  - Unibase key of Matrix.
C*          tfmat   - Transformation matrix to apply to matrix display.
C*          kflag   - 1 = Don't display matrix, just calculate label
C*                    position.
C*       OUTPUT :  
C*          labpos  - Default label position when 'kflag' = 1.
C*
C*********************************************************************
      subroutine dstmat (gmat,nclkey, tfmat,labpos,kflag)
c
      include 'com8a.com'
      include 'wrksys.com'
c
      real*8 gmat(12)
      integer*2 flag
      integer*4 nclkey
      real*8 tfmat(12),labpos(3)

      real*8 pt(3)
      real*8 vv(9),pto(3),bx(9),pta(3),dmat(4),axlen
      integer*4 nc
      integer nnx(6),i,n
      character*3 cax(3)
c
      data nnx /0,3,6,0,3,6/
      data cax /' X',' Y',' Z'/
c
c...get matrix axis from buffer
c
      nc   = 2
      call gtmxds (dmat,nclkey)
      j = 1
      do 105 i=1,3
         vv(i)   = gmat(j)
         vv(i+3) = gmat(j+1)
         vv(i+6) = gmat(j+2)
         pto(i)  = gmat(j+3)
         j = j+4
  105 continue
c
c...Disply relative to modsys matrix
c.....Do not convert through modsys if it is the modsys
c.....matrix - Andrew 3/5/13
c
      if (lwrk.and.(nclkey.ne.ifl4(14))) then
        call conent(vv(1),wrkmx,VECTOR)
        call conent(vv(4),wrkmx,VECTOR)
        call conent(vv(7),wrkmx,VECTOR)
        call conent(pto,wrkmx,POINT)
      endif
c
c...scale axis & box for display
c
      do 125 i=1,3
         k = nnx(i)
         axlen = dmax1(dmat(1), dmat(i+1))
         do 115 j=1,3
            bx(j+k) = dmat(1+i) * vv(j+k)  
            vv(j+k) = axlen * vv(j+k)
  115    continue
  125 continue
c
c...Calculate label position
c
      if (kflag .eq. 1) then
          labpos(1) = pto(1) + bx(1) + bx(4) + bx(7)
          labpos(2) = pto(2) + bx(2) + bx(5) + bx(8)
          labpos(3) = pto(3) + bx(3) + bx(6) + bx(9)
          go to 8000
      endif
c
c...display matrix axis with names
c
      n = 0
      do 205 i=1,7,3
          n = n + 1
c
c....add transformation
c....Yurong
c
          call dstptv (pto,vv(i),tfmat)
          call vcplvc (pto,vv(i),pta)
          pt(1) = pta(1)
          pt(2) = pta(2)
          pt(3) = pta(3)
          call cctmtf(pt, tfmat, pt)
          call dsxlab (pt,cax(n),nc, nclkey)
  205 continue
c
c...display box
c
      do 325 i=1,3
         call vctovc (pto,pta)
         do 315 j=1,3
            if (j .ne. 3) then
                call vcplvc (pta,bx(nnx(i+j)+1),pta)
            else
                call vcplvc (pto,bx(nnx(i+2)+1),pta)
            end if
            call dstlin (pta,bx(nnx(i)+1), tfmat) 
  315    continue
  325 continue
c
      call gdraw
c
c...End of routine
c
 8000 return
      end
