C*********************************************************************
C*    NAME         :  dsplsh.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       dsplsh.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:59
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine dsplsh (lnam,geonam)
C*                handles the   disply/sh1  command.
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
      subroutine dsplsh (nclkey,ietype, tfmat)

      include 'com8a.com'
      include 'const.com'

      integer*4 nclkey, savncl,idhold(350),nclky2,jsn(2)
      integer*4 icnt, nclky9, rmkey
      integer*2 ietype,np,q, inc,savflg
      integer*2 ksn(4),kp(4000)
      integer*2 f2d3d
		real*8 tfmat(12)
      real*8 p(1000), pt(3), ptout(3), buf(3),asn,pang,pi2
      real*8 ptpt(3), cent(3), strt(3)
      real*4 ang(2)
      equivalence (asn,jsn,ksn),(p,kp),(pang,ang)

C
C...Ifl(344) indicates the whether or not to display in 3D.
C
      savflg=ifl(344)
c
c....we have a flag f2d3d for every shape to know
c....whether or not to display in 3D. so use it instead
c....use ifl(344)
c....Yurong
C....f2d3d =0 then display 2D, f2d3d = 1 display wireframe
C....f2d3d =2 display shaded.  JLS 9/30/99
c
      call ncl_getshp_f2d3d(nclkey, f2d3d)
c
c...assign f2d3d to ifl(344) because some routine checkes ifl(344)
c...as  whether or not to display in 3D.
c...Yurong
c
      ifl(344) = f2d3d

1     continue

C
C...Clear out P, otherwise the shape may be displayed with
C...extra junk. JLS 10/7/98
C
      call fill_array (p(1),1000,0.d0)
C
C...q is the counter for the elements of idhold. JLS 10/7/98
C...inc is the pickid number.
C
      q=1
      inc=1
C
C...Get shape entity into p
C
C      asn=sc(11)
C      np=ksn(3)
C      call getent(p,np,ksn(1),ksn(2),0)
      call gtshap(p, nclkey)
C
C...Save the key number of the shape.
C
      savncl=nclkey
      np=kp(3)
      buf(3)=0.
      tol=sc(27)*4.
C
C...Find stpt (on dummy lim1) and goto same.
C
      do 10 i=8,np
      j=i
      k=4*i
10    if(kp(k).eq.5)goto 20
C
C...Error if here.  no start line found.
C
      ifl(2)=5
      goto 99
c
c.....added transformation here
c.....Yurong 10/3/97
c
20    pt(1) = p(i+1)
      pt(2) = p(i+2)
      pt(3) = 0
C
C...ptpt holds the starting point of the segment, this will 
C...be used when the user chooses to display the shape in 3D.
C...JLS 10/7/98
C
      ptpt(1)=pt(1)
      ptpt(2)=pt(2)
      ptpt(3)=pt(3)
      call cctmtf(pt, tfmat, ptout)
      buf(1)=ptout(1)
      buf(2)=ptout(2)
      xnd=buf(1)
      ynd=buf(2)
c      call plotm(buf,.true.)
c
c...When Drawing 3D, we don't need draw shape 2D
c...Yurong
c
      if (f2d3d.lt.1) then
      	call gmova3(buf(1),buf(2),buf(3))
      	call star(xnd,ynd)
		endif
C
C...Run thru p-list. plot each entity as found
C
      i=j+2
30    i=i+1
      k=4*i
C
C...If line or circle, go plot it.
C
      if (i.ge.np) goto 35
      if(kp(k).eq.5)goto 40
      if(kp(k).eq.7)goto 50
      i=i+kp(k-1)
      goto 30
C
C...All done.
C
    
c
c...When Drawing 3D, we don't need draw shape 2D
c...Yurong
c
35    if (f2d3d.lt.1) then
         call star(xnd,ynd)
         call gdraw
      endif
      goto 99
C
C...Line.
C
C...added transformation 
C...Yurong 10/3/97
C
40    pt(1) = p(i+1)
      pt(2) = p(i+2)
      pt(3) = 0
C   
C...Convert the information that has been held in p to a line that
C...will be used to construct the composite curve for revolution if
C...the user wants the shape to have a 3D display. Also the pickid
C...for each line segment is set here.  idhold holds the newly created line
C...and circle ids.  These will later be deleted from the unibase.
C...JLS 10/7/98
C...Added parameter nclkey to get the attributes of the shape in 
C...nclu_sh_line.  JLS 5/17/99
C
      call nclu_sh_line(ptpt,pt,nclky9,inc,nclkey)
      inc=inc+1
      idhold(q)=nclky9
      q=q+1
      ptpt(1)=pt(1)
      ptpt(2)=pt(2)
      ptpt(3)=pt(3)
      call cctmtf(pt, tfmat, ptout)
      buf(1)=ptout(1)
      buf(2)=ptout(2)
      xnd=buf(1)
      ynd=buf(2)
c      call plotm (buf,.false.)
c
c...When Drawing 3D, we don't need draw shape 2D
c...Yurong
c
      if (f2d3d.lt.1) then
          call glina3(buf(1),buf(2),buf(3))
      endif
      i=i+2
c
c...When Drawing 3D, we don't need draw shape 2D
c...Yurong
c
      if (f2d3d.lt.1) then
          call gdraw
      endif
      goto 30
C
C...Circle.
C
50    xst=xnd
      yst=ynd
C
C...strt holds the starting point of the circle segment.
C
      strt(1)=pt(1)
      strt(2)=pt(2)
      strt(3)=pt(3)
      pt(1) = p(i+1)
      pt(2) = p(i+2)
      pt(3) = 0
C
C...ptpt holds the ending point of the circle segment.
C
      ptpt(1)=pt(1)
      ptpt(2)=pt(2)
      ptpt(3)=pt(3)
      call cctmtf(pt, tfmat, ptout)
      xnd=ptout(1)
      ynd=ptout(2)
      pt(1) = p(i+3)
      pt(2) = p(i+4)
      pt(3) = 0
C
C...cent holds the center point of the circle.
C
      cent(1)=pt(1)
      cent(2)=pt(2)
      cent(3)=pt(3)
      call cctmtf(pt, tfmat, ptout)
		xc=ptout(1)
      yc=ptout(2)
C
C...need save the untranslated center point for
C...later use
C...Yurong
C
      old_xc=p(i+3)
      old_yc=p(i+4)
      r=p(i+5)
      pang=p(i+6)
      bst=ang(1)
      bnd=ang(2)
      delb=bnd-bst
      dx=xst-xc
      dy=yst-yc
      bst=datan2(dy,dx)
      if (bst.lt.0.) bst=bst+TWO_PI
      h=r-tol
      if(h.lt.r*.8)h=r*.8
      co=h/r
      db=acos(co)*2.
      anpts=abs(delb)/db
      npts=anpts+1.
C
C...3 points isn't enough to make a good circle, increasing it to 9.
C...JLS 5/17/99
C
C      if(npts.lt.3)npts=3
      if(npts.lt.9) npts = 9
      anpts=npts
      db=delb/anpts
      n=0
      b=bst
55    n=n+1
      b=b+db
C
C...need use untranslate center point to
C...calculate untranslate circle first
C
      buf(1)=old_xc+r*cos(b)
      buf(2)=old_yc+r*sin(b)
C
C....add transformation here
C....Yurong 10/3/97
C
      pt(1) = buf(1)
      pt(2) = buf(2)
      pt(3) = 0
      call cctmtf(pt, tfmat, ptout)
C      call plotm(buf,.false.)
c
c...When Drawing 3D, we don't need draw shape 2D
c...Yurong
c
      if (f2d3d.lt.1) then
          call glina3(ptout(1), ptout(2), ptout(3))
      endif
      if(n.lt.npts)goto 55
C  
C...Like for lines up above, create circle information that will be used
C...to create the composite curve that is used to create the
C...revolution. Additionally the circle pickids are set.
C...Added parameter nclkey to get the shape attributes in nclu_sh_circle
C
      call nclu_sh_circle(r,delb,cent,strt,ptpt,nclky9,inc,nclkey)
      inc=inc+1
      idhold(q)=nclky9
      q=q+1
      i=i+6
c
c...When Drawing 3D, we don't need draw shape 2D
c...Yurong
c
      if (f2d3d.lt.1) then
          call gdraw
      endif
      goto 30

C
C...If the shape will be displayed in 3-D, then ifl(344)=1 and
C...call cmpdef and shapcv to make the surface view.
C
99    nclkey=savncl
         icnt=q-1
      if (f2d3d.ge.1) then
         call ncl_free_uv
         asn = 0.0d0
         do i=1,icnt
           jsn(1)=idhold(i)
           call ncl_push_asn (asn)
         enddo
         call cmpdef(nclky2)
         call shapcv(nclky2,nclkey)
         call ncl_free_uv
      endif
C
C...Remove all information about the circles and lines that
C...were created above.
C
      do 107 i=1,icnt
         rmkey=idhold(i)
         call dlgeom(rmkey)
107   continue
C
C...If displaying in 3D, display the 3D portion and the go back and
C...display just the shape.  This way the pickids are accessible and not 
C...over written by the surface ids.
C
c...We don't need display 2D
c...Yurong
c
c      if(ifl(344).eq.1) then
c         ifl(344)=0
c         goto 1
c      endif

C
C...Added to draw label for shape
C
c99    if (lablsh) call drwlab(buf(1),buf(2),buf(3),nclkey)

C
C...Restore ifl(344) to original selection
C
c
c....we have to restore ifl(344) because now ifl(344) use as 
c....default value for f2d3d when create shape
c
      ifl(344)=savflg
      call drwlab(buf(1),buf(2),buf(3),nclkey)
      return
      end
