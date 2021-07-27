C*********************************************************************
C*    NAME         :  ssurf.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       ssurf.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:45
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ssurf (j)
c*       this routine displays the canonical form of a surface
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
      subroutine ssurf (j)

      include 'com8a.com'
      include 'const.com'

      integer*2 j

      integer*2 nwds,ietype
      integer*4 nclkey,strlen1,nc
      integer*2 ispan, nopan, typan, junk(8), ipatch, npats
      real*8 srfdat(20), pdata(16)
      real*8 ang
      real*4 r4sdat(40), r4pdat(2)
      integer*2 isfdat(80),tmp
      equivalence(junk,srfdat,r4sdat,isfdat),(srfdat(2),r4pdat)
      equivalence (typan,junk(1)),(nopan,junk(2)),(npats,junk(2))

c          j = line number for sgeo to start clearing screen from
c      j=17  not used

      call gtdesc (tv, nclkey, nwds, ietype)

      call sftype (nclkey, isf)
      if (isf .eq. 100) then
        cout = 'NCL Surface of Revolution'
        ityp = 0
        goto 50
      endif

      call isitwf (nclkey, iwf)
      if (iwf .eq. 1) then
        call wfstrg (nclkey, cout)
        goto 50
      endif

      call gtgeom(tv,srfdat,nclkey,nwds,ietype)

      if (isfdat(1) .eq. 26) then
        write (cout,1010) isfdat(2)
1010    format (5x,'Mesh Surface,',i5,' patches')
        goto 50
      endif
      if (isfdat(1) .eq. 25) then
        write (cout,1020) isfdat(2)
1020    format (5x,'Quilt Surface,',i5,' patches')
        call putmsg(cout,80,17,0)
        goto 99
      endif
      if (isfdat(1) .eq. 27) then
        write (cout,1030) isfdat(2)
1030    format (5x,'Net Surface,',i5,' surfaces')
        call putmsg(cout,80,17,0)
        goto 99
      endif

      npans = isfdat(2)
      ispan = 1
      call gtspan(nclkey, ispan, srfdat)
      if (isfdat(1) .eq. 0) then
        write (cout,1040) npans
1040    format (5x,'NCL Surface,',i5,' panels')
      else
        cout = '     NCL Ruled Surface'
      endif
50    nc = strlen1(cout)
70    call gtclsd(nclkey,0,iclsu)
      call gtclsd(nclkey,1,iclsv)
      if (iclsu.eq.1 .and. iclsv.eq.1) then
        cout(nc+1:) = ',  Closed in U and V'
      else if (iclsu.eq.1) then
        cout(nc+1:) = ',  Closed in U'
      else if (iclsv.eq.1) then
        cout(nc+1:) = ',  Closed in V'
      else
        cout(nc+1:) = ',  Open'
      endif
      nc = strlen1(cout)
      i = nc
      call putmsg(cout,i,17,0)
c
c... Primitive data ...jingrong 12/27/99
c
      call  gtprimt(nclkey,ifl(72),ityp,pdata)
      if (ityp.eq.0) then       
        write (cout,1110)
      else if (ityp.eq.1) then
        write (cout,1111)
      else if (ityp.eq.2) then
        write (cout,1112)
      else if (ityp.eq.3) then
        write (cout,1113)
        call putmsg(cout,80,17,0)
        write (cout,1170) (pdata(k),k=1,3)
        call putmsg(cout,80,17,0)
        write (cout,1171) pdata(4)
      else if (ityp.eq.4) then
        write (cout,1114)
        call putmsg(cout,80,17,0)
        write (cout,1172) (pdata(k),k=1,3)
        call putmsg(cout,80,17,0)
        write (cout,1173) pdata(4)
      else if (ityp.eq.5) then
        write (cout,1115)
        call putmsg(cout,80,17,0)
        write (cout,1172) (pdata(k),k=1,3)
        call putmsg(cout,80,17,0)
        write (cout,1174) (pdata(k),k=4,6)
        call putmsg(cout,80,17,0)
        write (cout,1173) pdata(7)
        call putmsg(cout,80,17,0)
        write (cout,1175) pdata(8)
      else if (ityp.eq.6) then
        write (cout,1116)
        call putmsg(cout,80,17,0)
        write (cout,1172) (pdata(k),k=1,3)
        call putmsg(cout,80,17,0)
        write (cout,1174) (pdata(k),k=4,6)
        call putmsg(cout,80,17,0)
        ang = pdata(7) * RADIAN
        write (cout,1176) ang
        call putmsg(cout,80,17,0)
        write (cout,1175) pdata(8)
        call putmsg(cout,80,17,0)
        write (cout,1177) pdata(9)
      else if (ityp.eq.7) then
        write (cout,1117)
      else if (ityp.eq.8) then
        write (cout,1118)
        call putmsg(cout,80,17,0)
        write (cout,1172) (pdata(k),k=1,3)
        call putmsg(cout,80,17,0)
        write (cout,1174) (pdata(k),k=4,6)
        call putmsg(cout,80,17,0)
        write (cout,1178) pdata(7)
        call putmsg(cout,80,17,0)
        write (cout,1179) pdata(8)
      end if
1110  format('Primitive Type     Unknown')
1111  format('Primitive Type     Freeform')
1112  format('Primitive Type     Ruled')
1113  format('Primitive Type     Plane')
1114  format('Primitive Type     Sphere')
1115  format('Primitive Type     Cylinder')
1116  format('Primitive Type     Cone')
1117  format('Primitive Type     Revolved')
1118  format('Primitive Type     Torus')
1170  format('normal =  <',3f11.4,'>')
1171  format('dis    = ',f11.4)
1172  format('center =  <',3f11.4,'>')
1173  format('radius = ',f11.4)
1174  format('axis   =  <',3f11.4,'>')
1175  format('height = ',f11.4) 
1176  format('angle  = ',f11.4,' degrees') 
1177  format('dis to top = ',f11.4) 
1178  format('major radius = ',f11.4)
1179  format('minor radius = ',f11.4)
      call putmsg(cout,80,17,0)
ccccccccccccccccccccccccccccccccccccccccccccc
c          don't allow user to display the canonical form
c          of a surface.
      if (debug) then
      call gtspan(nclkey, ispan, srfdat)
      write (cout,1120) typan, nopan
1120  format (7x,'panel type',i4,' num of patches: ',i4)
      call putmsg(cout,80,17,0)
      write(cout,1130) (r4pdat(k),k=1,5)
1130  format(10x,'parameters: ',5f10.5)
      call putmsg(cout,80,17,0)
      if(npats .gt. 5 ) then
         do 10 i=6,npats,5
c
c...Changed for RS6000. Paul
c...11/19/91
c
c          write(cout,1040) (r4pdat(k),k=i,min(npats,i+4))
c
           tmp = i+4
           write(cout,1040) (r4pdat(k),k=i,min(npats,tmp))
1140       format(22x,5f10.5)
10         call putmsg(cout,80,17,0)
      endif
      ipatch = 1
      if(typan .eq. 1) then
         num = 3
      else
         num = 7
      endif
      call gtsptt(nclkey, ispan, ipatch, srfdat,nwds)
      write(cout,1050) srfdat(1),srfdat(2),srfdat(3)
1050  format(10x,'x,y,z = ',3f12.5)
      call putmsg(cout,80,17,0)
      l = 3
      do 20 i=1,num
         write(cout,1060) i,r4pdat(l),r4pdat(i+1),r4pdat(l+2)
1060     format(10x,' indx x-del,y-del,z-del = ',i4,3f12.5)
         call putmsg(cout,80,17,0)
         l = l + 3 
20    continue

      endif     

99    return
      end
