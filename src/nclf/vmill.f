C**********************************************************************
C*    NAME         :  volumill.f
C*       CONTAINS:
C*          vmill2  vmpout  vmptrn
C*    COPYRIGHT 2011 (c) NCCS Inc.  All Rights Reserved. 
C*     MODULE NAME AND RELEASE LEVEL
C*      vmill.f , 25.1
C*     DATE AND TIME OF LAST MODIFICATION
C*      04/29/15 , 15:10:53
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine vmill2
C*       Execute VoluMill 2D pocket motion. 
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
      subroutine vmill2
c
      include 'com8a.com'
      include 'comgt.com'
      include 'cutter.com'
      include 'mocom.com'
      include 'rrdm.com'
      include 'vmpcom.com'
c
      integer*2 lnk,iclass,isubcl,jptk,ntk,itmp(4)
      integer*4 i,j,npts,npas,ntype
c
      real*8 cnv,frate,fdprev,buf(14),tmp8,dx,dy,dz,zdpt,pte(6),ptl(9),
     1       pts(9),ptt(9),tdat(420),cir(7),speed,sprev,pt1(9),pt2(9),
     2       ci1(7)
c
      equivalence(ntk,ifl(79))
c      
      real*8 FEEDR
      equivalence (FEEDR,MOTMAP(24))
c       
      equivalence (itmp,tmp8)
c
      logical lcoupl
c      
c...Initialize routine
c
      cnv = 1.0d0
      if (ifl(264).eq.1) cnv = 25.4d0
      lcoupl = .false. 
      ier = 0
      ntk  = 0
      jptk = 0
      lnk  = 1
      iclass = 5000
      isubcl = 5
      fdprev = 0.0
      sprev = CRPM
      speed = CRPM
c
c.....Make sure step up height is in inches - ASF 2/10/14.
c
      buf(4) = 0.
      buf(5) = 0.
      buf(6) = 1.
c
c...Create pocket motion
c
      if (wmeth.le.1) then
          call nclf_vmill_pocket(isc10(3),rsvasw,rsvtok,rsvsub,ier)
      else if (wmeth.eq.2) then
          call nclf_vmill_pocket3(ier)
      endif
      if (ier .ne. 0) then
          ifl(2) = ier
          if (ifl(2) .gt. 0) goto 9990
      endif
c
c...Output points to the cl file
c......Get the number of VoluMill records
c      
      call nclf_vm_clpath_getnpaths(npas)
      if (npas .eq. 0) go to 9530
c
c......Toolpath Start point
c
      call nclf_vm_clpath_getstartpt(pte,cnv)
c
c......Get the type of record and end point
c      
      do i = 1,npas
         call nclf_vm_clpath_gettype(i,ntype)
c
c......Feed rate record
c      
         if (ntype .eq. -1) then
            call nclf_vm_clpath_getfeed (i,frate,cnv)
c
c......Spindle speed record
c      
         else if (ntype .eq. 4) then
            call nclf_vm_clpath_getspindle (i,speed)
         endif
c
c......Output spindle speed
c
          if (sprev .ne. speed .and. speed .ne. 0.) then
              call vmpout (tdat,isubcl,speed,1,lnk,2000,1031)
              if (VMPSPD(4) .ne. 0.)
     1            call vmpout (tdat,isubcl,VMPSPD(4),1,lnk,2000,1010)
              sprev = speed
          endif
c
c......Standard clpoint
c
         if (ntype .eq. 0 .or. ntype .eq. 1) then
             call nclf_vm_clpath_getpt (i,pte,cnv)
c
c........Rapid move
c         
            if (ntype .eq. 0) then
                call vmpout (tdat,isubcl,frate,0,lnk,2000,5)
                call vctovc(pte,ptl)
                call vctovc(pte(4),ptl(4))
                fdprev = 0.0
c
c........Cutting move
c 
            else 
                if (fdprev .ne. frate) then
                    call vmpout (tdat,isubcl,frate,1,lnk,2000,1009)
                    fdprev = frate
                endif
                call vctovc(pte,ptl)
                call vctovc(pte(4),ptl(4))
            endif
c
c.........Output point
c
            call vmpout (tdat,isubcl,pte,jptk,lnk,5000,isubcl)
            call vctovc(pte,pts) 
            call vctovc(pte(4),pts(4))
         endif
c
c......Circular record needs to be output
c  
         if (ntype .eq. 2 .or. ntype .eq. 3)  then
            lcoupl = .false. 
            call nclf_vm_clpath_getarc(ntype,i,pts,ptl,cir,cnv)
            zdpt = pts(3) - ptl(3)
            if (zdpt .ne. 0) lcoupl = .true. 
c
c.........Output Feedrate
c 
            if (fdprev .ne. frate) then
                call vmpout (tdat,isubcl,frate,1,lnk,2000,1009)
                fdprev = frate
            endif

c                
c.........Helical motion
c        
            if (lcoupl) then
c
c.........Output starting point
c
                dx = pte(1) - pts(1)
                dy = pte(2) - pts(2)
                dz = pte(3) - pts(3)
                if (dx*dx + dy*dy + dz*dz .gt. 0.0001) then      
                    call vmpout (tdat,isubcl,pts,jptk,lnk,5000,isubcl)
                endif           
c
c.........Output COUPLE record
c
                itmp(1) = 1
                itmp(2) = 0
                itmp(3) = 0
                itmp(4) = 0    
                buf(1) = zdpt
                buf(2) = tmp8
                buf(3) = 0. 
                call vmpout (tdat,isubcl,buf,3,lnk,2000,1049)
c
c.........Output circular record
c
                ptt(1) = ptl(1)
                ptt(2) = ptl(2)
                ptt(3) = pts(3)
                
                do 100 j=1,9,1
                    pt1(j) = pts(j)
                    pt2(j) = ptt(j)
                    if (j .le. 7) ci1(j) = cir(j)
  100           continue
                if (IVMPMX) then
                    call transf (pt1,VMPMXI,3,POINT)
                    call transf (pt1(4),VMPMXI,3,VECTOR)
                    call transf (pt1(7),VMPMXI,3,VECTOR)
                    call transf (pt2,VMPMXI,3,POINT)
                    call transf (pt2(4),VMPMXI,3,VECTOR)
                    call transf (pt2(7),VMPMXI,3,VECTOR)
                    call transf (ci1,VMPMXI,3,POINT)
                    call transf (ci1(4),VMPMXI,3,VECTOR)
                endif
                call cidist(pt1,pt2,ci1)
c
c.........Output end point of helix
c
                call vmpout (tdat,isubcl,ptl,jptk,lnk,5000,isubcl)
c
c......Standard circular motion
c
            else
                call vmpout (tdat,isubcl,ptl,0,lnk,0,0)
                do 200 j=1,9,1
                    pt1(j) = pts(j)
                    pt2(j) = ptl(j)
                    if (j .le. 7) ci1(j) = cir(j)
  200           continue
                if (IVMPMX) then
                    call transf (pt1,VMPMXI,3,POINT)
                    call transf (pt1(4),VMPMXI,3,VECTOR)
                    call transf (pt1(7),VMPMXI,3,VECTOR)
                    call transf (pt2,VMPMXI,3,POINT)
                    call transf (pt2(4),VMPMXI,3,VECTOR)
                    call transf (pt2(7),VMPMXI,3,VECTOR)
                    call transf (ci1,VMPMXI,3,POINT)
                    call transf (ci1(4),VMPMXI,3,VECTOR)
                endif
                call cidist(pt1,pt2,ci1)
            endif
            call vctovc(ptl,pts) 
            call vctovc(ptl,pte)    
         endif    
c
c...end do i = 1,npas  
c
      enddo
   
      call vctovc(ptl,sc)
      call vctovc(ptl(4),sc(4))
      call unitvc(ptl(7),sc(7))
c
c.....Apply transformation if needed - ASF 1/29/14
c
      if (IVMPMX) then
          call transf (sc,VMPMXI,3,POINT)
          call transf (sc(4),VMPMXI,3,VECTOR)
          call transf (sc(7),VMPMXI,3,VECTOR)
      endif

      if (ifl(154).eq.0) call tdsply(lnk,ptl,jptk)
c
c...Flush clfile buffer
c
      call vmpout (tdat,isubcl,ptl,0,lnk,0,0)
c
c...Clean up VoluMill variables
c
      call nclf_vm_finish
c
c...End of routine
c
 8000 return
c
c...Could not generate pocket motion
c
9530  ifl(2) = 530
      go to 9990
9990  err = .true. 
      go to 8000
      end
C
C*********************************************************************
C*    E_SUBROUTINE : vmpout (gcldat,kcld,gpts,knpts,klin,kclass,ksubcl)
C*       Outputs VMPOCK clfile records.
C*    PARAMETERS
C*       INPUT  :
C*          gcldat  - Output buffer
C*          kcld    - Motion Subclass.  Should be initialize to 5
C*                    and then left alone.
C*          gpts    - Points to output
C*          knpts   - Number of points output to status window for
C*                    type 5000 records or number of reals to output
C*                    for type 2000 records.
C*          klin    - Status window line count. 
C*          kclass  - Class type (5000,2000).
C*          ksubcl  - Sub class for type 2000 records.
C*       OUTPUT :
C*          kcld    - Set to 6 for continuation record. 
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine vmpout (gcldat,kcld,gpts,knpts,klin,kclass,ksubcl)
c
      include 'com8a.com'
      include 'vmpcom.com'
c
      integer*2 knpts,klin,kclass,ksubcl,kcld
c
      real*8 gcldat(120),gpts(120)
c
      integer*2 i, n, numitm
      integer*2 ntk
      equivalence (ntk,ifl(79))
c
c...Initialize routine
c
      n = 3+ifl(82)*3
      max = 120
c
c...Non-Motion type record
c...Flush motion output
c
      if (kclass .ne. 5000) then
          if (ntk .ne. 0) then
              numitm = ntk / n
              call putcl (5000,kcld,numitm,gcldat)
              ntk = 0
              kcld = 5 
          endif
          if (kclass .ne. 0) then
              numitm = knpts + 1
              call putcl (kclass,ksubcl,numitm,gpts)
          endif
c
c...Motion record
c...Buffer points
c
      else
          if (ntk+n .gt. max) then
              numitm = ntk / n
              call putcl (kclass,kcld,numitm,gcldat)
              ntk = 0
              kcld = 6 
          endif
          do 100 i=1,n,1
              gcldat(ntk+i) = gpts(i)
  100     continue
          if (IVMPMX) then
              call transf (gcldat(ntk+1),VMPMXI,3,POINT)
              if (n .ge. 6) call transf (gcldat(ntk+4),VMPMXI,3,VECTOR)
          endif
          ntk = ntk + n
c
c......Update the status window
c
          if (ifl(154) .eq. 1 .or. knpts .eq. 1)
     1        call tdsply (klin,gcldat(ntk-n+1),knpts)
      endif
999   return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : vmptrn (gbuf,ktype)
c*    FUNCTION :  Transforms the VoluMill boundary geometry to the XY-plane.
C*    PARAMETERS   
C*       INPUT  : 
C*          gbuf   - Canonical data of geometry to transform.
C*          ktype  - Type of geometry being transformed.
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine vmptrn (gbuf,ktype)
c
      include 'com8a.com'
      include 'vmpcom.com'
c
      integer*2 ktype
c
      real*8 gbuf(*)
c
c...Transform the points if necessary
c
      if (IVMPMX) call transf (gbuf,VMPMX,3,ktype)
      return
      end
