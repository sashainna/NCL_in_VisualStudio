C*********************************************************************
C*    NAME         :  qsrfpn.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       qsrfpn.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:33
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine qsrfpn (u,v)
c*       this routine is called when a quilt srf p/n is reqd.       
c*       ipatch does the actual work.                               
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine qsrfpn (u,v)

      include 'com4a.com'
      include 'mocom.com'
c          motion common equivalences
      real*4 srf(10,3)
      equivalence (srf,s)
      real*8 asn
      real*4 ad(300),bsn(2)
      integer*4 srfkey
      integer*2 kd(600),ksn(4),ietype
      equivalence (d,ad,kd),(asn,bsn,ksn),(ifl(54),isrf)
c          set ptrs to srf in d-tbl
      idx=50*(isrf-1)
      jdx=2*idx
      kdx=4*idx
      igen=0
      imid=0
      if(iabs(kd(kdx+9)).lt.13)  ipat=kd(kdx+9)
      npats=kd(kdx+2)
c                  if pass start, force gensch
      if(ifl(49).eq.-3) ipat=0
c                     zero the used-tbl indicator
      kd(kdx+10)=0
      if(ipat.gt.0) goto 60
c                      gensch.  set iact=0 and get midpts in d(idx+11)
20    kd(kdx+9)=0
c      ipg=kd(kdx+5)
c      iel=kd(kdx+6)+2
c      if(iel.lt.36)goto 22
c      iel=iel-35
c      ipg=ipg+1
c22    call getent(d(idx+11),18,ipg,iel,0)
      call gtgeom(sc(isrf+143),d(idx+11),srfkey,nwds,ietype)
      jax=jdx+25
      dis=1.e9
      igen=1
      ipat=1
c                       pick pat midpt nearest ept
      do 25 i=1,npats
      dx=srf(8,isrf)-ad(jax)
      dy=srf(9,isrf)-ad(jax+1)
      dz=srf(10,isrf)-ad(jax+2)
      dissq=dx**2+dy**2+dz**2
      jax=jax+3
      if(dissq.ge.dis) goto 25
      dis=dissq
      ipat=i
25    continue
c                  see if this patch already tried.
30    nused=kd(kdx+10)
      do 32 i=1,nused
      kux=kdx+28+i
      if(kd(kux).eq.ipat) goto 70
32    continue
c                  get patch(ipat) into d(idx+11)
c                   point to patch in ranfil
c40    ipg=kd(kdx+5)
c      iel=kd(kdx+6)+20+40*(ipat-1)
c                   advance ipg, decrease iel by 35 until .lt. 36
c42    if(iel.lt.36)goto 44
c      iel=iel-35
c      ipg=ipg+1
c      goto 42
c44    continue
c      call getent(d(idx+11),40,ipg,iel,0)
      call gtqpat(srfkey,ipat,d(idx+11))
      kd(kdx+9)=ipat
      u=.5
      v=.5
      imid=1
c                  record this pat in used tbl  (to avoid fut repeat)
60    nused=kd(kdx+10)+1
      kd(kdx+10)=nused
      kd(kdx+nused+28)=ipat
605   call ipatch(u,v,iret)
      if(iret.eq.0) goto 99
      if(iret.ne.3) goto 62
c                  ipatch failed.  try recovery from midpt and gensch.
c                   if still ng, set errflg and exit.
      if(imid.ne.0) goto 61
      if(ifl(2).eq.128) ifl(2)=0
      imid=1
      u=.5
      v=.5
      goto 605
61    if(igen.eq.0)goto 20
      ifl(2)=128
      goto 99
c                 iret      isid exceeded
c                  -1         1       (u=0)
c                  +1         2       (u=1)
c                  -2         3       (v=0)
c                  +2         4       (v=1)
62    isid=-1-2*iret
      if(iret.gt.0) isid=2*iret
c                  see if any patch indicated. (bdy info is in d(+21)
      ksid=kd(kdx+40+isid)
      if(ksid.eq.0) goto 70
      ipat=ksid
      goto 30
c                   if gensch not tried, do it now
70    if(igen.eq.0) goto 20
c                   must exit.  everything has been tried and tool is
c                     apparently on apron.
99    return
      end
