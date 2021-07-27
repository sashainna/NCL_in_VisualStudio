C*********************************************************************
C*    NAME         :  sgeo.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       sgeo.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       08/17/15 , 17:48:25
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine sgeo
c*       this routine displays the cannonical     
c*       form of the desired geometry item to the user in window 3.    
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
      subroutine sgeo

c      include '../incf/com8.com'
      include 'com.com'

      integer*2 i, j
      integer*2 ietype
      integer*4 nclkey,nc
      logical trflg
      character*13 type(4)
      equivalence(cout,type)
      integer*2 POCKET
      parameter (POCKET=22)

      cout=' '
      call expnm2 (token2, ivxsub, i, cout)
c
c...remove the macro prefix if have any
c
      call rmlab_prefix(cout, cout, i)
c...      nc     = strlen1(cout) + 6
c
c...fill the cout with space after token
c
      do 100 j=i+1,80
        cout(j:j) = ' '
100   continue
      nc = i + 6
      if (ityp.eq.3.or.ityp.eq.4) then
          cout='expression'
      else if (ist.eq.1) then
          call error(9)
          go to 99999
      else if (ist.eq.2) then
          cout(nc:)='scalar'
      else if (ist.eq.3) then
          cout(nc:)='point'
      else if (ist.eq.4) then
          cout(nc:)='vector'
      else if (ist.eq.5) then
          cout(nc:)='line'
      else if (ist.eq.6) then
          cout(nc:)='plane'
      else if (ist.eq.7) then
          cout(nc:)='circle'
      else if (ist.eq.8) then
          cout(nc:)='curve'
      else if (ist.eq.9) then
          cout(nc:)='surf'
      else if (ist.eq.10) then
          cout(nc:)='matrix'
      else if (ist.eq.11) then
          cout(nc:)='macro'
      else if (ist.eq.12) then
          cout(nc:)='macro param'
      else if (ist.eq.13) then
          cout(nc:)='label'
      else if (ist.eq.14) then
          cout(nc:)='reserve var'
      else if (ist.eq.18) then
          cout(nc:)='shape'
      else if (ist.eq.20) then
          cout(nc:)='patern'
      else if (ist.eq.21) then
          cout(nc:)='pntvec'
      else if (ist.eq.POCKET) then
          cout(nc:)='Pocket Motion'
      else if (ist.eq.VANOTE) then
          cout(nc:)='Annotation'
      else if (ist.eq.VSOLID) then
          cout(nc:)='Solid'
      else
          cout(nc:)='undefined'
      endif
      j=19
      call putmsg(cout,80,15,0)
      if (ityp.eq.3.or.ityp.eq.4) then
          write(cout,130) tv
130       format (10x,f18.7)
          call putmsg(cout,80,16,0)
          j=17
          go to 890
      endif
      if ((ist.gt.2.and.ist.lt.8).or.ist.eq.10.or.ist.eq.21) then
          trflg = .true.
          call gtentt(tv, trflg, nclkey, ietype, sc(10))
      endif
      cout=' '
      if (ifl(72).ne.0 .and. ist .ne. 2 .and. ist .ne. VANOTE) then
          write (cout,1010)
1010      format ('refsys/on')
          call putmsg (cout,80,16,0)
      endif
      if (ist.ge.2.and.ist.le.7 .or. ist.eq.10 .or. ist.eq.21) then
          call sgeo1 (j)

c                                                    **** curve
      else if (ist.eq.8) then
          call scurve (j)

c                                                    **** surface
      else if (ist.eq.9) then
          call ssurf (j)

c                                                    **** patern
      else if (ist.eq.20) then
          call showpn(j)
c                                                    **** annotation
      else if (ist.eq.VANOTE) then
          call showan(j)
c                                                    **** solid
      else if (ist.eq.VSOLID) then
          call showso(j)

      else
          j=16
      endif

c          blank out the remaining lines on the crt
c
c...Added check for NCL-VT mode
c...Paul  -  10/3/91
c...Old version was:
c   890   if (.not.ksr.and.ifl(35).eq.0) call ersw3 (j,1)
c
890   if (.not.ksr .and.
     *(ifl(35) .eq. 0 .or. ifl(35) .eq. 2)) call ersw3 (j,1)

99999 return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine expnm2 (a1,isub, l, a2)
C*       Receive a name in argument a1 & subscript in arg isub
C*       convert it to all characters & return in argument a2
C*       with length in argument l
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
      subroutine expnm2 (a1,isub,l,a2)
      include 'com8a.com'

      character*(*) a1,a2
      integer*4 isub
      integer*2 l

      character*6 ctemp
      character*1 ctemp1(6)
      equivalence (ctemp,ctemp1)

      do 100 l=1,64
        if (a1(l:l).eq.' ') goto 110
        a2(l:l)=a1(l:l)
100   continue

110   if (isub.eq.0) then
        l=l-1
      else
        a2(l:l)='('
        l=l+1
        write(ctemp,1000) isub
1000    format(i6)
        do 120 i=1,6
          if (ctemp1(i).ne.' ') then
            a2(l:l)=ctemp1(i)
            l=l+1
          endif
120     continue
        a2(l:l)=')'
      endif

      if (a2(1:1).eq.'@') then
        a2(1:1)='('
c
c...put ) after the last character
c
        l=l+1
        a2(l:l)=')'
      endif
      a2(l+1:) = ' '

99999 return
      end
