C*********************************************************************
C*    NAME         :  putent.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       putent.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:32
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine putent (buf,nwds,pg,el,redef,it)
c*        last revision: to change the test for spanned geometry records. 
c*            if putent is called with only a partial amount of the words   
c*            to be written, as it can be done from dbget, this test is     
c*            necessary under certain conditions.                           
c*                                                                   
c*       This routine stores any number of 8 byte words in a 
c*       geometry type record in the ranfile.                          
C*    PARAMETERS   
C*       INPUT  : 
c*           buf  - (r8) name of area containing 8 byte  
c*                   words to be stored in the ranfile.   
c*           nwds - (i2) number of 8 byte words to be    
c*                   stored.                              
C*       OUTPUT :  
c*           pg   - (i2) the beginning page (record)     
c*                   where the words are stored in the    
c*                   ranfile.                             
c*           el   - (i2) the element number within the   
c*                   ranfile page that contains the first 
c*                   word.                                
c*           redef - (logical) if true the page and       
c*                   element passed in 'pg' and 'el' will 
c*                   be used instead of the current       
c*                   values of ifl(4), ifl(7) and ifl(8). 
c*                   this allows a restore of words where 
c*                   they were originally stored in the   
c*                   ranfile.  this option is used by     
c*                   callit and termac.                   
c*                                                                   
c*                   it    - (i2)  item type for refsys          
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine putent (buf,nwds,pg,el,redef,it)

      include 'com8a.com'

      integer*2 pg,el,it
      real*8 ranbuf(35),buf(35)
      logical first,redef,get
      equivalence (ranbuf,jb)

      if (debug) then
          write (6,9020) (ranbuf(i),i=1,35)
9020      format (' putent entry: ranbuf='/,7(5(f12.6)/))
          write (cout,9010) nwds,pg,el
9010      format ('putent nwds=',i3,' pg=',i3,' el=',i2)
          call putmsg (cout,80,16,0)
      endif
      if (ifl(72).eq.1.and.it.ne.0) call transf(buf,sc(56),nwds,it)
      get=.true.
      first=.true.
      inelm=0
      numwds=nwds
      if (redef) then
          ifl4sv=ifl(4)
          ifl7sv=ifl(7)
          ifl8sv=ifl(8)
          ifl(4)=pg
          ifl(7)=pg
          ifl(8)=el-1
          go to 1000
      endif
      if (ifl(8) .eq. 0) get=.false.
      if (numwds+ifl(8).gt.35) then
c                  if the next ranfile record is not the one immediately
c                  after the current geometry record, start a new
c                  geometry record.  spanned geometry data must be on
c                  contiguous records.
          if (ifl(4).ne.ifl(7)) go to 5000
      else
          go to 2000
      endif
1000  if (numwds+ifl(8).gt.35) then
          max=35
          go to 3000
      endif
2000  max=ifl(8)+numwds
3000  if (get) call getran (ranbuf,ifl(7))
      do 4000 i=ifl(8)+1,max
          inelm=inelm+1
4000      ranbuf(i)=buf(inelm)
      call putran (ranbuf,ifl(7))
      if (first) then
          pg=ifl(7)
          el=ifl(8)+1
          first=.false.
      endif
      numwds=numwds-(max-ifl(8))
      if (.not.(numwds .gt. 0)) go to 6000
5000      ifl(4)=ifl(4)+1
          ifl(7)=ifl(4)
          ifl(8)=0
          if (.not.redef) get=.false.
          go to 1000
6000  continue
      ifl(8)=max
      if (ifl(8).eq.35) then
          ifl(4)=ifl(4)+1
          ifl(7)=ifl(4)
          ifl(8)=0
      endif
      if (redef) then
          ifl(4)=ifl4sv
          ifl(7)=ifl7sv
          ifl(8)=ifl8sv
      endif
      if (debug) then
          write (6,9030) (ranbuf(i),i=1,35)
9030      format (' putent exit: ranbuf='/,7(5(f12.6)/))
          cout=' '
          call putmsg (cout,80,1,0)
      endif

      return
      end
