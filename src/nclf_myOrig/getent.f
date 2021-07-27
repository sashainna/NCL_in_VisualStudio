C*********************************************************************
C*    NAME         :  getent.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       getent.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:07
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getent (buf,nwds,pg,el,it)
C*     get any number of 8 byte words from a 
C*     geometry type record in the ranfile.                          
C*                                                                   
C*   calling variables: buf  - (r*8) name of area that 8 byte words  
C*                              are to be stored in.                 
C*                      nwds - (i*2) number of 8 byte words to be    
C*                              read from the ranfile record(s).     
C*                      pg   - (i*2) the beginning page (record)     
C*                              where the words are stored in the    
C*                              ranfile.
C*                      el   - (i*2) the element number within the   
C*                              ranfile page that contains the first 
C*                              word.                                
C*                                                                   
C*                      it   - (i*2) item type for refsys            
C*       description
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
      subroutine getent (buf,nwds,pg,el,it)

      include 'com8a.com'

      integer*2 page,elem,numwds,pg,el,nwds,it
      real*8 buf(35),r8jb(35)
      equivalence (ranbuf,r8jb(1),jb)
      if (debug) then
          write (cout,9010) pg,el,nwds,it
9010      format ('getent entry: pg=',i3,' el=',i2,' nwds=',i3,' it='i4)
          call putmsg (cout,80,17,0)
      endif
      page=pg
      elem=el
      numwds=nwds
      inelem=0
100   max=elem+numwds-1
      if (max .gt. 35) max=35
400   call getran (ranbuf,page)

c              move 'max' number of real*8 entities from ranfil input
c              buffer to requested area.
500   do 600 i=elem,max
          inelem=inelem+1
          buf(inelem)=r8jb(i)
600       continue
      numwds=numwds-(max-elem+1)
      if (numwds .le. 0) go to 700
      page=page+1
      elem=1
      go to 100

c          check for refsys
700   if (ifl(72).eq.1.and.it.ne.0) call transf(buf,sc(68),nwds,it)

      if (debug) then
          write (6,9020) (buf(i),i=1,35) 
9020      format (' getent exit: buf='/,7(5(f12.6)/)) 
          cout=' '
          call putmsg (cout,80,1,0)
      endif

      return
      end
