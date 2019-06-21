C*********************************************************************
C*    NAME         :  conent.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       conent.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:43
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine conent (rd,rmx,ietype)
C*     does the actual conversion through a matrix 
C*     of points and vectors.  points are always in real*8 form.     
C*     vectors can be in real*8 form or real*4 form.  if they are in 
C*     real*4 form, they are contained in 2 real*8's, either right  
C*     or left justified.                                          
C*                                                                   
C*        rd      data to be transformed                            
C*        rmx     matrix to be used                                
C*        ietype  item type                                       
C*                    3   point                                     
C*                    4   vector in real*8 form                    
C*                    41  vector in real*4 form, left justified    
C*                    42  vector in real*4 form, right justified  
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
      subroutine conent (rd, rmx, ietype)

      include 'com4a.com'

      integer*2 ietype
      real*4 t4(4),tt(4)
      real*8 t8(2)
      equivalence(t4,t8)
      real*8 rmx(15),rd(3),t(3)
      equivalence(t,tt)

      ix=0
      if(ietype .eq. 42) ix=1
      t8(1)=rd(1)
      t8(2)=rd(2)
      jj=1
      if (ietype .ne. 3 .and. ietype .ne. 4) go to 100
c                                                        ****** point
          do 10,j=1,3
              t(j)=rd(1)*rmx(jj)+rd(2)*rmx(jj+1)+rd(3)*rmx(jj+2)
              if (ietype .eq. 3) t(j)=t(j)+rmx(jj+3)
              jj=jj+4
10        continue
          go to 88888
c                                                        ****** vector
100       do 200,j=1,3
              tt(j+ix)=t4(1+ix)*rmx(jj)+t4(2+ix)*rmx(jj+1)+
     1                 t4(3+ix)*rmx(jj+2)
              jj=jj+4
200       continue
      if (ix.eq.0) tt(4)=t4(4)
      if (ix.eq.1) tt(1)=t4(1)
88888 rd(1)=t(1)
      rd(2)=t(2)
      if (ietype .eq. 3 .or. ietype .eq. 4) rd(3)=t(3)
99999 return
      end
