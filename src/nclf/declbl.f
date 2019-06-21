C*********************************************************************
C*    NAME         :  declbl.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       declbl.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:52
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine declbl
C*      this routine handles label declarations
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
      subroutine declbl

      include 'com8a.com'

      integer*2 ktv(4),irest(4)
      equivalence (tv,ktv),(rest,irest)
      integer*4 isvtk4,lablin,ifl5,svlrec
      if (ist.eq.13) then
c                              existing label
          call getran(jb,ktv(1))
c
c...Get line number of existing label
c
          call nclf_src_line_to_rec (svll,svlrec)

          if (jb4(ktv(2)) .ne. svlrec) then
           call nclf_src_rec_to_line (jb4(ktv(2)),lablin)
           call nclf_src_rec_to_line (ifl4(5),ifl5)
			  if (lablin.ge.ifl5.and.lablin.lt.svll)then
				  call error(76)
				  goto 99999
			  else
              call nclf_src_line_to_rec (svll,jb4(ktv(2)))
           endif
           call putran(jb,ktv(1))
          endif
      else
          if (ivxsub.ne.0) then
             isvtk4=ivxsub
             ivxsub=0
             call vstchk
             if (ityp.ne.2 .or. ist.ne.14) then
               ityp=1
               call error(88)
               goto 99999
             endif
             if (isvtk4.gt.1000000) then
               call error(85)
               goto 99999
             endif
             ivxsub=isvtk4
             call vstchk
          endif
          j=1
          if (lbltbl.eq.0) then
c                     this is the first label in the program
              ifl(4)=ifl(4)+2
              ifl(7)=ifl(4)
              ifl(8)=1
              lbltbl=ifl(4)-1

c                  write blank records for next ranfil records
              do 90 ind=1,144
90                jb(ind)=0
              call putran(jb,lbltbl)
              call putran(jb,ifl(7))
              call getran(jb,lbltbl)
              i=lbltbl
              jb(142)=1
              jb(143)=0
          else
c                      search the linkage for next available spot
              i=lbltbl
100           call getran(jb,i)
              if (jb(142).ge.70) then
                  j=j+1
                  if (jb(143).eq.0) then
                      ifl(4)=ifl(4)+2
                      ifl(7)=ifl(4)
                      ifl(8)=1
                      jb(143)=ifl(4)-1
                      call putran(jb,i)

c                  write blank records for next ranfil records
                      do 110 ind=1,144
110                       jb(ind)=0
                      call putran(jb,ifl(4)-1)
                      call putran(jb,ifl(7))
                      call getran(jb,ifl(4)-1)
                      i=ifl(4)-1
                      jb(142)=1
                      jb(143)=0
                  else
                      i=jb(143)
                      go to 100
                  endif
              else
                  jb(142)=jb(142)+1
              endif
          endif
c                  store the label in the vst
          isvsub = ivxsub
          savid2=token2
          ifl(9)=ifl(11)
          ifl(10)=ifl(12)
          irest(1)=i
          irest(2)=jb(142)
          irest(3)=0
          irest(4)=13
          idst=13
          call nclf_src_line_to_rec (svll,jb4(jb(142)))
          call putran(jb,i)
          call vstore
      endif
c             save place marker (inx) in ifl(281) for auto name gen
c             on a line with a label.
      ifl(281) = inx

99999  return
       end
