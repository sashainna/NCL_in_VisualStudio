C*********************************************************************
C*    NAME         :  loopst.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       loopst.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:15
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine loopst
C*      This routine handles a loopst command.
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
      subroutine loopst

      include 'com8a.com'
      include 'status.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      character*64 csc155,csc156,ssc155,ssc156,csc159,lname,blank
      integer*4 cinx155, cinx156, cinx159, sinx155, sinx156, sinx159

      equivalence (sc155,csc155),(sc156,csc156),(sc159,csc159)
      equivalence (inx155,cinx155),(inx156,cinx156),(inx159,cinx159)
      integer*2 ipos
      integer*2 jtemp1,jtemp2
      integer*4 nxtlin,endlin,curlin
      real*8 ssc157,ssc158
 
      data lname /'LOOPST'/
      data blank /'      '/
      data endln /-1/

      ifl(44)=9
c
c...Save the number of the first executable statement
c...of the looping region
c
      call nclf_src_line_to_rec (nline+1,nxtlin)
      call nclf_src_line_to_rec (svll,curlin)
      endlin = -1
c
c...DO Loop
c
      if (ist.eq.844) then
c                   if loop table pointer is zero, init it.
          if (ifl(287).eq.0) then
              ifl(4) = ifl(4) + 1
              ifl(287) = ifl(4) 
          endif
          if (nextyp.ne.5) then
              isvinx=inx
              call error(22)
              go to 99999
          endif
c
c......DO/Label
c
c
c...   parse the label in the DO/ statement with a : if LANG = CADRA
c
          if (ifl(374) .ne. 0) ifl(376)=1
          call parsit
          ifl(376)=0
c               this should be the terminating label
c               the label can be a vocab word or variable if lang=cadra
          if (ityp.eq.3.or.(ityp.eq.2.and.(ist.eq.1.or.
     1          ist.eq.13)).or.
     2          (ifl(374) .ne. 0 .and.(ityp.eq.1.or.ityp.eq.2.))) then
c
c...       if lang= cadra then add a : to the end of the label
c
              if (ifl(374) .ne. 0) then
                ipos = 1
   10           if (token2(ipos:ipos).eq.' '.or.ipos.eq.64) then
                  token2(ipos:ipos) = ':'
                  goto 20
                else
                  ipos = ipos + 1
                  goto 10
                endif
   20           continue 
              endif
              ssc155=token2
              sinx155 = IVXSUB
          else
              call error(82)
              goto 99999
          endif
          ifl(44)=1
c
c......DO/Label,Index=
c
          call parsit
          if (ityp.ne.2.or.(ist.ne.2.and.ist.ne.1)) then
              call error(282)
              go to 99999
          endif
          savid2 = token2
          isvsub = ivxsub
          ssc156 = token2
          sinx156 = ivxsub
          keyold = keyhld
          istold = ist
          ifl(9) = ifl(11)
          ifl(10) = ifl(12)
          idst = 2
          if (nextyp.ne.1) then
c                        equal sign expected
               call error(6)
               go to 99999
          endif
          ifl(44)=9
c
c......DO/Label,Index=start
c
          call parsit
          if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.
     1             ityp.eq.4) then
              rest = tv
              call vstore
              call parsit
          else
              call error(7)
              goto 99999
          endif
c
c......DO/Label,Index=start,end
c
          if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3
     1            .or.ityp.eq.4) then
              ssc157 = tv
          else
              call error(53)
              goto 99999
          endif
c
c......DO/Label,Index=start,end[,INCR]
c
          if (nextyp.ne.11) then
              call parsit
              if (ityp.eq.1.and.ist.eq.66) then
c                               *** incr
                  call parsit
              endif
              if (ityp.eq.2.and.ist.eq.2.or.ityp.eq.3
     1                .or.ityp.eq.4) then
                  if (tv.eq.0) then
                      call error(351)
                      go to 99999
                  endif
                  ssc158 = tv
              else
                  call error(7)
                  go to 99999
              endif
          else
c                       default incr is 1
              ssc158 = 1
          endif
          if (nextyp.ne.11) then
              call error(4)
              go to 99999
          endif
c                 the do statement syntax checked ok, now set up
c                 sc and ifl with the appropriate looping info. 
c                 if this is a recursive do loop, first save off 
c                 the current looping info in the looping table.
          if (ifl(286).gt.0) then
              call getran(jb,ifl(287))
              jtemp2 = ifl(287)
              if (ifl(286).gt.1) then
                  jtemp1 = ifl(286)
100               jtemp1 = jtemp1 - 1
                  if (jtemp1.eq.1) then
                     jbr(19) = ifl(4) + 1
                     ifl(4) = ifl(4) + 1
                     call putran (jb,jtemp2)
                  endif
                  jtemp2 = jbr(19)
                  if (jtemp1 .gt. 1) then
                      call getran(jb,jtemp2)
                      go to 100
                  endif
              endif
              jbc(1:64)=csc155
              jbc(65:128)=csc156
              jb4(53)= cinx155
              jb4(54)= cinx156              
              jbr(17)=sc(157)
              jbr(18)=sc(158)
              call putran(jb,jtemp2)
		else
c                  save looping region in case of macro  epm  2-1-89
              ifl4(11) = ifl4(5)
              ifl4(12) = ifl4(6)
          endif
c                  save the terminating label of the current do loop
c                  to check for do loop overlapping.
          csc159=csc155
          cinx159 = cinx155
          csc155=ssc155
          csc156=ssc156
          cinx155=sinx155
          cinx156=sinx156
          sc(157)=ssc157
          sc(158)=ssc158
          ifl(286)=ifl(286)+1
          call lrpush (csc155, cinx155, csc156, cinx156, curlin,
     x                 nxtlin,endlin)
          call nclf_getw2 (ifl(123),w2,ncw2,it)
          it = it + 20
          call wrtpp(w2,ncw2,nline-1,it,0)
c
c...LOOPST
c
      else 
          if (ifl(45).ne.0) then
              call error(77)
              go to 99999
          endif
          ifl(283)=1
          ifl(288)=svll
          tmp = 0
          call lrpush (lname, tmp, blank, tmp, curlin,nxtlin,endlin)
      endif
      if (nextyp.ne.11) then
          call error(4)
          go to 99999
      endif
      ifl(45)=1
      ifl(152)=3
      if (pgindx .lt. 30) pgindx = pgindx + 1
      pgmode(pgindx) = 3

c            the next executable line after do.
c      ifl(46)=svll+1
      ifl4(9) = nxtlin
      ifl4(5) = curlin
cc      ifl4(9) = nline + 1
cc      ifl4(5) = svll
99999 return
      end
