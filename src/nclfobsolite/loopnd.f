C*********************************************************************
C*    NAME         :  loopnd.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       loopnd.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:14
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine loopnd
c*      This routine handles loopnd commands.
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
      subroutine loopnd(iwhich)

      include 'com8a.com'
      include 'status.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      character*(MAX_LEN) buf
      character*64 csc155,csc156,tname,lname,blank,c8buf
      real*8 ttol
      integer*4 cinx155, cinx156
      equivalence (sc155,csc155),(sc156,csc156)
      equivalence (inx155,cinx155),(inx156,cinx156)
      integer*2 iwhich
      integer*4 isv67,isv68,stnum,endnum,nxtnum,zero,tmp,nc,strlen1,
     1          curlin
      integer*2 lopend,undo,dosame,i,nloops
      integer*4 lsub, tsub,esub,ilin
c
      character*64 errlab
      integer*4 errsub
      equivalence (sc182,errlab)
      equivalence (inx182,errsub)
c
      data lopend /1/, undo /2/, dosame /3/, zero /-1/
      integer*2 noloop, declar,exloop,exdo,m1
      data noloop /0/, declar /1/, exloop /2/, exdo /3/, m1 /-1/
      data blank /'        '/

c *****************************************************************
c         Loop is being defined.
c *****************************************************************
      call nclf_src_line_to_rec (nline,curlin)
      if (ifl(45) .eq. declar) then
          if (iwhich .eq. lopend) then
              tname = 'LOOPST  '
              tmp = 0
              call lrupdt (tname,tmp,blank,tmp,zero,zero,curlin,
     x               m1,m1,blank, tmp)
          else
              tname = csc155
              tsub = cinx155
              lname = csc156
              lsub = cinx156
              tmp = 0
              call lrupdt (tname,tsub,lname,lsub,zero,zero,curlin,
     x              m1,m1,blank, tmp)
          endif
          pgmode(pgindx) = 4
          ifl4(6) = curlin
cc          ifl4(6) = nline
          call putw2(nline)
c                   if loopnd
          if (iwhich .eq. lopend) then
             if (fromt) then
                irctyp = irctyp + 20
                call putsrc
             else
                call nclf_getw2 (ifl(123),w2,ncw2,i)
                i = i + 20
                call wrtpp(w2,ncw2,nline-1,i,0)
                nline=svll+ifl(123)
             endif
          else
             call putsrc
          endif
          fromt=.false.
          ifl(47)=1
          if (iwhich .eq. lopend) then
               ifl4(8) = curlin
cc               ifl4(8) = nline
               ifl(45) = exloop
          else
               ifl(45) = exdo
          endif
c                if its a loopnd statement, save nline
c          if (iwhich.eq.1) ifl(289)=svll
          ifl(152)=4
          if (pmode.lt.nline) pmode=nline
          call lrdata (tname, tsub,lname, lsub, stnum, ifl4(9), 
     x           endnum,i,i,c8buf, esub, 1)
          call nclf_src_rec_to_line (ifl4(9),ifl4(10))
c
c.....Stop the program if *RUN/LOOPST is set
c
        if (ifl(391).eq.3) then
          pmode = 0
          ifl(391) = 0
          cout = '***STOPPING AT LOOP START***'
          call putmsg(cout,80,nline+1,0)
c
c.....Set nline back to start of loop so the loop can be executed
c.....after the stop is made
c          
          if (ifl4(10).ne.0) nline = ifl4(10)
c
c.....Enter command mode if *SET/RUNCMD is set
c     
          if (ifl(392).eq.1)  call ncl_cmd_mode()
        endif

c *****************************************************************
c         Loop is being executed.
c *****************************************************************
      else if (ifl(45) .eq. exloop .or. ifl(45) .eq. exdo) then
          nloops = ifl(286)
          if (iwhich .eq. lopend .and. pgindx .gt. 1) 
     1            pgindx = pgindx - 1
c
c...Executing DO Loop
c
          if (ifl(45) .eq. exdo) then
c
c...Last line of a DO Loop
c
              if (iwhich .ne. undo) then
                 token2 = csc156
                 ivxsub = cinx156
                 call vstchk
                 keyold = keyhld
                 istold = ist
                 tv=tv + sc(158)
c
c......Loop is not over
c......Do it again
c
                 ttol = sc(170)
                 if (sc(169) .lt. 9.2499) ttol = 1.d-10
                 if (sc(158).gt.0.and.tv.le.sc(157)+ttol.or.
     1               sc(158).lt.0.and.tv.ge.sc(157)-ttol) then
c
cc                  commented to avoid double printing of contin. kathy
                     call putw2(nline)
                     ifl(9)=ifl(11)
                     ifl(10)=ifl(12)
                     savid2 = csc156
                     isvsub = cinx156
                     rest = tv
                     idst = 2
                     call vstore
                     call lrdata (tname,tsub,lname,lsub,stnum,ifl4(9),
     1                       endnum,i,i,c8buf,esub,1)
                     call nclf_src_rec_to_line (ifl4(9),ifl4(10))
                     go to 99999
                 endif
              endif
c                               the loop's over. see if it's a
c                               nested do loop.  if it is, restore
c                               the looping environment to the next
c                               outer level.
              if (pgindx .gt. 1) pgindx = pgindx - 1
              ifl(286)=ifl(286)-1
              call nclf_src_rec_to_line (ifl4(5),isv67)
              isv67 = isv67 - 1
cc              isv67 = ifl4(5) - 1
c
c...Save the ending line of the
c...current loop
c...Bobby  -  6/27/91
c
              call nclf_src_rec_to_line (ifl4(6),isv68)
              call lrpop
              if (ifl(286).gt.0) then
                  call getran(jb,ifl(287))
                  if (ifl(286).gt.1) then
                      jtemp1 = ifl(286)
100                   jtemp2 = jbr(19)
                      jtemp1 = jtemp1 - 1
                      call getran(jb,jtemp2)
                      if (jtemp1.gt.1) go to 100
                  endif
c                       see if the current loop and the next outer
c                       loop had the same terminating label.  if so,
c                       tell driver so it can call loopnd again.
                  if (csc155.eq.jbc(1:64)) iwhich = dosame
                  csc155=jbc(1:64)
                  csc156=jbc(65:128)
                  sc(157)=jbr(17)
                  sc(158)=jbr(18)
                  cinx155 = jb4(53)
                  cinx156 = jb4(54)            
c                  rtemp=jbr(ixtemp+4)
c                  ifl(67)=itemp(1)
c                  if (iwhich.eq.2) ifl(146) = ifl(68)+1
c                  ifl(68)=itemp(2)
c                  ifl(46)=itemp(3)
c                  go to 99999
              else
                  csc155=' '
                  call putw2(nline)
c                  if (iwhich.eq.2) ifl(146) = ifl(68)+1
c                       restore looping region in case of macro  epm  2-1-89
c                  if (svnam .ne. ' ') then
c                      token=svnam
c                      namsub(4) = xffff
c                      call vstchk
c                         If ktv(1) is zero, we must be at the top level MACRO
c                         so we should get the looping range numbers for it 
c                         ifl(295/296).
c                      if (ktv(1) .eq. 0) then
c                          ifl(67)=ifl(295)
c                          ifl(68)=ifl(296)
c                          limset = .true.
c                      else
c                          call getent (tv, 1, ifl(11), ifl(12), 0)
c                          ifl(67) = ktv(1)
c                          ifl(68) = ktv(3)
c                          limset = .true.
c                      endif
c                  else
c                      ifl(67) = 0
c                      ifl(68) = 0
c                      limset = .true.
c                  endif
              endif
              if (iwhich .eq. undo) then
                call nclf_src_rec_to_line (ifl4(6),ilin)
                call ifjump(ilin,i)
                if (i.gt.0) then
                  call error(509)
                  goto 99999
                endif
              endif
              call lrdata (tname,tsub,lname,lsub,ifl4(5),ifl4(9),
     1               ifl4(6),ifl(254),ifl(255),errlab,esub,1)
c
c...Changed 'ifl(68)' to 'isv68'
c...to reflect the next line of the
c...terminated loop instead of the
c...next line of the previous loop/macro
c...For UNDO command only
c...Bobby  -  6/27/91
c
              if (iwhich .eq. undo) ifl4(10) = isv68 + 1
              call getsrc (buf,nc,isv67,0)
              if (irctyp .ge. 20) irctyp = irctyp - 20
              call wrtpp (buf,nc,isv67,irctyp,0)
c
c...Executing loop
c
          else
              call nclf_getw2 (ifl(123),w2,nc,itp)
              if (itp .ge. 20 .and. itp .lt. 43) then
                  itp = itp - 20
                  call nclf_putw2 (ifl(123),w2,nc,itp)
                  call wrtpp (w2,nc,nline-1,itp,0)
              endif
              call lrpop
              call putw2(nline)
              ifl(283)=0  
c              ifl(67) = 0
c              ifl(68) = 0
          endif
c
c...Clean up looping region pointers
c
          if (ifl(286) .eq. 0) then
              if (ifl(283).eq.1) then
c                this do loop was called from within a loopst/loopnd
                  ifl(45) = exloop
                  call lrdata (tname,tsub,lname,lsub,stnum,nxtnum,
     1                  endnum,ifl(254),ifl(255),errlab,esub,1)
c
c...IFL(288) and IFL(289)
c...have not been used for some time
c...Bobby  -  6/23/95
c
cc                  ifl(67)=ifl(288)
cc                  ifl(68)=ifl(289)
                  ifl4(5)=stnum
                  ifl4(6)=endnum
              else
c
c.....Added for temporary variables in loops - ASF 12/31/13
c
                  if (ifl(38).eq.0) curmac = ' '
                  ifl(45) = noloop
                  ifl(152)=0
                  ifl(47)=1
               endif
          endif
      else
          call error(78)
          go to 99999
      endif

c
c.....Stop the program if *RUN/LOOPND is set
c
99999 if (ifl(391).eq.4.and.(ifl(45).eq.0.or.nloops.gt.ifl(286))) then
        pmode = 0
        ifl(391) = 0
        cout = '***STOPPING AT LOOP END***'
        call putmsg(cout,80,nline,0)
c
c.....Enter command mode if *SET/RUNCMD is set
c     
        if (ifl(392).eq.1) then
          svll = svll + 1
          nline = nline + 1
          ifl(47) = 0
          call ncl_cmd_mode()
        endif
      endif
      return
      end
