c*****************************************************
c**   NAME         :  draft2.f
c**   CONTAINS     :  Handles the DRAFT/... statement.
c**      drftvw  drftpnt  drftfit  drftcut  drftlab  parscc  drftdef
c**
c**     MODULE NAME AND RELEASE LEVEL
c**        draft2.f , 25.1
c**    DATE AND TIME OF LAST  MODIFICATION
c**        04/29/15 , 15:09:57
c*****************************************************
c**
c** COPYRIGHT (c) 1993 NCCS
c*********************************************************************
c*    E_SUBROUTINE     : subroutine drftvw  
c*     Handles the DRAFT/VIEW,... statement.
c*
c*    We call this routine from the DRAFT routine, file DRAFT.F
c*
c*********************************************************************

      subroutine  drftvw 

      include 'com4a.com'

c.....
      character*16 viewname
     
      integer*4 itracut, irefsys, imatrix, iparams, icenter, 
     *          inormal, iyaxis, iscale, itool,k,strlen1

      data itracut /812/, irefsys /644/, imatrix /610/, iparams /934/,
     *     icenter /634/, inormal /820/, iyaxis  /85/,  iscale  /25/,
     *     itool /617/

      integer*4 mxkey,ptkey,zveckey,upveckey,stat,fscale
      real*8    rsorig(3),rszvec(3),rsyvec(3),tmpmx(12),scale,fwd(3)
      integer*2 switch, nwds, ietype, option, status, ierr
      integer*2 i2v3, i2v4
      data i2v3 /3/, i2v4 /4/
c.....
c.....Set up some defaults:
c.....
      mxkey    = 0
      ptkey    = 0
      zveckey  = 0
      upveckey = 0
      scale    = 1.
      switch   = 0
      stat    = 0

c....
c....Loup through the statement to ensure the syntax is correct.
c....
      if (nextyp .ne. 1) then
         call error(6)
         return
      endif

      call parsit
      call parsit
c....
c.... NAMES... 
c....
      if (ITYP .ne. 1 .and. ITYP .ne. 2) then
         call error(87)
         return
      endif
      if (nextyp .ne. 9) then
         ITYP = 1
         call error(57)
         return
      endif
      viewname = token2 
c
c...why remove those before?
c...if we remove those statement, then, we should 
c...remove the following statement (10) after the k
c...assignment (because the k don't have value assigned
c...before test and it followed the do statement
c...Yurong
c...      do 1 k=1,15,1
c...         if(viewname(k:k) .eq. ' ') goto 10
c...    1 continue
c...10    if (k .eq. 1) then
c...         call error(434)
c...         return
c...      endif
c...      viewname(k:k) = char(0)
      k = strlen1(viewname)
      viewname(k+1:k+1) = char(0)

      call parsit
c....
c.... TRACUT
c....
      if (ITYP .eq. 1 .and. IST .eq. itracut) then
         option = 2
         call gtview(rsorig, rszvec, rsyvec, option, status)
         if (status .eq. 0) then
            switch = 1
            scale = sqrt(sc(41)*sc(41)+sc(42)*sc(42)+sc(43)*sc(43))
            ptkey = 1
            zveckey  = 1
            upveckey = 1
            fscale = 1
            goto 100
         else
            call error(437)
            return
         endif
      endif

c....
c.... REFSYS
c....
      if (ITYP .eq. 1 .and. IST .eq. irefsys) then 
         option = 1 
         call gtview(rsorig, rszvec, rsyvec, option, status) 
         if (status .eq. 0) then
            switch = 2 
            scale = sqrt(sc(56)*sc(56)+sc(57)*sc(57)+sc(58)*sc(58))
            ptkey = 1
            zveckey  = 1
            upveckey = 1
            fscale = 1
            goto 100 
         else
            call error(438)
            return
         endif
      endif

c....
c.... MATRIX
c....
      if (ITYP .eq. 1 .and. IST .eq. imatrix) then 
         if(nextyp .ne. 1) then
             call error(6)
             return
         endif
         call parsit
         call parsit
         if (ityp .ne. 2 .or. ist .ne. 10) then
             call error(94)
             return
         endif
         call gtdesc (tv, mxkey,nwds, ietype)
         call gtgeo(mxkey,tmpmx)
 
         rsorig(1) = tmpmx(4)
         rsorig(2) = tmpmx(8)
         rsorig(3) = tmpmx(12)
 
         rszvec(1) = tmpmx(3)
         rszvec(2) = tmpmx(7)
         rszvec(3) = tmpmx(11)
 
         rsyvec(1) = tmpmx(2)
         rsyvec(2) = tmpmx(6)
         rsyvec(3) = tmpmx(10)

         scale = sqrt(tmpmx(1)*tmpmx(1)+
     *                tmpmx(2)*tmpmx(2)+tmpmx(3)*tmpmx(3))

         switch = 3 
         ptkey = 1
         zveckey  = 1
         upveckey = 1
         fscale = 1
         goto 100 
      endif
c....
c....PARAMS
c....
      if (ITYP .eq. 1 .and. IST .eq. iparams) then
         if ((nextyp .ne. 9) .and. (nextyp.ne.11) ) then
            call error(57)
            return
         endif
  123    call parsit
         if (ITYP .eq. 7) then
            switch = 4 
            goto 100
         endif
         if(ITYP .eq. 1) then
           if (ist .eq. icenter) goto 1000
           if (ist .eq. inormal) goto 2000
           if (ist .eq. iyaxis ) goto 3000
           if (ist .eq. iscale ) goto 4000
           call error(436)
         endif
         call error(232)
         return
c....
c....CENTER
c....
 1000    if (nextyp .ne. 1) then
            call error(6)
            return
         endif
         call parsit
         call parscc (i2v3, rsorig, ierr)
         if (ierr .ne. 0) then
            call error(20)
            return
         endif
         ptkey = 1
         goto 123
c....
c....NORMAL
c....
 2000    if (nextyp .ne. 1) then
            call error(6)
            return
         endif
         call parsit
         call parscc (i2v4, rszvec, ierr)
         if (ierr .ne. 0) then
            call error(11)  
            return
         endif
         zveckey = 1
         goto 123 
c....
c....YAXIS 
c....
 3000    if (nextyp .ne. 1) then
            call error(6)
            return
         endif
         call parsit
         call parscc (i2v4, rsyvec, ierr)
         if (ierr .ne. 0) then
            call error(11)
            return
         endif
         upveckey = 1
         goto 123
c....
c....SCALE
c....
 4000    if (nextyp .ne. 1) then
            call error(6)
            return
         endif
         call parsit
         call parsit
         if (.not.scalar) then
            call error(282)
            return
         endif
         scale = tv
         fscale = 1
         goto 123
      endif
c
c...TOOL
c
      if (ITYP .eq. 1 .and. IST .eq. itool) then 
         option = 3
         call gtview(rsorig, rszvec, fwd, option, status) 
         call f_cross(rszvec,fwd,rsyvec)
         if (f_mag(rsyvec) .lt. .0001) call perpvc(rszvec,rsyvec)
         call unitvc(rsyvec,rsyvec)
         switch = 5 
         ptkey = 1
         zveckey  = 1
         upveckey = 1
         fscale = 1
         goto 100
      endif
      if(ITYP .eq. 7) then
        return
      else
        call error(232)
        return
      endif
c.....
c.....All syntax is ok
c.....
  100 continue
c
c....they because optional, so no error now.
c
c...      if (switch .eq. 4 .and. (ptkey.eq.0 .or.zveckey.eq.0 .or.
c...     *                    upveckey .eq.0 .or. scale.eq.0.0)) then
c...          call error(439)
c...          return
c...      endif
      call nclu_modify_view(viewname, ptkey, rsorig, zveckey, 
     *         rszvec, upveckey, rsyvec, fscale, scale, stat)
      if (stat .eq. -1) then
         call error(427)
         return
      endif
      return
      end

c*********************************************************************
c*    E_SUBROUTINE     : subroutine drftpnt
c*     Handles the DRAFT/REDRAW,... statement.
c*
c*    We call this routine from the DRAFT routine, file DRAFT.F
c*
c*********************************************************************

      subroutine  drftpnt

      include 'com4a.com'

      integer*2 iall, iview
 
      data iall /816/, iview /923/
 
      integer*4 viewkey,flag
c
      if (nextyp .ne. 9) then
         call error(57)
         return
      else
         call parsit
      endif
c....
c.... ALL 
c....
      if (ITYP .eq. 1 .and. IST .eq. iall) then            
         flag = -1
         go to 900
c....
c....VIEW
c....
      else if(ITYP .eq. 1 .and. IST .eq. iview) then
         if(nextyp .ne. 1) then
            call error(6)
            return
         endif
         call parsit
         call parsit
         if (ITYP .eq. 1 .or. (ITYP .eq. 2 .and. IST .eq. 1)) then
            flag = 0
            go to 900
         else if (ITYP .eq. 3) then
            flag = 1
            viewkey = itv
            go to 900
         else
            call error(87)
            return
         endif
      else if (ITYP .eq. 7) then
         return
      else   
         call error(436)
         return
      endif
c
c...Execute if not in batch
c
  900 if(ifl(35).eq.0.or.ifl(35).eq.2.and.ifl(350).gt.0) then
         if (flag .eq. 0) then
            call uvu_view_chk(token2,viewkey)
            if (viewkey .eq. 0) then
               call error(434)
               return
            endif
         end if
         call uv_repaint(flag,viewkey)
      end if
      return
      end

c*********************************************************************
c*    E_SUBROUTINE     : subroutine drftfit
c*     Handles the DRAFT/FIT,... statement.
c*
c*    We call this routine from the DRAFT routine, file DRAFT.F
c*
c*********************************************************************

      subroutine  drftfit

      include 'com4a.com'

      integer*2 iall, iview
 
      data iall /816/, iview /923/, isame /730/, iomit /172/
 
      integer*4 iviewkey(20),omviewkey(20), flag,nmcount,omcount,fsame,
     1          viewnum
c
      flag = -2
      nmcount = 0
      fsame = 0
      omcount = 0

      if (nextyp .ne. 9) then
         call error(57)
         return
      else
         call parsit
      endif
      if (ITYP .ne. 1) then
         call error(232)
         return
      endif
c....
c.... ALL 
c....
      if (ITYP .eq. 1 .and. IST .eq. iall) then            
         flag = -1
         if(nextyp .eq. 11) then
             goto 900
         else if (nextyp .eq. 9 ) then
             goto 500
         else
             call error(57)
             return
         endif
      else if(ITYP .eq. 1 .and. IST .eq. iview) then
         if(nextyp .ne. 1) then
           call error(6)
           return
         endif
c....
c....VIEW
c....
         call parsit
1150     call parsit
         if(ITYP .eq. 7) goto 900
         if (ITYP .eq. 3) then
            flag = 1
c
c.....we allow the VIEW name and number now, some view may not
c.....include in 'number', so we pass all view key instead of view number now
c.....to fitting function, so convert the number to the view key
c 
            viewnum = itv
            call uv_viewnum_key(viewnum, iviewkey(nmcount+1))
            nmcount = nmcount+1
            goto 1150
         else if (ITYP .eq. 1 .and. IST .eq. iall) then            
            flag = -1
            if(nextyp .eq. 11) then
               goto 900
            else if (nextyp .eq. 9 ) then
               goto 500
            else
               call error(57)
               return
            endif
         else if (ITYP .eq. 1 .or. (ITYP .eq. 2 .and. IST .eq. 1)) then
            if(ifl(35).eq.0.or.(ifl(35).eq.2.and.ifl(350).gt.0)) then
                call uvu_view_chk(token2,iviewkey(nmcount+1))
                if (iviewkey(nmcount+1) .eq. 0) then
                   if (ityp .eq. 1 ) then
                       goto 600 
                   else
                       call error(434)
                       return
                   endif 
                endif
                nmcount = nmcount+1
                flag = 0
                goto 1150
            else
                if (ityp .eq. 1 .and. (IST .eq. isame .or. 
     *                    IST .eq. iomit)) goto 600
                if (nextyp .ne. 9 .and. nextyp .ne. 11) then
                   call error(57)
                   return
                endif
                goto 1150
            endif
         endif
      else if (ITYP .eq. 7) then
         return
      else
         call error(436)
         return
      endif
c....
c....SAME  OMIT
c....or... it is a syntax error.
c....
  500 call parsit
c....
c.... SAME 
c....
  600 if (ITYP .eq. 1 .and. IST .eq. isame) then            
         fsame = 1
         go to 500
c....
c.... OMIT 
c....
      else if (ITYP .eq. 1 .and. IST .eq. iomit) then
         if(nextyp .ne. 1) then
           call error(6)
           return
         endif
c....
c....VIEW_list
c....
         call parsit
2150     call parsit
         if(ITYP .eq. 7) goto 900
         if (ITYP .eq. 1 .or. (ITYP .eq. 2 .and. IST .eq. 1)) then
            call uvu_view_chk(token2,omviewkey(omcount+1))
            if (omviewkey(omcount+1) .eq. 0) then
                call error(434)
                return
            endif
            omcount = omcount+1
            goto 2150
         else if (ITYP .eq. 3) then
c
c.....we allow the VIEW name and number now, also, some view may not
c.....include in number, so we pass all view key instead of view number now
c.....to fitting function, so convert the number to the view key
c 
            viewnum = itv
            call uv_viewnum_key(viewnum, omviewkey(omcount+1))
            omcount = omcount+1
            goto 2150
         else
            if (nextyp .ne. 9 .and. nextyp .ne. 11) then
                call error(57)
                return
            endif
            goto 2150
         endif 
      endif
c
c...Execute command if not batch
c
  900 if(ifl(35).eq.0.or.(ifl(35).eq.2.and.ifl(350).gt.0)) then
         if ((flag.eq.0).or.(flag.eq.1)) flag = 0
         call uv_fit(flag,iviewkey, nmcount, omviewkey, omcount, fsame)
      end if
      return
      end
c*********************************************************************
c*    E_SUBROUTINE     : subroutine drftcut
c*     Handles the DRAFT/CUTTER,... statement.
c*
c*    We call this routine from the DRAFT routine, file DRAFT.F
c*
c*********************************************************************

      subroutine  drftcut

      include 'com4a.com'

      integer*2 istep, icolor, ilintyp, irapid, itracut, ishank, inc,
     1          ihold

      integer*4 itransnum(3), istepnum, icolcut, icolmot, ilinmot,
     *          irapcolmot, iholdcol, iholdflg,
     *          iraplinmot, itracutflg, irapidflg, ishankcol, 
     x          ishankflg, indx, strlen1, isub

      integer*2  icolors(16)

      integer*2  isolid,  icenter, idashdt,  idash,   iphantm,
     *           idashsp, idotted, idashln
      character*256 comstr

      integer*2 ion, ioff,j

      data ion/71/, ioff/72/

      data  isolid   /123/,    icenter  /634/,    idashdt /918/,
     *      idash    /124/,    iphantm  /916/,   idashsp /919/,
     *      idotted  /125/,    idashln  /917/
c
c...BLACK WHITE BLUE    RED       GREEN  MAGNTA YELLOW CYAN   
c...BROWN TAN    LT-BLUE SEA-GREEN ORANGE PINK   PURPLE GREY
c
      data  icolors /904,905,906,907,908,909,910,911,
     *               912,913,914,915,1101,1102,1103,1104/
     
      data istep /92/, icolor /900/, ilintyp /131/, irapid /5/,
     *     itracut /812/, ishank /192/, itrans /1037/, ihold /157/
c....
c.... Set up some defaults...
c....
      istepnum   = -1  
      icolcut    = -1 
      icolmot    = -1
      ilinmot    = -1
      irapcolmot = -1
      iraplinmot = -1
      ishankcol  = -1
      iholdcol   = -1
      itracutflg = -1
      irapidflg  =  0
      ishankflg  =  0
      iholdflg   =  0
      itransnum(1) = -1
      itransnum(2) = -1
      itransnum(3) = -1

      if (nextyp .ne. 9) then
         call error(57)
         return
      endif

  200 call parsit   

      if(ITYP .eq. 1) then
         if(IST .eq. istep ) goto 1000
         if(IST .eq. icolor) goto 2000
         if(IST .eq. ilintyp)goto 3000
         if(IST .eq. irapid) goto 4000
         if(IST .eq. itracut)goto 5000
         if(IST .eq. ishank) goto 6000
         if(IST .eq. itrans) goto 7000
         if(IST .eq. ihold) goto 8000
         call error(436)
         return
      else if (ITYP .eq. 7) then
         goto 100
      else
         call error(232)
         return
      endif
c....
c....STEP
c....
 1000 if (nextyp .ne. 1) then
         call error(6)
         return
      endif
      call parsit
      call parsit
      if (ITYP .ne. 3 .or. tv .le. 0) then
         call error(224)
         return
      endif
      istepnum = tv 
      irapidflg = 0
      ishankflg = 0
      iholdflg  = 0
      goto 200
c
c...COLOR
c
 2000 if (nextyp .ne. 1) then
         call error(6)
         return
      endif
      call parsit
      call parsit
      if (ITYP .eq. 1 ) then
         inc = -1
         do 2100 i=1,16,1
             if (IST .eq. icolors(i)) inc = i - 1
 2100    continue
         if (inc .eq. -1) then
            call error(428)
            return
         endif
         if (irapidflg .eq. 1) then
             irapcolmot = inc
         else if (ishankflg .eq. 1) then
             ishankcol = inc
         else if (iholdflg .eq. 1) then
             iholdcol = inc
         else
             icolmot = inc
         endif
         
      else if ((ITYP .eq. 2 .and. ist.eq.1 ).or.
     x      (ITYP .eq. 2 .and. ist.eq.24 ).or.
     x       (ityp.eq.9)) then
         if ((ityp.eq.9).or.(ITYP.eq.2 .and. ist.eq.1)) then
             comstr = token2
             j = strlen1(token2)
             isub = ivxsub
         endif
         if (ityp.eq.2.and.ist.eq.24) then
             j = 0
             call gttext(comstr,j)
             isub = 0
         endif
         call ncl_getclr_inx(comstr, j, isub, indx)
         if (irapidflg .eq. 1) then
             irapcolmot = indx
         else if (ishankflg .eq. 1) then
             ishankcol = indx
         else if (iholdflg .eq. 1) then
             iholdcol = indx
         else
             icolmot = indx
         endif  
           
      else if (ITYP .eq. 3 ) then
         if (ITV .ge. 0 .and. ITV .le. 63) then
              if(irapidflg .eq. 1) then 
                   irapcolmot = ITV
              else if (ishankflg .eq. 1) then
                   ishankcol = ITV
              else if (iholdflg .eq. 1) then
                   iholdcol = ITV
              else 
                   icolmot  = ITV
              endif
         else
              call error(428)
              return
         endif
      else
         call error(428)
         return
      endif
      if (irapidflg .eq. 0 .and. ishankflg .eq. 0 .and. iholdflg .eq. 0)
     1        then
         if(nextyp .ne. 9) then
             call error(57)
             return
         endif
         call parsit
         if (ITYP .eq. 1 ) then
             inc = -1
             do 2200 i=1,16,1
                 if (IST .eq. icolors(i)) inc = i - 1
 2200        continue
             if (inc .eq. -1) then
                 if ((ITYP .eq. 2 .and. ist.eq.1 ).or.
     x              (ITYP .eq. 2 .and. ist.eq.24 ).or.
     x              (ityp.eq.9)) then
                     if ((ityp.eq.9).or.(ITYP.eq.2 .and. ist.eq.1)) then
                        comstr = token2
                        j = strlen1(token2)
                        isub = ivxsub
                     endif
                     if (ityp.eq.2.and.ist.eq.24) then
                        j = 0
                        call gttext(comstr,j)
                        isub = 0
                     endif
                     call ncl_getclr_inx(comstr, j, isub, indx)
                     if (indx .gt. 15 .and. indx .lt. 64) then
                        inc = indx
                     else
                        call error(428)
                        return
                     endif   
                 endif 
             endif
             icolcut = inc
          else if ((ITYP .eq. 2 .and. ist.eq.1 ).or.
     x      (ITYP .eq. 2 .and. ist.eq.24 ).or.
     x       (ityp.eq.9)) then
             if ((ityp.eq.9).or.(ITYP.eq.2 .and. ist.eq.1)) then
                 comstr = token2
                 j = strlen1(token2)
                 isub = ivxsub
             endif
             if (ityp.eq.2.and.ist.eq.24) then
                 j = 0
                 call gttext(comstr,j)
                 isub = 0
             endif
             call ncl_getclr_inx(comstr, j, isub, indx)
             icolcut = indx
         else if (ITYP .eq. 3 ) then
            if (ITV .ge. 0 .and. ITV .le. 63) then
                 icolcut  = ITV
            else
                 call error(428)
                 return
            endif
         else
            call error(428)
            return
         endif
      endif
      goto 200
c
c.....LINTYP
c
c
 3000 if (nextyp .ne. 1) then
         call error(6)
         return
      endif
      call parsit
      call parsit
      if (ITYP .eq. 1 ) then
         if (IST .eq.   isolid )       then
            if(irapidflg .eq. 0) ilinmot  = 0
            if(irapidflg .eq. 1) iraplinmot = 0
         else if (IST .eq.   idash  )  then
            if(irapidflg .eq. 0) ilinmot  = 1
            if(irapidflg .eq. 1) iraplinmot = 1
         else if (IST .eq.   idotted  )  then
            if(irapidflg .eq. 0) ilinmot  = 2
            if(irapidflg .eq. 1) iraplinmot = 2
         else if (IST .eq.   icenter)  then 
            if(irapidflg .eq. 0) ilinmot  = 3 
            if(irapidflg .eq. 1) iraplinmot = 3
         else if (IST .eq.   iphantm)  then 
            if(irapidflg .eq. 0) ilinmot  = 4 
            if(irapidflg .eq. 1) iraplinmot = 4
         else if (IST .eq.   idashln)  then 
            if(irapidflg .eq. 0) ilinmot  = 5 
            if(irapidflg .eq. 1) iraplinmot = 5
         else if (IST .eq.   idashdt)  then 
            if(irapidflg .eq. 0) ilinmot  = 6 
            if(irapidflg .eq. 1) iraplinmot = 6
         else if (IST .eq.   idashsp)  then 
            if(irapidflg .eq. 0) ilinmot  = 7 
            if(irapidflg .eq. 1) iraplinmot = 7
         endif
         if ((irapidflg .eq. 0 .and. ilinmot .lt. 0) .or.
     *       (irapidflg .eq. 1 .and. iraplinmot .lt. 0)) then
              call error(222)
              return
         endif
      else if (ITYP .eq. 3 ) then
         if (ITV .ge. 1 .and. ITV .le. 8) then
              if(irapidflg .eq. 0) ilinmot  = ITV - 1
            if(irapidflg .eq. 1) iraplinmot = ITV - 1
         else
              call error(222)
              return
         endif
      else
         call error(222)
         return
      endif
      ishankflg = 0
      iholdflg  = 0
      goto 200
c....
c....RAPID
c....
 4000 irapidflg = 1
      ishankflg = 0
      iholdflg  = 0
      goto 200
c....
c....TRACUT
c....
 5000 if (nextyp .ne. 1) then
         call error(6)
         return
      endif
      call parsit
      call parsit
      if (ITYP .eq. 1 .and. (IST .eq. ion .or. IST .eq. ioff)) then
         itracutflg = ion - IST + 1
      else
         call error(56)
         return
      endif
      irapidflg = 0
      ishankflg = 0
      iholdflg  = 0
      goto 200
c....
c....SHANK
c....
 6000 ishankflg = 1
      irapidflg = 0
      iholdflg  = 0
      goto 200
c....
c....TRANS: translucency
c....
 7000 if (nextyp .ne. 1) then
         call error(6)
         return
      endif
      call parsit
      call parsit
      if (ITYP .ne. 3 .or. tv .le. 0) then
         call error(224)
         return
      endif
      if (ishankflg .eq. 1) then
          itransnum(2) = tv 
      else if (iholdflg .eq. 1) then
          itransnum(3) = tv 
      else
          itransnum(1) = tv 
          itransnum(2) = tv 
          itransnum(3) = tv 
      endif
      goto 200
c
c...HOLDER
c
 8000 iholdflg  = 1
      irapidflg = 0
      ishankflg = 0
      goto 200
c....
c....All sintax is ok. 
c....
 100  continue
      call ncl_tool_attr1(itransnum, istepnum,icolcut,icolmot,ilinmot,
     *                    irapcolmot,iraplinmot,itracutflg,ishankcol,
     2                    iholdcol)
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine drftlab
C*      Handle DRAFT/LABEL statement.
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
      subroutine drftlab

      include 'com.com'

      integer*4 ichoice,labon
      integer*2 inxsv, nxtsv
      logical set
      integer*2 IALTER, ION, IOFF, IMODFY
      parameter (IALTER=951)
      parameter (ION=71)
      parameter (IOFF=72)
      parameter (IMODFY=732)
      parameter (ILEADER=1013)
      parameter (IBOX=340)
      parameter (ICOLOR=900)
      parameter (ISIZE=196)
      parameter (IOFFSET=666)
      labon =-1
      call parsit
      if (ityp.ne.1 .or. (ist.ne.IALTER .and. ist.ne.ION .and.
     1    ist.ne.IOFF .and. ist.ne.ICOLOR .and. ist.ne.ISIZE .and.
     2   ist.ne.ILEADER .and. ist.ne.IOFFSET .and. ist.ne.IBOX))then
        call error(61)
        goto 999
      endif
	
	
      if (ist.eq.ILEADER .or. ist.eq.ICOLOR .or. ist.eq.ISIZE.or.
     1    ist.eq.IBOX .or. ist.eq.IOFFSET) then
        call labatt(labon)
        return
      endif
      
      if (ist.eq.IALTER) then
        call labalt
      else
        ichoice = 1
        if (ist.eq.IOFF) ichoice = 2
        inxsv = inx
        nxtsv = nextyp
        call parsit
        if (ityp.eq.1 .and. ist.eq.IMODFY) then
          if (nextyp.ne.1) then
            call error(6)
            goto 999
          endif
          call parsit
          call labset(ichoice)
        else
	    if (ityp.eq.1 .and. (ist.eq.ILEADER .or. ist.eq.ICOLOR .or.
     1          ist.eq.ISIZE .or. ist.eq.IBOX .or. ist.eq.IOFFSET)) then
            if (nextyp.ne.1) then
	        labon =ichoice
              call labatt(labon)
            endif
          else
            inx = inxsv
            nextyp = nxtsv
            nxteos = nextyp.eq.11
            set = ichoice.eq.1
            call labmdl(set)
          endif
        endif
      endif

999   return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine parscc (it, buf, ierr)
C*      Parse the input line for a point or vector or 3 coordinates
C*    PARAMETERS
C*       INPUT  :
C*          it         - =3 point expected
C*                       =4 vector expected
C*       OUTPUT :
C*          buf        - corrdinates of point or vector
C*          ierr       - =0 no error else = 1
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine parscc (it, buf, ierr)

      include 'com.com'

      integer*2 it
      real*8 buf(3)
      integer*2 ierr

      real*8 pvbuf(6)
      integer*4 nclkey
      integer*2 nwds, ietype, i, ix
c
c...WinNT
c
cc      integer*2 PNTVEC
cc      parameter (PNTVEC=21)

      ierr = 0
      call parsit
      if (scalar) then
        buf(1) = tv
        call parsit
        if (.not.scalar) then
          ierr = 1
          goto 999
        endif
        buf(2) = tv
        buf(3) = 0.0
        if (.not.nxteos) then
          isvinx = inx
          call parsit
          if (.not.scalar) then
            inx = isvinx
            goto 999
          endif
          buf(3) = tv
        endif
        if (ifl(264).eq.1.and.it.eq.POINT) then
          buf(1) = buf(1) / 25.4d0
          buf(2) = buf(2) / 25.4d0
          buf(3) = buf(3) / 25.4d0
        endif
      else if (ityp.eq.2 .and. ist.eq.it) then
          call gtdesc(tv, nclkey, nwds, ietype)
          call gtgeo (nclkey, buf)
      else if (ityp.eq.2 .and. ist.eq.PNTVEC) then
          call gtdesc(tv, nclkey, nwds, ietype)
          call gtgeo (nclkey, pvbuf)
          ix = 0
          if (it.eq.VECTOR) ix = 3
          do 100 i=1,3
100       buf(i) = pvbuf(i+ix)
      else
        ierr = 1
      endif

999   return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine drftdef
C*      Handle DRAFT/DEFAULT statement.
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
      subroutine drftdef

      include 'com.com'
      
      integer*2  ipoint, ivector, iline, iplane, icircle, icurve, isurf,
     *           ishape, ipatern, ipt_vec, imatrix, isolid, imaxis,
     *           iwaxis
      integer*2  iwhite, ired,  iblue,  igreen,
     *           imagnta,  iyellow,icyan, ibrown, ilttan, iltblue,
     *           iseagrn, iorange, ipink, ipurple, igrey,icolor
      integer*2 idum,gtype,j
      integer*4 newcolor,stype,g4type, indx, strlen1, isub
      character*256 comstr

      data ipoint  /603/, ivector /604/, iline    /605/, iplane  /606/,
     *     icircle /607/, icurve  /608/, isurf    /609/, ishape  /602/,
     *     ipatern /636/, ipt_vec /613/, imatrix  /610/, isolid  /123/
     *     imaxis  /870/, iwaxis  /1096/
      data  icolor  /900/

      data  iwhite  /905/, ired   /907/,
     *      idefalt /903/,  iblue   /906/, igreen /908/,
     *      imagnta /909/,  iyellow /910/, icyan  /911/,
     *      ibrown  /912/,   ilttan  /913/,  iltblue/914/,
     *      iseagrn /915/,   iorange /1101/, ipink  /1102/,
     *      ipurple /1103/,  igrey   /1104/
      
      stype =0
      call parsit
c.....
c.....Only the following vocabulay words are allowed here:
c.....point,vector,line,plane,circle,curve,surf,shape,patern,pt_vec
c.....
      if (ITYP .ne. 1 .or. (IST .ne. ipoint .and. IST .ne. ivector .and. 
     x       IST .ne. iline    .and.    IST   .ne.   iplane  .and.
     x       IST .ne. icircle  .and.    IST   .ne.   icurve  .and. 
     x       IST .ne. isurf    .and.    IST   .ne.   ishape  .and.
     x       IST .ne. ipatern  .and.    IST   .ne.   ipt_vec .and. 
     x       IST .ne. imatrix  .and.    IST   .ne.   isolid  .and.
     x       IST .ne. imaxis  .and.     IST   .ne.   iwaxis)) then
           call error(61)
           goto 999
      endif

      if (ist .eq. imaxis) then
          gtype = -1
      else if (ist .eq. iwaxis) then
          gtype = -2
      else
          call vctoid (ist,gtype,idum)
      endif

      if(ityp .eq.1 .and. ist .eq. isurf) then
        call parsit
        if(tv .lt.0 .or. tv .gt.4) then
          call error(61)
          goto 999
        endif
        stype = tv
      else if(ityp .eq.1 .and. ist .eq. icurve) then
        call parsit
        if(tv .lt.0 .or. tv .gt.3) then
          call error(61)
          goto 999
        endif
        stype = tv
      endif

      call parsit
      if (ityp.ne.1 .or. ist.ne.ICOLOR) then
        call error(61)
        goto 999
      endif

      call parsit
      call parsit
      if (ITYP .eq. 1 ) then
         if (IST .eq.  idefalt)  newcolor = -1
         if (IST .eq.  iblack)  newcolor = 0
         if (IST .eq.  iwhite  )  newcolor = 1
         if (IST .eq.  iblue   )  newcolor = 2
         if (IST .eq.  ired    )  newcolor = 3
         if (IST .eq.  igreen  )  newcolor = 4   
         if (IST .eq.  imagnta )  newcolor = 5
         if (IST .eq.  iyellow )  newcolor = 6
         if (IST .eq.  icyan   )  newcolor = 7
         if (IST .eq.  ibrown  )  newcolor = 8
         if (IST .eq.  ilttan  )  newcolor = 9
         if (IST .eq.  iltblue )  newcolor = 10
         if (IST .eq.  iseagrn )  newcolor = 11
         if (IST .eq.  iorange )  newcolor = 12
         if (IST .eq.  ipink   )  newcolor = 13
         if (IST .eq.  ipurple )  newcolor = 14
         if (IST .eq.  igrey   )  newcolor = 15
         if (newcolor .lt. -1) then
            call error(428)
            goto 999
         endif
      else if ((ITYP .eq. 2 .and. ist.eq.1 ).or.
     x      (ITYP .eq. 2 .and. ist.eq.24 ).or.
     x       (ityp.eq.9)) then
         if ((ityp.eq.9).or.(ITYP.eq.2 .and. ist.eq.1)) then
             comstr = token2
             isub = ivxsub
             j = strlen1(token2)
         endif
         if (ityp.eq.2.and.ist.eq.24) then
             j = 0
             call gttext(comstr,j)
             isub = 0
         endif
         call ncl_getclr_inx(comstr, j, isub, indx)
         if (indx .ge. 16 .and. indx .le.63) then
              newcolor = indx
         else
              call error(428)
              return 
         endif 
      else if (ITYP .eq. 3 .or. (ityp .eq. 2 .and. ist .eq. 2)) then
         if (TV .ge. -1 .and. TV .le. 63) then
            newcolor = TV
         else
            call error (428)
            return
         endif
      else
         call error(428)
         goto 999
      endif

      g4type = gtype
      call ncl_set_geo_color(g4type,stype,newcolor)

999   return
      end
