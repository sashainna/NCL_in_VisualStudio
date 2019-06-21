c*****************************************************
c**   NAME         :  labset.f
c**   CONTAINS     :  Handles the *SET/LABEL,ON|OFF...
c**                           and *SET/LABEL,ALTER... statements.
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       labset.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:13
c**
c*****************************************************
c**
c** COPYRIGHT (c) 1993 NCCS
c*********************************************************************
c*    E_SUBROUTINE     : subroutine labset
c*     Handle * [RE]SET / LABEL,MODIFY, geo-type | ALL
c*
c*********************************************************************

      subroutine labset(choice)
  
      include 'com4a.com'

      integer*4 choice,leader

      integer*4 nclkey,isub,kwds,ipg,iel
      integer*2 isvix,ivoc,ietype,nwds,j
      integer*2 gtype,idum,i

      real*8 origin(3)

      integer*2  ipoint, ivector, iline, iplane, icircle, icurve, isurf,
     *           ishape, ipatern, ipt_vec, ilabel, iall, imx, first,
     2           ianote, isymbol

      data ipoint  /603/, ivector /604/, iline   /605/, iplane  /606/,
     *     icircle /607/, icurve  /608/, isurf   /609/, ishape  /602/,
     *     ipatern /636/, ipt_vec /613/, ilabel  /700/, iall    /816/,
     *     imx     /610/, ileader /1013/,ion      /71/ , ioff    /72/
     4     ianote  /616/, isymbol /877/, isolid  /123/

c....
c....Set up some defaults
c....
c      if (nextyp .ne. 9) then
c         call error(57)
c         return
c      endif
c
      isvix = inx
      first = 0
      leader =0
c
 1100 idtype = 0
      call parsit
      first = first +1
c.....
c.....Only the following vocabulay words are allowed here:
c.....point,vector,line,plane,circle,curve,surf,shape,patern,pt_vec
c.....
      if(ITYP .eq. 1 .and. first .ne. 1 .and. ist .eq. ileader) then
        if(.not.nxteos) then
          call parsit
          if (ist.ne.ION .and. ist.ne.IOFF) then
            call error(56)
            return
          endif
          if(.not.nxteos) then
            call error(1)
            return
          endif
          if(ist.eq.ION) leader = 1
          if(ist.eq.IOFF) leader = 2
          goto 100
        endif	   

      else if (ITYP .eq. 1) then
        if (IST .eq. ipoint   .or.    IST   .eq.   ivector .or.
     x      IST .eq. iline    .or.    IST   .eq.   iplane  .or.
     x      IST .eq. icircle  .or.    IST   .eq.   icurve  .or.
     x      IST .eq. isurf    .or.    IST   .eq.   ishape  .or.
     x      IST .eq. ipatern  .or.    IST   .eq.   ipt_vec .or.
     x      IST .eq. ilabel   .or.    IST   .eq.   iall    .or.
     x      IST .eq. imx      .or.    IST   .eq.   ianote  .or.
     x      IST .eq. isymbol .or.     IST   .eq.   isolid) then
          goto 1100
        else
          call error(203)
          return 
        endif
 
      else if (ITYP .eq. 2) then
        if (IST .eq. 1) then
          call ub_symbol_name (token2,ivxsub,nclkey,origin,i)
          if (i .eq. 2) goto 1100
          call error (9)
          return
        else if (IST.eq.3 .or. IST.eq.4 .or. IST.eq.5 .or. IST.eq.6 .or.
     x       IST.eq.7 .or. IST.eq.8 .or. IST.eq.9 .or. IST.eq.18.or.
     x       IST.eq.20.or. IST .eq. 21.or. IST.eq.13 .or. ist.eq.VSOLID
     x       .or. IST .eq. VANOTE) then
          goto 1100
        else 
          call error(9)
          return
        endif
      else if ( ITYP .eq. 7) then
        goto 100
      else
        call error(203)
        return
      endif
c....
c....All syntax is o'k.
c....

  100 inx = isvix
c
 2000 idtype = 0
      call parsit
      if(ityp .eq. 7) return
c
c.....ALL geometry types
c
      if (ityp .eq. 1 .and. IST .eq. iall) then
          call vxlfst
200       continue
          call vxlnxt (token2, isub, nclkey, kwds, ietype, ipg, iel)
          if (ietype.eq.1) return
          call idtovc (ivoc,ietype,idum)
          if ((ivoc .eq. ipoint  .or.
     x         ivoc .eq. ivector .or.  ivoc .eq. iline   .or.
     x         ivoc .eq. iplane  .or.  ivoc .eq. icircle .or.
     x         ivoc .eq. icurve  .or.  ivoc .eq. isurf   .or.
     x         ivoc .eq. ishape  .or.  ivoc .eq. ipatern .or.
     x         ivoc .eq. ipt_vec .or.  ivoc .eq. imx     .or.
     x         ivoc .eq. isolid  .or.  ivoc .eq. ianote  .or.
     x         ivoc .eq. isymbol)) then
                 call nclu_toggle_labels1(choice,nclkey,leader)
          endif
          goto 200
      endif
c
c..... geometry type name
      if (ITYP .eq. 1 .and. ist .ne. iall) then
          call vctoid (ist,gtype,idum)
          call vxlfst
210       continue
          call vxlnxt (token2, isub, nclkey, kwds, ietype, ipg, iel)
          if (ietype.eq.1) goto 2000
          if (ietype.eq.gtype) then
              call nclu_toggle_labels1(choice,nclkey,leader)
          endif
          goto 210
      endif
      
      if(ityp .eq. 2) then
         if (IST .eq. 1) then
           call ub_symbol_name (token2,ivxsub,nclkey,origin,i)
           ist = 32
           call ptdesc (nclkey,ist,tv)
         endif
         call gtdesc (tv, nclkey,nwds, ietype)
         call nclu_toggle_labels1(choice,nclkey,leader)
         goto 2000
      endif
      return
      end

C*******************************************************************
C*     SUBROUTINE     : subroutine labset1
c*     Used to display labels of fillet curves if *SET/LABEL
C*     is on.
c*
c*********************************************************************

      subroutine labset1(choice)

      include 'com4a.com'

      integer*4 choice,leader
      
      integer*4 nclkey,isub,kwds,ipg,iel
      integer*2 ivoc,ietype
      integer*2 idum

      integer*2  icircle

      data icircle /607/
C
C...Since labset1 has been called, we know that *SET/LABEL has
C...been implemented and the user hsa made fillets that need their
C...label displayed, thus it is neccessary to set choice to 1, so that
C...they will be displayed.
C
      choice=1
c
c...set leader to 0 to ignore this flag
c
      leader =0
c
c...The following will get all the geometry types and their labels and display
C...them, in the process getting the fillet curves and their labels.
C
      call vxlfst
200   continue
      call vxlnxt (token2, isub, nclkey, kwds, ietype, ipg, iel)
      if (ietype.eq.1) goto 250
      call idtovc (ivoc,ietype,idum)
      if (ivoc.eq.icircle) then
C
C...If isub is equal to 0 then it is a circle and not
C...a fillet, so go to 200 to continue without displaying
C...the circles label.
C

         if (isub.eq.0) goto 200
         call nclu_toggle_labels1(choice,nclkey,leader)
      endif
      goto 200
250   return
      end


c*********************************************************************
c*    E_SUBROUTINE     : subroutine labalt
c*     Handle DRAFT/LABEL,ALTER statement.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
c*********************************************************************
      subroutine labalt()

      include 'com4a.com'
      include 'wrksys.com'

      integer*4 nclkey,iabs,idel,keydst,leader,def
      integer*2 nwds,ietype, it
      real*8 buf(6),buf2(6),origin(3)
      integer*2 i2v3, i2zro
      data i2v3 /3/, i2zro /0/, ileader /1013/, ion /71/, ioff /72/
cc      integer*2 PNTVEC
c      parameter (PNTVEC=21)

      if (nextyp .ne. 9) goto 9057
 
      iabs = 1
      idel = 2
      leader =0
      def =0
 
      call parsit    
      if (ITYP .eq. 2 .and. IST .eq. 1) then
        call ub_symbol_name (token2,ivxsub,nclkey,origin,i)
        if (i .eq. 2) then
            ist = VPLACE
            call ptdesc (nclkey,ist,tv)
        endif
      endif
      if (ITYP .eq. 2 .and. 
     x      (IST.eq.3 .or. IST.eq.4 .or. IST.eq.5 .or. IST.eq.6 .or.
     x       IST.eq.7 .or. IST.eq.8 .or. IST.eq.9 .or. IST.eq.18.or.
     x       IST.eq.20.or. IST.eq.21.or. IST.eq.13.or. IST.eq.10.or.
     x       IST.eq.VSOLID .or. IST .eq. VANOTE .or. IST .eq. VPLACE))
     x           then
         call gtdesc (tv, nclkey,nwds, ietype)
         if (nxteos) then
           call nclu_alter_label1(nclkey,i2zro,buf(1),buf(2),buf(3),
     x        leader,def,buf2(1),buf2(2),buf2(3))
           goto 999
         endif
         if (nextyp .ne. 9) goto 9057
         call parsit 
         it = 0
         if (ITYP.eq.1 .and. (ist.eq.603 .or. ist.eq.604)) then
           it = ist-600
           call parsit
         endif
         if(scalar) then
            buf(1) = tv
            if (nextyp .ne. 9) goto 9057
            call parsit
            if(.not. scalar) goto 9007
            buf(2) = tv
            buf(3) = 0.0
            if (.not.nxteos) then
              if (nextyp .ne. 9) goto 9057
              call parsit 
              if(.not. scalar) goto 9007
              buf(3) = tv
            endif
            if (it .eq. 0) it = POINT
            if (lwrk) call transf(buf,wrkmx,i2v3,it)
            if (ifl(264).eq.1) then
              buf(1) = buf(1)/25.4d0
              buf(2) = buf(2)/25.4d0
              buf(3) = buf(3)/25.4d0
            endif
            if (ifl(72).eq.1) call transf(buf,sc(56),i2v3,it)
         else
            if (ityp.ne.2 .or. (ist.ne.POINT .and. ist.ne.VECTOR
     x          .and. ist.ne.PNTVEC)) goto 9002
            call gtdesc (tv, keydst, nwds, ietype)
            call gtgeo (keydst ,buf)
            if (ietype.eq.PNTVEC) then
              if (it.eq.4) then
                buf(1) = buf(4)
                buf(2) = buf(5)
                buf(3) = buf(6)
              else
                if (it .eq. 0) it = POINT
              endif
            else
              if (it .eq. 0) it = ietype
            endif
         endif
         if (.not. nxteos) then
           call parsit
           if(ist .ne.ileader) goto 9004
           if (nxteos) goto 9056
           call parsit
           if (ist.ne.ion .and. ist.ne.ioff) goto 9056
           if(ist.eq.ion) leader = 1
           if(ist.eq.ioff) leader = 2
           if (.not. nxteos) then
             call parsit 
             if (ityp.ne.2 .or. (ist.ne.POINT .and. ist.ne.VECTOR))
     x        goto 9002
             def =1
             call gtdesc (tv, keydst, nwds, ietype)
             call gtgeo (keydst ,buf2)
             if (.not. nxteos) goto 9004
           endif
         endif
         if(it .eq. 4) then
           call nclu_alter_label1(nclkey,idel,buf(1),buf(2),buf(3),
     x          leader,def,buf2(1),buf2(2),buf2(3))
         else
           call nclu_alter_label1(nclkey,iabs,buf(1),buf(2),buf(3),
     x          leader,def,buf2(1),buf2(2),buf2(3))   
         endif
      else
         call error(87)
      endif
999   return
C                        --- Point, Number or Vector expected
9002  call error(2)
      return
C                        --- End of statement expected
9004  call error(4)
      return
C                        --- Number or scalar expected
9007  call error(7)
      return
C                        --- ON/OFF expected
9056  call error(56)
      return
C                        --- Comma expected
9057  call error(57)
      return

      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine labdef
C*      Set geometry label to default position.
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
      subroutine labdef

      return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine labmdl
C*      Set the geometry labelling flags.
C*    PARAMETERS
C*       INPUT  :
C*          set         - true = set labels flags on
C*                      - false = set labels flags off
C*       OUTPUT :
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine labmdl (set)

      include 'com.com'

      logical set

      logical ldrset,lpt,lve,lln,lci,lcv,lsf,lsh,lpl,lpn,lpv,lmx,lall,
     1        lan,lsy,lso
      integer*2  ION, IOFF,ILEADER
      parameter (ION=71)
      parameter (IOFF=72)
      parameter (ILEADER=1013)

      lall = .true.
      if (nxteos) then
        lablpt = set
        lablve = set
        lablln = set
        lablpl = set
        lablci = set
        lablcv = set
        lablsf = set
        lablsh = set
        lablpn = set
        lablpv = set
        lablmx = set
        lablan = set
        lablsy = set
        lablso = set
      else
        lpt=.false.
        lve=.false.
        lln=.false.
        lci=.false.
        lcv=.false.
        lsf=.false.
        lsh=.false.
        lpl=.false.
        lpn=.false.
        lpv=.false.
        lmx=.false.
        lan=.false.
        lsy=.false.
        lso=.false.

100     if (nextyp.ne.9) then
          ifl(2)=57
          goto 999
        endif
        call parsit
        if (ist.eq.603) then
          lpt=.true.
        else if (ist.eq.604) then
          lve=.true.
        else if (ist.eq.605) then
          lln=.true.
        else if (ist.eq.607) then
          lci=.true.
        else if (ist.eq.608) then
          lcv=.true.
        else if (ist.eq.609) then
          lsf=.true.
        else if (ist.eq.602) then
          lsh=.true.
        else if (ist.eq.606) then
          lpl=.true.
        else if (ist.eq.636) then
          lpn=.true.
        else if (ist.eq.610) then
          lmx=.true.
        else if (ist.eq.616) then
          lan=.true.
        else if (ist.eq.877) then
          lsy=.true.
        else if (ist.eq.123) then
          lso=.true.
        else if (ist.eq.816) then
          lpt=.true.
          lve=.true.
          lln=.true.
          lci=.true.
          lcv=.true.
          lsf=.true.
          lsh=.true.
          lpl=.true.
          lpn=.true.
          lmx=.true.
          lan=.true.
          lsy=.true.
          lso=.true.
        else if(ist.eq.ILEADER) then
	    call parsit
          
          if (ist.ne.ION .and. ist.ne.IOFF) then
            call error(56)
            goto 999
          endif
         if(.not.nxteos) then
           call error(1)
           goto 999
         endif
         if(ist.eq.ION) ldrset = .true.
         if(ist.eq.IOFF) ldrset = .false.
         if (lall) then
            lablpt = set
            lablve = set
            lablln = set
            lablpl = set
            lablci = set
            lablcv = set
            lablsf = set
            lablsh = set
            lablpn = set
            lablpv = set
            lablmx = set
            lablan = set
            lablsy = set
            lablso = set
            lbldpt = ldrset
            lbldve = ldrset
            lbldln = ldrset
            lbldpl = ldrset
            lbldci = ldrset
            lbldcv = ldrset
            lbldsf = ldrset
            lbldsh = ldrset
            lbldpn = ldrset
            lbldpv = ldrset
            lbldmx = ldrset
            lbldan = ldrset
            lbldsy = ldrset
            lbldso = ldrset
          else
            if (lpt) lbldpt=ldrset
            if (lve) lbldve=ldrset
            if (lln) lbldln=ldrset
            if (lci) lbldci=ldrset
            if (lcv) lbldcv=ldrset
            if (lsf) lbldsf=ldrset
            if (lsh) lbldsh=ldrset
            if (lpl) lbldpl=ldrset
            if (lpn) lbldpn=ldrset
            if (lpv) lbldpv=ldrset
            if (lmx) lbldmx=ldrset
            if (lan) lbldan=ldrset
            if (lsy) lbldsy=ldrset
            if (lso) lbldso=ldrset
          endif
        endif
        lall =.false.
        if (nextyp.ne.11) goto 100
        if (lpt) lablpt=set
        if (lve) lablve=set
        if (lln) lablln=set
        if (lci) lablci=set
        if (lcv) lablcv=set
        if (lsf) lablsf=set
        if (lsh) lablsh=set
        if (lpl) lablpl=set
        if (lpn) lablpn=set
        if (lpv) lablpv=set
        if (lmx) lablmx=set
        if (lan) lablan=set
        if (lsy) lablsy=set
        if (lso) lablso=set
      endif

999   return
      end

C*******************************************************************
C*     SUBROUTINE     : subroutine labatt
c*     Used to set label attributes
c*
c*********************************************************************

      subroutine labatt(labon)

      include 'com4a.com'
      integer*4 labon,box,colorfg,colorbg,indx,strlen1,isub
      integer*4 arrow,ldrclr,sizew, sizeh,leader,ovrlpdis
      integer*2 ION, IOFF, IMODFY, ILEADER, IBOX, ICOLOR, ISIZE,IARROW
      integer*2  icolors(16),j
      character*256 comstr
      parameter (IALTER=951)
      parameter (ION=71)
      parameter (IOFF=72)
      parameter (IMODFY=732)
      parameter (ILEADER=1013)
      parameter (IBOX=340)
      parameter (ICOLOR=900)
      parameter (ISIZE=196)
      parameter (IARROW=925)
      parameter (IOFFSET=666)

c
c...BLACK  WHITE    BLUE    RED   GREEN   MAGNTA YELLOW CYAN   
c...BROWN TAN    LT-BLUE SEA-GREEN ORANGE PINK   PURPLE GREY
c
      data  icolors /904,905,906,907,908,909,910,911,
     *               912,913,914,915,1101,1102,1103,1104/
      colorfg = -1 
      colorbg = -1 
      box = -1
      sizew = -1.0
      sizeh = -1.0
      ldrclr =-1
      arrow =-1
      leader = -1
      ovrlpdis = -1    
      if (nxteos) goto 9061

      if (ist .eq. IBOX) then
        call parsit
        if (ist.ne.ION .and. ist.ne.IOFF) goto 9056
        if(ist.eq.ION) box = 1
        if(ist.eq.IOFF) box = 2
        if(nxteos) goto 500
        call parsit
        if (ityp.ne.1 .or. (ist.ne.ICOLOR .and. ist.ne.ISIZE.and.
     1    ist.ne.IOFFSET .and.ist.ne.ILEADER ) .or. nxteos) goto 9061
      endif

      if (ist .eq. ICOLOR) then
        call parsit
        do 100 i=1,16,1
          if (IST .eq. icolors(i)) colorfg = i - 1
 100    continue
        if (colorfg .eq. -1) then
             if ((ITYP .eq. 2 .and. ist.eq.1 ).or.
     x          (ITYP .eq. 2 .and. ist.eq.24 ).or.
     x          (ityp.eq.9)) then
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
                 if (indx .ge. 16 .and. indx .le.63) then
                     colorfg = indx
                 else
                      goto 9428 
                 endif 
             endif        
        endif
        if(nxteos) goto 9061
        call parsit
        do 120 i=1,16,1
          if (IST .eq. icolors(i)) colorbg = i - 1
 120    continue
        if (colorbg .eq. -1) then
             if ((ITYP .eq. 2 .and. ist.eq.1 ).or.
     x          (ITYP .eq. 2 .and. ist.eq.24 ).or.
     x          (ityp.eq.9)) then
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
                 if (indx .ge. 16 .and. indx .le.63) then
                     colorbg = indx
                 else
                      goto 9428 
                 endif 
             endif        
        endif
        if(nxteos) goto 500
        call parsit
        if (ityp.ne.1 .or. (ist.ne.ISIZE.and.
     1      ist.ne.ILEADER .and. ist.ne.IOFFSET) .or. nxteos) goto 9061
      endif

      if (ist .eq. ISIZE) then
        call parsit
        if(ityp .ne.3 .or. nxteos) goto 9053
        sizew=tv
        call parsit
        if(ityp .ne.3) goto 9053
        sizeh=tv
        if(nxteos) goto 500
        call parsit
        if (ityp.ne.1 .or. (ist.ne.ILEADER .and. ist.ne.IOFFSET) 
     1      .or. nxteos) goto 9061
      endif

      if (ist .eq. IOFFSET) then
        call parsit
        if(ityp .ne.3) goto 9053
        ovrlpdis=tv
        if(nxteos) goto 500
        call parsit
        if (ityp.ne.1 .or. ist.ne.ILEADER.or. nxteos) goto 9061
      endif

      if (ist .eq. ILEADER) then
        call parsit
        if (ityp.ne.1 .or. (ist.ne.ION .and.ist.ne.IOFF .and. 
     1       ist.ne.ICOLOR .and.ist.ne.IARROW)) goto 9061

        if (ist .eq. IOFF .or. ist .eq.ION) then
        if (ist .eq. IOFF) leader =2
        if (ist .eq. ION) leader =1
        if(nxteos) goto 500
          call parsit
          if (ityp.ne.1 .or. (ist.ne.ICOLOR .and.ist.ne.IARROW)
     1         .or. nxteos) goto 9061
        endif

        if (ist .eq. ICOLOR) then
          call parsit
          do 150 i=1,16,1
            if (IST .eq. icolors(i)) ldrclr = i - 1
 150      continue
          if (ldrclr .eq. -1) then
             if ((ITYP .eq. 2 .and. ist.eq.1 ).or.
     x          (ITYP .eq. 2 .and. ist.eq.24 ).or.
     x          (ityp.eq.9)) then
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
                 if (indx .ge. 16 .and. indx .le.63) then
                     ldrclr = indx
                 else
                      goto 9428 
                 endif 
             endif        
          endif
          if(nxteos) goto 500
          call parsit
          if (ityp.ne.1 .or. ist.ne.IARROW .or. nxteos) goto 9061
        endif

        if (ist .eq. IARROW) then
          call parsit
          if (ist.ne.ION .and. ist.ne.IOFF) goto 9056
          if(ist.eq.ION) arrow = 1
          if(ist.eq.IOFF) arrow = 2
          if(.not.nxteos) goto 9061
        endif
      endif

500   if(labon .ne. -1) then
        if(labon .eq.1) call setlbt()
        if(labon .eq.2) call setlbf()
      endif
      if(leader .ne. -1) then
        if(leader .eq.1) call setldt()
        if(leader .eq.2) call setldf()
      endif
      if(box .ne. -1 .or. colorfg .ne.-1 .or. colorbg .ne.-1 .or. 
     1     sizew.ne.-1.or. sizeh .ne.-1 .or. ovrlpdis .ne.-1 .or.
     2     ldrclr .ne.-1 .or. arrow.ne.-1) call uv_redisp_label0(box,
     3     colorfg,colorbg,sizew,sizeh,ovrlpdis,ldrclr,arrow)
999	return


C                        --- Integer expected
9053  call error(53)	
      return
C                        --- ON/OFF expected
9056  call error(56)	
      return
C                        --- Comma expected
9057  call error(57)	
      return
C                        --- 	Invalid Syntax format
9061  call error(61)	
      return
C                        --- 	Invalid Color Parameter
9428  call error(428)	
      return

      end

