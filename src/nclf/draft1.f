c********************************************************************
c**   NAME         :  draft1.f
c**   CONTAINS     :  Handles the DRAFT/... statement.
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       draft1.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:57
c**
c*****************************************************
c**
c** COPYRIGHT (c) 1993 NCCS
c*********************************************************************
c*    E_SUBROUTINE     : subroutine drftmod 
c*     Handles the DRAFT/MODIFY,... statement.
c*
c*    We call this routine from the DRAFT routine, file DRAFT.F    
c*
c*********************************************************************

      subroutine  drftmod

      include 'com4a.com'

      common/dstcom/keydt, ixdt, nxthld, nxtret
      integer*4 keydt, indx
      integer*2 ixdt, nxthld, nxtret

      integer*2  ipoint, ivector, iline, iplane, icircle, icurve, isurf,
     *           imatrix, ishape, ipatern, ipt_vec, ilabel, iall

      integer*2  icolor, ipen, ilintyp, ilinwgt, ilayer, itrans, ishd,
     1           imatrl, iedge
C
C...Added colors brown thru grey.  JLS 4/15/99
C
      integer*2  idefault, iwhite, ired, iblack ,  iblue,  igreen,
     *           imagnta,  iyellow,icyan, ibrown, ilttan, iltblue,
     *           iseagrn, iorange, ipink, ipurple, igrey
     
      integer*2  isolid,  icenter, idashdt,  idash,   iphantm,
     *           idashsp, idotted, idashln

      integer*2  istd, iheavy,  imedium, iexhvy
      integer*2 ion, ioff
      integer*2  idot, iplus, istar, icirc, icross, itrian, idimond,
     *           isquare, idblci, ilgdot, icube
c.....
     
      integer*4 newcolor, newpen, newlintyp, newlayer, kwds,
     *            newshd, newtrans, newmat, newedg, newmkr
      real*8    fnewlinwgt

      integer*4 nclkey, isub
      integer*2 nwds,ietype,ivoc,j
 
      integer*4 ipg, iel
      real*8 asw(36)
      integer*2 ksw(144)
      character*8 cvs(35)
      integer*2 gtype,idum
      integer*4 nc,strlen1
      character*80 cmatrl
      character*256 comstr

      logical all

      equivalence (vs, cvs, jb)
      equivalence (asw,ksw)


c.....
c.....Set up some defaults:
c.....
      data ipoint  /603/, ivector /604/, iline   /605/, iplane  /606/,
     *     icircle /607/, icurve  /608/, isurf   /609/, imatrix  /610/,
     *     ishape  /602/,ipatern /636/, ipt_vec /613/, ilabel  /700/, 
     *     iall    /816/

      data  icolor  /900/, ipen /128/, ilintyp /131/,
     *      ilinwgt /901/, ilayer /902/, itrans /1037/, ishd /578/ 
     2      imatrl  /584/, iedge  /860/, imkr /952/

      data  idefault /903/,  iwhite /905/, ired /907/,
     *      iblack   /904/,  iblue   /906/, igreen /908/,
     *      imagnta  /909/,  iyellow /910/, icyan  /911/,
     *      ibrown   /912/,   ilttan  /913/,  iltblue/914/,
     *      iseagrn  /915/,   iorange /1101/, ipink  /1102/,
     *      ipurple  /1103/,  igrey   /1104/

      data  isolid   /123/,    icenter  /634/,    idashdt /918/,
     *      idash    /124/,    iphantm  /916/,   idashsp /919/,
     *      idotted  /125/,    idashln  /917/

      data  istd /920/,  iheavy /921/,  imedium /61/,  iexhvy /922/
      data  ion/71/, ioff/72/
      data  idot /561/, iplus /19/, istar /953/, icirc /607/,
     *      icross /819/, itrian /954/, idimond /956/, isquare /955/
     *      idblci /957/, ilgdot /958/, icube /959/
c.....
c.....
      all =.false.
      call svpars

      newcolor = -2
      newpen   = -1
      newlintyp= -1
      fnewlinwgt=-1.
      newlayer = -1
      newtrans = -1
      newshd = -1
      newmat = -1
      newedg = -1
      newmkr = -1

c....
c....Loup through the statement to ensure the syntax is correct.
c....
      if (nextyp .eq. 9 ) then
         all = .true.
         goto 500
      else if (nextyp .ne. 1) then
         call error(6)
         return
      else
         idtype = 0
         call parsit
      endif

 1100 idtype = 0
      call parsit
c.....
c.....Only the following vocabulay words are allowed here:
c.....point,vector,line,plane,circle,curve,surf,shape,patern,pt_vec
c.....
      if (ITYP .eq. 1) then
         if (IST .eq. ipoint   .or.    IST   .eq.   ivector .or. 
     x       IST .eq. iline    .or.    IST   .eq.   iplane  .or.
     x       IST .eq. icircle  .or.    IST   .eq.   icurve  .or. 
     x       IST .eq. isurf    .or.    IST   .eq.   ishape  .or.
     x       IST .eq. ipatern  .or.    IST   .eq.   ipt_vec .or. 
     x       IST .eq. ilabel   .or.    IST   .eq.   imatrix .or.
     x       IST .eq. isolid   .or.    IST   .eq.   iall   ) then
             goto 1100
         else 
             goto 600          
         endif

      else if (ITYP .eq. 2) then
         if (IST.eq.3 .or. IST.eq.4 .or. IST.eq.5 .or. IST.eq.6 .or. 
     x       IST.eq.7 .or. IST.eq.8 .or. IST.eq.9 .or. IST.eq.10.or.
     x       IST.eq.18.or. IST.eq.20.or. IST.eq.21.or. IST.eq.13.or.
     x       IST.eq.VSOLID)
     x           then
             goto 1100
         else  
             call error(9)
             return
         endif
      else if ( ITYP .eq. 7) then
         goto 100      
      else 
         call error(232)
         return
      endif
c....
c....  COLOR    PEN     LINTYP     LINWGT     LAYER   TRANSLUCENCY  SHADED
c....or... it is a syntax error.
c....
  500 idtype = 0
      call parsit
  600 if (ITYP .eq. 1 ) then
         if ((ist .eq. icolor .or. ist .eq. ipen .or. ist .eq. ilintyp
     *       .or. ist .eq. ilinwgt .or. ist .eq. ilayer .or. 
     *       ist .eq. ishd .or. ist .eq. itrans .or.
     *       ist .eq. imatrl .or. ist .eq. iedge .or. ist .eq. imkr) 
     *       .and.  nextyp .ne. 1) then
             call error(6)
             return
         endif
         if (IST .eq.  icolor  ) goto 2000
         if (IST .eq.  ipen    ) goto 3000
         if (IST .eq.  ilintyp ) goto 4000
         if (IST .eq.  ilinwgt ) goto 5000
         if (IST .eq.  ilayer  ) goto 6000
         if (IST .eq.  itrans  ) goto 7000
         if (IST .eq.  ishd    ) goto 8000
         if (IST .eq.  imatrl  ) goto 8100
         if (IST .eq.  iedge   ) goto 8200
         if (IST .eq.  imkr    ) goto 8300
         ist = 2
         call error(436)
         return
      endif
      if (ITYP .ne. 1) then
         call error(232)
         return
      endif
      if (nextyp .ne. 1) then
         call error(6)
         return
      endif

c....
c....COLOR. Colors available:
c....MAGNTA    YELLOW  CYAN   DEFAULT   WHITE   RED   BLACK   BLUE   GREEN
c...Added BROWN, LTTAN, LTBLUE, SEAGRN, ORANGE, PINK, PURPLE, GREY
C...JLS 4/15/99
C
 2000 idtype = 0
      call parsit
      idtype = 0
      call parsit

      if (ITYP .eq. 1 ) then
         if (IST .eq.  idefault)  newcolor = -1
         if (IST .eq.  iblack  )  newcolor = 0
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
            return
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
      else if (ITYP .eq. 3 .or. (ityp .eq. 2 .and. ist .eq. 2) ) then
C
C...New colors were added and the changes need to be made here also.
C...TV used to have to be less than 7.  JLS 4/15/99
C
c
c.....0 to 63 as black to grey defined in wsgl.h
c.....-1 as default
c
         if (TV .ge. -1 .and. TV .le.63) then
              newcolor = TV        
         else
              call error(428)
              return 
         endif 
      else 
         call error(428)
         return
      endif

      if(nextyp .eq. 11) then
             goto 100
      else if (nextyp .eq. 9 ) then
             goto 500
      else
         call error(57)
         return
      endif
c.....
c.....Pen (1-256)
c.....
 3000 idtype = 0
      call parsit
      idtype = 0
      call parsit
c
c......'ITV' is not set to a value
c......when a variable (ITYP=2) is specified
c......instead of an actual scalar (ITYP = 3/4)
c......Yurong  -  3/5/97
c
      if (.not.scalar) then
          call error(53)
          return
      endif
    
      ITV    = tv
      if(ITV .le. 0 .or. ITV .gt. 256) then
          call error(181) 
          return
      endif
      newpen = ITV

      if(nextyp .eq. 11) goto 100
      if (nextyp .eq. 9 ) goto 500 

      call error(57)
      return
c.....
c.....LINTYP. Line types available: SOLID   CENTER  DASHDT    DASH  
c.....                              PHANTM  DASHSP  DOTTED    DASHLN
c.....
 4000 idtype = 0
      call parsit
      idtype = 0
      call parsit
 
      if (ITYP .eq. 1 ) then
         if (IST .eq.   isolid )  newlintyp = 1
         if (IST .eq.   idash  )  newlintyp = 2
         if (IST .eq.   idotted)  newlintyp = 3
         if (IST .eq.   icenter)  newlintyp = 4
         if (IST .eq.   iphantm)  newlintyp = 5
         if (IST .eq.   idashln)  newlintyp = 6 
         if (IST .eq.   idashdt)  newlintyp = 7
         if (IST .eq.   idashsp)  newlintyp = 8
         if(newlintyp .lt. 0) then
              call error(222)
              return
         endif 
      else if (scalar) then
         ITV    = tv
         if (ITV .gt. 0 .and. ITV .le. 8) then
              newlintyp = ITV
         else
              call error(222)
              return
         endif
      else
         call error(222)
         return
      endif
         
      if(nextyp .eq. 11) goto 100
      if (nextyp .eq. 9 ) goto 500 

      call error(57)
      return

c.....
c.....LINWGT Line weights available: STD  HEAVY   MEDIUM  EXHVY
c.....
 5000 idtype = 0
      call parsit
      idtype = 0
      call parsit

      if (ITYP .eq. 1 ) then
         if (IST .eq.  istd   )  fnewlinwgt = 1
         if (IST .eq.  imedium)  fnewlinwgt = 2
         if (IST .eq.  iheavy )  fnewlinwgt = 3
         if (IST .eq.  iexhvy )  fnewlinwgt = 4
         if (fnewlinwgt .lt. 0) then
              call error(430)
              return
         endif       
      else if (scalar) then
         ITV    = tv
         if (ITV .gt. 0 .and. ITV .le. 4) then
              fnewlinwgt = ITV
         else
              call error(430)
              return
         endif
      else
         call error(430)
         return
      endif

      if(nextyp .eq. 11) goto 100
      if (nextyp .eq. 9 ) goto 500 

      call error(57)
      return

c.....
c.....LAYER
c.....
 6000 idtype = 0
      call parsit
      idtype = 0
      call parsit
     
      if (.not.scalar) then
          call error(53)
          return
      endif

      ITV    = tv
      if(ITV .lt. 0 .or. ITV .gt. 9999) then
          call error(431)
          return
      endif
      newlayer = ITV

      if(nextyp .eq. 11) goto 100
      if (nextyp .eq. 9 ) goto 500 

      call error(57)
      return

c.....
c.....TRANSLUCENCY (1-100)
c.....added by Yurong 4/12/99
c.....
 7000 idtype = 0
      call parsit
      idtype = 0
      call parsit

      if (.not.scalar) then
          call error(53)
          return
      endif
    
      ITV    = tv
      if(ITV .le. 0 .or. ITV .gt. 100) then
          call error(224) 
          return
      endif
      newtrans = ITV

      if(nextyp .eq. 11) goto 100
      if (nextyp .eq. 9 ) goto 500 

      call error(57)
      return
c
c...SHADE (ON/OFF)
c...added by Yurong 4/12/99
c
 8000 idtype = 0
      call parsit
      idtype = 0
      call parsit

      if (ITYP .eq. 1 ) then
         if (IST .ne.  ion .and. IST .ne.  ioff    ) then
            call error(56)
            return
         endif
      else 
         call error(232)
         return
      endif

      newshd   = ion - IST + 1

      if(nextyp .eq. 11) goto 100
      if (nextyp .eq. 9 ) goto 500 

      call error(57)
      return
c
c...MATERL=material
c
 8100 idtype = 0
      call parsit
      idtype = 0
      call parsit

      if (scalar) then
          if (tv .lt. 1 .or. tv .gt. 16) then
              call error (534)
              goto 9000
          endif
          newmat = tv
      else if (lstrng) then
          ix = 0
          cmatrl = ' '
          call gttext (cmatrl,ix)
          nc = strlen1(cmatrl)
          call umf_get_material_number(cmatrl,nc,newmat)
          if (newmat .eq. -1) then
              call error (534)
              goto 9000
          endif
      else
          call error(534)
          go to 9000
      endif

      if(nextyp .eq. 11) goto 100
      if (nextyp .eq. 9 ) goto 500 

      call error(57)
      go to 9000
c
c...EDGE=ON/OFF/color
c
 8200 idtype = 0
      call parsit
      idtype = 0
      call parsit

      if (ITYP .eq. 1 ) then
         if (IST .eq. ioff) then
             newedg = 0
         else
c
c...color order changed, defined in wsgl.h
c
             if (IST .eq.  idefault)  newedg = 1
             if (IST .eq.  iblack)  newedg = 2
             if (IST .eq.  iwhite  )  newedg = 3
             if (IST .eq.  iblue   )  newedg = 4
             if (IST .eq.  ired    )  newedg = 5
             if (IST .eq.  igreen  )  newedg = 6   
             if (IST .eq.  imagnta )  newedg = 7
             if (IST .eq.  iyellow )  newedg = 8
             if (IST .eq.  icyan   )  newedg = 9
             if (IST .eq.  ibrown  )  newedg = 10
             if (IST .eq.  ilttan  )  newedg = 11
             if (IST .eq.  iltblue )  newedg = 12
             if (IST .eq.  iseagrn )  newedg = 13
             if (IST .eq.  iorange )  newedg = 14
             if (IST .eq.  ipink   )  newedg = 15
             if (IST .eq.  ipurple )  newedg = 16
             if (IST .eq.  igrey   )  newedg = 17
             if (newedg .lt. 0) then
                 call error(428)
                 return
             endif
         endif
      else if (scalar) then
c
c.....0 to 15 as black to grey defined in wsgl.h
c.....-1 as default
c
          if (TV .ge. -1 .and. TV .le.63) then
              newedg = TV + 2
          else
              call error(428)
              return 
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
         if (indx .ge. 16 .and. indx .le.63) then
              newedg = indx + 2
         else
              call error(428)
              return 
         endif 
      else 
          call error(428)
          return
      endif
 
      if(nextyp .eq. 11) goto 100
      if (nextyp .eq. 9 ) goto 500 

      call error(57)
      return

8300  idtype = 0
      call parsit
      idtype = 0
      call parsit

      if (ITYP .eq. 1 ) then
        if (IST .eq.  idot)    newmkr = 1
        if (IST .eq.  iplus)   newmkr = 2
        if (IST .eq.  istar)   newmkr = 3
        if (IST .eq.  icirc)   newmkr = 4
        if (IST .eq.  icross)  newmkr = 5
        if (IST .eq.  itrian)  newmkr = 6
        if (IST .eq.  idimond) newmkr = 7
        if (IST .eq.  isquare) newmkr = 8
        if (IST .eq.  idblci)  newmkr = 9
        if (IST .eq.  ilgdot)  newmkr = 10
        if (IST .eq.  icube)   newmkr = 11
        if (newmkr .lt. 1) then
          call error (182)
          goto 9000
        endif
      else if(scalar) then
        if (tv .lt. 1 .or. tv .gt. 11) then
          call error (445)
          goto 9000
        endif
        newmkr = tv
      else
        call error (035)
        goto 9000
      endif
      
      if(nextyp .eq. 11) goto 100
      if (nextyp .eq. 9 ) goto 500 
      
  100 continue
c
c...All syntax is ok. Loop thru the statement again and do the draft
c...operations.
c
      if (all) then
         call umf_set_def_attr(newcolor,newpen,newlintyp,fnewlinwgt,
     1       newlayer,newshd,newtrans,newmat,newedg,newmkr)
         return
      endif

      call rtpars     
      idtype = 0
      call parsit
  205 idtype = 0
      call parsit
      if (ITYP .eq. 7) return
      if (ITYP .eq. 1) then
         if (IST .eq. ipoint   .or.    IST   .eq.   ivector .or.
     x       IST .eq. iline    .or.    IST   .eq.   iplane  .or.
     x       IST .eq. icircle  .or.    IST   .eq.   icurve  .or.
     x       IST .eq. isurf    .or.    IST   .eq.   ishape  .or.
     x       IST .eq. ipatern  .or.    IST   .eq.   imatrix .or.
     x       IST .eq. ipt_vec  .or.    IST   .eq.   ilabel  .or.
     x       IST .eq. isolid) then    
             call vctoid (ist,gtype,idum)
             call vxlfst
  210        continue
             call vxlnxt (token2, isub, nclkey, kwds, ietype, ipg, iel)
             if (ietype.eq.1) goto 205
             if (ietype.eq.gtype)
     *          call umf_set_ent_attr(nclkey,newcolor,newpen,
     *                            newlintyp,fnewlinwgt,newlayer,
     *                            newshd,newtrans,newmat,newedg,newmkr)
             goto 210
         endif

c..... ALL geometry types
         if (IST .eq. iall) then
            call vxlfst
  221       continue    
            call vxlnxt (token2, isub, nclkey, kwds, ietype, ipg, iel)
             if (ietype.eq.1) goto 205
             call idtovc (ivoc,ietype,idum)
             if (ivoc .eq. ipoint  .or.
     x           ivoc .eq. ivector .or.  ivoc .eq. iline   .or. 
     x           ivoc .eq. iplane  .or.  ivoc .eq. icircle .or.
     x           ivoc .eq. icurve  .or.  ivoc .eq. isurf   .or. 
     x           ivoc .eq. ishape  .or.  ivoc .eq. ipatern .or.
     x           ivoc .eq. ipt_vec .or.  ivoc .eq. ilabel  .or.
     x           ivoc .eq. isolid) then
                    call umf_set_ent_attr(nclkey,newcolor,newpen,
     *                            newlintyp,fnewlinwgt,newlayer,
     *                            newshd,newtrans,newmat,newedg,newmkr)
             endif
            goto 221
         endif
c..... Next parameter, get out of here.
         return
      else if (ITYP .eq. 2) then
c..... entity name
          call gtdesc (tv, nclkey,nwds, ietype)
          call umf_set_ent_attr(nclkey,newcolor,newpen,
     *                            newlintyp,fnewlinwgt,newlayer,
     *                            newshd,newtrans,newmat,newedg,newmkr)
          goto 205
      endif

 9000 return
      end


c*********************************************************************
c*    E_SUBROUTINE     : subroutine drftfmt 
c*     Handles the DRAFT/FORMAT,... statement.
c*
c*    We call this routine from the DRAFT routine, file DRAFT.F
c*
c*********************************************************************

      subroutine  drftfmt

      include 'com4a.com'

      integer*4 iviewnum, newformat,  newborder(20), newaxis(20),
     *          newnamed(20), newsize(20), newmotion(20), newmode(20),
     *          fborder, faxis, fnamed, fsize, fmotion, fmode, strlen1

      character*20 screenname
      integer*2 isingle, iside, istack, ifour, ifive, isix

      integer*2   iname, iborder, iaxis, inamed, isize, imotion, 
     *            imode 
      integer*2 ion, ioff
      integer*2 ishd, iwire, ihid, iboth

      data isingle /926/, iside  /94/, istack /927/, ifour /928/,
     *     ifive   /929/, isix /930/

      data iname /933/, iborder /932/, iaxis /132/, inamed /931/,
     *     isize  /196/, imotion  /836/, imode /1003/

      data ion/71/, ioff/72/
      data ishd /578/, iwire /1028/, ihid /579/, iboth /83/

      integer*4 iviewkey(20)

      newformat =  0
      nmcount   =  0
      iviewnum  =  0
      fborder = 0
      faxis = 0
      fnamed = 0 
      fsize = 0 
      fmotion = 0
      fmode = 0
c.....
c.....
      isvix = inx
c....
c....Loop through the statement to ensure the syntax is correct.
c....
      if (nextyp .eq. 9 ) then
         goto 500
      else if (nextyp .ne. 1) then
         call error(6)
         return
      else
         call parsit
      endif

      call parsit

c....
c.... FORMAT PARAMETERS
c....
      if (ITYP .eq. 2 ) then
c
c....the format can be any screen, not just fixed 6 screen format now
c   
          screenname = token2
          k = strlen1(screenname)
10        if (k .eq. 1) then
              call error(434)
              return
          endif
          screenname(k+1:k+1) = char(0)
          call uv_screen_chk(screenname, screenname, iviewnum)
      else if (ITYP .eq. 1 ) then  
         if (IST .eq.  isingle )  then
             screenname = 'single'
             iviewnum  = 1
         else if (IST .eq.  iside   ) then
             screenname = 'vert. dual'
             iviewnum  = 2
         else if (IST .eq.  istack  ) then
             screenname = 'horiz. dual'
             iviewnum  = 2
         else if (IST .eq.  ifour   ) then
             screenname = 'quad'
             iviewnum  = 4
         else if (IST .eq.  ifive   ) then
             screenname = 'five and one'
             iviewnum  = 6
         else if (IST .eq.  isix    ) then
             screenname = 'six equal'
             iviewnum  = 6
         else
            ITYP = 2
            call error(432)
            return
         endif
         k = strlen1(screenname)
         screenname(k+1:k+1) = char(0)
      else if (scalar) then
         ITV    = tv
         if (ITV .gt. 0 .and. ITV .le. 6) then
              newformat = ITV        
              if ( newformat .eq. 1) then
                  iviewnum  = 1
                  screenname = 'single'
              endif
              if ( newformat .eq. 2) then
                  iviewnum  = 2
                  screenname = 'vert. dual'
              endif
              if ( newformat .eq. 3) then
                  iviewnum  = 2
                  screenname = 'horiz. dual'
              endif
              if ( newformat .eq. 4) then
                  iviewnum  = 4
                  screenname = 'quad'
              endif
              if ( newformat .eq. 5) then
                  iviewnum  = 6
                  screenname = 'five and one'
              endif
              if ( newformat .eq. 6) then
                  iviewnum  = 6
                  screenname = 'six equal'
              endif
              k = strlen1(screenname)
              screenname(k+1:k+1) = char(0)
         else
              call error(432)
              return 
         endif 
      else 
         call error(432)
         return
      endif

      if(nextyp .eq. 11) then
             goto 100
      else if (nextyp .eq. 9 ) then
             goto 500
      else
         call error(57)
         return
      endif
c....
c....  NAME    BORDER   AXIS   NAMED  SIZE   MOTION
c....or... it is a syntax error.
c....
  500 call parsit
  600 if (ITYP .eq. 1 .and. nextyp .eq. 1) then
         if (IST .eq.  iname   ) goto 1000
         if (IST .eq.  iborder .or. IST .eq. iaxis .or.
     *       IST .eq.  inamed  .or. IST .eq. isize .or.  
     *       IST .eq.  imotion ) then
             goto 2000
c
c....added display mode
c....Yurong 4/15/99
c
			else if (IST .eq. imode) then
             goto 3000
         endif
         ityp = 2
         call error(436)
         return
      endif
      if (ITYP .ne. 1) then
         call error(232)
         return
      endif
      if (nextyp .ne. 1) then
         call error(6)
         return
      endif
c....
c....NAME
c....
 1000 call parsit
 1150 call parsit
      if(ITYP .eq. 7) goto 100
      if(ifl(35).eq.0.or.(ifl(35).eq.2.and.ifl(350).gt.0)) then
         call uvu_view_chk(token2,iviewkey(nmcount+1))
         if (iviewkey(nmcount+1) .eq. 0) then
            if (ityp .eq. 1 ) then
               if(nmcount .ne. iviewnum) then
                 call error(435)
                 return
               endif
               goto 600 
            else
               call error(434)
               return
            endif 
         endif
         nmcount = nmcount+1
         goto 1150
      else
c
c....added display mode
c
         if (ityp .eq. 1 .and.(IST .eq. iname .or. IST .eq. iborder .or. 
     *       IST .eq. iaxis .or. IST .eq. inamed  .or. IST.eq.imode .or.
     *       IST .eq. isize .or. IST .eq. imotion ))goto 600
         if (nextyp .ne. 9 .and. nextyp .ne. 11) then
            call error(57)
            return
         endif
         goto 1150
      endif
c....
c....BORDER  ON/OFF or AXIS ON/OFF or NAMED ON/OFF or SIZE ON/OFF or
c....MOTION ON/OFF
c....
 2000 itmp = IST
      call parsit           
      call parsit

      if (ITYP .eq. 1 ) then
         if (IST .ne.  ion .and. IST .ne.  ioff    ) then
            call error(56)
            return
         endif
         do 2010, i=1,iviewnum,1
            if ( itmp .eq. iborder)  then
                newborder(i) = ion - IST + 1
                fborder = 1
            endif
            if ( itmp .eq. iaxis  )  then
                newaxis(i)   = ion - IST + 1
                faxis = 1
            endif
            if ( itmp .eq. inamed )  then
                newnamed(i)  = ion - IST + 1
                fname = 1
            endif
            if ( itmp .eq. isize  )  then
                newsize(i)   = ion - IST + 1
                fsize = 1
            endif
            if ( itmp .eq. imotion)  then
                newmotion(i) = ion - IST + 1
                fmotion = 1
            endif
 2010    continue
      else 
         call error(232)
         return
      endif

      if(nextyp .eq. 11) then
             goto 100
      else if (nextyp .eq. 9 ) then
             goto 500
      else
         call error(57)
         return
      endif

c
c....added display mode
c....Yurong 4/15/99
c
 3000 call parsit
      call parsit
	
      if (ITYP .eq. 1 ) then
         do 3010, i=1,iviewnum,1
            if (IST .eq.  iwire)  then
                newmode(i) = 0
                fmode = 1
            endif
            if (IST .eq.  iboth)  then
                newmode(i) = 1
                fmode = 1
            endif
            if (IST .eq.  ihid)  then
                newmode(i) = 2
                fmode = 1
            endif
            if (IST .eq.  ishd)  then
                newmode(i) = 3
                fmode = 1
            endif
 3010 continue
      endif
    
      if(nextyp .eq. 11) then
             goto 100
      else if (nextyp .eq. 9 ) then
             goto 500
      else
         call error(57)
         return
      endif

  100 if(ifl(35).eq.0.or.(ifl(35).eq.2.and.ifl(350).gt.0)) then
        if(nmcount .ne. iviewnum) then
           call error(435)
           return
        endif
      else
        return
      endif
c.....
c.....All syntax is ok
c.....
      call uvu_select_screen_format1(screenname,iviewkey,
     *          fborder, newborder, faxis, newaxis, fname, 
     *          newnamed, fsize, newsize, fmotion,
     *          newmotion, fmode, newmode)
      return
      end

c*********************************************************************
c*    E_SUBROUTINE     : subroutine drftnam 
c*     Handles the DRAFT/NAME... statement.
c*
c*    We call this routine from the DRAFT routine, file DRAFT.F
c*
c*********************************************************************

      subroutine  drftnam

      include 'com4a.com'

c.....
     
      integer*4 newborder, newaxis, newnamed, newsize, newmotion,
     *          newmode

      integer*2 iborder, iaxis, inamed, isize, imotion, imode 
 
      integer*2 ion, ioff, IRESET
      integer*2 ishd, iwire, ihid, iboth


      data iborder /932/, iaxis /132/, inamed /931/,
     *     isize  /196/, imotion  /836/ , imode /1003/
      data ishd /578/, iwire /1028/, ihid /579/, iboth /83/

      data ion/71/, ioff/72/
      parameter (IRESET=15)

      integer*4 iviewkey(36)
c
c.....Set up some defaults:
c
      nmcount = 1
      newborder = -1
      newaxis   = -1 
      newnamed  = -1
      newsize   = -1
      newmotion = -1
      newmode = -1
c....
c....Loop through the statement to ensure the syntax is correct.
c....
      if (nextyp .ne. 1) then
         call error(6)
         goto 999
      endif

      call parsit
      call parsit
c....
c.... NAME PARAMETERS
c....
  100 if(ITYP .eq. 7) return

      if(ifl(35).eq.0.or.(ifl(35).eq.2.and.ifl(350).gt.0))then
         call uvu_view_chk(token2,iviewkey(nmcount))
         if (iviewkey(nmcount) .eq. 0) then
            if (ityp .eq. 1 ) then
               goto 500
            else
               ITYP = 1
               call error(434)
               return
            endif
         endif
         nmcount = nmcount+1
         if (nmcount .gt. 36) then
           call error(209)
           goto 999
         endif
         call parsit
         goto 100 
      else
         if (ityp .eq. 1 .and. (ist .eq. isize .or. ist .eq. iaxis .or.
     -       ist .eq. inamed .or. ist .eq. iborder .or. 
     -       ist .eq. imotion .or. ist .eq. ireset .or.
     -       ist .eq. imode)) goto 500
         call parsit
         goto 100
      endif
c
c...   Look for RESET
c
  500 if (ityp.eq.1 .and. ist.eq.IRESET) then
        if (.not. nxteos) then
          call error (4)
          goto 999
        endif
        if(ifl(35).eq.0.or.(ifl(35).eq.2.and.ifl(350).gt.0))then
          call rstvus (iviewkey)
        endif
        goto 999
      endif
c....
c....BORDER   AXIS   NAMED  SIZE   MOTION
c....or... it is a syntax error.
c....
  600 if (ITYP .eq. 1 .and. nextyp .eq. 1) then
         if (IST .eq.  iborder .or. IST .eq. iaxis .or.
     *       IST .eq.  inamed  .or. IST .eq. isize .or.  
     *       IST .eq.  imotion ) then
             goto 2000
c
c....added display mode
c....Yurong
c
			else if (IST .eq.  imode) then
             goto 3000
         endif
         ityp=2
         call error(436)
         return
      endif
      if (ITYP .ne. 1) then
         call error(232)
         return
      endif
      if (nextyp .ne. 1) then
         call error(6)
         return
      endif
c....
c....BORDER  ON/OFF or AXIS ON/OFF or NAMED ON/OFF or SIZE ON/OFF or
c....MOTION ON/OFF
c....
 2000 itmp = IST
      call parsit           
      call parsit

      if (ITYP .eq. 1 ) then
         if (IST .ne.  ion .and. IST .ne.  ioff    ) then
            call error(56)
            return
         endif
         if ( itmp .eq. iborder)  newborder = ion - IST + 1
         if ( itmp .eq. iaxis  )  newaxis   = ion - IST + 1
         if ( itmp .eq. inamed )  newnamed  = ion - IST + 1
         if ( itmp .eq. isize  )  newsize   = ion - IST + 1
         if ( itmp .eq. imotion)  newmotion = ion - IST + 1
      else 
         call error(56)
         return
      endif
      go to 3100
c
c....added display mode
c....Yurong 4/15/99
c
 3000 call parsit
      call parsit
	
      if (ITYP .eq. 1 ) then
          if (IST .eq.  iwire)  newmode = 0
          if (IST .eq.  iboth)  newmode = 1
          if (IST .eq.  ihid)  newmode = 2
          if (IST .eq.  ishd)  newmode = 3
          if (newmode .lt. 0 .or. newmode .gt. 3) then
             call error(428)
             return
          endif
      else if (ITYP .eq. 3 .or. (ityp .eq. 2 .and. ist .eq. 2) ) then
          if (TV .ge. 0 .and. TV .le. 7) then
              newmode = TV
          else
              call error(428)
              return
          endif
      else
          call error(428)
          return
      endif
c
c...Go get next parameter
c
 3100 if(nextyp .ne. 11) then
          if (nextyp .eq. 9 ) then
              call parsit
              goto 600
          endif
          call error(57)
          goto 999
      endif
    
c.....
c.....All syntax is ok
c.....
      if (ifl(35).eq.0) call uv_change_vport_attr(iviewkey, newborder,
     *                    newaxis,newnamed,newsize, newmode, 
     *                    newmotion)
999   return
      end
