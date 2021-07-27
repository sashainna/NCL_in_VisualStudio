C*********************************************************************
C*    NAME         :  prompt.f
C*       CONTAINS:
C*    COPYRIGHT 1993 (c) NCCS.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*        prompt.f , 25.3
C*    DATE AND TIME OF LAST  MODIFICATION
C*        10/19/15 , 10:01:51
C********************************************************************/
C
c*********************************************************************
c*
c*    subroutine prompt
c*
c*    The purpose of this routine to read the "prompt" statement and to
c*    fill up the appropriate places in the macro definition table if it
c*    is necesury.
c*
c***********************************************************************
      subroutine prompt2

      include 'com8a.com'

      character*24  vword(20)
      character*80  varname
      character*41  prmpt,defstr,prmpt0, tmpprmt
      character*65  scal, descript, tempdesp
      character*21  classname, classtr_sav
      character*80  label
      character*481 wrdlist, wrdlist0
      character*256 clrstr

      integer*4     outflag, dispflag, substr, pflag, substr0, pflag0
      integer*4     k,counter,counter0,strlen1,n,iend,llen,psel0, psel
      logical       minmax,defv,defs,defp,lgeo,clasdefs,prsflag

      integer*2     j,found1,imc, jv

      real*8        rdata(36),min,max,min0,max0
      integer*2     prsvalue,lenvalue,prsvalue0,lenvalue0,ietype,
     1              len1, len2, len3, geomflag, geomflag0
      integer*2     idata(144),nwds,sub(4),n2,len,clas
      character*288 cdata
      equivalence   (rdata,idata,cdata), (idata(2),nwds)
      equivalence   (rest,sub)
   
      character*(MAX_LEN) tmpcin,ucin,tmpcn
      integer*2     tmpinx
      character*200 tmpout

      integer*4     klin, kcol, knc, kmxc, geotype,nclkey
      integer*4     clr, isub, pclr0

      character*8   fmt(14)
      data fmt /' ','SHAPE/','POINT/','VECTOR/','LINE/','PLANE/',
     *              'CIRCLE/','CURVE/','SURF/','MATRIX/','PNTVEC/',
     *              'PATERN/','SOLID/',' '/

      character*64   macname
      equivalence    (sc165,macname)
 
      logical leq5
      integer*2 ktv(4),numprm,head(12),mode
      integer*4 maclin,termln,next,jhead(4)
      equivalence (head,leq5),(jhead,leq5)
      equivalence (maclin,jhead(1)),(next,jhead(2))
      equivalence (termln,jhead(3)),(mode,head(7))
      equivalence (ktv,tv)

c
c...Initiation block
c
      varname = ' '
      scal = ' '
      tempdesp = ' '
      descript = ' '
      min =      0
      max =      0
      prmpt =    ' '
      counter =  0
      prsvalue = 4
      lenvalue = 9
      geotype  = 1
      defstr   = ' '
      minmax =   .false.
      defv   =   .false.
      defs   =   .false.
      defp   =   .false.
      clasdefs = .false.
      prsflag = .false.
      substr = -1
      pflag = -1
      psel = -1
      imc    = 0
      geomflag = 0
      clr = -2
c
c...If we are executing a macro now - not to do anything.
c...Or if we are in the batch mode - use defaults only.
c
      if (ifl(38) .eq. 2 ) return
c
c...Not /
c
      if (nextyp .ne. 5) then
         call error(22)
         return
      endif
 
      ldtext = .true.
      ifl(44) = 9
      call parsit
c
c...Have got something else instead of identifier at the "NAME" place.
c
      if ((ityp .eq. 1) .and. (ist .eq. 601)) then
c
c...SCALAR
c...prompt/SCALAR, name [,class], desp_text
c
         call parsit
c
c...if it is not a scalar name, then wrong
c
         if ((ityp .ne. 2) .or. (ist.ne.2)) then
             call error(87)
             goto 99999
         endif         
c
c...Get full variable name from the token
c
         call expnm2(token2, ivxsub, jv,scal)
c
c...',' expected.
c
         if(nextyp .ne. 9) then
            ityp = 1
            call error(57)
            goto 99999
         endif
         call parsit
         len1 = 0
         len2 = 0
         len3 = 0
   60    if(ityp .eq. 9) then
            if ((.not.clasdefs).and.(.not.defp)) then
c
c....text string, it could be class name
c
                 classname = token2
                 tempdesp = token2
                 clasdefs = .true.
                 len2 = strlen1 (classname)
            else if (.not.defp) then
                 descript = token2
                 defp  = .true.
                 len3 = strlen1 (descript)
            else
                 call error(4)
                 goto 99999
            endif
            call parsit
            goto 60
         else if (ityp .eq. 2 .and. ist.eq.TEXTVAR) then
            if ((.not.clasdefs).and.(.not.defp)) then
c
c....text variable, it could be class name
c
                len = 20
                call gtdesc(tv,nclkey,nwds,ietype)
                call ncl_gttext(nclkey,classname,len)
                do 250 j=len+1,20,1
                    classname(j:j) = ' '
  250           continue 
                tempdesp(1:20) = classname(1:20)
                len2 = strlen1 (classname)
                clasdefs = .true.
            else if (.not.defp) then
                defp  = .true.
                len   = 40
                call gtdesc(tv,nclkey,nwds,ietype)
                call ncl_gttext(nclkey,descript,len)
                len3 = strlen1 (descript)
            else
                call error(4)
                goto 99999
            endif
            call parsit
            goto 60
         endif
c
c...If next elmt is not "End Of Input" and not ','
c
         if(nextyp .ne. 11 .and. nextyp .ne. 9) then
              ityp = 1
              call error(57)
              goto 99999
         endif
c
c...If the next elmt is EOI and clasdefs = .false.
c
         if ((nextyp .eq. 11) .and. (.not.clasdefs) 
     1                        .and. (.not.defp) ) then
             call error(440)
             goto 99999
         endif

         if(clasdefs .and. (.not.defp) ) then
              descript = tempdesp
              defp  = .true.
              clasdefs  = .false.
              len2 = 0
              len3 = strlen1 (descript)
         endif

         len1 = strlen1(scal)
         call ncl_savscalar(scal, len1, classname, len2,
     1           descript, len3)
         goto 99999
      else if (ityp .ne. 2) then
         call error(87)
         goto 99999
      endif
c
c...Not macro mode: Any type of identifier allowed.
c      
      if(ifl(38) .eq. 0) then
         if (ityp .ne. 2) then
            call error(282)
            goto 99999
         endif
      else 
c
c...Defining macro mode: macro parameter only allowed.
c...Allow for the Macro name to have a description
c...Bobby  -  10/2/97
c
         if (ityp .ne. 2 .or. (ist .ne. 11 .and. ist .ne. 12)) then
            call error(70)
            goto 99999
         endif
         if (ist .eq. 11 .and. token2 .ne. macname) then
            call error(70)
            goto 99999
         endif
         if (ist .eq. 11) imc = 1
      endif
c
c...Get full variable name from the token
c
      call expnm2(token2,ivxsub,jv,varname)
c
c...',' expected.
c
      if(nextyp .ne. 9) then
         ityp = 1
         call error(57)
         goto 99999
      endif
c
c...This part of the command is going to be different for
c...the macro definition mode and the command mode.
c
c...Macro Definition mode.
c...PROMPT/NAME[,len,pres],"PROMPT_STR"[,min,max]
c...           ^                       [,vocword list]
c...           |
c
      if (ifl(38) .eq. 1) then
         call ncl_getmac_parms0(classname, outflag, dispflag,
     1               prmpt)
         classtr_sav = classname
         call parsit
         if(.not.scalar) goto 80
c
         if (imc.eq.1) then
            call error(440)
            goto 99999
         endif

         lenvalue = tv
         call parsit
         if(.not.scalar) then
            call error(282)
            goto 99999
         endif
         prsvalue = tv
         prsflag = .true.
c     
         call parsit
c
c...Prompt string expected
c
   80    if (imc.eq.1) then
             if (ityp .eq. 9) then
                 if ((.not.clasdefs).and.(.not.defp)) then
c
c....text string, it could be class name
c
                     classname = token2
                     tmpprmt = token2
                     clasdefs = .true.
                 else if (.not.defp) then
                     prmpt = token2
                     defp  = .true.
                 else
                     call error(4)
                     goto 99999
                 endif
                 call parsit
                 goto 80
             else if (ityp .eq. 2 .and. ist.eq.TEXTVAR) then
                 if ((.not.clasdefs).and.(.not.defp)) then
c
c....text variable, it could be class name
c
                     len = 20
                     call gtdesc(tv,nclkey,nwds,ietype)
                     call ncl_gttext(nclkey,classname,len)
                     do 150 j=len+1,20,1
                         classname(j:j) = ' '
  150                continue 
                     tmpprmt(1:20) = classname(1:20)
                     clasdefs = .true.
                 else if (.not.defp) then
                     defp  = .true.
                     len   = 40
                     call gtdesc(tv,nclkey,nwds,ietype)
                     call ncl_gttext(nclkey,prmpt,len)
                 else
                     call error(4)
                     goto 99999
                 endif
                 call parsit
                 goto 80
             endif

             if (ityp .eq. 1 .and. ist.eq.172) then
c...OMIT
                 outflag = 0
                 call parsit
                 goto 80
             else if (ityp .eq. 1 .and. ist.eq.653) then
c...OUT
                 outflag = 1
                 call parsit
                 goto 80
             endif
             if (ityp .eq. 1 .and. ist.eq.329) then
c...RETAIN
                 dispflag = 1
                 call parsit
                 goto 80
             else if (ityp .eq. 1 .and. ist.eq.903) then
c...DEFALT
                 dispflag = 0
                 call parsit
                 goto 80
             endif
c
c...If next elmt is not "End Of Input" and not ','
c
             if(nextyp .ne. 11 .and. nextyp .ne. 9) then
                 ityp = 1
                 call error(57)
                 goto 99999
             endif
c
c...If the next elmt is EOI and clasdefs = .false.
c
             if ((nextyp .eq. 11) .and. (.not.clasdefs) 
     1                        .and. (.not.defp) ) then
                 call error(440)
                 goto 99999
             endif

             if(clasdefs .and. (.not.defp) ) then
                 prmpt = tmpprmt
                 defp  = .true.
                 classname= classtr_sav
                 clasdefs  = .false.
             endif
         endif
         if (imc.eq.0) then
            if (ityp .eq. 9) then
               prmpt = token2
               call parsit
            else if (ityp .eq. 2 .and. ist.eq.TEXTVAR) then
               prmpt = ' '
               len = 40
               call gtdesc(tv,nclkey,nwds,ietype)
               call ncl_gttext(nclkey,prmpt,len)
               call parsit
            else
               call error(440)
               goto 99999
            endif
         endif
      else
c
c...Command mode.
c...PROMPT/NAME[,defvalue],"PROMPT_STR"[,min,max]
c...           [,"defstr"]             [,geotype]
c...           ^
c...           |
c
         call parsit
         if(scalar) then
            defval = tv
            defv   = .true.
         else if(ityp .eq. 9) then
            defstr = token2
            defs   = .true.
         else if (ityp .eq. 2 .and. ist.eq.TEXTVAR) then
            defstr = ' '
            len = 40
            call gtdesc(tv,nclkey,nwds,ietype)
            call ncl_gttext(nclkey,defstr,len)
            defs   = .true.
         else
            call error(440)
            goto 99999
         endif
c
c...If next elmt is not "End Of Input" and not ','
c
         if(nextyp .ne. 11 .and. nextyp .ne. 9) then
            ityp = 1
            call error(57)
            goto 99999
         endif
c
c...If the next elmt is EOI and defs = .false.
c
         if (nextyp .eq. 11 .and. (.not.defs)) then
            call error(440)
            goto 99999
         endif
         call parsit
         if(ityp .eq. 9) then
            prmpt = token2
            defp  = .true.
            call parsit
         else if (ityp .eq. 2 .and. ist.eq.TEXTVAR) then
            prmpt = ' '
            defp  = .true.
            len   = 40
            call gtdesc(tv,nclkey,nwds,ietype)
            call ncl_gttext(nclkey,prmpt,len)
            call parsit
         endif
      endif
c
c...Simple case. No limits. 
c
      if (ityp .eq. 7) goto 1000
c...
c...MIN / MAX
c...
      if (imc.eq.1) goto 1000
      if (scalar) then
         if (defs .and. defp) then
            call error(446)
            goto 99999
         endif
         if (nextyp .ne. 9) then
            call error(57)
            goto 99999
         endif
         min = tv
         call parsit
         if(.not. scalar) then
            call error(282)
            goto 99999
         endif
         max = tv
         minmax = .true.
c
c...MIN/MAX is the end of the statement
c
         call parsit
	   if (ityp .eq. 7) goto 1000
         call error(4)
         goto 99999
      else if(ityp .eq. 1) then
c...
c... A LIST OF VOCABULARY WORDS
c...
c
c... Vocabulary words are not allowed in the command mode.
c... geometry types are only allowed in this mode. They are:
c... SCALER-601, SHAPE-602, POINT-603, VECTOR-604, LINE-605, PLANE-606, 
c... CIRCLE-607, CURVE-608, SURF-609,  MATRIX-610, PV-613,   PATERN-636
c
         if(ifl(38) .ne. 1) then 
            if(ist .lt. 601 .or.  
     *        (ist.gt.610 .and. ist.ne.613 .and. ist.ne.636))then 
               call error(1) 
               goto 99999 
            endif 
            if(defv .and. ist .ne. 601) then
               call error(448)
               goto 99999
            endif
            iend = 1
            if(ist .le. 610) geotype = ist - 600
            if(ist .eq. 613) geotype = 11
            if(ist .eq. 636) geotype = 12
            if(ist .eq. 123) geotype = 13
         else
c
c...added SUBSTR, RETAIN    and NOW
c...              LABEL         NEXT
c...              NUM
            if (ityp .eq. 1 .and. ist.eq.580) then
c...SUBSTR
                call parsit
                if (ityp .eq. 1 .and. ist.eq.329) then
c...RETAIN
                    substr = 0
                    call parsit
                else if (ityp .eq. 1 .and. ist.eq.936) then
c...LABEL
                    substr = 1
                    call parsit
                else if (ityp .eq. 1 .and. ist.eq.563) then
c...NUM
                    substr = 2
                    call parsit
                else 
                    call error(232)
	              goto 99999
                endif
            endif

            if (ityp .eq. 7) goto 1000

            if (ityp .eq. 1 .and. ist.eq.161) then
c...NOW
                pflag = 0
                call parsit
            else if (ityp .eq. 1 .and. ist.eq.162) then
c...NEXT
                pflag = 1
                call parsit
            endif
            if (ityp .eq. 7) goto 1000
           
            if (ityp .eq. 1 .and. ist.eq.1074) then
c....SELECT                
                psel = 1
                call parsit
                if (ityp .eq. 7) goto 1000
c
c...if SELECT, need followed a color
c
                 if (ITYP .eq. 1) then
                    clrstr = token2
                    j = strlen1(token2)
                    isub = 0
                    call ncl_getclr_inx(clrstr, j, isub, clr)
                 else if ((ITYP .eq. 2 .and. ist.eq.1 ).or.
     x              (ITYP .eq. 2 .and. ist.eq.24 ).or.
     x              (ityp.eq.9)) then
                    if ((ityp.eq.9).or.(ITYP.eq.2 .and. ist.eq.1)) then
                      clrstr = token2
                      j = strlen1(token2)
                      isub = ivxsub
                    endif
                    if (ityp.eq.2.and.ist.eq.24) then
                       j = 0
                       call gttext(clrstr,j)
                       isub = 0
                    endif
                    call ncl_getclr_inx(clrstr, j, isub, clr)
                 else if (.not.( (ityp.eq.3).or.(ityp.eq.4).or.
     x              ((ityp.eq.2).and.(ist.eq.2)) )) then
                    ifl(2)=53
                    go to 99999
                 else
                    clr = tv
                 endif
                 call parsit
            else if (ityp .eq. 1 .and. ist.eq.325) then
c....ONCE          
                psel = 0
                call parsit
            endif
c...MIN/MAX
c
            if (ityp .eq. 7) goto 1000
            if (scalar) then
                if (defs .and. defp) then
                   call error(446)
                   goto 99999
                endif
                if (nextyp .ne. 9) then
                   call error(57)
                   goto 99999
                endif
                min = tv
                call parsit
                if(.not. scalar) then
                   call error(282)
                   goto 99999
                endif
                max = tv
                minmax = .true.
c
c...MIN/MAX is the end of the statement
c
                call parsit
                if (ityp .eq. 7) goto 1000
                call error(4)
                goto 99999
            else
                iend = 20
                geomflag = 1
                do 300 i=1,iend,1 
c
c....if the word is not geometry types, set geom-list flag = 0
c... if the wordlist include all word area geometry types, then geomflag=1
c... SCALER-601, SHAPE-602, POINT-603, VECTOR-604, LINE-605, PLANE-606, 
c... CIRCLE-607, CURVE-608, SURF-609,  MATRIX-610, PV-613,   PATERN-636
C... SOLID-123
c
                   if ((ist .lt. 601 .and. ist.ne.123) .or.  
     *             (ist.gt.610 .and. ist.ne.613 .and. ist.ne.636)) then
                       geomflag = 0
                   endif
                   vword(i) = token2(1:24)
                   counter  = i
                   if (nextyp .eq. 11) goto 1000
                   if (nextyp .ne. 9) then
                       call error(57)
                       goto 99999
                   endif
c
c...'thru' claud not allow, return word set idtype=-1 to avoid error
c...message from parser thru claud  
c...yurong
c
                   idtype = -1
                   call parsit
                   if (ityp .ne. 1) then
                       call error(282)
                       goto 99999
                  endif
  300          continue
            endif
	   endif
      else
         call error(442)
         goto 99999
      endif

 1000 continue
c...
c...All syntax is ok. Now if we are in the "macro definition" mode
c...save this information in the ranfil, othewise interrupt the execution,
c...print out the prompt and wait for an answer.
c...
      if(ifl(38) .eq. 0) then
c
c...Running (running step) mode
c
         tmpcin = cin
         tmpinx = inx
         if(defs .and. (.not.defp) ) then
            prmpt = defstr
            defp  = .true.
            defstr= ' '
            defs  = .false.
         endif
         call putw2(nline)
         cout = ' '
         if(minmax) then
            write(tmpout,20) min,max
   20       format(' [Range: ',f20.4,'-',f20.4,']')
            call delsp(tmpout,strlen1(tmpout))
            cout = tmpout
         endif
c
c...Get the defstr 
c
         if(defs) then
            n = strlen1(cout)
            if (n .gt. 0) then
               write(tmpout,30)cout(1:n),defstr  
   30          format(A,'[Default: ',A,']')
            else
               write(tmpout,35)defstr
   35          format('[def:',A,']')
            endif
            call delsp(tmpout,strlen1(tmpout))
            cout = tmpout
         endif
c
c...Get the defval 
c
         if(defv) then
            n = strlen1(cout)
            if (n .gt. 0) then
               write(tmpout,31)cout(1:n),defval
   31          format(A,'[def:',f20.4,']')
            else    
               write(tmpout,36)defval
   36          format('[def:',f20.4,']')
            endif   
            call delsp(tmpout,strlen1(tmpout))
            cout = tmpout
         endif
c
c...Get the prompt 
c
         n = strlen1(cout)
         if(n .lt. 1) n = 1
         write(tmpout,40)prmpt,cout(1:n)
   40    format(A,A)
         cout = tmpout
         inx = 1
         if (ifl(35) .eq. 2) then
c
c...NCL-VT output style
c
            n = strlen1(cout)
            n2 = n
            call putmsg(cout,n2,2,3)
            nccin = 0
            kmxc =5
            kcol = 9
            klin = 1
            call gvtlin(cin,nccin,kmxc,klin,kcol)
            klin = 3
            kcol = 1
            call plot(klin,kcol)
            call clreol
            call wflush
         else if(ifl(35) .eq. 0) then
c
c...Regular NCL output style
c
            nccout = strlen1(cout)
            call nclpmt (cout, nccout, cin, nccin)
         else if(ifl(35) .eq. 1) then
c
c...Batch mode. No output, use defaults only.
c
            cin = ' '
            nccin = 0
         endif
         call convrt(cin,ucin,nccin)
         cin = ucin(1:nccin)
         call clswin()
         call parsit
c
c...Handle the answer
c
         if(ityp .ne. 7) then
             if(scalar .and. geotype .eq. 1 .and. minmax) then
                if (tv .gt. max .or. tv .lt. min) then
                   call error(445)
                   goto 99999
                endif
             endif
c
             tmpcn= cin(1:nccin)
c
             write(cin,601) varname(1:jv),fmt(geotype)
             nccin = strlen1(cin)
             call delsp(cin,nccin)
             write(cin(n+1:),602)tmpcn(1:255-n)
  601        format(A,'=',A,A)
  602        format(A)
         else
            if (defs) then
               write(cin,601) varname(1:jv),fmt(geotype),defstr
            else if (defv) then
               write(cin,110) varname(1:jv),defval
  110          format(A,'=',f20.10) 
            else
               call error(447)
               goto 99999 
            endif
         endif
         nccin = strlen1(cin)
         call delsp(cin,nccin)
c
         ifl(124) = 0
         do 111 j=1,nccin,1
            if(cin(j:j) .eq. '('.and. cin(j+1:j+1).eq.'*') then
                  ifl(124) = j 
                  go to 112
            endif
  111    continue
c
  112    if (ifl(124) .ne. 0) then
            inx = 1
            ifl(111)=1
            found1=0
            ifl(2) = 0
            err = .false.
            ifl(44)=5
            call chknst(found1)
         endif
c
         inx = 1
         ifl(111)=0
         ifl(2) = 0 
         err = .false. 
         ifl(44)=5
c
         call parsit
         err = .false.
         call assgn
         if (err)  goto 99999
         if (ifl(262) .eq. 0) then
            if (idst.gt.6.and.idst.lt.10.or.idst.eq.18.or.
     *      isc10(1).eq.575) then
                if (isc10(1).eq.575) then
                    call mocntl
                else
                    call geogn2
                endif
            else if ((idst.gt.2.and.idst.lt.7).or.idst.eq.10) then
                call geogn1
            endif
         else
            ifl(262) = 0
         endif
         if (ifl(35) .eq. 0) then
           call gtdesc(rest,nclkey,nwds,ietype)
           call ifidge (ietype,lgeo)
           if (lgeo) call dspent(nclkey,ietype)
         endif
         call vstore
         cin = tmpcin
         nccin = strlen1(cin)
         inx = tmpinx
         goto 99999
c
c...Macro Definition Mode
c
      else
         token2  = macname
         call vstchk
         numprm = ktv(3)
         next   = jhead(3)
         if (imc .eq. 1) then
             len1 = strlen1(classname)
             len2 = strlen1(prmpt)
             call ncl_putmac_parms0(classname, len1, outflag, dispflag,
     1               prmpt, len2)
             goto 99999
         endif                
         do 1500 k=1,numprm,1
c
c...get the macro parameters
c
c...reset label
c...it will have error if not
c
            label = '        '
            call ncl_getmac_parms(k, label, clas,
     1               prmpt0, counter0, wrdlist0, geomflag0,
     2               min0, max0, prsvalue0, 
     3               lenvalue0,substr0, pflag0, psel0, pclr0)
            llen = strlen1(label)
            if (jv .gt. llen) llen = jv
            if (label(1:llen).eq.varname(1:llen)) then
                do 350 i = 1,counter,1
                     wrdlist((i-1)*24+1:i*24) = vword(i)
  350           continue
                if (.not.minmax) then
                    min = min0
                    max = max0
                endif
                if (.not.prsflag) then
                    prsvalue = prsvalue0
                    lenvalue = lenvalue0
                endif
                if ((counter.eq.0).and.(.not.minmax)) then
                    counter0 = counter0
                    wrdlist = wrdlist0
                endif
                if (substr.eq.-1) substr = substr0
                if (pflag.eq.-1) pflag = pflag0
                if (psel.eq.-1) psel = psel0
                if (clr.eq.-2) clr = pclr0
                len1 = strlen1(prmpt)
                len2 = strlen1(wrdlist)
                if (counter .eq.0.and.(.not.minmax)) clas = 0
                if (minmax)                          clas = -1
                if (counter .ne. 0)                  clas = counter
                call ncl_putmac_parms(k, clas,
     1               prmpt, len1, counter, wrdlist, len2, geomflag, 
     2               min, max, prsvalue, 
     3               lenvalue,substr, pflag, psel, clr)
                goto 99999
            endif                
 1500    continue
      endif
99999 continue
      ldtext = .false.
      return
c
      end
c*********************************************************************
c*
c*    subroutine delsp
c*
c***********************************************************************

      subroutine delsp(str,len)

      character*(*)str
      integer*4 i, k, len

      do 800 i=len,1,-1
         if (str(i:i) .gt. ' ') goto 700
  800 continue
  700 len = i
c
  100 continue
      do 500 i=1,len-1,1
         if(str(i:i) .eq. ' ') then
            do 600 k=i,len-1,1
  600          str(k:k)=str(k+1:k+1)
            str(len:len) = ' '
            len = len-1
            goto 100
         endif
  500 continue
      return
      end
