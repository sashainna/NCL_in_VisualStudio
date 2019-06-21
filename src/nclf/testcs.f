C***********************************************************************
C*    NAME         :  testcs.f
C*       CONTAINS:
C*             testcs  reptok
C*    COPYRIGHT 2015 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*        testcs.f , 25.4
C*    DATE AND TIME OF LAST  MODIFICATION
C*        01/20/17 , 11:20:16
C***********************************************************************
C
c
c***********************************************************************
c
c   SUBROUTINE:  testcs (cbuf,knc,knvar)
c
c   FUNCTION:  Parses the last processed line and returns this line
c              modified with the correct entity references updated.  It
c              assumes that all preparsing has been done ({}, data elements,
c              etc.).  The following entity label substitutions will be
c              made in this routine.
c
c                  Macro Arguments
c                  Scalars
c
c              The key list of referenced geometry entities will be stored
c              in the 'skylst'.
c
c   INPUT:  none.
c
c
c   OUTPUT: cbuf     C*n  D1  -  Number of chars in 'fname'.
c
c           knc      I*4  D1  -  Number of chars in 'cbuf'.
c
c           knvar    I*4  D1  -  Number of entities stored in 'skylst'.
c
c           kpock    I*4  D1  -  1 = Active command is  a POCKET statement.
c
c***********************************************************************
c
      subroutine testcs (cbuf,knc,knvar,kpock)
c
      include 'com.com'
c
      integer*2 knc(10)
      integer*4 knvar,kpock
c
      character*80 cbuf(10)
c
      integer*2 inxsv,ietype,nwds,nsf,isav,ifl44,ndwds,i,ivoc,nxtyp
      integer*4 nclkey,nc,ncsav,inc,ix,rindex1,iprev,dtkey,isub,strlen1
c
      character*64 pretok,label
      character*80 tbuf,sbuf
      character*1536 cinsv
c
c...Initialize routine
c
      cinsv  = cin
      ncsav  = nccin
      ifl44  = ifl(44)
      ifl(44) = 0
      kpock  = 0
      ifl(2) = 0
      nsf = 0
c
c...Get the input line
c
      jquit = 0
      isav = ifl(35)
      ifl(35) = 1
      call getmsg (jquit)
      ifl(35) = isav
      nline = svll
      call substx
c
c...Get next token
c
      ityp   = 0
      ist    = 0
  100 iprev  = ityp
      iprev  = iprev*10000 + ist
      pretok = token2
      inxsv  = inx
  120 call parsit
      if (ityp .eq. 5 .and. ist .eq. 1 .and. iprev .eq. 10902) go to 120
      if (ityp .eq. 11) then
        do 150 i=1,10,1
          cbuf(i) = ' '
          knc(i) = 0
  150   continue
        go to 8000
      endif
      if (iprev .eq. 0 .and. ityp .eq. 1 .and. ist .eq. 738) kpock = 1
c
c...Store variable key and
c...replace token in commmand line if necessary
c
      if (ityp .eq. 2) then
c
c......Geometry label
c......Could be Macro argument
c
        call ifidge (ist,geom)
        if (geom) then
          nc     = length
          i4     = 0
          sbuf = token2
          call nclf_format_label(sbuf,ivxsub,tbuf,i4)
          nc     = strlen1(tbuf)
          call reptok (cin,nccin,tbuf,nc,inxsv,inx)
          call gtdesc (tv,nclkey,nwds,ietype)
          call addsky (0,nclkey,0,nsf)
c
c......Scalar label
c......Replace with value
c
        else if (ist .eq. 2) then
          call rtoc (tv,tbuf,nc)
          call reptok (cin,nccin,tbuf,nc,inxsv,inx)
c
c......Subscripted entity
c
        else if (ist .eq. 14) then
          nc     = length
          call reptok (cin,nccin,token2,nc,inxsv,inx)
c
c......Data statement
c
        else if (ist .eq. 23) then
          ldata  = .true.
          call gtdesc (tv,dtkey,nwds,ietype)
          call dtgtnw (dtkey,ndwds)
          call addsky (0,dtkey,0,nsf)
          do 180 i=1,ndwds,1
            call dtgetv (dtkey,i,tv,ityp,nxtyp,token2,isub)
            if (ityp .eq. 2) then
              call vstchk
              call ifidge (ist,geom)
              if (geom) then
                call gtdesc (tv,nclkey,nwds,ietype)
                call addsky (0,nclkey,0,nsf)
              endif
            endif
  180     continue
        endif
      endif
c
c...Real value
c...Output actual value instead of equation
c
      if (ityp .eq. 3 .or. ityp .eq. 4) then
          call rtoc (tv,tbuf,nc)
          call reptok (cin,nccin,tbuf,nc,inxsv,inx)
c
c...LAYER=n
c...Output geometry on this layer
c
        if (iprev .eq. 10902) then
          nclkey = tv
          call addsky (1,nclkey,0,nsf)
        endif
      endif
      if (nextyp .ne. 11) go to 100
c
c...Return command string
c
      inc    = 0
  200 if (nccin .gt. 78) then
        ix     = rindex1(cin(1:78),',')
        if (ix .eq. 0) ix = 78
        inc    = inc    + 1
        cbuf(inc) = cin(1:ix) // '$'
        cin = cin(ix+1:)
        nccin = nccin - ix
        knc(inc) = ix + 1
        go to 200
      else
        inc    = inc    + 1
        cbuf(inc) = cin(1:nccin)
        knc(inc) = nccin
        do 250 i=inc+1,10,1
            knc(i) = 0
  250   continue
      endif
c
c...End of routine
c
 8000 knvar  = nsf
      cin    = cinsv
      nccin  = ncsav
      ifl(44) = ifl44
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  reptok (cbuf,knc,ctoken,knctok,knxsv,knx)
c
c   FUNCTION:  Replaces the specfied text in the inputl line 'cbuf' with
c              the provided token 'tokbuf'.
c
c   INPUT:  cbuf     C*n  D1  -  Input line to replace text in.
c
c           knc      I*4  D1  -  Number of chars in 'cbuf'.
c
c           ctoken   C*n  D1  -  Text to place in 'cbuf'.
c
c           knctok   I*4  D1  -  Number of chars in 'ctoken'.
c
c           knxsv    I*4  D1  -  Pointer to beginning of token to be
c                                replaced in 'cbuf'.
c
c           knx      I*4  D1  -  Pointer to character after token to be
c                                replaced.
c
c   OUTPUT: cbuf     C*n  D1  -  Modified input line.
c
c           knc      I*4  D1  -  Number of chars in 'cbuf'.
c
c           knx      I*4  D1  -  Modified index to character after replaced
c                                token.
c
c***********************************************************************
c
      subroutine reptok (cbuf,knc,ctoken,knctok,knxsv,knx)
c
      include 'com.com'
c
      integer*2 knxsv,knx
      integer*4 knc,knctok
c
      character*(*) cbuf,ctoken
c
      character*1536 tbuf
c
      integer*4 nc
c
c...Replace token
c
      tbuf   = cbuf
      if (knxsv .eq. 1) then
        tbuf   = ctoken
        nc     = knctok
      else
        tbuf   = cbuf(1:knxsv-1) // ctoken
        nc     = knxsv-1 + knctok
      endif
c
      if (knx .gt. knc) then
        cbuf   = tbuf(1:nc)
        knc    = nc
      else
        cbuf   = tbuf(1:nc) // cbuf(knx:knc)
        knc    = nc     + knc-knx + 1
      endif
      knx    = nc     + 2
c
c...End of routine
c
 8000 return
      end

