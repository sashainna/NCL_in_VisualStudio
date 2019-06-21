/********************************************************************* 
**  NAME:  gdidd.h
**
**      Define workstation dispatch table entries.
**
**  COPYRIGHT  2000  NCCS, Inc.
**     MODULE NAME AND RELEASE LEVEL 
**       gdidd.h , 25.6
**    DATE AND TIME OF LAST  MODIFICATION
**       07/05/16 , 11:28:37
*********************************************************************/
#ifndef GDIDDH

/** control functions **/
#define UG_DOPENWS 0 				/* open workstation */
#define UG_DCLOSEWS 1 				/* close workstation */
#define UG_DACTWS 2 				/* activate workstation */
#define UG_DDEACTWS 3 				/* deactivate workstation */
#define UG_DCLEARWS 4 				/* clear workstation */
#define UG_DREDRAWWS 5 				/* redraw all segs on ws */
#define UG_DUPDATE 6 				/* update workstation */
#define UG_DCHGXFORM 7				/* an xform has changed, update segs in rect */
#define UG_DSETDEFST 8 				/* set deferral state */
#define UG_DMSG 9 					/* message */
#define UG_DESCAPE 10 				/* escape */
#define UG_DCALL 11 				/* call segment */
#define UG_DIMAGE 12				/* set imaging mode */
#define UG_DINTRACTV 13				/* set interactive mode */
#define UG_DHLHSRMODE 14			/* set hidden line/hidden surface removal mode */
#define UG_DSIGNAL 15				/* set interrupt function handler */
/* output primitives **/
#define UG_DPOLYLN 16 				/* polyline */
#define UG_DPOLYLN3 17 				/* polyline (3-d) */
#define UG_DPOLYMK 18 				/* polymarker */
#define UG_DPOLYMK3 19 				/* polymarker (3-d) */
#define UG_DTEXT 20 				/* text */
#define UG_DFLAREA 21 				/* fill area */
#define UG_DFLAREA3 22 				/* fill area  3D */
#define UG_DFLAREANM3 23			/* fill area  with normals 3D */
#define UG_DCELL 24 				/* cell array */
#define UG_DCELL3 25				/* cell array (3D) */
#define UG_DCELLRUN 26				/* runlength encoded 2D cell array */
#define UG_DCELLRUN3 27				/* runlength encoded 3D cell array */
#define UG_DGDP 28 					/* generalized drawing primitive */
#define UG_DPOLYLNRAS 29			/* raster polyline */
#define UG_DPOLYMKRAS 30			/* raster polymarker */
#define UG_DRASTEXT 31				/* raster text */
#define UG_DFLAREARAS 32			/* raster fill area */
#define UG_DCELLRAS 33				/* raster cell array */
#define UG_DCELLRUNRAS 34			/* raster runlength encoded cell array */
#define UG_DGDPRAS 35				/* raster GDP */
#define UG_DBEEP 36					/* beep the bell */
/** output attributes **/
#define UG_DLNINDEX 37 				/* set polyline  */
#define UG_DLINETYPE 38 			/* set linetype */
#define UG_DLINEWIDTH 39 			/* set linewidth scale factor */
#define UG_DLNCINDEX 40 			/* set polyline color  */
#define UG_DMKINDEX 41 				/* set polymarker  */
#define UG_DMKTYPE 42 				/* set marker type */
#define UG_DMKSIZE 43 				/* set marker size scale factor */
#define UG_DMKCINDEX 44 			/* set polymarker color  */
#define UG_DTEXTINDEX 45 			/* set text  */
#define UG_DTEXTFP 46 				/* set text font and precision */
#define UG_DCHAREXP 47 				/* set character expansion factor */
#define UG_DCHARSPACE 48 			/* set character spacing */
#define UG_DTXCINDEX 49 			/* set text color  */
#define UG_DCHARHT 50 				/* set character height */
#define UG_DTXPLANE 51 				/* set character plane */
#define UG_DCHARUP 52 				/* set character up vector */
#define UG_DCHARUP3 53 				/* set character up vector (3-d) */
#define UG_DTEXTPATH 54 			/* set text path */
#define UG_DTEXTALIGN 55 			/* set text alignment */
#define UG_DFAINDEX 56 				/* set fill area  index */
#define UG_DFAINTSTYLE 57 			/* set fill area interior style */
#define UG_DFASTYLEINDEX 58 		/* set fill area style  */
#define UG_DFACINDEX 59 			/* set fill area color  */
#define UG_DPATSIZE 60 				/* set pattern size */
#define UG_DPATREFPT 61 			/* set pattern reference point */
#define UG_DASFS 62 				/* set aspect source flags */
#define UG_DPICKID 63 				/* set pick identifier */
#define UG_DLNREP 64 				/* set polyline representation */
#define UG_DMKREP 65 				/* set polymarker representation */
#define UG_DTEXTREP 66 				/* set text representation */
#define UG_DFAREP 67 				/* set fill area representation */
#define UG_DPATREP 68 				/* set pattern representation */
#define UG_DCOLORREP 69 			/* set color representation */
#define UG_DEDGEFLAG 70				/* set edge flag */
#define UG_DINTRCOLOR 71			/* set polygon interior color */
#define UG_DINTRSHADE 72			/* set polygon shading method */
#define UG_DINTRLIGHT 73			/* set polygon lighting method */
#define UG_DSURFPROP 74				/* set polygon surface props */
#define UG_DLIGHTREP 75				/* set light source representations */
#define UG_DLIGHTSTATE 76			/* set light source states */
/** transformations **/
#define UG_DWIND 77 				/* set window  (3D) */
#define UG_DVPORT 78 				/* set viewport (3D) */
#define UG_DVPPRI 79 				/* set viewport input priority */
#define UG_DNORMTRAN 80 			/* select normalization transform */
#define UG_DCLIP 81 				/* set clipping indicator */
#define UG_DWSWIND 82 				/* set workstation window (2D) */
#define UG_DWSVPORT 83 				/* set workstation viewport (2D) */
#define UG_DVREF3 84 				/* set view reference point (3D) */
#define UG_DVPN3 85 				/* set view plane (3D) */
#define UG_DVUP3 86 				/* set view up-vector (3D) */
#define UG_DMOD3 87 				/* set modeling matrix (3D) */
#define UG_DVMAP 88					/* set view mapping */
/** segments **/
#define UG_DCRESEG 89 				/* create segment */
#define UG_DOPNSEG 90 				/* open segment */
#define UG_DCLOSEG 91 				/* close segment */
#define UG_DRENSEG 92 				/* rename segment */
#define UG_DDELSEG 93 				/* delete segment */
#define UG_DDELSEGWS 94 			/* delete segment from workstation */
#define UG_DASSSEG 95 				/* associate segment with workstation */
#define UG_DCOPYSEG 96 				/* copy segment to workstation */
#define UG_DINSSEG 97 				/* insert segment */
#define UG_DSEGTRAN 98 				/* set segment transformation */
#define UG_DSEGVIS 99				/* set visibility */
#define UG_DHILITE 100 				/* set hilighting */
#define UG_DSEGPRI 101 				/* seg segment priority */
#define UG_DSEGDET 102 				/* set detectability */
/** segment element functions */
#define UG_DINLABEL 103 			/* insert label */
#define UG_DSETEPT 104   			/* set element pointer */
#define UG_DOFEPT 105    			/* offset element pointer */
#define UG_DEPTLBL 106    			/* set element pointer to a label */
#define UG_DELELT 107    			/* delete element */
#define UG_DELELTR 108    			/* delete element range */
#define UG_DELELTLBL 109    		/* delete element between labels */
/** input functions */
#define UG_DINITLOC 110 			/* initialize locator */
#define UG_DINITSTROKE 111 			/* initialize stroke */
#define UG_DINITVAL 112 			/* initialize valuator */
#define UG_DINITCHOICE 113 			/* initialize choice */
#define UG_DINITPICK 114 			/* initialize pick */
#define UG_DINITSTRING 115 			/* initialize string */
#define UG_DLOCMODE 116 			/* set locator mode */
#define UG_DSTROKEMODE 117 			/* set stroke mode */
#define UG_DVALMODE 118 			/* set valuator mode */
#define UG_DCHOICEMODE 119 			/* set choice mode */
#define UG_DPICKMODE 120 			/* set pick mode */
#define UG_DSTRINGMODE 121 			/* set string mode */
#define UG_DREQLOC 122 				/* request locator */
#define UG_DREQSTROKE 123 			/* request stroke */
#define UG_DREQVAL 124 				/* request valuator */
#define UG_DREQCHOICE 125 			/* request choice */
#define UG_DREQPICK 126 				/* request pick */
#define UG_DREQSTRING 127 			/* request string */
#define UG_DSAMPLOC 128 				/* sample loc */
#define UG_DSAMPSTROKE 129 			/* sample stroke */
#define UG_DSAMPVAL 130 				/* sample valuator */
#define UG_DSAMPCHOICE 131 			/* sample choice */
#define UG_DSAMPPICK 132 			/* sample pick */
#define UG_DSAMPSTRING 133 			/* sample string */
#define UG_DAWAITDEV 134 			/* await event */
#define UG_DPUTSTRING 135 			/* write to scrolling text window */
#define UG_DCHGCHOICEAREA 136		/* change choice area */
#define UG_DMENUTEXTSIZE 137		/* return menu text size, in DC */
#define UG_DCHOICEHILITE 138		/* choice device highlighting */
/** metafile **/
#define UG_DWRITEGKSM 139 			/* write item to GKSM */
#define UG_DTYPEGKSM 140 			/* get item type from GKSM */
#define UG_DREADGKSM 141 			/* read item type from GKSM */
/** inquiry **/
#define UG_DTXTEXT 142 				/* text extent */
#define UG_DPIXDIM 143 				/* inquire pixel array dimensions */
#define UG_DPIXARRAY 144 			/* inquire pixel array */
#define UG_DPIXEL 145 				/* inquire pixel */

/** secondary entries (lower level, called by simulation lib routines) */
#define UG_DAWAITCHOICE 146 		/* await choice */
#define UG_DAWAITPICK 147 			/* await pick */
#define UG_DAWAITLOC 148 			/* await locator */
#define UG_DAWAITVAL 149 			/* await valuator */
#define UG_DAWAITSTRING 150 		/* await string */
#define UG_DAWAITSTROKE 151 		/* await stroke */
#define UG_DECHOOP 152 				/* echo */
#define UG_DNDCDEV 153 				/* convert from NDC to device coords */
#define UG_DDEVNDC 154 				/* convert from device to NDC coords */
#define UG_DPAGOP 155 				/* page (clears the screen) */
#define UG_DSAVSCROP 156 			/* save screen */
#define UG_DRESSCROP 157 			/* restore screen */
#define UG_DATEXT 158 				/* alpha text */
#define UG_DRASGET 159 				/* read a raster rectangle */
#define UG_DRASPUT 160 				/* put a raster rectangle */
#define UG_DRASCPY 161 				/* copy a raster rectangle */
#define UG_DRASALLOC 162			/* allocate a raster rectangle */
#define UG_DRASDEALLOC 163			/* deallocate a raster rectangle */
#define UG_DRASLINE 164 			/* draw a line between raster coords */
#define UG_DMARKERRAS 165			/* draw a raster marker */
#define UG_DKBD 166 				/* get keyboard */
#define UG_D1CHAR 167 				/* get 1 char from keyboard */
#define UG_DKEYPAD 168 				/* get a keypad key */
#define UG_DTRK 169 				/* track a loc cursor and get a loc */
#define UG_DPIK 170 				/* track a pick cursor and pick something */
#define UG_DCHOICE 171 				/* get a choice from a phys. choice device */
#define UG_DBUTTON 172 				/* get settings from a button (switch) device */
#define UG_DVAL 173 				/* get valuator data */
#define UG_DSTREAM 174 				/* get data from a stream (stroke) device */
#define UG_DMOVOP 175 				/* move to x,y */
#define UG_DDRWOP 176 				/* draw to x,y */
#define UG_DPNTOP 177 				/* marker at x,y  NDC coords */
#define UG_DCHHTNDC 178 			/* set character height (simulation routine) */
#define UG_DERASE 179 				/* set xor mode */

/* ANSI Terminal Functions */

#define UG_DANSION 180				/* turn ON ansi terminal mode */
#define UG_DANSIOFF 181				/* turn OFF ansi terminal mode */
#define UG_DANSIVIS 182				/* make ansi terminal VISible */
#define UG_DANSINVIS 183			/* make ansi terminal iNVISible */
#define UG_DANSIUP 184				/* move cursor UP one row */
#define UG_DANSIDWN 185				/* move cursor DoWN one row */
#define UG_DANSILFT 186				/* move cursor LeFT one column */
#define UG_DANSIRGT 187				/* move cursor RiGhT one column */
#define UG_DANSIEC2E 188			/* Erase from Cursor to End of line */
#define UG_DANSIEB2C 189			/* Erase Begin of line to Cursor */
#define UG_DANSIEL 190				/* Erase current Line */
#define UG_DANSIEC2ES 191			/* Erase Cursor to End of Screen */
#define UG_DANSIEBS2C 192			/* Erase Begin of Screen to Cursor */
#define UG_DANSIESCR 193			/* Erase ansi SCReen */
#define UG_DANSISPOS 194			/* Set cursor POSition (row, col) */
#define UG_DANSIQPOS 195			/* inQuire cursor POSition */

#define UG_DPROMPT 196 				/* pop up alpha prompt */
#define UG_DDNPROMPT 197 			/* take down alpha prompt */
#define UG_DMENU 198 				/* pop up menu */
#define UG_DDNMENU 199 				/* take down menu */
#define UG_DRASPNTS 200 			/* take down menu */
/*
... aak aug-07-98: new output primitive
*/
#define UG_DSHADEAREA 201 			/* 3-d shaded area */
/*
.....added by Yurong
*/
#define UW_FORM		202
#define UW_SET_LIST		203
#define UW_GET_FIELD		204
#define UW_CLOSE_FORM		205
#define UW_GET_FILENAME     206
#define UW_GET_FNAME_ADV     207
#define UW_OPEN_WINDOW		208
#define UW_WIN_OUT		209
#define UW_CLOSE_WINDOW		210
#define UW_APP_EXIT			211
#define UZ_LOAD_ACCEL		212
#define UW_EVENT			213
#define UW_OPEN_POCKET		214
#define UW_CLOSE_POCKET		215
#define UW_SIGNON_LOAD		216
#define UW_ERROR_MSG		217
#define UW_DOWN_MENU		218
#define UW_SIGNOFF			219
#define UW_PRINT_SCREEN		220
#define UW_LOAD_LAYOUT		221
#define UW_SAVE_LAYOUT		222
#define UW_MENU_DESIGN		223
#define UW_VIEW_SEG			224
#define UW_DEL_CUTSEG		225
#define UW_ERASE_CUTSEG		226
#define UW_RESET_CUTSEG		227
#define UW_OPEN_CUTSEG		228
#define UW_POSTN_CUTSEG		229
#define UW_CLOSE_CUTSEG		230
#define UW_GETSURF			231
#define UW_SETSURF			232
#define UW_GRAPHSURF		233
#define UW_POPSURF			234
#define UW_CLEAR_VP			235
#define UW_FLUSH			236
#define UW_HILITE			237
#define UW_FORM_CKDATA		238
#define UW_PRMERR			239
#define UW_WRPRM			240
#define UW_WRSTAT			241
#define UW_MENU				242
#define UW_RESET_PROMPT		243
#define UW_WINDOW			244
#define UW_SIGNON			245
#define UW_GET_CLIP			246
#define UW_SET_CLIP			247
#define UW_DYNDRAW			248
#define UW_GET_DEPMASK		249
#define UW_SET_DEPMASK		250
#define UW_GET_WSSHADE		251
#define UW_GET_LUCENCY		252
#define UW_YESNO			253
#define UW_BCOMLINE		254
#define UW_ECOMLINE		255
#define UW_FORM_VIS		256
#define UW_FORM_INVIS		257
#define UW_FORM_DISPLAY		258
#define UW_CLOSE_DISPFRM		259
#define UW_GETFRM_FIELD		260
#define UW_DISPFRM_SET_LIST		261
#define UW_UPDATE_FRM		262
#define UG_RPPICK_SEG		263
#define UW_GET_DIRNAME     264
#define UW_DSPFRM_INVIS     265
#define UW_DSPFRM_VIS     266
#define UW_YESNOCANCEL			267
#define UW_DISPFRM_SET_LABEL		268
#define UW_DISPFRM_SET_BUTLABEL		269
#define UW_MARKER_SIZE		270
#define UW_UPDATE_INPUT		271
#define UW_GET_FOCUSFRM		272
#define UW_DISPFRM_SET_TLIST		273
#define UW_FORM_SORTTABLE		274
#define UW_SETWIN_TITLE		275
#define UW_SETFORM_TFUNC		276
#define UW_NEW_SESSION			277
#define UW_CHKKEY_COM			278
#define UG_DDNMENUNUM			279
#define UW_DISPFRM_SET_FOCUS		280
#define UW_DISPFRM_SET_COLOR		281
#define UW_CHKWIN_EVENT		282
#define UW_FRMSEC_ENABLE		283
#define UW_FRMSEC_SET_COLOR		284
#define UW_FRMSEC_ACTIVE 	285
#define UW_DISPFRM_SET_INIT_DATATYP 286
#define UW_DISPFRM_GET_FORM_DATATYP 287
#define UW_DISPFRM_SET_ATTR 288
#define UW_FRM_ENABLE_OK 289
#define UW_FRM_ENABLE_CLOSE 290
#define UG_MAXWSFCTN 291			/* number of op-codes */


/*  raster op rules */
#define UG_COPY 0
#define UG_OR 1
#define UG_XOR 2
#define UG_AND 3
#define UG_NOT 4
#define UG_CLEAR 5
#define UG_SEG 6
typedef Gint (*UG_wsenttbl[UG_MAXWSFCTN])();	/* workstation entry point table */

#define GDIDDH
#endif
