/*********************************************************************
**
**    NAME         :  dselmask.h
**
**       CONTAINS:
**          select masks for DAS limit purposes
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       dselmask.h , 25.4
**    DATE AND TIME OF LAST  MODIFICATION
**       07/05/16 , 11:27:25
**
*********************************************************************/

/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!                                                             !!!!!!
!!!!!!   NOTE:                                                     !!!!!!
!!!!!!      Recompile routine D1INITD.C (in .das directory) to     !!!!!!
!!!!!!      cause any changes made in this include file to be      !!!!!!
!!!!!!      reflected in the executable.                           !!!!!!
!!!!!!                                                             !!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/


/*      --  defines various bit masks for limiting the DAS --   */

#ifndef SELMASK

#include "dasnog.h"

#ifdef DPGM
#define EXT
#else
#define EXT extern
#endif

#ifdef DPGM


/*
.....CAM entity selection masks - individual entities
.....The following definitions hold:
.....LN = CAM AND CADD lines
.....PT = CAM AND CADD points
.....CI = CAM AND CADD circles
.....PN = CAM PATERN
.....CV = CAM CURVE
.....PL = CAM PLANE
.....SH = CAM SHAPE
.....VE = CAM VECTOR
.....SF = CAM SURFACE
.....MX = CAM MATRIX
.....MESH = MESH SURFACE
.....QUILT = QUILT SURFACE
.....NET = NET SURFACE
.....EVSF = EVALUATED SURFACE (PDES ONLY)
.....EVCV = EVALUATED CURVE (PDES ONLY)
.....AGSF = Applied Geometry Surface (CADD, IGES ONLY)
.....AGCV = Applied Geometry Curve (CADD, IGES ONLY)
.....CO = CONIC
.....RBCV = RATIONAL BSPLINE CURVE
.....RBSF = RATIONAL BSPLINE SURFACE
.....TRIM = TRIMMED SURFACE
.....REV = SURFACE OF REVOLUTION
*/
    unsigned UD_ncl_pt[UD_NMENTWD]       = { 1, 0, 0x0800000 };  /* POINTS   CAM&CADD */
    unsigned UD_ncl_ln[UD_NMENTWD]       = { 2, 0, 0x1000000 };  /* LINES    CAM&CADD */
    unsigned UD_ncl_ci[UD_NMENTWD]       = { 4, 0, 0x2000000 };  /* CIRCLES  CAM&CADD */
    unsigned UD_ncl_pl[UD_NMENTWD]       = { 0, 0, 0x4000000 };  /* PLANES   CAM */
    unsigned UD_ncl_cv[UD_NMENTWD]       = { 0, 0, 0x0020000 };  /* CURVES   CAM */
    unsigned UD_ncl_ve[UD_NMENTWD]       = { 0, 0, 0x0008000 };  /* VECTORS  CAM */
    unsigned UD_ncl_pv[UD_NMENTWD]       = { 0, 0, 0, 2};        /* POINTVECTOR CAM&CADD */
    unsigned UD_ncl_mx[UD_NMENTWD]       = { 0, 0, 0x0010000 };  /* MATRIX   CAM */
    unsigned UD_ncl_patern[UD_NMENTWD]   = { 0, 0, 0x08000000 }; /* PATERN   CAM */
    unsigned UD_ncl_sh[UD_NMENTWD]       = { 0, 0, 0x0400000 };  /* SHAPE    CAM */
    unsigned UD_ncl_sf[UD_NMENTWD]       = { 0, 0, 0x0040000 };  /* SURFACE  CAM */
    unsigned UD_ncl_mesh[UD_NMENTWD]     = { 0, 0, 0x0100000 };  /* MESH SF  CAM */
    unsigned UD_ncl_quilt[UD_NMENTWD]    = { 0, 0, 0x0200000 };  /* QUILT SF CAM */
    unsigned UD_ncl_netsf[UD_NMENTWD]    = { 0, 0, 0x10000000 }; /* NET SF   CAM */
    unsigned UD_ncl_evalsf[UD_NMENTWD]   = { 0, 0, 0, 1};        /* EVAL SF  CAM */
    unsigned UD_ncl_evalcv[UD_NMENTWD]   = { 0, 0, 0x80000000 }; /* EVAL CV  CAM */
    unsigned UD_ncl_trimsf[UD_NMENTWD]   = { 0, 0, 0, 4};        /* TRIM SF  CAM */
    unsigned UD_ncl_cvonsf[UD_NMENTWD]   = { 0x1000, 0, 0};      /* UVCV     CAM */
    unsigned UD_ncl_revsf[UD_NMENTWD]   = { 0, 0, 0, 8};         /* REV SF   CAM */

	unsigned UD_dimensions[UD_NMENTWD]   = { 0,0x8000,0 };
    unsigned UD_text[UD_NMENTWD] = { 0,0,0x1000 };
/*
.....CAM entity selection masks - multiple entities
*/
    unsigned UD_ncl_lnpl[UD_NMENTWD]     = { 2, 0, 0x5000000 };  /* RMILL:            LN,PL */
    unsigned UD_ncl_pvln[UD_NMENTWD]     = { 2, 0, 0, 2 };       /* PV:               LN,PV */
    unsigned UD_ncl_ptcv[UD_NMENTWD]     = { 1, 0, 0x0820000 };  /* PV:               PT,CV */
    unsigned UD_ncl_vepv[UD_NMENTWD]     = { 0, 0, 0x0008000,2}; /* PV:               VE,PV */
    unsigned UD_ncl_ptpve[UD_NMENTWD]    = { 1, 0, 0x0808000,2}; /* CI:            PT,VE,PV */
    unsigned UD_ncl_ptpvpn[UD_NMENTWD]   = { 1, 0, 0x8800000,2}; /* PN:            PT,PN,PV */
    unsigned UD_ncl_ptpv[UD_NMENTWD]     = { 1, 0, 0x0800000,2}; /* PV:               PT,PV */
    unsigned UD_ncl_ptpvln[UD_NMENTWD]   = { 3, 0, 0x1800000,2}; /* SF/REVOLV:        PT,LN,PV */
    unsigned UD_ncl_ptpl[UD_NMENTWD]     = { 1, 0, 0x4800000 };  /* DIST,RMILL:       PT,PL */
    unsigned UD_ncl_lnci[UD_NMENTWD]     = { 6, 0, 0x3000000 };  /* PT/ENTPT:         LN,CI */
    unsigned UD_ncl_lncipv[UD_NMENTWD]   = { 6, 0, 0x3000000,2}; /* PT/ENTPT:      PV,LN,CI */
	
	unsigned UD_ncl_vepvln[UD_NMENTWD]   = {0x2, 0, 0x0008000, 0x2}; /*SMILL:	   VE,PV,LN*/

    unsigned UD_ncl_lncicv[UD_NMENTWD]   = { 0x56, 0, 0x3020000}; /* NSHAPE:      LN,CI,CV */

/*
....Added to be able to pick LN, CV, or PV. JLS 2/15/99
....And another to pick any curve or a circle.
*/
    unsigned UD_ncl_allcvlnpv[UD_NMENTWD]   = {0x107E, 0, 0x0020000,2}; /*         PV,LN,CV */
    unsigned UD_ncl_cicv[UD_NMENTWD]    = { 0x74, 0, 0x2020000 };  /*               CI,CV */

    unsigned UD_ncl_cvpn[UD_NMENTWD]     = { 0, 0, 0x8020000 };  /* SF/FILLET:        CV,PN */
    unsigned UD_ncl_revers[UD_NMENTWD]   = {0x107E,0,0x4028000,2};/* REVERS: LN,CI,VE,CV,PV,PL  */
    unsigned UD_ncl_curve[UD_NMENTWD]    = { 6, 0, 0x3020000 };  /* REDEF,SF/:     LN,CI,CV */
    unsigned UD_ncl_lncish[UD_NMENTWD]   = { 6, 0, 0x3400000 };  /* SHAPE:         LN,CI,PL */
    unsigned UD_ncl_rldentity[UD_NMENTWD] = { 7, 0, 0x3820000};  /* SF RULED:   PT,LN,CI,CV */
    unsigned UD_ncl_redef[UD_NMENTWD]    = { 7, 0, 0x3820000,2}; /* REDEF:   PV,PT,LN,CI,CV */
    unsigned UD_ncl_slope[UD_NMENTWD]    = { 7, 0, 0x3828000,2}; /* SF BNDSLP:
                                                                      PT,PV,LN,CI,VE,CV*/
    unsigned UD_ncl_ptpn[UD_NMENTWD]     = { 1, 0, 0x8800000};   /* PATERN:           PT,PN */

/* -- All surfaces: RB, AG NCL,mesh,quilt,eval,net,trim,rev
    unsigned UD_ncl_allsf[UD_NMENTWD]       = { 0x600, 0, 0x1340000 , 0xd};
..... fixed the mask - had lines instead of net surfaces
*/
    unsigned UD_ncl_allsf[UD_NMENTWD]      = { 0x600, 0,0x10340000 , 0xd};

/*
.....Added for shading and unshading entities, all
.....surfaces plus shapes  JLS 9/30/99
..... fixed the mask - had lines instead of net surfaces
*/
    unsigned UD_ncl_allsfsh[UD_NMENTWD]    = { 0x100600, 0, 0x10740000 , 0xd};

/* -- CV SF EDGE, Direct U,V evaluation on: NCL SF, MESH SF, EVALSF, AGSF, TRIM, REV     -- */
    unsigned UD_ncl_uvsf[UD_NMENTWD]       = { 0x600, 0, 0x0140000 , 0xd};

/* -- CV/INTOF,SF,SF:ADV POK:PSIS: SF,MESH,QUILT,NET,EVALSF,AGSF,PLANE,TRIM, REV - */
    unsigned UD_ncl_allsfpl[UD_NMENTWD]    = { 0x600, 0, 0x14340000, 0xd };

/* -- CV/INTOF,SF,SF:ADV POK:PSIS: SF,MESH,QUILT,NET,EVALSF,AGSF,PLANE,TRIM, REV, SSPLIN - */
    unsigned UD_ncl_psis[UD_NMENTWD]    = { 0x1610, 0, 0x14340000, 0xd };

/* -- Curve offset:CONIC, AGCV, RBSPLINE CV, COMP CV, NCL CV, EVCV                  -- */
    unsigned UD_ncl_offcv[UD_NMENTWD]      = { 0x10de, 0, 0x83020000 };
    unsigned UD_ncl_offss[UD_NMENTWD]      = { 0x1010, 0, 0 };

/* -- Curve for creating NURB surfaces */
    unsigned UD_ncl_nsfcv[UD_NMENTWD]      = { 0x10fe, 0, 0x80020000 };

/* -- Slope control for NURB surfaces */
    unsigned UD_ncl_nsfslope[UD_NMENTWD]   = { 0x10fe, 0, 0x83828000, 2};

/* -- Curve for creating NURB surfaces */
    unsigned UD_ncl_nsfcvthru[UD_NMENTWD]      = { 0x10f8, 0, 0x80020000 };

/* -- CURVE/COMPOS,...: LINE,CIRCLE,COMPOSITE CURVE
... aak 31-oct-1997: added uv curves on surfaces; before was
    unsigned UD_ncl_compcvelm[UD_NMENTWD] = { 0x56, 0, 0x3020000};
.....  ASF 11/14/13. Added conics. before it was
    unsigned UD_ncl_compcvelm[UD_NMENTWD] = { 0x1056, 0, 0x3020000};
*/
    unsigned UD_ncl_compcvelm[UD_NMENTWD] = { 0x105E, 0, 0x3020000};

/* -- NUM RES,RENAME,LABEL,REMOVE:'mvclngeom',CONIC,COMPCV,RBCV,AGCV,AGSF,AGSH,SH,TRIMSF -- */
    unsigned UD_ncl_geometry[UD_NMENTWD]   = { 0x1816df, 0, 0x9ff78000, 0xf};
    unsigned UD_ncl_labels[UD_NMENTWD]   = { 0x001816df, 0x20000200, 0x9ff79000, 0xf};

/* -- RMILL: AG SHELL,AG SF,NET SF,PL,MESH,QUILT,NCL SF,EVAL SF, TRIMSF    -- */
    unsigned UD_ncl_rmill_sf[UD_NMENTWD]   = { 0x80600, 0, 0x14340000, 0xd};

/* -- RMILL:AGSH,AGSF,LN,CI,CONIC,AGCV,COMPCV,RBCV,PL,NET,EVCV,MESH,QUILT,CV,SF,EVSF,TRIMSF-- */
    unsigned UD_ncl_rmill_geo[UD_NMENTWD]  = { 0x806de, 0, 0x97360000, 0xd};

/* -- AGSH,AGSF,PT,PV,SF,MESH,QUILT,PL,NET,EVAL SF,TRIMSF -- */
    unsigned UD_ncl_ptplsf[UD_NMENTWD]     = { 0x80601, 0, 0x14b40000, 0xf};

/* -- MV/, CLONE/: PT,LN,CI,CO,CMP,RBS,AG,VE,MX,CV,SF,MESH,QUILT,PL,PN,NET,         -- */
/* --              EVALCV,EVALSF,TRIMSF                                             -- */
    unsigned UD_ncl_mvclngeom[UD_NMENTWD]  = { 0x1006df, 0x20000000, 0x9fb79000,
		0xf};

/* -- CAN/: PT,LN,CI,NET,PATERN,MESH,QUILT,VE,PL,PV                                 -- */
    unsigned UD_ncl_canonical[UD_NMENTWD]  = {7, 0, 0x1fb08000, 2};

/* -- DIST/: PT,LN,COMPCV,RBCV,AGCV,AGSF,CV,SF,PL,EVCV,EVSF,TRIMSF,PV               -- */
    unsigned UD_ncl_dist[UD_NMENTWD]       = { 0x016d3, 0, 0x85860000, 0xf};

/* -- NET SURFACE: AGSF, SF, MESH, EVALSF, TRIMSF                                   -- */
    unsigned UD_ncl_netentity[UD_NMENTWD]  = {0x600, 0, 0x0140000, 0xd};

/* -- NCL: FILLET surface made up of:NCL SF,MESH,QUILT,NET,EVALSF,AGSF,PL           -- */
    unsigned UD_ncl_filletents[UD_NMENTWD] = { 0x600, 0, 0x14340000, 0xd};

/* -- MOTION GEO:PT,LN,CI,CO,COMPCV,RBCV,AGCV,AGSF,AGSH,CV,SF,MESH,QUILT,PL,NET,EV* -- */
    unsigned UD_ncl_motion[UD_NMENTWD]     = { 0x000806df, 0, 0x97368000, 0xd};

/* -- SHOW/:PT,LN,CI,CO,CMPCV,RBCV,AGCV,AGSF,AGSH,MX,CV,SF,MESH,QUILT,PL,PN,NET,EV* -- */
    unsigned UD_ncl_show[UD_NMENTWD]       = { 0x001816df, 0x20000000,
		0x9ff79000, 0xf};
/*
.....REDEF/OPEN,CLOSE
.....CV, SF, MESH, EVALSF, EVALCV, CONIC, COMPCV, BSCV, RBSPCV, AGCV, AGSRF,
.....RBSPSF
.....Bobby  -  3/9/92
*/
    unsigned UD_ncl_close[UD_NMENTWD] = {0x6f8, 0, 0x80160000, 1};
/*
.....Entities for Surface trimming.
*/
/*  unsigned UD_ncl_sftrim[UD_NMENTWD] = {0x600, 0, 0x04140000, 9}; */
    unsigned UD_ncl_sftrim[UD_NMENTWD] = {0x6fc, 0, 0x06160000, 0xd};
    unsigned UD_ncl_cvtrim[UD_NMENTWD] = {0x10fC, 0, 0x2020000};
/*
.....Entities for point vector tanto/perpto
*/
    unsigned UD_ncl_pvttpe[UD_NMENTWD] = {0x8167E,0,0x17360000,0xd};/* LN,CI,CV,PL,SF  */

/*
...   All curves used in NCL statements
*/
    unsigned UD_ncl_allcv[UD_NMENTWD] = { 0x10F8, 0, 0x80020000 };  /* ALL CURVES */
/*
.....Cutter symbols
........All Curves & Surface of Revolution
*/
    unsigned UD_ncl_holder[UD_NMENTWD] = { 0x1010F8, 0, 0x8002000, 8};
/*
.....CADD entity selection masks - individual entities
*/
    unsigned UD_points[UD_NMENTWD]       = { 0x1 };
    unsigned UD_line[UD_NMENTWD]         = { 0x2 };
    unsigned UD_circle[UD_NMENTWD]       = { 0x4 };
    unsigned UD_conics[UD_NMENTWD]       = { 0x8 };
    unsigned UD_compcrv[UD_NMENTWD]      = { 0x10 };
    unsigned UD_bspline[UD_NMENTWD]      = { 0x20 };
    unsigned UD_rbspline[UD_NMENTWD]     = { 0x40 };
    unsigned UD_agcrv[UD_NMENTWD]        = { 0x80 };
    unsigned UD_agsrf[UD_NMENTWD]        = { 0x200 };
    unsigned UD_rbsplsrf[UD_NMENTWD]     = { 0x400 };
/*
... aak 31-oct-1997: added uv curves on surfaces
*/
    unsigned UD_cvonsf[UD_NMENTWD]       = { 0x1000 };
    unsigned UD_agshell[UD_NMENTWD]      = { 0x80000 };
    unsigned UD_solid[UD_NMENTWD]        = { 0x100000 };
    unsigned UD_body[UD_NMENTWD]         = { 0x40000000 };
    unsigned UD_polygon[UD_NMENTWD]      = { 0, 0x080 };
    unsigned UD_polyline[UD_NMENTWD]     = { 0, 0x200 };
    unsigned UD_group[UD_NMENTWD]        = { 0, 0x800 };
    unsigned UD_symbol[UD_NMENTWD]       = { 0, 0x20000000 };    /* for symbol instances */
    unsigned UD_shell[UD_NMENTWD]        = { 0, 0, 0x20000}; 
/*
.....CADD entity selection masks - multiple entities
*/

/*  -- points, lines and arcs -- */

    unsigned UD_ptlncir[UD_NMENTWD]  = { 1+2+4 };

/*  -- lines and arcs -- */

    unsigned UD_lncir[UD_NMENTWD]    = { 2+4 };

/*  -- lines, arcs, and composite curves -- */

    unsigned UD_cmplncir[UD_NMENTWD] = { 2+4+16 };

/*  -- lines, arcs, and bsplines -- */

    unsigned UD_splncir[UD_NMENTWD]  = { 0x83};

/*  -- lines, arcs, composite curves and bsplines -- */

    unsigned UD_spcmplncir[UD_NMENTWD]   = { 0x96};

/* -- lines, arcs, conics, composite curves and bsplines -- */

    unsigned UD_lncirconcmpsp[UD_NMENTWD] = {0x9E};

/* -- lines, arcs, conics, and composite curves -- */
    unsigned UD_lncci[UD_NMENTWD] = { 2+4+8 };
    unsigned UD_lncirconcmp[UD_NMENTWD] = { 2+4+8+16 };

/* -- Unicad surfaces: revs, ruled, tabs, and (rational) bsplines  -- */
    unsigned UD_surface[UD_NMENTWD] = { 0x0400, 0, 0x00140000, 0xd};

/* -- Unicad "faces" surfaces and shells -- */
    unsigned UD_face[UD_NMENTWD] = { 0x080200};

/* -- renderable entities: revs, ruled, tabs, and (rational) bsplines, shells  -- */
    unsigned UD_render[UD_NMENTWD] = { 0x80600, 0, 0x0140000, 0xd};

/* -- surface renderings (bit 35 = 32 + 3)  -- */
    unsigned UD_surfrend[UD_NMENTWD] = { 0, 4 };

/* -- all geometry  -- */
    unsigned UD_allgeom[UD_NMENTWD] = {0x400802FF, 0xA80};

/* -- all curves except uv curves -- */
    unsigned UD_allcurves[UD_NMENTWD] = { 0x00FE};

/* -- all curves including uv curves -- */
    unsigned UD_allcurvess[UD_NMENTWD] = { 0x00010FE};

/* -- all curves and points -- */
    unsigned UD_allcurves_pts[UD_NMENTWD] = { 0x00FF,0,0x03820000};

/* 
... aak 31-oct-1997: all curves including uv curves and points
*/
    unsigned UD_allcurvess_pts[UD_NMENTWD] = { 0x10FF,0,0x03820000};

/* -- all "virtual" curves (i.e. all curves plus curves from solids -- */
/* aak: was
... unsigned UD_vircurves[UD_NMENTWD] = { 0x5E ,0,0x03020000};
*/
    unsigned UD_vircurves[UD_NMENTWD] = { 0x105E ,0,0x03020000};

/* -- all geometry, plus symbols -- */
    unsigned UD_geomandsym[UD_NMENTWD] = { 0x400802FF, 0x20000A80};

/* -- curves for trim operation -- */
/* aak 31-oct-1997: added uv-curves on surfaces; was
... unsigned UD_trimcurves[UD_NMENTWD] = { 0x05E,0,0x03020000};
*/
    unsigned UD_trimcurves[UD_NMENTWD] = { 0x105E,0,0x03020000};

/* -- curves for midtrim operation -- */
/* aak 31-oct-1997: added uv-curves on surfaces; was
... unsigned UD_midtrimcurves[UD_NMENTWD] = { 0x0FE,0,0x03020000};
*/
    unsigned UD_midtrimcurves[UD_NMENTWD] = { 0x10FE,0,0x03020000};

/* -- curves for split operation -- */
/*
.....RAZ PSB#210, REL 8.1 - delete composite curves from this list 
.....WAS: int UD_splitable_curves[UD_NMENTWD] = {0x0FE}; 
*/
    unsigned UD_splitable_curves[UD_NMENTWD] = {0x0EE};

/* -- curves for offset operation -- */
    unsigned UD_offset_curves[UD_NMENTWD] = {0x80FE};

/* -- surfaces for converting to rational bspline -- */
    unsigned UD_cvt_to_rbsplsrf[UD_NMENTWD] = { 0x0200};

/* -- curves for creating ruled surfaces -- */
    unsigned UD_ruled_surf_curves[UD_NMENTWD] = {0x0FE};

/* -- surfaces for converting to rational bspline -- */
    unsigned UD_revsrf_curves[UD_NMENTWD] = { 0x0FE};

/* -- draftable entities -- */
    unsigned UD_draftable[UD_NMENTWD] = {0x400000ff, 0x20000000, 0x03820000};

    unsigned UD_draft_line[UD_NMENTWD]           = { 2, 0, 0x1000000 };

    unsigned UD_editable[UD_NMENTWD] = { 0,0x8000,0x1000 };

/* -- xhatchable entities -- */
    unsigned UD_xhatchcurves[UD_NMENTWD] = {0x0FE};

/* -- featureable entities -- */
	unsigned UD_features[UD_NMENTWD] = {0x50F806FE, 0, 0x80360000, 0x0000000d};

/* -- 2D analysis entities (composite curves & polygons) -- */
    unsigned UD_2D_analysis[UD_NMENTWD] = { 0x10, 0x080 };

/* -- translatable entities -- */
    unsigned UD_translate[UD_NMENTWD] = {0x401806FF, 0x20008AA1, 0x141000, 0xc};

/* -- rotateable entities -- */
    unsigned UD_rotate[UD_NMENTWD] = {0x401806FF, 0x20008AA1, 0x141000, 0xc};

/* -- mirrorable entities -- */
    unsigned UD_mirror[UD_NMENTWD] = {0x401806FF, 0x20008AA1, 0x141000, 0xc};

/* -- scaleable entities -- */
    unsigned UD_scale[UD_NMENTWD] = {0x401806FF, 0x20008AA1, 0x141000, 0xc};

/* -- copy and translate -- */
    unsigned UD_copytranslate[UD_NMENTWD] = {0x401806FF, 0x20008AA1, 0x141000, 0xc};

/* -- copy and rotate -- */
    unsigned UD_copyrotate[UD_NMENTWD] = {0x401806FF, 0x20008AA1, 0x141000, 0xc};

/* -- copy and mirror -- */
    unsigned UD_copymirror[UD_NMENTWD] = {0x401806FF, 0x20008AA1, 0x141000, 0xc};

/* -- copy and scale -- */
    unsigned UD_copyscale[UD_NMENTWD] = {0x401806FF, 0x20008AA1, 0x141000, 0xc};

/* -- near point on entity -- */
    unsigned UD_near_on_entity[UD_NMENTWD] = { 0x2E, 0, 0x38000 };

/* -- entities to convert to AG non-uniform rational bspline -- */
    unsigned UD_convert_to_ag[UD_NMENTWD] = { 0xFE, 0x200, 0x80000000 };

/* -- can be projected onto plane -- */
    unsigned UD_proj_to_plane[UD_NMENTWD] = { 0x00FF};

/* -- can be a symbol -- */
    unsigned UD_canbe_asymbol[UD_NMENTWD];

/* -- can be a group -- */
    unsigned UD_canbe_agroup[UD_NMENTWD];

/* -- can be dissolved -- */
    unsigned UD_dissolve[UD_NMENTWD] = { 0x90};

/* -- can extract faces from -- */
    unsigned UD_extract_faces[UD_NMENTWD]            = { 0x40000000 };

/* -- cam point, patern, circle and cadd point, circle, comp curves.-- 
    unsigned UD_pocket_perimeter[UD_NMENTWD]         = { 21, 0 ,  0xA800000 };*/
    unsigned UD_pocket_perimeter[UD_NMENTWD]         = { 0x47d, 0, 0xa860000, 0xc};

/* -- Curves and Surfaces for optimal plane */
	unsigned UD_cvsf[UD_NMENTWD] = {0x16fc,0,0x92360000,0xd};

/* -- Geometry for bounding box solid */
	unsigned UD_solid_bound[UD_NMENTWD] = {0x10167F, 0x20000000, 0x18760000,0xf};
/*
.....GODLTA geometry
.....PT, PV, VE, PL, SF
*/
/*	unsigned UD_godlta[UD_NMENTWD] = {0x601, 0, 0x1C348000, 0x080000F}; */
/*
.....the old GODLTA seems not right, also it should not include PT per Ken request
*/
	unsigned UD_godlta[UD_NMENTWD] = {0x600, 0, 0x14348000, 0xf};
#else


    EXT unsigned UD_ncl_sh[UD_NMENTWD];  /* added by kathy */
    EXT unsigned UD_ncl_pt[UD_NMENTWD];
    EXT unsigned UD_ncl_pv[UD_NMENTWD];  /* vp3.3.93       */
    EXT unsigned UD_ncl_ln[UD_NMENTWD];
    EXT unsigned UD_ncl_ci[UD_NMENTWD];
    EXT unsigned UD_ncl_ve[UD_NMENTWD];
    EXT unsigned UD_ncl_cv[UD_NMENTWD];
    EXT unsigned UD_ncl_cvonsf[UD_NMENTWD];
    EXT unsigned UD_ncl_cvpn[UD_NMENTWD];
    EXT unsigned UD_ncl_lncicv[UD_NMENTWD];  /* added by eduard */
    EXT unsigned UD_ncl_lncish[UD_NMENTWD];  /* added by kathy */
    EXT unsigned UD_ncl_rldentity[UD_NMENTWD];
    EXT unsigned UD_ncl_pl[UD_NMENTWD];
    EXT unsigned UD_ncl_lnpl[UD_NMENTWD];
    EXT unsigned UD_ncl_ptpl[UD_NMENTWD];
    EXT unsigned UD_ncl_sf[UD_NMENTWD];
    EXT unsigned UD_ncl_mesh[UD_NMENTWD];
    EXT unsigned UD_ncl_uvsf[UD_NMENTWD];
    EXT unsigned UD_ncl_quilt[UD_NMENTWD];
    EXT unsigned UD_ncl_trimsf[UD_NMENTWD];
    EXT unsigned UD_ncl_revsf[UD_NMENTWD]; 
    EXT unsigned UD_ncl_mx[UD_NMENTWD];
    EXT unsigned UD_ncl_lnci[UD_NMENTWD];
    EXT unsigned UD_ncl_lncipv[UD_NMENTWD];
    EXT unsigned UD_ncl_allcvlnpv[UD_NMENTWD]; /*Added by JLS 2/19/99*/
    EXT unsigned UD_ncl_cicv[UD_NMENTWD];      /*Added by JLS 2/19/99*/
    EXT unsigned UD_ncl_compcvelm[UD_NMENTWD];
    EXT unsigned UD_ncl_curve[UD_NMENTWD];
    EXT unsigned UD_ncl_slope[UD_NMENTWD];
    EXT unsigned UD_ncl_allsf[UD_NMENTWD];
    EXT unsigned UD_ncl_allsfsh[UD_NMENTWD];     

    EXT unsigned UD_ncl_revers[UD_NMENTWD]; /* added by kathy */
    EXT unsigned UD_ncl_mvclngeom[UD_NMENTWD]; /* added by kathy */
    EXT unsigned UD_ncl_geometry[UD_NMENTWD];
    EXT unsigned UD_ncl_labels[UD_NMENTWD];
    EXT unsigned UD_ncl_redef[UD_NMENTWD];
    EXT unsigned UD_ncl_rmill_sf[UD_NMENTWD];
    EXT unsigned UD_ncl_rmill_geo[UD_NMENTWD];
    EXT unsigned UD_ncl_patern[UD_NMENTWD];
    EXT unsigned UD_ncl_netsf[UD_NMENTWD];
    EXT unsigned UD_ncl_allsfpl[UD_NMENTWD];
	 EXT unsigned UD_ncl_psis[UD_NMENTWD];/* added by andrew */
    EXT unsigned UD_ncl_offcv[UD_NMENTWD];
    EXT unsigned UD_ncl_offss[UD_NMENTWD];
    EXT unsigned UD_ncl_nsfcv[UD_NMENTWD];
    EXT unsigned UD_ncl_nsfslope[UD_NMENTWD];
    EXT unsigned UD_ncl_nsfcvthru[UD_NMENTWD];
    EXT unsigned UD_ncl_ptpn[UD_NMENTWD];
    EXT unsigned UD_ncl_pvln[UD_NMENTWD];
    EXT unsigned UD_ncl_ptcv[UD_NMENTWD];
    EXT unsigned UD_ncl_vepv[UD_NMENTWD];
    EXT unsigned UD_ncl_ptpv[UD_NMENTWD];
    EXT unsigned UD_ncl_ptpve[UD_NMENTWD];
    EXT unsigned UD_ncl_ptpvln[UD_NMENTWD];
    EXT unsigned UD_ncl_ptpvpn[UD_NMENTWD];
	EXT unsigned UD_ncl_vepvln[UD_NMENTWD];
    EXT unsigned UD_ncl_canonical[UD_NMENTWD];
    EXT unsigned UD_ncl_ptplsf[UD_NMENTWD];
    EXT unsigned UD_ncl_dist[UD_NMENTWD];

    EXT unsigned UD_ncl_netentity[UD_NMENTWD];
    EXT unsigned UD_ncl_filletents[UD_NMENTWD];
    EXT unsigned UD_ncl_motion[UD_NMENTWD];
    EXT unsigned UD_ncl_show[UD_NMENTWD];

    EXT unsigned UD_points[UD_NMENTWD];
    EXT unsigned UD_line[UD_NMENTWD];
    EXT unsigned UD_circle[UD_NMENTWD];
    EXT unsigned UD_conics[UD_NMENTWD];
    EXT unsigned UD_compcrv[UD_NMENTWD];
    EXT unsigned UD_bspline[UD_NMENTWD];
    EXT unsigned UD_rbspline[UD_NMENTWD];
    EXT unsigned UD_cvonsf[UD_NMENTWD];
    EXT unsigned UD_body[UD_NMENTWD];
    EXT unsigned UD_polyline[UD_NMENTWD];
    EXT unsigned UD_surface[UD_NMENTWD];
    EXT unsigned UD_surfrend[UD_NMENTWD];
    EXT unsigned UD_symbol[UD_NMENTWD];

    EXT unsigned UD_ncl_close[UD_NMENTWD];
    EXT unsigned UD_ncl_sftrim[UD_NMENTWD];
    EXT unsigned UD_ncl_cvtrim[UD_NMENTWD];
    EXT unsigned UD_ncl_pvttpe[UD_NMENTWD];
    EXT unsigned UD_ncl_allcv[UD_NMENTWD];
    EXT unsigned UD_ncl_holder[UD_NMENTWD];

    EXT unsigned UD_agcrv[UD_NMENTWD];
    EXT unsigned UD_agsrf[UD_NMENTWD];
    EXT unsigned UD_rbsplsrf[UD_NMENTWD];
    EXT unsigned UD_agshell[UD_NMENTWD];
    EXT unsigned UD_solid[UD_NMENTWD];
    EXT unsigned UD_polygon[UD_NMENTWD];
    EXT unsigned UD_group[UD_NMENTWD];
    EXT unsigned UD_face[UD_NMENTWD];
    EXT unsigned UD_render[UD_NMENTWD];
    EXT unsigned UD_shell[UD_NMENTWD];

    EXT unsigned UD_ptlncir[UD_NMENTWD];
    EXT unsigned UD_lncir[UD_NMENTWD];
    EXT unsigned UD_cmplncir[UD_NMENTWD];
    EXT unsigned UD_splncir[UD_NMENTWD];
    EXT unsigned UD_spcmplncir[UD_NMENTWD];
    EXT unsigned UD_allgeom[UD_NMENTWD];
    EXT unsigned UD_allcurves[UD_NMENTWD];
    EXT unsigned UD_allcurvess[UD_NMENTWD];
    EXT unsigned UD_allcurves_pts[UD_NMENTWD];
    EXT unsigned UD_allcurvess_pts[UD_NMENTWD];
    EXT unsigned UD_vircurves[UD_NMENTWD];
    EXT unsigned UD_geomandsym[UD_NMENTWD];
    EXT unsigned UD_trimcurves[UD_NMENTWD];
    EXT unsigned UD_midtrimcurves[UD_NMENTWD];
    EXT unsigned UD_splitable_curves[UD_NMENTWD];
    EXT unsigned UD_offset_curves[UD_NMENTWD];
    EXT unsigned UD_cvt_to_rbsplsrf[UD_NMENTWD];
    EXT unsigned UD_ruled_surf_curves[UD_NMENTWD];
    EXT unsigned UD_revsrf_curves[UD_NMENTWD];
    EXT unsigned UD_draftable[UD_NMENTWD];
    EXT unsigned UD_draft_line[UD_NMENTWD];
    EXT unsigned UD_dimensions[UD_NMENTWD];
    EXT unsigned UD_text[UD_NMENTWD];
    EXT unsigned UD_editable[UD_NMENTWD];
    EXT unsigned UD_xhatchcurves[UD_NMENTWD];
    EXT unsigned UD_features[UD_NMENTWD];
    EXT unsigned UD_2D_analysis[UD_NMENTWD];
    EXT unsigned UD_translate[UD_NMENTWD];
    EXT unsigned UD_rotate[UD_NMENTWD];
    EXT unsigned UD_mirror[UD_NMENTWD];
    EXT unsigned UD_scale[UD_NMENTWD];
    EXT unsigned UD_copytranslate[UD_NMENTWD];
    EXT unsigned UD_copyrotate[UD_NMENTWD];
    EXT unsigned UD_copymirror[UD_NMENTWD];
    EXT unsigned UD_copyscale[UD_NMENTWD];
    EXT unsigned UD_near_on_entity[UD_NMENTWD];
    EXT unsigned UD_convert_to_ag[UD_NMENTWD];
    EXT unsigned UD_canbe_asymbol[UD_NMENTWD];
    EXT unsigned UD_canbe_agroup[UD_NMENTWD];
    EXT unsigned UD_dissolve[UD_NMENTWD];
    EXT unsigned UD_proj_to_plane[UD_NMENTWD];
    EXT unsigned UD_extract_faces[UD_NMENTWD];
    EXT unsigned UD_pocket_perimeter[UD_NMENTWD];
    EXT unsigned UD_lncci[UD_NMENTWD];
    EXT unsigned UD_cvsf[UD_NMENTWD];
    EXT unsigned UD_solid_bound[UD_NMENTWD];
    EXT unsigned UD_godlta[UD_NMENTWD];

#define UD_inselmask(relnum, selmask) \
    !uu_tst_bit(selmask, relnum-1)

#define UD_set_selmask(relnum, selmask) \
    !uu_set_bit(selmask, relnum-1)

#endif

#undef EXT
#define SELMASK
#endif
