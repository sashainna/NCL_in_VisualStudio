/* NAME :  ag_srf_def.h       MODULE : Surface 
** VERSION : 0.7              DATE LAST MODIFIED : 03/12/88
** CONTAINS: Surface defining data structures
**  ag_pln_data  defining data for planes 
**  ag_cyl_data  defining data for cylinders  
**  ag_cne_data  defining data for cones  
**  ag_tor_data  defining data for torus  
**  ag_sph_data  defining data for spheres  
**  ag_auto_spline used for surface of revolution data 
**  ag_srv_data  defining data for surface of revolution  
**  ag_srf_data  defining data for all surfaces 
Copyright (c) 1988 Applied Geometry Corporation. All rights reserved.
********************************************************************/


/* *** SURFACE DEFINING DATA *** */
                                   
typedef struct ag_pln_data {        /* defining data for planes    */
    struct ag_surface  *srf;        /* pointer to surface          */
    double             P[3];
    double             U[3];
    double             V[3];
    double             N[3];
    double             UdotU;
    double             VdotV;
    double             UdotV;
    double             discrm;      /* discrm = (U dot V)**2 
                                       - (U dot U)*(V dot V) */
    } AG_PLN_DATA, *AG_PLN_DATAP;  

typedef struct ag_cyl_data {  /* defining data for cylinders       */
    struct ag_surface  *srf;      /* pointer to surface            */
    int                kind;
    int                num_spans;
    double             P0[3],P1[3];
    double             AXIS[3];
    double             radius;
    double             height; 
    struct ag_crv_data bs_data[5];
    } AG_CYL_DATA, *AG_CYL_DATAP;   

typedef struct ag_cne_data {  /* defining data for cones           */
    struct ag_surface  *srf;      /* pointer to surface            */
    int                kind;
    int                num_spans;
    double             P0[3],P1[3];
    double             AXIS[3];
    double             rad0,rad1;
    double             height; 
    struct ag_crv_data bs_data[5];
    } AG_CNE_DATA, *AG_CNE_DATAP;   

typedef struct ag_tor_data {      /* defining data for torus       */
    struct ag_surface  *srf;      /* pointer to surface            */
    int                kind;
    int                num_spans1;
    int                num_spans2;
    double             CENTER[3];
    double             AXIS[3];
    double             major_radius,minor_radius;
    double             X[3];
    double             C1[3],C2[3]; 
    double             rad1,rad2;
    struct ag_crv_data bs_data1[5], bs_data2[5];
    } AG_TOR_DATA, *AG_TOR_DATAP;   

typedef struct ag_sph_data {      /* defining data for sphere      */
    struct ag_surface  *srf;      /* pointer to surface            */
    int                kind;
    int                num_spans1;
    int                num_spans2;
    double             CENTER[3];  /* sphere center                */
    double             AXIS[3];    /* axis direction               */
    double             X[3];
    double             C1[3],C2[3]; 
    double             radius;     /* radius                       */
    double             rad1,rad2;
    struct ag_crv_data bs_data1[5], bs_data2[5];
    } AG_SPH_DATA, *AG_SPH_DATAP;   

typedef struct ag_auto_spline {  /* used in  surface revolution    */
    struct ag_spline   bs;
    struct ag_cnode    *bs_nodes; 
    int                num_nodes; 
    struct ag_mmbox    bs_box;
    double             bs_min[3],bs_max[3]; 
    } AG_AUTO_SPLINE, *AG_AUTO_SPLINEP;

typedef struct ag_srv_data {/* defining data for surface revolution*/
    struct ag_surface  *srf;      /* pointer to surface            */
    int                kind;
    int                num_spans;
    double             C[3];      /* point on axis of revolution   */
    double             AXIS[3];   /* axis of revolution            */
    double             X[3];
    double             rad;       /* radius of revolution at C     */
    struct ag_auto_spline bs1,bs2; 
    struct ag_crv_data bs_data[5];
    } AG_SRV_DATA, *AG_SRV_DATAP;   

typedef struct ag_srf_data {     /* defining data for all surfaces */
    int                stype;    /* type of structure              */
    union {
      struct ag_pln_data plane; 
      struct ag_cyl_data cylinder;      
      struct ag_cne_data cone;
      struct ag_tor_data torus; 
      struct ag_sph_data sphere;
      struct ag_srv_data srfrev;
      }                srf_data;   /* shared structure area        */
    } AG_SRF_DATA, *AG_SRF_DATAP; 

