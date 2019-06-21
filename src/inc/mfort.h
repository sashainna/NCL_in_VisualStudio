/*********************************************************************
**    NAME         :  mfort.h
**       CONTAINS: FORTRAN type definitions for C programs
**    DATE AND TIME OF LAST MODIFICATION
**       mfort.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:30
*********************************************************************/

#ifndef UM_MFORT

#define UM_real8  double
#define UM_real4  float
#define UM_int2   short int
#define UM_int4   int

#if UU_COMP==UU_VAXVMS

#include descrip

typedef struct 
   {
   unsigned short len;
   char  type;
   char  class;
   char  *cstr;
   } UM_f77_str;
typedef UM_f77_str *UM_f77_str_ptr;
#define UM_char_of_f77_str(a,i) a->cstr[i]
#define UM_addr_of_f77_str(a) (&a)
#define UM_cstr_of_f77_str(a) a->cstr
#define UM_init_f77_str(a,ptrcstr,length)\
a.cstr = ptrcstr;\
a.len = length;\
a.type = DSC$K_DTYPE_T;\
a.class = DSC$K_CLASS_S

#else

typedef char *UM_f77_str;
typedef char *UM_f77_str_ptr;
#define UM_char_of_f77_str(a,i) a[i]
#define UM_addr_of_f77_str(a) a
#define UM_cstr_of_f77_str(a) a
#define UM_init_f77_str(a,ptrcstr,len) a = ptrcstr

#endif

#define UM_MFORT 1
#endif
