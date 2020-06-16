

/* this ALWAYS GENERATED file contains the IIDs and CLSIDs */

/* link this file in with the server and any clients */


 /* File created by MIDL compiler version 7.00.0555 */
/* at Fri Aug 15 12:04:57 2014
 */
/* Compiler settings for SwUnibase.idl:
    Oicf, W1, Zp8, env=Win32 (32b run), target_arch=X86 7.00.0555 
    protocol : dce , ms_ext, c_ext, robust
    error checks: allocation ref bounds_check enum stub_data 
    VC __declspec() decoration level: 
         __declspec(uuid()), __declspec(selectany), __declspec(novtable)
         DECLSPEC_UUID(), MIDL_INTERFACE()
*/
/* @@MIDL_FILE_HEADING(  ) */

#pragma warning( disable: 4049 )  /* more than 64k source lines */


#ifdef __cplusplus
extern "C"{
#endif 


#include <rpc.h>
#include <rpcndr.h>

#ifdef _MIDL_USE_GUIDDEF_

#ifndef INITGUID
#define INITGUID
#include <guiddef.h>
#undef INITGUID
#else
#include <guiddef.h>
#endif

#define MIDL_DEFINE_GUID(type,name,l,w1,w2,b1,b2,b3,b4,b5,b6,b7,b8) \
        DEFINE_GUID(name,l,w1,w2,b1,b2,b3,b4,b5,b6,b7,b8)

#else // !_MIDL_USE_GUIDDEF_

#ifndef __IID_DEFINED__
#define __IID_DEFINED__

typedef struct _IID
{
    unsigned long x;
    unsigned short s1;
    unsigned short s2;
    unsigned char  c[8];
} IID;

#endif // __IID_DEFINED__

#ifndef CLSID_DEFINED
#define CLSID_DEFINED
typedef IID CLSID;
#endif // CLSID_DEFINED

#define MIDL_DEFINE_GUID(type,name,l,w1,w2,b1,b2,b3,b4,b5,b6,b7,b8) \
        const type name = {l,w1,w2,{b1,b2,b3,b4,b5,b6,b7,b8}}

#endif !_MIDL_USE_GUIDDEF_

MIDL_DEFINE_GUID(IID, IID_ISwUnibase,0x547A6AD7,0xA7D0,0x4931,0x8A,0x28,0x5C,0xEC,0x56,0x69,0x87,0x1D);


MIDL_DEFINE_GUID(IID, IID_ISwDocument,0xDB6A55BA,0x3107,0x4D83,0x9A,0xAE,0xC8,0x70,0xB1,0x67,0x6A,0xB6);


MIDL_DEFINE_GUID(IID, IID_IDocView,0x4A336EFB,0x40A0,0x420D,0xA2,0x5F,0xC8,0x40,0x07,0xE3,0x89,0x3B);


MIDL_DEFINE_GUID(IID, IID_IBitmapHandler,0xDCCD1577,0xCC64,0x4459,0x8E,0x92,0x8A,0x56,0x5B,0x9C,0x3B,0xB4);


MIDL_DEFINE_GUID(IID, IID_IPMPageHandler,0xCEC42F44,0xA5F2,0x4633,0xB4,0xCB,0x94,0x83,0xC6,0x0D,0x30,0xFC);


MIDL_DEFINE_GUID(IID, IID_IUserPropertyManagerPage,0x287C3DC8,0x5A30,0x499A,0x94,0x65,0xA4,0x46,0x2F,0xB4,0x23,0x77);


MIDL_DEFINE_GUID(IID, LIBID_SwUnibaseLib,0x6FB9D0CD,0xB720,0x4EE6,0xB2,0x70,0xC0,0xC0,0x3A,0xD6,0x06,0xE5);


MIDL_DEFINE_GUID(CLSID, CLSID_SwUnibase,0x3A46B873,0xBBA2,0x4787,0x84,0x89,0x9F,0xDA,0x19,0xFB,0xE5,0xD6);


MIDL_DEFINE_GUID(CLSID, CLSID_SwDocument,0x569655C9,0xD4B8,0x470A,0x98,0xAC,0xCF,0x7B,0xCA,0x98,0x33,0x53);


MIDL_DEFINE_GUID(CLSID, CLSID_DocView,0x65866DB4,0x8A06,0x4CE9,0x98,0x6F,0xC3,0x86,0x79,0x1B,0xF8,0x28);


MIDL_DEFINE_GUID(CLSID, CLSID_BitmapHandler,0xB3E4297F,0xE47E,0x4E20,0x99,0x7B,0x1C,0x5E,0x57,0x82,0xE6,0x45);


MIDL_DEFINE_GUID(CLSID, CLSID_PMPageHandler,0xE7393FA7,0xF505,0x4622,0x80,0x57,0x2B,0x40,0x81,0x56,0xCC,0x24);


MIDL_DEFINE_GUID(CLSID, CLSID_UserPropertyManagerPage,0xB2F593EF,0x9EEC,0x4C59,0x98,0xF9,0x78,0x93,0x08,0x35,0xD8,0xC9);

#undef MIDL_DEFINE_GUID

#ifdef __cplusplus
}
#endif



