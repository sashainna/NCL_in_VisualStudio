/********************************************************************* 
**  NAME:  wsgcdev.h 
**
**      Graphicon workstation include file.  This is a unicad copy
**			of the Graphicon file gap_device.h
**
**  MODULE NAME AND RELEASE LEVEL 
**       wsgcdev.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:09
**
*********************************************************************/

/*                       *** DEVICE.H ***		     */

/* Device types */
typedef enum{ KEYBRD,   /* Keyboard */
              HOSTIF,   /* Host interface */
              KURTAB,   /* Kurta Tablet */
              DIALS,    /* Valuator Dials */
              ALTDIG}   /* Altek Digitizer */
              devtyp ;

/* Device flags */
#define DEVEND 0X10  /* Enable the device */
#define DEVENC 0X20  /* Enable local cursor tracking */
#define DEVHST 0X40  /* Receive data through host Interface */

/* Only one of the next three device flags may be used */
#define DEVRQM 0X4000  /* In Request Mode */
#define DEVSPM 0X8000  /* In Sample Mode */
#define DEVEVM 0XC000  /* In Event Mode */

/* Device structure for input modules with opndev */
struct ipdata{
    int ip;   /* Input module Number */
    float initval;      /* Initial Value */
    float trigev;       /* Smallest value To Trigger Event */
    float min;          /* Minimum Number */
    float max;          /* Maximum Number */
    };

/* Defined input modules for locator */
#define X_IP 1       /* Input module number For X On A Locator */
#define Y_IP 2       /* Input module number For Y On A Locator */

#define DEV_PEN_UP   1
#define DEV_PEN_DOWN 2

/* Data expected back during a read of KURTAB */
struct dev_kurtab{
    int flag;		/* Flag is either DEV_PEN_UP or DEV_PEN_DOWN */
    int x;		/* IEEE flt value. Call fm_ieee() for real value */
    int y;    		/* IEEE flt value. Call fm_ieee() for real value */
};

/* Data expected back during a read of KEYBRD */
struct dev_keybrd{
    int buf;		/* Buf (LSB) contains the character */
};
