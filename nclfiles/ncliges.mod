!  Automatic Naming Conventions for geometry during conversion
!	from IGES to Unibase.
!
!  In order to change the default abbreviation for geometry,
!  find the geometry that you want to change and edit the abbreviation.
!	If you want to use prefix numbering (ex. pt1, pt2, pt3, ...), make
!	the second entry should be "no".  Prefix must be 2 characters.
!	However, if you want to use subscript numbering 
!	(ex. pt(1), pt(2), pt(3), ...), make the second entry "yes".
!	The abbreviations for subscripting must be between 2 and 6
!	characters in length. 
!
!    EXAMPLE:  /planes/pl,no -> /planes/myPlx,yes
!			This changes naming from (pl1, pl2, pl3, ...) to 
!			(myPlx(1), myPlx(2), myPlx(3), ...).
!
#GEOMETRY_NAME_MODALS#
/POINTS/PT,   *NO
/PNTVECS/PV,  *NO
/LINES/LN,    *NO
/VECTORS/VE,  *NO
/PLANES/PL,   *NO
/CIRCLES/CI,  *NO
/CURVES/CV,   *NO
/SURFACES/SF, *NO
/SHAPES/SH,   *NO
/MATRICES/MX, *NO
/PATTERNS/PN, *NO


!	Entity Filtering/Masking During Translation from IGES to Unibase
!
!	This allows the user to specify which entities to filter out or
!	mask during the translation.  Everything that ends with "YES" will
!	be translated while everything that end with "NO" will not
!	be in the Unibase.
!
!	The only thing that is changed is "YES" <-> "NO".  Nothing else
!	is to be modified.
!
!     Example:
!        120,SURFACES OF REVOLUTION,  *NO   -> not translated
!        122,TABULATED CYLINDERS,     *YES  -> translated
!
#ENTITY_FILTER_MASK#
/100/CIRCULAR ARCS,             *YES
/102/COMPOSITE CURVES,          *YES
/104/CONICS,                    *YES
/106/POLYLINE 2D,               *YES
/606/POLYLINE 3D,               *YES
/906/POLYLINE 6D,               *YES
/108/BOUNDED PLANE,             *YES
/110/LINES,                     *YES
/112/PARAMETRIC SPLINES CURVES, *YES
/114/PARAMETRIC SURFACES,       *YES
/116/POINTS,                    *YES
/118/RULED SURFACES,            *YES
/120/SURFACES OF REVOLUTION,    *YES
/122/TABULATED CYLINDERS,       *YES
/126/NURB SPLINE CURVES,        *YES
/128/NURB SURFACES,             *YES
/140/OFFSET SURFACES,           *YES
/141/BOUNDED CURVES,            *YES
/142/SURFACE CURVES,            *YES
/143/BOUNDED SURFACES,          *YES
/144/TRIMMED SURFACES,          *YES
/202/ANGULAR DIMENSIONS,        *YES
/206/DIAMETER DIMENSIONS,       *YES
/210/LABELS,                    *YES
/212/NOTES,                     *YES
/214/ARROWS,                    *YES
/216/LINEAR DIMENSIONS,         *YES
/222/RADIUS DIMENSIONS,         *YES
/228/SYMBOLS,                   *YES
/402/GROUP,                     *YES
/404/DRAWING,                   *YES
/406/NAME,                      *YES
/408/SUBFIGURE INSTANCE,        *YES
/410/VIEW,                      *YES
/502/VERTEX,                    *YES
/602/ASSOCIATION,               *YES
/907/POLYLINE CURVE		*NO


!	Attributes
!
#ATTRIBUTES#
/SHADED/          	*YES
/INIT_LABELS/     	*YES
/DUPLICATES/      	*NO
/COLORS/          	*IGES
/CONVERSION/		*ALL
/SURF_CURVE/	  	*CURVE
/LABELS/	  	*IGES
/MAX_LABEL/	  	0
/SUBSCRIPTS/	  	*NO
/CONCATENATE/	  	*NO
/EDGE_DISPLAY/	  	*YES
/EDGE_COLOR/	  	*BLACK
/DECOMPOSE/       *NO
!
!  Label Matching
!
#LABEL_MATCHING#
/LEVEL/           	*EXACT
/TOLER/			.001in
/LAYER_EXACT/		1
/LAYER_1/		2
/LAYER_2/		3
/LAYER_3/		4
/LAYER_4/		5
/LAYER_MATCH/		6
/LAYER_IMPORT/		7
/COLOR_EXACT/  		*AUTO
/COLOR_1/  		*AUTO
/COLOR_2/  		*AUTO
/COLOR_3/  		*AUTO
/COLOR_4/  		*AUTO
/COLOR_MATCH/  		*AUTO
/COLOR_IMPORT/  	*AUTO
/IMPORT_GEO/  		*NO
/START_UNMATCH/  	*NEXT
/REGRESSIVE/		*YES

#INCLUDE#
/FILE/ ncliges_color.mod

