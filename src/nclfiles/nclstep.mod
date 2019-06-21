!  Automatic Naming Conventions for geometry during conversion
!	from STEP to Unibase.
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
!    EXAMPLE:  /PLANES/PL,*NO or /PLANES/MYPLX,*YES
!			This changes naming from (PL1, PL2, PL3, ...) to 
!			(MYPLX(1), MYPLX(2), MYPLX(3), ...).
!
#GEOMETRY_NAME_MODALS#
/POINTS/PT,  *NO
/PNTVECS/PV, *NO
/LINES/LN,   *NO
/VECTORS/VE, *NO
/PLANES/PL,  *NO
/CIRCLES/CI, *NO
/CURVES/CV,  *NO
/SURFACES/SF,*NO
/SHAPES/SH,  *NO
/MATRICES/MX,*NO
/PATTERNS/PN,*NO
/SOLIDS/SO,  *NO
!
!	Attributes
!    Note: /ON_LAYERS/ is used to turn on automatic layer assignments for all
!          shapes and to assign the starting layer number. When turned on, each
!          shape will be assigned to its own layer starting at the given layer
!          number.
!
#ATTRIBUTES#
/SHADED/          	*YES
/INIT_LABELS/     	*YES
/DUPLICATES/      	*NO
/COLORS/          	*STEP
/CONVERSION/			*ALL
/LABELS/				  	*RECORD
/SUBSCRIPTS/		  	*NO
/CONCATENATE/		  	*NO
/EDGE_DISPLAY/		  	*YES
/EDGE_COLOR/		  	*BLACK
/ON_LAYERS/				*NO
/LAYER_START/			1
!
!  Label Matching
!
#LABEL_MATCHING#
/LEVEL/ *LEVEL4
/TOLER/ .001in
/LAYER_EXACT/ 1
/LAYER_1/ 2
/LAYER_2/ 3
/LAYER_3/ 4
/LAYER_4/ 5
/LAYER_MATCH/ 6
/LAYER_IMPORT/ 7
/COLOR_EXACT/ *WHITE
/COLOR_1/ *BLUE
/COLOR_2/ *RED
/COLOR_3/ *GREEN
/COLOR_4/ *YELLOW
/COLOR_MATCH/ *ORANGE
/COLOR_IMPORT/ *PURPLE
/IMPORT_GEO/ *NO
/START_UNMATCH/ *NEXT
/REGRESSIVE/ *YES
!
!  Entity Filtering/Masking During Translation from STEP to Unibase
!
!  This allows the user to specify which entities to filter out or
!  mask during the translation.  Everything that ends with "YES" will
!  be translated while everything that end with "NO" will not
!  be in the Unibase.
!
!        /CURVES/    *YES -> import surface boundaries as standalone curves
!			/SOLIDS/    *NO  -> import shells as individual surfaces
!			/SOLIDS/    *YES -> import shells as composite solids
!			/WIREFRAME/ *YES -> import wireframe geometry
!			/PLANES/    *YES -> import reference planes
!			/POINTS/    *YES -> import points
!
!  There are three options for translating composite curves: 
!        
!        /COMPONENTS/  *NO     -> Only curves are translated.
!        /COMPONENTS/  *YES    -> Only components of curves are translated.
!        /COMPONENTS/  *BOTH   -> Both curves and components translated.
!
!
#FILTER#
/CURVES/ *NO
/COMPONENTS/ *NO
/SOLIDS/ *NO
/WIREFRAME/ *YES
/PLANES/ *YES
/POINTS/ *YES

!
!  General Options During Translation from Unibase to STEP
!
!			/FORMAT/    Either *AP203 or *AP214 (attributes are output)
!			/UNITS/     *SAME (use Unibase units), *INCH, or *MM
!			/NORMALS/   *YES use surface normals to determine side of surface
!                    facing out from part.  Used UNDEFINED if set to *NO.
!
#STEP_OUT#
/FORMAT/ *AP214
/UNITS/ *INCH
/NORMALS/ *YES

!
!  Entity Filtering/Masking During Translation from Unibase to STEP
!
!  This allows the user to specify which entities to filter out or
!  mask during the translation.  Everything that ends with "YES" will
!  be translated while everything that ends with "NO" will not be
!  translated.
!
!			/SOLIDS/    *YES -> export composite solids as closed shells
!			/SURFACES/  *YES -> export trimmed surfaces
!			/NETSURFS/  *YES -> export NET surfaces
!			/UNTRIMMED/ *YES -> export untrimmed surfaces
!			/WIREFRAME/ *YES -> export wireframe geometry
!			/PLANES/    *YES -> export reference planes
!			/POINTS/    *YES -> export points
!
#FILTER_OUT#
/SOLIDS/ *YES
/SURFACES/ *YES
/NETSURFS/ *YES
/UNTRIMMED/ *NO
/WIREFRAME/ *no
/PLANES/ *YES
/POINTS/ *YES

#INCLUDE#
/FILE/ nclstep_color.mod
