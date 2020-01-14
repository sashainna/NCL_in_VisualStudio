/*********************************************************************
**    NAME         :  iunihep.c
**       CONTAINS:
**       	uu_uerror0		uu_uerror1		uu_uerror2
**			uu_uprompt0		uu_uprompt1		uu_uprompt2
**			uui_msgtemp
**       	uu_inithep		uu_uerhep		uui_ugetbuf		uu_upmthep		
**  		uu_initerr		uu_outputerr	uu_usysinfor
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       uniehep.c , 1.1
**    DATE AND TIME OF LAST MODIFICATION
**       12/14/84 , 03:48:24
*********************************************************************/


#include     <stdio.h>
#include	<string.h>
#include	<ctype.h>
#include	<stdlib.h>
#include     "usysdef.h"
#include     "gtbl.h"
#include     "g.h"
#include     "usysg.h"
#include     "derror.h"
#include     "tmsg.h"
#include     "udebug.h"
#include     "xenv1.h"

#define	UMAXSUB	17


static UTI_HEADER hdmsg[]= {
{-1,-1,-1,-1,-1,-1},{0,106,106,70,-1,0},{-1,-1,-1,-1,-1,-1},{176,57,-1,0,-1,0},
{233,11,-1,0,-1,0},{244,5,-1,0,-1,0},{249,14,263,1,-1,0},{264,328,592,349,-1,0},{941,28,969,12,-1,0},
{981,1,982,8,-1,0},{990,33,1023,11,-1,0},{1034,124,1158,127,-1,0},{1285,30,1315,20,-1,0},{1335,60,1395,172,-1,0},
{1567,16,1583,3,-1,0},{1586,1,-1,0,-1,0},{1587,6,1593,94,-1,0},{1687,19,1706,706,-1,0},};

static UTI_ERMSG pemsg[]= {
{1,0,"Valuator input not allowed; use locate, pick or text string. ",-1},
{2,0,"Attempted pick was unsuccessful.  Retry.  [More info in HELP.]",0},
{3,0,"Default selected when no default in effect.  [More info in HELP.]",1},
{4,0,"Menu in use.  Complete current operation (DONE).  [More info in HELP.]",2},
{5,0,"Scalar variable not allowed; enter coordinate.  [More info in HELP.]. ",3},
{6,1,"DAS#6; unrecognizable input from text parser. [More info in HELP.]",4},
{7,1,"DAS#7; invalid input event. [More info in HELP.]",5},
{8,1,"Invalid input; use choice. [More info in HELP.]",6},
{9,1,"DAS#9; invalid DAS request. [More info in HELP.]",7},
{10,0,"Increment limit exceeded.",-1},
{11,1,"DAS#11; unsupported symbol table data type. [More info in HELP.]",8},
{12,0,"DAS#12; undefined control action encountered. [More info in HELP.]",9},
{13,0,"Last entity already deleted; no entity to delete. [More info in HELP.]",10},
{14,0,"%s - Undefined subsystem invoked.  [More info in HELP.]",11},
{15,0,"Zero or negative increments not allowed.",-1},
{16,0,"Incremental form not allowed; scalar input only.",-1},
{17,0,"Coordinate form not allowed; scalar input only.",-1},
{-1},
{-1},
{20,0,"Menu in use.  Complete current operation (DONE).  [More info in HELP.]",12},
{-1},
{22,0,"Attempted pick was unsuccessful.  Retry.  [More info in HELP.]",13},
{23,0,"DAS#23; invalid event type in select DAS.  [More info in HELP.]",14},
{-1},
{25,0,"Invalid input; use location pick.?",-1},
{26,0,"DAS#26; all increment counts are zero; re-enter.  [More info in HELP.]",15},
{-1},
{28,3,"DAS#28; invalid event played from record/playback file=%s.  [See HELP.]",16},
{29,3,"DAS#29; invalid event being recorded.  [More info in HELP.]",17},
{-1},
{31,4,"DAS#31; d_select returned from d_jmpmark.  [More info in HELP.]",18},
{32,0,"Cursor not on an icon.  Try again.  [More info in HELP.]",19},
{-1},
{34,2,"DAS#34; invalid feature picked; data error.  [More info in HELP.]",20},
{35,2,"DAS#35; unsupported symbol table data type.  [More info in HELP.]",21},
{36,0,"Text entered is longer that allowed; re-enter.  [More info in HELP.]",22},
{-1},
{38,0,"Illegal input; enter string. ;?",-1},
{-1},
{-1},
{-1},
{42,0,"Incremental form not allowed; real number input only.  [More info in HELP.]",23},
{43,0,"Ref. coordinate not allowed; real number input only.  [More info in HELP.]",24},
{44,0,"Coordinate form not allowed; real number input only.  [More info in HELP.]",25},
{45,0,"Unrecognizable input; enter real number. [More info in HELP.]",26},
{-1},
{47,1,"Choice not allowed for real number input.  [More info in HELP.]",27},
{-1},
{49,0,"Location not allowed for vector input.  [More info in HELP.]",28},
{50,0,"The keyboard 'v' key must be used to pick a vector.  [More info in HELP.]",-1},
{-1},
{-1},
{53,0,"Choice not allowed; enter vector. ",29},
{54,0,"Incremental form not allowed; vector input only.",30},
{55,0,"Reference coordinate definition not allowed; vector input only.",31},
{56,0,"Scalar input not allowed; vector input only.",32},
{57,0,"Unrecognizable input; vector input only.",33},
{58,2,"DAS#58; invalid event in vector input.  [More info in HELP.]",34},
{59,1,"Vector <0,0,0> invalid.  Re-enter.  [More info in HELP]",35},
{60,1,"Unable to find the playback file %s.",-1},
{61,0,"DAS#61; cannot open file.  [More info in HELP.]",36},
{62,0,"Cannot start recording; currently recording or paused.  [More info in HELP.]",37},
{63,0,"Cannot playback; currently recording or paused.  [More info in HELP.]",38},
{64,0,"Playback limit exceeded.",39},
{-1},
{66,0,"Cannot playback; playback already started.  [More info in HELP.]",40},
{67,0,"Illegal keyboard character used to pick entity.  [More info in HELP.]",41},
{68,0,"Too many subsystems selected.  Forcing jump to root.  [More HELP.]",42},
{69,0,"Input constrained to %s, re-enter.;?",-1},
{70,0,"Cannot pick this entity now",-1},
{71,0,"Number of interactions are constrained between %d and %d.;?",-1},
{72,0,"You must enter a value between %g and %g.",-1},
{73,0,"Input not allowed; input stroke with stroke device",-1},
{74,1,"Invalid input; use stroke. ",-1},
{75,1,"DAS#75; invalid input event. [More info in HELP.]",43},
{76,1,"Restricted entity picked; retry. [More info in HELP.]",44},
{77,1,"Global select buffer is busy, complete previous operation",-1},
{78,1,"The two points must be in the same viewport.",-1},
{79,1,"Illegal icon rectangle.",-1},
{80,5,"Can't open icon file %s",-1},
{81,5,"Can't create icon file %s (file already exists)",-1},
{82,5,"Can't create icon file %s",-1},
{83,5,"Corrupted icon file %s",-1},
{84,1,"Cursor not in graphics area - reenter",-1},
{85,4,"Zero length icon file %s",-1},
{86,4,"Environment stack overflow - flushing system",45},
{87,0,"Warning - go to MAIN soon; too many subsystem calls.  [More info in HELP.]",46},
{88,0,"3D vector not allowed while in drawing management.  [More info in HELP.]",47},
{89,0,"3D coordinate not allowed while in drawing management.  [More info in HELP.]",48},
{90,0,"Construction plane is orthogonal to view plane.  [More info in HELP.]",49},
{91,0,"Form window is currently in use.  [More info in HELP.]",50},
{92,0,"No entities passed select filter.",-1},
{93,0,"Only a 'y', 'Y', 'n' or 'N' response is valid",-1},
{94,0,"No help area defined in this layout. Help and tutorials unavailable.",-1},
{95,0,"Window creation error has occured.  [More info in HELP.]",51},
{96,0,"You may not chain select on lines that have line thickness applied.",-1},
{97,4,"MARK stack out of balance !!!",52},
{98,0,"Internal Select Buffer is Full",-1},
{99,0,"Coordinates must be between 0.0 and 1.0",53},
{100,0,"You have exeeded the maximum number of open windows",54},
{101,0,"This entity may not be used for plane definition.",-1},
{102,0,"Plane not yet defined.",-1},
{103,0,"3-D space defined: Restart plane specification.",-1},
{104,0,"Auto-test tables are not linked into this application.",-1},
{105,0,"Subsystems are disabled, complete current input sequence",-1},
{106,0,"Menu Design is already active",-1},
{1,-1,"Enter Z depth:",-1},
{2,-1,"Enter delta vector:",55},
{3,-1,"Enter prompt line: ",-1},
{4,-1,"Enter message",-1},
{-1},
{6,-1,"Enter lower left corner of display window.",-1},
{7,-1,"Enter record file name: ",-1},
{8,-1,"File exists - overwrite?",-1},
{9,-1,"Append to existing file?",-1},
{10,-1,"Enter playback file name: ",-1},
{11,-1,"by choice",-1},
{12,-1,"by value ",-1},
{13,-1,"by location ",-1},
{14,-1,"by pick ",-1},
{15,-1,"by text ",-1},
{16,-1,"Press command reject key to terminate; any other key to continue.",-1},
{17,-1,"END OF MESSAGE; press any key. ",-1},
{18,-1,"/* cartesian coordinate input request */",56},
{-1},
{20,-1,"/*  value input request */",57},
{21,-1,"/*choice input request */",-1},
{22,-1,"/* integer input request */",58},
{23,-1,"/* vector input request */",59},
{24,-1,"/* pick input request */",60},
{25,-1,"/* location pick input request */",61},
{26,-1,"/* string input request */",62},
{27,-1,"/* choice input request */",63},
{28,-1,"/* cartesian ndc coordinate input request */",-1},
{29,-1,"/* angle input */",-1},
{30,-1,"/* unitless real number input */",-1},
{31,-1,"iteration count exceeds 1024 coordinates, is this what you want?",-1},
{32,-1,"",-1},
{33,-1,"",-1},
{34,-1,"",-1},
{35,-1,"",-1},
{36,-1,"(Vector)",-1},
{37,-1,"",-1},
{38,-1,"",-1},
{39,-1,"",-1},
{40,-1,"",-1},
{41,-1,"",-1},
{42,-1,"",-1},
{43,-1,"",-1},
{44,-1,"by stroke:",-1},
{45,-1,"enter new screen number",-1},
{46,-1,"enter number of screen to copy",-1},
{47,-1,"enter layout filename",-1},
{48,-1,"enter layout filename",-1},
{49,-1,"enter new color index",-1},
{50,-1,"enter number of rows in this icon menu",-1},
{51,-1,"enter number of columns in this icon menu",-1},
{52,-1,"enter number of icons in this menu:",-1},
{53,-1,"choose an icon position ",-1},
{54,-1,"pick the area",-1},
{55,-1,"enter new icon menu filename",-1},
{56,-1,"enter new icon filename",-1},
{57,-1,"Pick start item of chain",-1},
{58,-1,"Enter first corner of region",-1},
{59,-1,"Enter second corner of region",-1},
{60,-1,"Enter save screen filename",-1},
{61,-1,"Pick entities for icon",-1},
{62,-1,"Pick lower left corner of icon",-1},
{63,-1,"Pick upper right corner of icon",-1},
{64,-1,"PRESS ANY KEY TO CONTINUE.",-1},
{65,-1,"Indicate the entity to select",-1},
{66,-1,"Is this the desired entity?",-1},
{67,-1,"Reverse direction of vector?",-1},
{68,-1,"Enter the origin of the plane",-1},
{69,-1,"Enter the normal vector to the plane",-1},
{70,-1,"Enter new pick aperture",-1},
{1,0,"Undefined Calculator variable name.  [More info in HELP.]",64},
{2,0,"Syntax error; re-enter.",-1},
{3,0,"Calculator calls exceed limit of 4; action denied.  [More info in HELP.]",65},
{4,0,"Invalid input; illegal expression. ",-1},
{5,0,"CALC#5; error reading lexicon file.  [More info in HELP.]",66},
{6,0,"Expression contains too many operators.  [More info in HELP.] ",67},
{7,0,"Invalid input; undefined data type.;?",-1},
{8,0,"CALC#8; data type not supported in symbol table.  [More info in HELP.]",68},
{9,0,"Illegal input; letter 'f' reserved for function names.  [More info in HELP.]",69},
{10,0,"Illegal use of reserved word.;?",-1},
{11,0,"Dot not allowed in variable name. ",-1},
{12,0,"No input; re-enter. ",-1},
{13,0,"CALC#13; variable not supported in symbol table.  [More info in HELP.]",70},
{14,0,"Invalid input; enter coordinate variable.",-1},
{15,0,"Invalid input; enter scalar variable.",-1},
{16,0,"Function name undefined.  [More info in HELP.]",71},
{17,0,"Parameters exceed limit of 4. ;?",-1},
{18,0,"Nested functions exceed limit of 20.",-1},
{19,0,"CALC#19; parameter type not supported.  [More info in HELP.]",72},
{-1},
{21,0,"Number of parameters must match previous definition.  [More info in HELP.]",73},
{22,0,"CALC#22; empty table.",-1},
{23,0,"Invalid input; enter 0 or positive number only.",-1},
{24,0,"Invalid input; number is larger than nine.",-1},
{25,0,"Warning - notation unchanged.  [More info in HELP.]",74},
{26,0,"CALC#26; operation not supported.  [More info in HELP.]",75},
{27,0,"Illegal input; improper operand. ",-1},
{28,0,"Coordinate cannot multiply a coordinate.  [More info in HELP.]",76},
{29,0,"Coordinate cannot divide a coordinate.  [More info in HELP.]",77},
{30,0,"Cannot divide by zero.",-1},
{31,0,"Coordinate cannot divide a scalar.",-1},
{32,0,"Illegal exponential expression. ",-1},
{33,0,"Invalid input; use scalar operands.",-1},
{34,0,"Invalid input; use coordinate operands. ",-1},
{-1},
{36,0,"Value exceeds limit of 10 ^ 15.",-1},
{37,0,"Invalid input; square root of negative number not allowed.",-1},
{38,0,"CALC#38; operator type not supported.  [More info in HELP.]",78},
{39,0,"CALC#39; vector and angle operation not supported.;?",-1},
{40,0,"CALC#40; coordinate component not supported.;?",-1},
{41,0,"CALC#41; coordinate type not supported.;?",-1},
{42,0,"Variable must be a coordinate. ",-1},
{43,0,"Stroke input not implemented.  [More info in HELP.]",79},
{44,0,"CALC#44; choice unsupported.  [More info in HELP.]",80},
{-1},
{46,0,"Tangent of 90 degrees or 270 degrees not allowed.",-1},
{47,0,"Name must start with a non-numeric character other than 'f'.",-1},
{48,0,"'Help' not supported as a command.  Use the help icon.",-1},
{49,0,"Cannot open file 'calc.grm.  [More info in HELP.]",81},
{50,0,"Invalid variable name;?",-1},
{51,0,"Cannot open lexicon file %s",82},
{52,0,"Cannot read lexicon file (%s) length",-1},
{53,0,"Cannot read lexicon file (%s) data",-1},
{54,0,"Invalid input; logarithm of zero or negative number not allowed.",-1},
{55,0,"Input string is longer than the buffer size",-1},
{56,0,"Reserved word cannot be used as a CALC symbol.",-1},
{57,0,"Syntax error - illegal function type.",-1},
{1,0,"Text input not in format required by prompt.  [More info in HELP.]",83},
{2,0,"DASIN#2; stack overflow.  [More info in HELP.]",84},
{3,0,"DASIN#3; stack is underflow.  [More info in HELP.]",85},
{4,0,"Incremental number must be positive.",-1},
{5,0,"DASIN#5; improper translation code.  [More info in HELP.]",86},
{6,0,"Invalid input; check units. ;?",-1},
{7,0,"Enter angle or number. ;?",-1},
{8,0,"Invalid input; units required.;?",-1},
{9,0,"Invalid unit input.  Valid units are: in ft mi mm cm m km.;?",-1},
{10,0,"DASIN#10; type not supported.  [More info in HELP.]",87},
{11,0,"DASIN#11; input value type not supported.  [More info in HELP.]",88},
{1,0,"Text input not valid.  Use cursor to select an icon.  [More info in HELP.]",89},
{2,0,"This icon or function key not valid at this time.  [More info in HELP.]",90},
{3,0,"%s",-1},
{4,0,"MENU#4, menu stack overflo.  [More info in HELP.]",91},
{5,0,"MENU#5, menu subroutine stack overflo.  [More info in HELP.]",92},
{1,1,"Feature order out of bounds.  [More info in HELP.]",93},
{2,1,"error - selected arc has zero angl.  [More info in HELP.]",94},
{3,0,"No features display for entity selected.  [More info in HELP.]",95},
{4,1,"error in retrieve geometr.  [More info in HELP.]",96},
{5,1,"error - pick id out of range for %.  [More info in HELP.]",97},
{6,0,"error - too many picks;?",-1},
{7,0,"error - invalid pick identifier.  [More info in HELP.]",98},
{8,0,"eature display not not available for point entities.  [More info in HELP.]",99},
{9,1,"m_feareal: graphics pick id of %d is out of rang.  [More info in HELP.]",100},
{10,1,"m_feavect: graphics pick id of %d is out of vector rang.  [More info in HELP.]",101},
{11,1,"m_feacoord: graphics pick id of %d is out of coordinate rang.  [See HELP.]",102},
{12,0,"Too many entities for Feature display (type 'real').  [More info in HELP.]",103},
{13,0,"Too many entities for Feature display (type 'coord').  [More info in HELP.]",104},
{14,0,"Too many entities for Feature display (type 'vector').  [More info in HELP.]",105},
{1,-1,"Select Geometry for Feature Display",-1},
{-1},
{2,3,"MODEL#2; c6_bsplcrv: insufficient memory to allocate.  [More info in HELP.]",106},
{3,1,"MODEL#3; c6_bsplcrv: coefficient list full.  [More info in HELP.]",107},
{4,1,"Number of control points for spline must be >= %d.  [More info in HELP.]",108},
{5,5,"Order of b-spline must be 2, 3 or 4.  [More info in HELP.]",109},
{6,5,"MODEL#6; tr06_tranbsplcrv: error on update_var_geom.  [More info in HELP.]",110},
{7,5,"MODEL#7; rt06_bsplcrv: error on update_var_geom.  [More info in HELP.]",111},
{8,5,"MODEL#8; cp06_bsplcrv: error on create_geom.  [More info in HELP.]",112},
{9,5,"MODEL#9; cp06_bsplcrv: error on update_var_geom.  [More info in HELP.]",113},
{10,1,"Axis direction vector must be longer than %f.  [More info in HELP.]",114},
{11,0,"Difference between angles must be between -360 and +360 degrees.  [See HELP.]",115},
{12,0,"Selected entity not a curve.  Re-select.",116},
{13,5,"MODEL#13; integrate: stack overflow.  [More info in HELP.]",117},
{14,0,"Selected entity not a line.  Re-select.",-1},
{15,0,"Angle entered must be between 0 and 180 degrees.  [More info in HELP.]",118},
{16,0,"Lines 1 and 2 do not intersect.  [More info in HELP.]",119},
{17,0,"Chamfer distance entered is too large.  [More info in HELP.]",120},
{18,0,"Chamfer angle entered is too large.  [More info in HELP.]",121},
{19,0,"Radius too small.  [More info in HELP.]",122},
{20,0,"Center and circumference points identical.  [More info in HELP.]",123},
{21,0,"Warning: point will be projected onto plane of the arc.  [More info in HELP.]",124},
{22,0,"Point on radius same point as circle center.  [More info in HELP.]",125},
{23,0,"Wrong type picked; pick a line, circle, or circular arc.  [More info in HELP.]",126},
{24,0,"Points are collinear.  [More info in HELP.]",127},
{25,0,"Center identical to first point.  [More info in HELP.]",128},
{26,0,"Center identical to second point.  [More info in HELP.]",129},
{27,0,"Length of arc would be too small.  [More info in HELP.]",130},
{28,0,"Angle not in range -360 to 360 degrees.",-1},
{-1},
{-1},
{31,0,"Entities not coplanar.  [More info in HELP.]",131},
{32,0,"MODEL#32; cir_ttr: illegal entities.  [More info in HELP.]",132},
{33,0,"Wrong type picked; pick a line, circle, or circular arc.  [More info in HELP.]",133},
{34,0,"Cannot calculate circle tangent to specified entities.  [More info in HELP.]",134},
{35,0,"Same entity picked more than once.  [More info in HELP.]",135},
{36,0,"Circle radius is too small.",136},
{37,0,"Curves to be composited are not connected.  [More info in HELP.]",137},
{38,0,"Selected curve not part of a composite.  [More info in HELP.]",138},
{39,0,"Dissolve not implemented for this entity type: %d",-1},
{40,0,"Illegal entity for composite curve.  [More info in HELP.]",139},
{41,3,"MODEL#41; get_endpts: illegal relation %d.  [More info in HELP.]",140},
{42,3,"MODEL#42; d_pdas: illegal type specification.  [More info in HELP.]",141},
{43,0,"MODEL#43; d_pdas: illegal entity picked. ",142},
{44,0,"MODEL#44; d_pldas: illegal type specification.  [More info in HELP.]",143},
{45,0,"MODEL#45; d_pldas: illegal entity picked.  [More info in HELP.]",144},
{46,3,"MODEL#46; dsegtomtid: illegal display segment id 0x=%x.  [More info in HELP.]",145},
{47,0,"Illegal edge picked for current operation.  [More info in HELP.]",146},
{48,3,"MODEL#48; freegeom, illegal relation.  [More info in HELP.]",147},
{49,3,"Wrong entity type picked.  You must pick a geometric entity.  [See HELP.]",148},
{50,0,"MODEL#50; illegal evaluation request.  [More info in HELP.]",149},
{51,3,"MODEL#51; evcrv, illegal curve relation.  [More info in HELP.]",150},
{52,3,"MODEL#52; init_relations: illegal relation %d.  [More info in HELP.]",151},
{53,0,"MODEL#53; p_geometry: undefined relation type %d.  [More info in HELP.]",152},
{54,3,"MODEL#54; drw_geometry: undefined entity %d.  [More info in HELP.]",153},
{55,0,"Start and endpoints identical.  [More info in HELP.]",154},
{56,0,"Wrong entity type picked; you must pick a curve.  [More info in HELP.]",155},
{57,0,"Point entered does not lie in the plane of the circle.  [More info in HELP.]",156},
{58,0,"The point entered lies inside the circle.",-1},
{59,0,"The new vertex point does not lie in the same plane as the polygon.",-1},
{60,0,"Same curve picked both times.  Pick two different curves.  [See HELP.]",157},
{61,0,"Wrong entity type picked; you must pick a circle.  [More info in HELP.]",158},
{62,0,"The circles picked do not lie in the same plane.",-1},
{-1},
{64,0,"One circle lies entirely inside the other.",-1},
{-1},
{66,0,"Distance is too small.",159},
{67,0,"Wrong entity type picked.  You must pick a line.  [More info in HELP.]",160},
{68,0,"Line is parallel to working plane normal.  [More info in HELP.]",161},
{69,0,"Enter a non-zero angle > -180 degrees and < 180 degrees [More info in HELP.]",162},
{70,0,"Angle must be greater than zero.  [More info in HELP.]",163},
{71,0,"Autotrimmed curve would be to small.  Curve will not be trimmed. [See HELP.]",164},
{72,0,"MODEL#72;coplanar: test not implemented for this entit.  [More info in HELP.]",165},
{73,0,"Cannot intersect line with curve %d",-1},
{74,0,"Cannot intersect circle with curve %d",-1},
{75,0,"Cannot intersect curves.",-1},
{76,0,"Zero vector (0,0,0) cannot be used as a delta vector.",-1},
{77,0,"Center and start points identical.  [More info in HELP.]",166},
{78,0,"Radius of circular array of points too small.  [More info in HELP.]",167},
{79,0,"Same curve picked twice.  You must pick different curves.  [More info in HELP.]",168},
{80,0,"The selected entities do not intersect.  [More info in HELP.]",169},
{81,0,"Magnitude of the vector entered is too small.  [More info in HELP.]",170},
{82,0,"Only composite curves consisting of lines and arcs may be swept into solids.",-1},
{-1},
{84,0,"Only planar composite curves can be swept into solids.",-1},
{85,0,"Rotation angle is essentially zero; enter larger angle.",-1},
{86,0,"Rotation vector is essentially zero; enter longer vector.",-1},
{-1},
{-1},
{89,0,"Only closed, planar composite curves can be lifted into solids.",-1},
{90,0,"Direction vector is essentially zero; enter longer vector.",-1},
{91,0,"Block dimensions cannot be essentially zero.",-1},
{92,0,"Both radii cannot be zero.",-1},
{93,0,"Circle centers are identical.",-1},
{94,0,"Pick two solids. ",-1},
{95,0,"Picked entities are identical.",-1},
{-1},
{97,0,"Pick a solid.",-1},
{98,0,"Translation not implemented for this entity",-1},
{99,0,"Rotation not implemented for this entity",-1},
{100,0,"Copy not implemented for this entity",-1},
{101,0,"Trimmed line would be too small.  [More info in HELP.]",171},
{102,0,"Can not trim to this curve",-1},
{103,0,"Trim entity and trim to element(s) are the same.  [More info in HELP.]",172},
{104,0,"Composite curves cannot be trimmed.  [More info in HELP.]",173},
{105,0,"Illegal trim element.",-1},
{106,0,"Filleted line too small to be trimmed.  [More info in HELP.]",174},
{107,0,"NOTE: circles cannot be auto-trimmed when filleted.  [More info in HELP.]",175},
{108,0,"Filleted arc too small to be trimmed.  [More info in HELP.]",176},
{109,0,"Cannot auto trim entity.  [More info in HELP.]",-1},
{110,0,"Only curves other than composite curves can be trimmed.  [More info in HELP.]",177},
{111,0,"Arc resulting from trim would be too small.  [More info in HELP.]",178},
{112,0,"The curve does not/cannot intersect the trim-to curve(s).  [See HELP.]",179},
{113,0,"The entities intersect on the entity to be trimmed.  [More info in HELP.]",180},
{114,0,"Cannot trim line.",-1},
{115,0,"Trimmed line too small for trim.",-1},
{116,0,"Circle in ambiguous viewplane; cannot trim.",-1},
{117,0,"Magnification factor must be greater than zero.",-1},
{118,0,"Entity picked is not a composite curve.  [More info in HELP.]",181},
{119,3,"MODEL#119; rotatept: direction vector (rotvec) too small.  [More info in HELP.]",182},
{120,0,"Number of points entered (%d) exceeds maximum allowed (%d) ",-1},
{121,0,"Y axis cannot be parallel to normal.",-1},
{122,0,"New control point not accepted; too close to previous control point.",-1},
{123,0,"New ctl point not accepted; spline would not have continuous 1st derivative.",-1},
{124,0,"ROMULUS failure; attempting to recover; press any key to continue.",-1},
{125,0,"Pick a solid.",-1},
{126,0,"Cannot split a composite curve containing a single constituent",-1},
{127,0,"Cannot split a composite curve at the first constituent",-1},
{128,0,"Mirroring not implemented for this entity.",-1},
{129,0,"Scaling not implemented for this entity.",-1},
{130,0,"Entity picked was not a point.  You must enter a point. [More info in HELP.]",183},
{131,0,"Entity picked was not a line.  You must enter a line.  [More info in HELP.]",184},
{132,0,"Entity picked was not a circle.  You must pick a circle.  [See HELP.]",185},
{133,5,"MODEL#133, um_tf06_bsplcrv: error on update_geom_var.  [More info in HELP.]",186},
{134,0,"Not an arc; break circle first.",-1},
{135,0,"Arc with endpoints cannot be broken.",-1},
{136,5,"MODEL#133, um_sc06_scalbsplcrv: error on update_geom_var.  [More info in HELP.]",187},
{137,0,"Yes, there is interference between these two bodies.",-1},
{138,0,"No interference between these two bodies.",-1},
{139,5,"um_get_all_geom: not enough space for retrieval of relation %d",-1},
{140,5,"umi_setup_evaluator: undefined relation of %d",-1},
{141,5,"umi_getarclen: undefined relation of %d",-1},
{142,0,"Entity picked was not a curve.  You must pick a curve.  [More info in HELP.]",188},
{143,0,"MODEL#143 An extra %d bytes have been allocated to draw surface(s).",-1},
{144,0,"MODEL#144 From um_del_geometry, undefined relation of %d.  [More info in HELP.]",189},
{145,5,"MODEL#145, font %s symbol %s not define.  [More info in HELP.]",190},
{146,0,"Current alignment (horiz,vert) = (%s,%s)",-1},
{147,0,"Current %s = %s",-1},
{148,5,"MODEL#148, font %s file symbol %s invalid forma.  [More info in HELP.]",191},
{149,5,"MODEL#149, font %s file symbol %s not define.  [More info in HELP.]",192},
{150,0,"Solids picked didn't intersect.",-1},
{151,0,"Sectioning object doesn't intersect the body.",-1},
{152,0,"Picked edges do not lie on same face.",-1},
{153,0,"You've picked the same edge twice.",-1},
{154,0,"A unique face is not defined by this edge.",-1},
{155,0,"um_drwsrf_dispatcher: illegal relation of %d.",-1},
{156,0,"um_getparampts: illegal relation for a defining curve to draw a surface.",-1},
{157,0,"um_create_geom: entity not stored, rel_num = %d",-1},
{158,0,"um_create_geom: can't store attributes, unknown class for relation: %d",-1},
{159,1,"um_create_geom: can't store attributes, no class for relation: %d",-1},
{160,1,"um_update_geom: can't update the entity that has key = %d",-1},
{161,1,"um_update_geom: can't update transform for entity that has key = %d",-1},
{162,1,"um_create_geom: can't store transform for entity that has key = %d",-1},
{163,1,"um_get_transformation: transformation not retrieved for entity with key = %d",-1},
{164,1,"um_transform_evaluator: undefined class of %d for entity with key = %d",-1},
{165,1,"um_update_transformation: transformation not updated for entity with key = %d",-1},
{166,5,"cp7_rbsplcrv: error on um_create_geom.",-1},
{167,1,"Invalid color",-1},
{168,5,"um_init_rel: UNIBASE relation creation for entity relation %d failed",-1},
{169,5,"um_init_rel: UNIBASE relation creation for transformation relation %d failed",-1},
{170,5,"um_init_rel: UNIBASE relation creation for attribute relation %d failed",-1},
{171,1,"Fillet/chamfer on this edge(s) would result in bad topology.",-1},
{172,1,"um_evsrf: surface relation %d not yet implemented.",-1},
{173,1,"Plane does not intersect the solid.",-1},
{174,1,"Polygon must have at least three vertices.",-1},
{175,1,"Not a filled polygon region.",-1},
{176,1,"Illegal geometry, body not created.",-1},
{177,0,"Ellipse with the given axes lengths would be too narrow.  [See HELP.]",193},
{178,0,"Axis length must be shorter.",-1},
{179,0,"Axis length must be longer",-1},
{180,0,"Vertex too close to center",-1},
{181,0,"Range too small",-1},
{182,0,"Foci are too close together",-1},
{183,0,"Point is too close to axis",-1},
{184,0,"Focal length is too short",-1},
{185,0,"umu_projcirc: error projecting circle  %d",-1},
{186,0,"Entity picked was not an ellipse.  You must pick an ellipse.  [See HELP.]",194},
{187,0,"Hyperbola was not picked; re-enter.",-1},
{188,0,"Parabola was not picked; re-enter.",-1},
{189,0,"Conic was not picked; re-enter.",-1},
{190,0,"Curvature value is too small",-1},
{191,0,"No conic satisfies given conditions",-1},
{192,0,"Endpoints must be distinct",-1},
{193,0,"Can't get endpoint conditions from picked entity",-1},
{194,0,"Vector condition rejected: perpendicular to working plane",-1},
{195,0,"Cannot modify closed ellipse",-1},
{196,0,"Entity with key %d would not report its span",-1},
{197,0,"Dimension of space spanned by entities is %d",-1},
{198,0,"Pen number must be in the range 1 - 256.",-1},
{199,0,"There is no view named %s in the database.",-1},
{200,0,"There is already a view named %s in the database.",-1},
{201,0,"There is no sheet named %s in the database.  [More info in HELP.]",-1},
{202,0,"sheet %s already exists in database",-1},
{203,0,"There is no drawing named %s in the database.  [More info in HELP.]",195},
{204,0,"There is already a drawing named %s in the database.  [More info in HELP.]",196},
{205,0,"Layer number must be in the range 0 to 9999",-1},
{206,0,"You cannot change the normal vector in a reference view",-1},
{207,0,"Reference views cannot be saved",-1},
{208,0,"There is no view named %s in the database.  [More info in HELP.]",197},
{209,0,"Normal vector cannot be parallel to the up vector.  [More info in HELP.]",198},
{210,0,"You have not assigned that name to a layer.",-1},
{211,0,"Cannot delete a reference view",-1},
{212,0,"Cannot delete a displayed view",-1},
{213,0,"MODEL#213: error in retrieving relation number for key=%d.  [More info in HELP.]",199},
{214,0,"CONFLICT! new layer number %d deleted",-1},
{215,0,"CONFLICT! new view name %s deleted",-1},
{216,0,"The lines are not coplanar.  [More info in HELP.]",200},
{217,0,"The curve cannot be constructed because all three lines are parallel.  ",-1},
{218,0,"The curve cannot be constructed because two of the lines are collinear.",-1},
{219,0,"The curve cannot be constructed - three of the lines intersect at one point.",-1},
{220,0,"The location of the circle is ambiguous from the picked locations",-1},
{221,0,"No surface picked, (%s).",-1},
{222,0,"Can't copy default attributes, (%s).  [More info in HELP.]",201},
{223,0,"Invalid modeling relation: %d, (%s).  [More info in HELP.]",202},
{224,0,"No active drawing",-1},
{225,0,"Enitity picked was not a curve. You must pick a curve.  [More info in HELP.]",203},
{226,0,"Cannot multifillet intersections of arcs with arcs.  [More info in HELP.]",204},
{227,0,"Scale factor must be positive.  [More info in HELP.]",205},
{228,0,"Illegal screen name %s.  [More info in HELP.]",206},
{229,0,"No form file for screen name %s.  [More info in HELP.]",207},
{230,0,"Drawing capacity exceeded.  [More info in HELP.]",208},
{231,0,"Locate cursor (+) cannot be used when const. plane perp. to view.  [See HELP.]",209},
{232,0,"Grid spacing is smaller that 1/20 of aperture.  [More info in HELP.]",210},
{-1},
{234,0,"drawing archive file %s does not exist",-1},
{235,0,"don't have read access to archive file %s",-1},
{236,0,"don't have write access to archive file %s",-1},
{237,0,"no methods available to modify this entity",-1},
{238,0,"entities not connected",-1},
{239,0,"Fillet radius is too large.",211},
{240,0,"Line and arc are tangent, cannot fillet",-1},
{241,0,"Illegal (sub)curve to offset of relation type, %d  (%s).",-1},
{242,0,"Offset direction vector is too small, try again,  (%s).",-1},
{243,0,"Can't determine how to offset, try again  (%s).",-1},
{244,0,"Can't determine offset direction for subcurve curve with key: %d,  (%s).",-1},
{245,0,"Invalid trim status, (%s).",-1},
{246,0,"Can't determine offset direction for curve, %d,  (%s).",-1},
{247,0,"Can't determine which side to offset a subcurve of composite,  (%s).",-1},
{248,0,"Intersector failed on entities with relation numbers of: %d and %d,  (%s).",-1},
{249,0,"No points found from which to trace offset  (%s).",-1},
{250,0,"%d is too many intersections between offset subcurves,  (%s).",-1},
{251,0,"Can't find next intersection on offset,  (%s).",-1},
{252,0,"No offset curve found to trim/extend between current intersections,  (%s).",-1},
{253,0,"Trimmed offset curve of entity, %d, is too small,  (%s).",-1},
{254,0,"Can't find next intersection of offset,  (%s).",-1},
{255,0,"Composite curve picked is not planar  (%s).",-1},
{256,0,"Error, curve and picked location are not planar,  (%s).",-1},
{257,0,"Error, can't project pick onto working plane,  (%s).",-1},
{258,0,"Can't get point beyond intersection of offset subcurves (%s).",-1},
{259,0,"Can't get point prior to intersection of offset subcurves (%s).",-1},
{260,0,"Solids not available in this version",-1},
{261,5,"Illegal relation of %d,  (%s).",-1},
{262,0,"Can't offset curve with given direction and distance (%s).",-1},
{263,0,"Can't offset picked subcurve, reset minimum offset distance,  (%s).",-1},
{264,0,"Can't determine offset direction for subcurve of composite (%s).",-1},
{265,0,"No intersections found from which to construct offset.",-1},
{266,0,"Warning there may be large numerical inaccuracies in the offset, (%s).",-1},
{267,0,"Warning, an apparently small offset curve may not be included in the offset.",-1},
{268,0,"Can't trim the end of the curve which has key:%d, (%s).",-1},
{269,0,"Can't determine the 'in' and 'out' curves at an intersection, (%s).",-1},
{270,0,"Lift direction must be perpendicular to plane of composite.",-1},
{271,0,"Cannot load a displayed view.",-1},
{272,0,"Grid space is too small. Please redefine it.",-1},
{273,5,"Out of digs segment space.  ",212},
{274,0,"Delta angle would result in closed circle, not an arc.",-1},
{275,0,"Point not on curve.",-1},
{276,0,"Can't place 'drawing view' on a drawing.",-1},
{277,0,"You must pick a line/curve. Retry.",-1},
{278,0,"Point and the curve are not on the same plane.",-1},
{279,0,"Can't create %s from given constraints.",-1},
{280,0,"Unable to calculate line tangent to curve through point.",-1},
{281,0,"Unable to calculate line tangent to these two curves.",-1},
{282,0,"Unable to trim this curve.",-1},
{283,5,"Can't clear save/load bit map.",-1},
{284,0,"Polygon picked is not planar.",-1},
{285,0,"Polygon picked is not in work plane axes x-y plane.",-1},
{286,0,"Composite curve picked is not planar.",-1},
{287,0,"Composite curve picked is not in work plane axes x-y plane.",-1},
{288,0,"Composite curve picked is not closed.",-1},
{289,0,"Unsupported curve for 2-D analysis in composite curve.",-1},
{290,0,"Outer radius is smaller than the inner radius.",-1},
{291,0,"Outer radius is almost the same as the inner radius.",213},
{292,0,"Error: can't selectively save a drawing.",-1},
{293,0,"Error occurred in attempting to selectively save solids.",-1},
{294,0,"Error occurred in attempting to selectively save splines and/or surfaces.",-1},
{295,0,"Can't generate profile for %s(s).",-1},
{296,0,"The solids do not fit.",-1},
{297,0,"The solids DO NOT FIT at the indicated points.",-1},
{298,0,"Error: The curves picked must constitute a simple closed curve.",-1},
{299,0,"Error: Entity picked is a solid but not on the cross section.",-1},
{300,0,"Error: Edge picked is not on the first cross section body picked.",-1},
{301,0,"Can't get 2 unique points, one on 1st cross section, one on it's offset.",-1},
{302,0,"The solids do not fit; the original solid cross sections intersect.",-1},
{303,0,"The solids do not fit; the 1st cross section intersects the 2nd offset.",-1},
{304,0,"Tolerance value is too small; must be greater than %g.",-1},
{305,0,"The solids do not fit; the 1st offset intersects the 2nd cross section.",-1},
{306,0,"The solids do not fit; the 1st offset intersects the 2nd offset.",-1},
{307,0,"The solids do NOT fit.",-1},
{308,0,"The solids fit.",-1},
{309,0,"Cross section edge is neither a line nor a circle.",-1},
{310,0,"Can't make closed curve out of picked cross section.",-1},
{311,0,"Too many edges in body.",-1},
{312,0,"Error: same solid picked twice.",-1},
{313,0,"Can't %s solid body.",-1},
{314,0,"Error: length of plane must greater than %g.",-1},
{315,0,"Error: Subscripted geometry name not allowed.",-1},
{316,0,"Error: Selected name is in use and REDEFINE is NO.",-1},
{317,0,"Error: Illegal geometry picked (WIREFRAME only).",-1},
{318,0,"Error: Illegal geometry picked (Same Type geometry only).",-1},
{319,0,"Error: There is no drawing in the data base.",-1},
{320,0,"Error: Empty drawing name is not allowed.",-1},
{321,0,"Error: Must pick a shape.",-1},
{322,0,"Error: Unable to find command.  Please select from the list.",-1},
{323,0,"Entity picked is not appropriate for the circle construction.  ",214},
{324,0,"Same entity picked twice.  You must pick different entities.  ",-1},
{325,0,"Circle through a point tangent to a curve is not implemented.",-1},
{326,0,"Circle tangent to two curves is not implemented.",-1},
{327,0,"Cannot intersect a circle and a plane, please try again.",-1},
{328,0,"Entity picked was not a curve/surface. You must pick a curve/surface.",-1},
{1,-1,"inverted",-1},
{2,-1,"planar",-1},
{3,-1,"open  ",-1},
{4,-1,"order",-1},
{5,-1,"control points",-1},
{6,-1,"point on rotation axis:",-1},
{7,-1,"direction vector of axis:",-1},
{8,-1,"start angle for surface",-1},
{9,-1,"terminating angle for surface",-1},
{10,-1,"distance and direction for lift:",-1},
{11,-1,"distance (from picked line):",-1},
{12,-1,"distance (along first line):",215},
{13,-1,"angle:",216},
{14,-1,"Arc/circle center:",217},
{15,-1,"radius of circle:",218},
{-1},
{17,-1,"point on circumference:",219},
{-1},
{19,-1,"first point on circle/arc:",220},
{20,-1,"second point on circle/arc:",221},
{21,-1,"third point on circle/arc:",222},
{22,-1,"arc center:",223},
{23,-1,"first endpoint of arc:",224},
{24,-1,"approximate endpoint of arc (to define arc angle):",225},
{25,-1,"arc/circle radius:",226},
{26,-1,"pick entity:",-1},
{27,-1,"pick curves to merge: ",-1},
{28,-1,"axis length:",-1},
{29,-1,"start point of new line:",227},
{30,-1,"endpoint of new line:",228},
{31,-1,"point on line:",-1},
{32,-1,"point new line goes through:",-1},
{33,-1,"length of new line:",229},
{34,-1,"approximate endpoint:",-1},
{35,-1,"coordinates of point:",230},
{36,-1,"starting point:",-1},
{37,-1,"<delta x, delta y, delta z>",-1},
{38,-1,"number of points in pattern:",-1},
{-1},
{-1},
{41,-1,"angle between points:",231},
{-1},
{43,-1,"working plane z-depth:",-1},
{44,-1,"working plane base point:",-1},
{45,-1,"working plane z axis:",-1},
{46,-1,"working plane y axis: ",-1},
{47,-1,"rotation vector:",-1},
{48,-1,"rotation angle:",-1},
{49,-1,"direction vector: ",-1},
{50,-1,"MODEL#50; enter ROMULUS command: ",232},
{51,-1,"corner point of block:",-1},
{52,-1,"block dimensions <x,y,z>: ",-1},
{-1},
{54,-1,"radius of cylinder: ",-1},
{55,-1,"start point of cylinder axis:",-1},
{56,-1,"distance and direction of cylinder axis:",-1},
{57,-1,"radius of sphere: ",-1},
{58,-1,"center of sphere:",-1},
{59,-1,"radius of circle 1: ",-1},
{60,-1,"center of circle 1:",-1},
{61,-1,"radius of circle 2: ",-1},
{62,-1,"center of circle 2:",-1},
{63,-1,"chamfer distance:",-1},
{64,-1,"enter fillet radius:",-1},
{65,-1,"pick connected edges in sequence: ",-1},
{66,-1,"coordinates of light source: ",-1},
{67,-1,"file name: ",-1},
{68,-1,"pick entities to delete:",-1},
{69,-1,"MODEL#69; enter mti.  [More info in HELP.]",233},
{70,-1,"MODEL#70; display mti.  [More info in HELP.]",234},
{71,-1,"pick entities to translate:",-1},
{72,-1,"from point: ",-1},
{73,-1,"to point: ",-1},
{74,-1,"pick entities to rotate: ",-1},
{75,-1,"rotation axis point: ",-1},
{76,-1,"axis direction vector: ",-1},
{77,-1,"angle of rotation",-1},
{78,-1,"pick entities to copy/translate:",-1},
{79,-1,"pick entities to copy/rotate:",-1},
{80,-1,"rotation direction vector: ",-1},
{81,-1,"Pick intersection of trim entity and trim to element",-1},
{82,-1,"window center:",-1},
{83,-1,"Indicate lower left corner",-1},
{84,-1,"Indicate upper right corner",-1},
{85,-1,"magnification factor: (smaller than 1 displays more of part - zooms out)",-1},
{86,-1,"view reference point:",-1},
{87,-1,"view normal vector:",-1},
{88,-1,"Continue?",-1},
{89,-1,"Enter control point:",-1},
{90,-1,"Should control/interpolating points be on the curve?",-1},
{91,-1,"Pick a solid.",-1},
{92,-1,"point on rotation axis:",-1},
{93,-1,"axis direction vector:",-1},
{94,-1,"point lying on u and v axis:",-1},
{95,-1,"u axis direction vector:",-1},
{96,-1,"v axis direction vector:",-1},
{97,-1,"Is this the desired direction for ruling?",-1},
{98,-1,"Pick entities to mirror:",-1},
{99,-1,"Mirror plane normal:",-1},
{100,-1,"Point on mirror plane:",-1},
{101,-1,"Pick entities to scale:",-1},
{102,-1,"Point for scale origin:",-1},
{103,-1,"Scale factor:",-1},
{104,-1,"Pick point to modify:",-1},
{105,-1,"New point coordinates:",-1},
{106,-1,"New endpoint of line:",-1},
{107,-1,"Pick circle to be modified:",-1},
{108,-1,"New center point:",-1},
{109,-1,"New radius:",-1},
{110,-1,"New endpoint of arc:",-1},
{111,-1,"Pick arc to be closed:",-1},
{112,-1,"New angle for circle:",-1},
{113,-1,"Enter a vector to be rotated:",-1},
{114,-1,"Enter direction of vector after rotation:",-1},
{115,-1,"Enter rotation axis direction vector:",-1},
{116,-1,"ROMULUS failure; attempting to recover; press any key to continue.",-1},
{117,-1,"Enter interpolation point:",-1},
{118,-1,"Location of text:",-1},
{119,-1,"Text string:",-1},
{120,-1,"Font name:",-1},
{121,-1,"Character expansion:",-1},
{122,-1,"Character height:",-1},
{123,-1,"Character spacing:",-1},
{124,-1,"Character rotate angle:",-1},
{125,-1,"Planar hatching pitch value:",-1},
{126,-1,"Pick first solid for interference check.",-1},
{127,-1,"Pick second solid for interference check.",-1},
{128,-1,"Combine this solid",-1},
{129,-1,"With this solid.",-1},
{130,-1,"Pick first solid body",-1},
{131,-1,"Pick second solid body.",-1},
{132,-1,"Pick composite curve to split.",-1},
{133,-1,"Pick first constituent of second composite.",-1},
{134,-1,"Pick composite curve to dissolve.",-1},
{135,-1,"Pick curve to rotate.",-1},
{136,-1,"Pick curve to lift into a surface.",-1},
{137,-1,"Pick a line.",-1},
{138,-1,"Pick first entity to intersect.",-1},
{139,-1,"Pick one or more entities to intersect with first.",-1},
{140,-1,"Pick first surface.",-1},
{141,-1,"Pick second surface.",-1},
{142,-1,"Unite this solid",-1},
{143,-1,"to this solid.",-1},
{144,-1,"Pick solid.",-1},
{145,-1,"Subtract this solid",-1},
{146,-1,"from this solid.",-1},
{147,-1,"Pick a connected sequence of edges.",-1},
{148,-1,"Pick a composite curve.",-1},
{149,-1,"Pick solid to cross section.",-1},
{150,-1,"Pick the first surface edge.",-1},
{151,-1,"Pick the direction to chain surface edges.",-1},
{152,-1,"Pick the last surface edge.",-1},
{153,-1,"Pick entity.",-1},
{154,-1,"Pick first line.",-1},
{155,-1,"Pick second line.",-1},
{156,-1,"Pick tangent line/circle.",-1},
{157,-1,"Indicate first curve.",-1},
{158,-1,"Indicate second curve.",-1},
{159,-1,"Pick a curve.",-1},
{160,-1,"Pick first curve.",-1},
{161,-1,"Pick line near end to be moved.",-1},
{162,-1,"Pick circle at point to be broken.",-1},
{163,-1,"Pick arc near end to be moved.",-1},
{164,-1,"Pick entity to trim/extend on side to be trimmed/extended",-1},
{165,-1,"Pick element to trim/extend to",-1},
{166,-1,"Pick part of entity to keep ",-1},
{167,-1,"Pick first element to trim to",-1},
{168,-1,"Pick second element to trim to",-1},
{169,-1,"Pick first curve: ruling direction starts at nearest endpoint.",-1},
{170,-1,"Pick second curve: ruling direction starts at nearest endpoint.",-1},
{171,-1,"Number of u paths.",-1},
{172,-1,"Number of v paths.",-1},
{173,-1,"Number of points per u path.",-1},
{174,-1,"Number of points per v path.",-1},
{175,-1,"Enter new knot value.",-1},
{176,-1,"Desired multiplicity.",-1},
{177,-1,"Origin of cross section plane.",-1},
{178,-1,"Normal vector of cross section plane.",-1},
{179,-1,"X length of plane.",-1},
{180,-1,"Y length of plane.",-1},
{181,-1,"Enter point defining plane.",-1},
{182,-1,"Enter normal to plane.",-1},
{183,-1,"Enter radius of cylinder.",-1},
{184,-1,"Enter point on cylinder axis.",-1},
{185,-1,"Enter cylinder axis vector.",-1},
{186,-1,"Enter resolution.",-1},
{187,-1,"Drawing error (%g of radius)",-1},
{188,-1,"Pick second curve.",-1},
{189,-1,"Font arc drawing error (%g of radius):",-1},
{190,-1,"Pick circles to have their centers changed.",-1},
{191,-1,"Approximate center point",-1},
{192,-1,"New location of polygon vertex",-1},
{193,-1,"Pick a filled polygon region near vertex to be changed",-1},
{194,-1,"Vertices of polygon",-1},
{195,-1,"Enter base vector",-1},
{196,-1,"Pick viewport",-1},
{197,-1,"Two focus points",-1},
{198,-1,"Point on ellipse",-1},
{199,-1,"Center of ellipse",-1},
{200,-1,"Length of semi-minor axis",-1},
{201,-1,"Center (intersection of asymptotes) of hyperbola",-1},
{202,-1,"Length of transverse axis of hyperbola",-1},
{203,-1,"Length of conjugate axis of hyperbola",-1},
{204,-1,"Focal point of parabola",-1},
{205,-1,"Range out major axis",-1},
{206,-1,"Conic not in working plane.  Supply another point in plane",-1},
{207,-1,"Six coefficients for quadratic equation",-1},
{208,-1,"Pick an ellipse",-1},
{209,-1,"Total length of major axis",-1},
{210,-1,"Total length of minor axis",-1},
{211,-1,"Pick a conic curve",-1},
{212,-1,"Pick a hyperbola",-1},
{213,-1,"Pick a parabola",-1},
{214,-1,"Curvature at vertex",-1},
{215,-1,"Point to be on conic",-1},
{216,-1,"Tangent vector at this point (OPTIONAL)",-1},
{217,-1,"Pick first endpoint",-1},
{218,-1,"Pick first endpoint (OPTIONAL)",-1},
{219,-1,"Pick second endpoint",-1},
{220,-1,"Pick starting entity",-1},
{221,-1,"Pick terminating entity",-1},
{222,-1,"Entities for test",-1},
{223,-1,"Pick entities",-1},
{224,-1,"Pick entities to make invisible",-1},
{225,-1,"Pen number",-1},
{226,-1,"Enter view name",-1},
{227,-1,"Enter drawing name",-1},
{228,-1,"Enter new drawing name",-1},
{229,-1,"enter drawing to rename",-1},
{230,-1,"enter drawing to be archived",-1},
{231,-1,"enter archival name",-1},
{232,-1,"enter drawing to be retrieved",-1},
{233,-1,"enter drawing to delete",-1},
{234,-1,"Layer number",-1},
{235,-1,"Layer name",-1},
{236,-1,"Indicate lower left of area",-1},
{237,-1,"Indicate upper right of area",-1},
{238,-1,"Plot area border?",-1},
{239,-1,"enter logical pen number for border",-1},
{240,-1,"select area for view placement",-1},
{241,-1,"select area to delete",-1},
{242,-1,"procede with area delete?",-1},
{243,-1,"indicate area to align",-1},
{244,-1,"indicate base point",-1},
{245,-1,"indicate new base point",-1},
{246,-1,"View name",-1},
{247,-1,"Parallel to which vector?",-1},
{248,-1,"Indicate base point",-1},
{249,-1,"Enter offset delta",-1},
{250,-1,"Diametrically opposite point",-1},
{251,-1,"Indicate curve to lift",-1},
{252,-1,"Number of layer to name",-1},
{253,-1,"Offset layer numbers by what value",-1},
{254,-1,"Length of semi-major axis",-1},
{255,-1,"Orientation angle",-1},
{256,-1,"Pick first tangent line",-1},
{257,-1,"Pick second tangent line",-1},
{258,-1,"Pick third tangent line",-1},
{259,-1,"Pick entities to blank",-1},
{260,-1,"working plane x axis",-1},
{261,-1,"Enter number of translation copies of each entity",-1},
{262,-1,"Enter number of rotation copies of each entity",-1},
{263,-1,"Point on drawing to place view center",-1},
{264,-1,"Pick a connected sequence of lines",-1},
{265,-1,"Pick a connected sequence of lines/arcs",-1},
{266,-1,"Pick next line/arc",-1},
{267,-1,"Pick splitting curve",-1},
{268,-1,"Pick curve to split",-1},
{269,-1,"Pick line/circle.",-1},
{270,-1,"Pick entity to modify",-1},
{271,-1,"Can't dynamically allocate storage for form (%s). ",-1},
{272,-1,"Warning, form truncated to fit in the graphics area (%s).",-1},
{273,-1,"Error in loading form (%s).",-1},
{274,-1,"Form data fill error (%s).",-1},
{275,-1,"Form presentation error (%s).",-1},
{276,-1,"Form activate error (%s).",-1},
{277,-1,"Unknown form error (%s).",-1},
{278,-1,"start angle:",235},
{279,-1,"end angle:",236},
{280,-1,"Enter distance to offset.",-1},
{281,-1,"Pick planar curve to offset; side picked on is offset side.",-1},
{282,-1,"Pick solid to tweak:",-1},
{283,-1,"Coordinate of point to move:",-1},
{284,-1,"New coordinate of point:",-1},
{285,-1,"Enter number of scaled copies of each entity",-1},
{286,-1,"Render solids for drawing (Hidden lines on) ?",-1},
{287,-1,"Pick polygons or composite curves for positive areas",-1},
{288,-1,"Pick polygons or composite curves for negative areas",-1},
{289,-1,"Pick polygon to calculate perimeter",-1},
{290,-1,"Pick curve near new point",-1},
{291,-1,"Pick curve that line is to be tangent to",-1},
{292,-1,"Pick point on curve that tangent line starts at",-1},
{293,-1,"Pick curve that line is to be normal to",-1},
{294,-1,"Pick point on curve that normal line starts at",-1},
{295,-1,"Distance to extend line",-1},
{296,-1,"Projection point used to extend line",-1},
{297,-1,"Enter a point:",-1},
{298,-1,"Pick a line/curve:",-1},
{299,-1,"Pick a curve:",-1},
{300,-1,"Pick entities to include in group",-1},
{301,-1,"Pick group to dissolve",-1},
{302,-1,"Pick an entity to calculate the minimum distance.",-1},
{303,-1,"Pick an entity to calculate the perpendicular distance.",-1},
{304,-1,"Do you really want to delete all displayable entities?",-1},
{305,-1,"Enter center of torus.",-1},
{306,-1,"Enter axis of torus.",-1},
{307,-1,"Enter major radius of torus.",-1},
{308,-1,"Enter minor radius of torus.",-1},
{309,-1,"Enter corner point.",-1},
{310,-1,"Enter first point of parabola.",-1},
{311,-1,"Enter second point of parabola.",-1},
{312,-1,"Enter third point of parabola.",-1},
{313,-1,"Enter fourth point of parabola.",-1},
{314,-1,"Enter focus of parabola.",-1},
{315,-1,"Enter vertex of parabola.",-1},
{316,-1,"Pick a curve to trim conic to.",-1},
{317,-1,"Enter tangent vector at point.",-1},
{318,-1,"Pick entity to project onto plane.",-1},
{319,-1,"Center of inscribed circle.",-1},
{320,-1,"Number of sides of polygon.",-1},
{321,-1,"Define plane to project entities onto.",-1},
{322,-1,"Pick entity to Rename",-1},
{323,-1,"Enter U arc length parameter on curve",-1},
{324,-1,"Enter U arc length parameter on surface",-1},
{325,-1,"Enter V arc length parameter on surface",-1},
{326,-1,"Pick curve to evaluate.",-1},
{327,-1,"Pick surface to evaluate.",-1},
{328,-1,"Pick entities to save.",-1},
{329,-1,"Pick entities to make editable.",-1},
{330,-1,"Pick entities to make non-editable.",-1},
{331,-1,"Enter selective save file name: ",-1},
{332,-1,"Enter outer radius of tube",-1},
{333,-1,"Enter inner radius of tube",-1},
{334,-1,"Enter starting point of tube",-1},
{335,-1,"Enter distance and direction of tube axis",-1},
{336,-1,"Pick first body to cross section.",-1},
{337,-1,"Pick closed curve of x-section (pick on tolerance side).",-1},
{-1},
{339,-1,"Press any key to continue.",-1},
{340,-1,"Pick second body to cross section.",-1},
{341,-1,"Model system origin:",-1},
{342,-1,"Model system x axis:",-1},
{343,-1,"Model system y axis:",-1},
{344,-1,"Model system z axis:",-1},
{345,-1,"Pick entity to redefine.",-1},
{346,-1,"Pick entities to unblank:",-1},
{347,-1,"Pick entities to get:",-1},
{348,-1,"Pick entities to put:",-1},
{349,-1,"Pick entities to redisplay:",-1},
{1,0,"Cannot save; empty part. [See Help]",237},
{2,0,"File could not be replaced; save with a different name.  [More info in HELP.]",238},
{3,0,"UB#3; cannot open specified save file",-1},
{4,0,"Cannot allocate necessary memory. [See Help]",239},
{5,0,"UB#5; error saving variable list [See Help]",240},
{6,0,"UB#6; error writing to file [See Help]",241},
{7,0,"Cannot open load file.",-1},
{8,0,"Cannot load incompatible file.",-1},
{9,0,"UB#9; illegal geometry relation in mtid [See Help]",242},
{10,0,"UB#10; error reading mtid relations [See Help]",243},
{11,0,"UB#11; unable to allocate necessary memory [See Help]",244},
{12,0,"UB#12; encountered illegal relation control block [See Help]",245},
{13,0,"UB#13; error reading atom in variable list [See Help]",246},
{14,0,"UB#14; cannot load non-existent variable list.  [More info in HELP.]",247},
{15,0,"UB#15; illegal mtid relation found in geometry",-1},
{16,0,"UB#16; error reading mtid relations",-1},
{17,0,"UB#17; RDBMS not available.",-1},
{18,0,"UB#18; relation not found.  [More info in HELP.]",248},
{19,0,"UB#19; No such part.",-1},
{20,0,"UB#20; Database error in transcription.  [More info in HELP.]",249},
{21,0,"Autosave occurring.   [More info in HELP.]",250},
{22,0,"Answer must begin with t or n.",-1},
{23,0,"Part contains no environment info.",-1},
{24,0,"Unable to open autosave file.",-1},
{25,0,"Data dictionary file is illegal or damaged.",-1},
{26,0,"Cannot load partial part file, must merge data. ",-1},
{27,0,"Error, Geometry label %s already exists, Unibase did not merge.",-1},
{28,0,"Answer must begin with a or b.",-1},
{1,-1,"Enter Unibase filename to save:",-1},
{2,-1,"Overwrite existing part?",-1},
{3,-1,"Remove existing geometry?",-1},
{4,-1,"Enter load filename:",-1},
{5,-1,"Enter part name:",-1},
{6,-1,"Delete existing part?",-1},
{7,-1,"Autosave by Time or Number of Unibase changes? (t/n)",-1},
{8,-1,"Enter time interval in minutes:",-1},
{9,-1,"Enter number of Unibase changes:",-1},
{10,-1,"Destroys existing work. Are you sure?",-1},
{11,-1,"Save existing Unibase?",-1},
{12,-1,"Enter merge filename:",-1},
{1,1,"Cannot write to filename; enter new name. ",-1},
{1,-1,"Enter light source direction vector:",-1},
{2,-1,"Enter polygon output file name: ",-1},
{3,-1,"File exists; overwrite?",-1},
{4,-1,"Pick surfaces to render:",-1},
{5,-1,"Press any key to return to NCL.",-1},
{6,-1,"Pick rendering(s) to convert to wireframe.",-1},
{7,-1,"File does not exist; enter new file name.",-1},
{8,-1,"Escape NCL for maximum rendering?",-1},
{1,1,"File %s wasn't successfully opened for reading",-1},
{2,1,"File %s doesn't have %s access from ux_access0",-1},
{3,1,"Operation %s isn't implemented for modenv operations",-1},
{4,1,"Bad syntax in file: no '='",-1},
{5,1,"Can't find symbol name %s in table to replace it",-1},
{6,1,"Environmental variable %s was not found",-1},
{7,1,"Unable to expand identifier %s in ux_expand_string",-1},
{8,1,"Unable to write block to STREAM file",-1},
{9,1,"Unable to read a block from STREAM file",-1},
{10,1,"Unable to read file that isn't STREAM file",-1},
{11,1,"Unable to write to file that isn't STREAM file",-1},
{12,1,"Unable to get the extra header information",-1},
{13,1,"Error in VAX pathname to filearea syntax",-1},
{14,1,"Error, %s already exists (%s)",-1},
{15,1,"Error loading file %s into UNIBASE (%s)",-1},
{16,1,"Error is: File %s doesn't exist and can't create it (%s)",-1},
{17,1,"Error -- file %s does not exist or is not xio compatible (%s)",-1},
{18,1,"File %s has a UNIBASE file header",-1},
{19,1,"File %s has no UNIBASE file header",-1},
{20,1,"No file area or archive found at %s (%s)",-1},
{21,1,"Error: %s is a file area (%s)",-1},
{22,1,"Error: File %s doesn't exist or no access to it (%s)",-1},
{23,1,"Error: Target File %s already exists (%s)",-1},
{24,1,"Error: Filename too long (> 40 characters). Enter another name. ",-1},
{25,1,"Error: Your filename has illegal characters in it. Enter another name.",-1},
{26,1,"Error: No %s library was found at %s. (%s)",-1},
{27,1,"Error: Error in the pathname to the %s library. (%s)",-1},
{28,1,"Error: The %s archive area, %s, is not a directory. (%s)",251},
{29,1,"Error: The library name %s is not legal. (%s)",-1},
{30,1,"Can't remove %s archive, %s,  (%s) not empty.  [More info in HELP.]",252},
{31,1,"Error, can't determine if library, %s, is in area, %s, (%s).",-1},
{32,1,"Can't create %s library; %s already exists (%s).",-1},
{33,0,"Archive path, %s, is out of user local area,  (%s). ",-1},
{1,-1,"Hit any key to continue",-1},
{2,-1,"Desired file not found or unreadable in local area; search system area?",-1},
{3,-1,"Ignore this file and continue loading other files?",-1},
{4,-1,"Type the operating system command string.",-1},
{5,-1,"Type the filename of a file to send the results to.",-1},
{6,-1,"Send the results of the operating system command to the screen?",-1},
{7,-1,"Type the filename of the executable image to spawn.",-1},
{8,-1,"Illegal %s name, was changed to: %s, ok?",-1},
{9,-1,"Give new library name. Local area is used unless prefix \"sys:\" is given.",-1},
{10,-1,"Enter l to list local area; s to list system area.",-1},
{11,-1,"Give library name; for system area, prefix name with \"sys:\".",-1},
{-1},
{2,0,"No allowable geometric entities picked (ubu_create_symbol).",-1},
{3,2,"Error in loading form.  [More info in HELP.]",253},
{4,1,"Form data fill error.  [More info in HELP.]",254},
{5,1,"Form presentation error.  [More info in HELP.]",255},
{6,1,"Form activate error.  [More info in HELP.]",256},
{7,1,"Unknown form error.  [More info in HELP.]",257},
{8,0,"Symbol contains maximum number of text nodes allowed (%d).  [See HELP.]",258},
{9,1,"Error in UNIBASE data retrieval (ubi_get_snap_nodes).  [More info in HELP.]",259},
{10,1,"Error in attribute update (ubi_get_snap_nodes).  [More info in HELP.]",260},
{11,0,"Symbol contains maximum number of snap nodes allowed (%d).  [See HELP.]",261},
{12,1,"Error in UNIBASE symbol setup from symbol subsystem.   [More info in HELP.]",262},
{13,1,"Cannot create entity tuple for rel. %d (%s).  [See HELP.]",263},
{14,1,"Cannot store transform for entity that has key: %d.  [More info in HELP.]",264},
{15,1,"Cannot store attribute bundle for entity that has key: %d.  [See HELP.]",265},
{16,1,"Can't delete composite entity record, %d, (%s).  [More info in HELP.]",266},
{17,1,"Error in loading form (%s).  [More info in HELP.]",267},
{18,1,"Form data fill error (%s).  [More info in HELP.]",268},
{19,1,"Form presentation error for symbol instance (%s).  [More info in HELP.]",269},
{20,1,"Form activate error for symbol instance (%s).  [More info in HELP.]",270},
{21,1,"Unknown symbol instance form error (%s).  [More info in HELP.]",271},
{22,1,"Cannot retrieve data for key = %d (ubi_fix_unibase_geom_for_symbol). [See HELP.]",272},
{23,1,"Relation number is: %d.  [More info in HELP.]",273},
{24,1,"Can not retrieve master symbol data for key: %d (%s).  [More info in HELP.]",274},
{25,1,"Can not find master symbol, %s, (%s).  [More info in HELP.]",275},
{26,1,"Error on retrieve of symbol record with key=%d (ub_retrieve_sym).  [See HELP.]",276},
{27,1,"Error on retrieve of attributes, symbol key=%d (ub_get_sym_attr).  [See HELP.]",277},
{28,1,"Error on retrieve of transform, symbol key=%d (ub_get_sym_transf).  [See HELP.]",278},
{29,1,"Error, nothing in UNIBASE to save (%s).",-1},
{30,1,"Can't delete version of master sym at: %s (%s). Use another name.[See HELP.]",279},
{31,1,"Can't set flag to save master sym. (sub)entity with key = %d (%s).  [See HELP.]",280},
{32,1,"Error in archiving master symbol: %s  (%s).",-1},
{33,1,"Master symbol archive path name not found (%s).  [More info in HELP.]",281},
{34,1,"Unable to delete geometry with key=%d (%s).  [More info in HELP.]",282},
{35,1,"Unable to delete symbol instance with key=%d (%s).  [More info in HELP.]",283},
{36,1,"Unknown relation type: %d, for key = %d, (%s).  [See HELP.]",284},
{37,1,"Entity picked has relation type %d and is not a symbol instance (%s).;?",-1},
{38,1,"Unable to retrieve display segment id for key=%d, (%s).  [More info in HELP.]",285},
{39,1,"Master symbol not found in local library and no system library specified.",-1},
{40,1,"Master symbol %s not found with prefix path name: %s.",-1},
{41,1,"Can't retrieve attribute bundle for key=%d, relation number=%d.  [See HELP.]",286},
{42,1,"Can't update UNIBASE master symbol record having key = %d  (%s).  [See HELP.]",287},
{43,1,"Master symbol does not exist in %s (%s).;?library",-1},
{44,1,"No master symbol library found at %s  (%s).",-1},
{45,1,"Can't create symbol library; %s already exists (%s).",-1},
{46,1,"Can't update views for symbol instance (%s).  [More info in HELP.]",288},
{47,1,"Can't set displayability for symbol: %d, relation: %d  (%s).  [See HELP.]",289},
{48,1,"Error, symbol %s does not exist or is not compatible (%s).  [See HELP.]",290},
{49,1,"Invalid file area type: %s  (%s).  [More info in HELP.]",291},
{50,1,"Entity type %s, not allowed in symbol; master not created (%s).  [See HELP.]",292},
{51,1,"Unable to delete snap node with key=%d, (%s).",-1},
{52,1,"Unable to delete text node with key=%d, (%s).  [More info in HELP.]",-1},
{53,1,"Can't delete entity record, %d, from UNIBASE  (%s).  [More info in HELP.]",293},
{54,1,"Can't store symbol instance in UNIBASE, (%s).  [More info in HELP.]",294},
{55,1,"Can't dynamically allocate storage for forms  (%s).  [More info in HELP.]",295},
{56,1,"Illegal color number of %d, (%s).  [More info in HELP.]",296},
{57,1,"Illegal text node visibility specification of %d,  (%s).  [More info in HELP.]",297},
{58,1,"Error in path name to archive, (%s).",-1},
{59,1,"Error in symbol archive path name, symbol created but not archived. (%s).",-1},
{60,1,"Can't remove master symbol archive, %s,  (%s) not empty.  [More info in HELP.]",298},
{61,1,"Illegal text node type,  (%s).  [More info in HELP.]",299},
{62,1,"Symbol archive area, %s, is not a directory  (%s).  [More info in HELP.]",300},
{63,1,"Can't set displayability for subentity with key:%d  (%s).  [More info in HELP.]",301},
{64,1,"Error in retrieving UNIBASE index of loaded symbol  (%s).  [More info in HELP.]",302},
{65,1,"Can't set displayability for subentity with key: %d, rel.: %d (%s).  [See HELP.]",303},
{66,1,"-",-1},
{67,1,"%s name, \"%s\", is illegal  (%s).",-1},
{68,0,"Warning, form truncated to fit in the graphics area (%s).",-1},
{69,1,"Error, can't determine if library, %s, is in area, %s, (%s).",-1},
{70,1,"Master symbol not created  (%s).",-1},
{71,1,"No symbol library by this name exists (%s).",-1},
{72,1,"Bad text node visibility number, %d,  (%s).",-1},
{73,0,"Too many master symbols. You selected %d; the maximum is %d (%s). [See HELP.]",304},
{74,0,"%s symbol area, %s, doesn't exist (%s).",-1},
{75,0,"Error in updating application list, %d, for key, %d,  (%s).",-1},
{76,4,"Can't unload master symbol, %s; current library version not loaded (%s).",-1},
{77,5,"Can't clear save/load bit map (%s).",-1},
{78,2,"Can't get version of master file, %s, at %s; no auto load done.",-1},
{79,2,"Can't retrieve list, %d, for entity with key, %d,  (%s).",-1},
{80,2,"Can't find data packet for instance with key, %d, (%s).",-1},
{81,2,"Error in deleting from list, %d, master sym with key, %d, (%s).",-1},
{82,2,"Error in updating UNIBASE list, %d, for key, %d, (%s).",-1},
{83,2,"Can't mark currently loaded master symbols (%s).",-1},
{84,0,"Can't find master symbol, %s, loaded from lib. %s, (%s).",-1},
{85,2,"Can't set displayability for instance with key, %d, (%s).",-1},
{86,2,"Too many master symbols to process during a load/merge (%s).",-1},
{87,2,"Can't transform snap node with key:%d  (%s).",-1},
{88,1,"Can't find master symbol, %s, (%s).",-1},
{89,2,"Test of uu_uerror3, %d, %d, (%s).",-1},
{90,2,"Error in updating UNIBASE symbol record with key:%d,  (%s).",-1},
{91,2,"Symbol library path, %s, doesn't exist (%s).",-1},
{92,5,"Data base maybe inconsistent; sign off and start over. (%s).",-1},
{93,5,"Failure on UNIBASE deletion from list: %d of key: %d  (%s).",-1},
{94,0,"Mandatory symbol UB_UPDATE_LOAD_SYMS not defined, (%s).",-1},
{95,0,"Can't update position of text entity %d in old instance %d (%s).",-1},
{96,0,"Symbol %s name, %s, is illegal, (%s).",-1},
{97,0,"No check for %s being current; file path not associated with %s's name.",-1},
{98,0,"Warning, rename does not change symbol file names in libraries.",-1},
{99,0,"Can't delete master, %s, instances of it exist, (%s).",-1},
{100,5,"Error in freeing dynamic storage for instance with key: %d.",-1},
{101,5,"Error in retrieving relation type for entity with key: %d  (%s).",-1},
{102,0,"Subentities of symbol instances can't be queried.",-1},
{103,0,"No snap node picked.",-1},
{104,0,"Error updating master sym, %s; data base inconsistency req'd a reset.",305},
{105,0,"Note, reloading %s from symbol lib, %s.",-1},
{106,0,"Can't rename, symbol %s is loaded in Unibase: save part, unload master. (%s)",-1},
{107,0,"Archive path, %s, is out of user local area,  (%s). ",-1},
{108,0,"Unable to create a description file for the symbol %s.",-1},
{109,0,"Use %s script file to enter the description.",-1},
{110,0,"Unable to create the script file for entering a symbol description. ",-1},
{111,0,"Unable to create a modify script for the description file: symbol %s.",-1},
{112,0,"Use %s script file to modify the description.",-1},
{113,0,"Unable to show a description for symbol %s.",-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{120,1,"No standard parts library found at %s  (%s).",-1},
{121,1,"No standard parts library by this name exists (%s).",-1},
{122,1,"The system can't delete the standard part in the library: %s (%s).",-1},
{123,1,"Can't set flag to save standard part (sub)entity with key = %d (%s).",306},
{124,1,"Standard part does not exist in %s (%s) library.",-1},
{1,-1,"Enter symbol name",-1},
{2,-1,"Enter symbol origin",-1},
{3,-1,"Pick symbol geometry",-1},
{4,-1,"Enter next snap node.",-1},
{5,-1,"Enter text.",-1},
{6,-1,"Continue entering text nodes? If so, current node must be redone.",-1},
{7,-1,"Enter angle of symbol label from working plane x-axis.",-1},
{8,-1,"Is text to be a prompt?",-1},
{9,-1,"Non-geometric or text entity in UNIBASE, delete it and continue?.",-1},
{10,-1,"Enter location of next snap node.",-1},
{11,-1,"Enter location of instance origin.",-1},
{12,-1,"Master symbol exists; replace old version?.",-1},
{13,-1,"Pick instance to be decomposed.",-1},
{14,-1,"Enter new name of master symbol.",-1},
{15,-1,"Give symbol lib. name; for system area, prefix name with \"sys:\".",-1},
{16,-1,"Pick entities to be in master symbol.",-1},
{17,-1,"Symbol not found or unreadable in local area; search system area?",-1},
{18,-1,"Symbol created but not archived; enter a previously created lib.",-1},
{19,-1,"Enter new library name.",-1},
{20,-1,"Ignore this file and continue loading master symbols? ",-1},
{21,-1,"Master symbol, %s, is already in the data base; overwrite?",-1},
{22,-1,"Pick entities to be put in master symbol.",-1},
{23,-1,"Enter text node position.",-1},
{24,-1,"Change color of: geometry (g); snap nodes (s); text (t); all (a).",-1},
{25,-1,"Pick symbol instances to change visibility.",-1},
{26,-1,"Make visible: snap nodes (s); text: all (t), graphic only (g); all (a).",-1},
{27,-1,"Number of copies to be created is %d, ok?",-1},
{28,-1,"Enter l to list local symbol area; s to list system area.",-1},
{29,-1,"Ok to create default symbol library, \"symlib\" in local area?",-1},
{30,-1,"Illegal %s name, was changed to: %s, ok?",-1},
{31,-1,"%s already exists; overwrite?.",-1},
{32,-1,"Hit any key to continue.",-1},
{33,-1,"Reloading, %s, from lib, will regenerated instances use current text node data?",-1},
{34,-1,"Redo this instance? Otherwise, required text will be \"***\" in this instance.",-1},
{35,-1,"Updating instances of %s; shall current text nodes be reused?",-1},
{36,-1,"Currently loaded master, %s, is from a different library, overwrite?",-1},
{37,-1,"Enter symbol library name:",-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{100,-1,"Pick snap node on symbol to start new connector",-1},
{101,-1,"Pick snap node on symbol to end new connector",-1},
{102,-1,"Enter intermediate coodinates of new connector",-1},
{103,-1,"Enter text of new connector",-1},
{104,-1,"To remove master, %s, from UNIBASE all instances of it will be removed; ok?",-1},
{105,-1,"Give new symbol lib. name; for system area, prefix name with \"sys:\".",-1},
{106,-1,"Enter new symbol file name:",-1},
{107,-1,"Must reload master sym, %s; give lib (for sys area, prefix \"sys\")",-1},
{108,-1,"Can't construct library path to master, %s; enter lib name.",-1},
{109,-1,"Master symbol file for %s not readable at %s; reenter lib.",-1},
{110,-1,"No access to get status of master, %s, at %s; enter new lib.",-1},
{111,-1,"%s doesn't correspond to a library; enter new lib.",-1},
{112,-1,"Do you want to use the master in the part file regardless of lib path? ",-1},
{113,-1,"Do you want to use the master in Unibase prior to load?",-1},
{114,-1,"Do you want the text node data of current instances of %s inherited?",-1},
{115,-1,"Use currently loaded master version without updating?",-1},
{116,-1,"2 masters named %s loaded; use newest loaded?",-1},
{117,-1,"Target symbol filename %s already exists. Continue, overwriting the file?",-1},
{118,-1,"Do you want to create a description for the symbol %s? ",-1},
{119,-1,"Symbol %s from diff. lib. or int.rename already in Unibase, overwrite it? ",-1},
{120,-1,"Give standard part lib. name; for system area, prefix name with \"sys:\".",-1},
{121,-1,"Enter standard part name",-1},
{122,-1,"Enter location of the part copy's origin.",-1},
{123,-1,"Enter new standard part file name:",-1},
{124,-1,"Pick entities to be put in standard part.",-1},
{125,-1,"Target standard part filename %s already exists. Continue, overwriting the file?",-1},
{126,-1,"Ok to create default standard part library, \"splib\" in local area?",-1},
{127,-1,"Standard Part of this name already exists; replace it?.",-1},
{1,0,"Setup file %s doesn't exist or contains error information. [More info in HELP.]",307},
{2,0,"Pen table %s doesn't exist or contains error information.",308},
{3,0,"Illegal paper size.  [More info in HELP.]",309},
{4,0,"Null drawing name !!",-1},
{5,0,"Null setup file name !!",-1},
{6,0,"Construction plane not parallel to the view selected for Quickplot.  [See HELP.]",310},
{7,0,"Can't open file %s",-1},
{8,0,"Illegal input",-1},
{9,0,"Item number out of range",-1},
{10,0,"Grid is not on on the picked view port",-1},
{11,0,"Drawing '%s' does not exist  [More info in HELP.]",311},
{12,0,"%s",-1},
{13,0,"Null plotfile name",-1},
{14,0,"Grid spacing is smaller that 1/20 of aperture.  [More info in HELP.]",312},
{15,1,"Can't dynamically allocate storage for form  (%s).  [More info in HELP.]",313},
{16,1,"Error in loading form  (%s).   [More info in HELP.]",314},
{17,1,"Form data fill error (%s).  [More info in HELP.]",315},
{18,1,"Form presentation error (%s).  [More info in HELP.]",316},
{19,1,"Form activate error (%s).  [More info in HELP.]",317},
{20,1,"Unknown form error (%s).  [More info in HELP.]",318},
{21,1,"Illegal toggle index of %d for %s  (%s).  [More info in HELP.]",319},
{22,0,"Warning, form truncated to fit in the graphics area (%s).",-1},
{-1},
{24,1,"Error: Unable to remove the plot file %s.",-1},
{25,1,"Error: Unable to rename the plot file %s to %s.",-1},
{26,1,"Error: Unable to copy the plot file %s to %s.",-1},
{27,1,"Error: query line too long.",-1},
{28,2,"Can't find name of entity with key: %d,  (%s).",-1},
{29,0,"Null pen table name!!",-1},
{30,0,"Can not create file %s !!",-1},
{1,-1,"Enter plotter setup file name: ",-1},
{2,-1,"Enter pen table name: ",-1},
{3,-1,"Enter drawing name: ",-1},
{4,-1,"Enter setup file name to be copied:",-1},
{5,-1,"Enter setup file name to copy to:",-1},
{6,-1,"Enter pen table name to be copied:",-1},
{7,-1,"Enter pen table name to copy to:",-1},
{8,-1,"Enter pathname to list the file:",-1},
{9,-1,"Enter filename to delete:",-1},
{10,-1,"Delete %s?",-1},
{11,-1,"Enter filename to rename:",-1},
{12,-1,"Enter newfile name:",-1},
{13,-1,"Enter filename to copy from:",-1},
{14,-1,"Enter filename to copy to:",-1},
{15,-1,"Enter plotfile name:",-1},
{16,-1,"Select entity for data display:",-1},
{17,-1,"END OF DATA, press RETURN to continue.",-1},
{18,-1,"%s picked; this entity is hierarchical; do you want a subentity?",-1},
{19,-1,"Plot an empty drawing?",-1},
{20,-1,"Plot file %s already exists - overwrite?",-1},
{1,0,"Cannot open drafting default file named %s.  [More info in HELP.]",320},
{2,0,"Defaults loaded from %s",-1},
{3,0,"Defaults saved to %s",-1},
{4,1,"WARNING -- Version of %s not current",-1},
{5,0,"%s not valid font name.  Font not changed.  [More info in HELP.]",321},
{6,0,"Wrong entity type picked.  You must pick and arc or a circle.  [See HELP.]",322},
{7,0,"First and second points picked are the same.  [More info in HELP.]",323},
{8,0,"Entity picked not a line.  [More info in HELP.]",324},
{9,0,"Selected lines are parallel.  [More info in HELP.]",325},
{10,0,"Invalid choice selection",-1},
{11,0,"More than %d boundary curves picked for crosshatching.  [More info in HELP.]",326},
{12,0,"Line pick at intersection center",-1},
{13,0,"Boundary geometry not co-planar.  [More info in HELP.]",327},
{14,0,"",-1},
{15,0,"The curves selected do not form a closed boundary.  [More info in HELP.]",328},
{16,0,"Geometry projects to zero length - invalid for this type dimension.  [See HELP.]",329},
{17,0,"Arc plane and working plane are not parallel",-1},
{18,0,"Cannot open drafting standards file -- DRAFTSTD symbol not defined",330},
{19,0,"Lines are not parallel  [More info in HELP.]",331},
{20,0,"First and second lines picked are the same  [More info in HELP.]",332},
{21,0,"No user modifiable text found in drafting entity selected.  [See HELP.]",333},
{22,0,"Wrong entity type picked.  You must pick a drafting entity.  [See HELP.]",334},
{23,0,"Cannot add tolerance to user dimension text.",-1},
{24,0,"Tolerance method not set, cannot add tolerance.",-1},
{25,0,"Appended text method not set, cannot add text.",-1},
{26,0,"Cannot fit text between extension lines, reposition.",-1},
{27,0,"Form-Positional Tolerance text not editable, delete and recreate.",-1},
{28,0,"Currently, only lines and arcs are supported by this function.",-1},
{29,0,"Exceeded internal limit for entries in this line.",-1},
{30,0,"Exceeded internal limit for characteristic entries in this line.",-1},
{31,0,"Exceeded internal limit for tolerance entries in this line.",-1},
{32,0,"Exceeded internal limit for datum entries in this line.",-1},
{33,0,"Exceeded internal limit for lines.",-1},
{34,0,"Current drafting entities inconsistent with new drafting standard. Load failed.",-1},
{35,0,"Text origin inconsistent with entity selected - try again.",-1},
{36,0,"No Alt. Act. available for this prompt.",-1},
{37,0,"Cannot open text file named %s.  ",-1},
{38,0,"Invalid line number.",-1},
{39,0,"Input exceeded character storage limit - last line ignored.",-1},
{40,0,"Wrong entity type picked for this function - try again.",-1},
{41,0,"Centerline pattern does not fit between selected entities - try again.",-1},
{42,0,"Cannot delete only line in a note.",-1},
{43,0,"Dimension not associated with geometry - cannot regenerate.",-1},
{44,0,"Wrong entity picked. You must pick a Parts List Table.",-1},
{45,0,"Limited to 3 characters for ID numbers.",-1},
{46,0,"Wrong entity picked. You must pick a BALLOON.",-1},
{47,0,"Wrong BALLON picked, or BALLOON has only one leader.",-1},
{48,0,"Can not add a multiplier to a BALLOON with more than one leader.",-1},
{49,0,"Can not add a leader to a BALLOON with a multiplier.",-1},
{50,0,"Duplicate FIND NUMBER (re-enter line).",-1},
{51,0,"Table contains duplicate FIND NUMBER entries. (not loaded).",-1},
{52,0,"Non-integer FIND NUMBER data (re-enter line).",-1},
{53,0,"Non-integer MULTIPLIER data (re-enter line).",-1},
{54,0,"Must select a PARTS LIST table entry.",-1},
{55,0,"Pick a PARTS LIST table entry from the wrong table.",-1},
{56,0,"PARTS LIST TABLES limited to 40 entries.",-1},
{57,0,"Limited to 3 characters for QUANTITY numbers.",-1},
{58,0,"Non-integer QUANTITY data (re-enter line).",-1},
{59,0,"Dimension values are not equal, cannot use REPETITIVE dimension type.",-1},
{60,0,"Repetitive type does not currently support 180 or 360 degree angles.",-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{28,-1,"Select first entity",-1},
{29,-1,"Select second entity",-1},
{30,-1,"Indicate dimension origin",-1},
{31,-1,"Select baseline/margin",-1},
{32,-1,"Select first arc or circle",-1},
{33,-1,"Select second arc or circle",-1},
{34,-1,"Select arc to dimension",-1},
{35,-1,"Indicate dimension line endpoint",-1},
{36,-1,"Enter flange angle in degrees",-1},
{37,-1,"Select first line",-1},
{38,-1,"Select second line",-1},
{39,-1,"Indicate datum origin",-1},
{40,-1,"Indicat positive quadrant",-1},
{41,-1,"Select line or margin",-1},
{42,-1,"Indicate position",-1},
{43,-1,"Indicate dogleg position",-1},
{44,-1,"Select point, line or arc",-1},
{45,-1,"Enter main dimension text",-1},
{46,-1,"Enter dual dimension text",-1},
{47,-1,"Enter dimension text",-1},
{48,-1,"Enter appended text",-1},
{49,-1,"Indicate alignment",-1},
{50,-1,"Indicate new entity origin",-1},
{51,-1,"Indicate note text location",-1},
{52,-1,"Enter note text",-1},
{53,-1,"Enter label text",-1},
{54,-1,"Indicate label text location",-1},
{55,-1,"Enter ID-symbol text",-1},
{56,-1,"Indicat ID-symbol origin",-1},
{57,-1,"Select point",-1},
{58,-1,"Select line",-1},
{59,-1,"Enter margin offset distance",-1},
{60,-1,"Indicate margin offset position",-1},
{61,-1,"Enter line spacing ratio",-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{70,-1,"Indicate new entity origin",-1},
{71,-1,"Select drafting entity",-1},
{72,-1,"Indicate alignment ",-1},
{73,-1,"Enter character size offset, Delta X",-1},
{74,-1,"Enter character size offset, Delta Y",-1},
{75,-1,"Enter offset, Delta X",-1},
{76,-1,"Enter offset, Delta Y",-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{92,-1,"Enter dim text A",-1},
{93,-1,"Enter dim text B",-1},
{94,-1,"Enter dimension text",-1},
{95,-1,"Enter appended text",-1},
{-1},
{-1},
{-1},
{-1},
{100,-1,"Select entity for Form/Positional tol",-1},
{101,-1,"Select entity for label",-1},
{102,-1,"Enter text for tolerance",-1},
{103,-1,"Enter text for symbol",-1},
{104,-1,"Enter text for datum",-1},
{105,-1,"Locate end of witness line",-1},
{106,-1,"Select arc or circle",-1},
{107,-1,"Select center line of the shaft",-1},
{108,-1,"Select shaft diameter section",-1},
{109,-1,"Indicate endpoint for arrowhead location",-1},
{110,-1,"",-1},
{111,-1,"Select cross hatch boundary geometry ",-1},
{112,-1,"Enter load file name (standard file)",-1},
{113,-1,"Enter save file name (standard file)",-1},
{114,-1,"Select arrowhead for alignment",-1},
{115,-1,"Select first entity - extend from",-1},
{116,-1,"Select second entity - dimension to",-1},
{117,-1,"Indicate dimension offset",-1},
{118,-1,"Indicate new dimension origin",-1},
{119,-1,"Indicate new dimension offset",-1},
{120,-1,"Select drafting entity for edit",-1},
{121,-1,"Enter the number of the line to be modified",-1},
{122,-1,"Enter the number of the line to be deleted",-1},
{123,-1,"Enter the line number after which text is to be added",-1},
{124,-1,"Enter the line number before which text is to be added",-1},
{125,-1,"Modify text using standard text cursor control characters",-1},
{126,-1,"Enter text for the new line",-1},
{127,-1,"Enter text file name",-1},
{128,-1,"Select baseline or datum.",-1},
{129,-1,"Reposition origin outside extension lines.",-1},
{130,-1,"Indicate points for center-line (in order).",-1},
{131,-1,"Locate center point of bolt circle.",-1},
{132,-1,"Select circles for center-line (in order).",-1},
{133,-1,"Select circles for center-line (in clockwise order).",-1},
{134,-1,"Select drafting entities for regeneration.",-1},
{135,-1,"Select notes for modification.",-1},
{136,-1,"Select dimensions for modification.",-1},
{137,-1,"Select dimension for repositioning.",-1},
{138,-1,"Select line for polar axis.",-1},
{139,-1,"Select an intersecting line to determine origin.",-1},
{140,-1,"Select polar coordinate entity.",-1},
{141,-1,"Select cross-hatch for modification.",-1},
{142,-1,"Locate Lower Left corner of the table.",-1},
{143,-1,"Enter number of lines in the table.",-1},
{144,-1,"Enter text for the current line.",-1},
{145,-1,"Select the PARTS LIST table to move.",-1},
{146,-1,"Locate new lower left corner of the table.",-1},
{147,-1,"Select Parts List Table for editting.",-1},
{148,-1,"Select Table entry for modification.",-1},
{149,-1,"Select Table for report generation.",-1},
{150,-1,"Enter report file name.",-1},
{151,-1,"Enter the location of the find number.",-1},
{152,-1,"Select table for delete entry operation.",-1},
{153,-1,"Select entry for deletion.",-1},
{154,-1,"Select table for add entry operation.",-1},
{155,-1,"Select entry after which new entry is to be placed.",-1},
{156,-1,"Report file exits. Overwrite? (y or n).",-1},
{157,-1,"Select centerlines for modification.",-1},
{158,-1,"Locate centerline start point.",-1},
{159,-1,"Locate centerline end point.",-1},
{160,-1,"Pick entity for Balloon label.",-1},
{161,-1,"Locate endpoint of Balloon leader.",-1},
{162,-1,"Select Table for Update operation.",-1},
{163,-1,"Select entries for Update operation.",-1},
{164,-1,"Select BALLOON for Edit operation.",-1},
{165,-1,"Select BALLOON arrowhead for deletion.",-1},
{166,-1,"Enter Multiplier value.",-1},
{167,-1,"Select Table for the Merge operation.",-1},
{168,-1,"Enter the multiplier value.",-1},
{169,-1,"Enter the find number.",-1},
{170,-1,"Enter row text (column entries separated by ;'s).",-1},
{171,-1,"Select next entity.",-1},
{172,-1,"Select next arc to dimension.",-1},
{1,0,"Function unavailable until after sign on",-1},
{2,0,"User is already signed on, sign off first",-1},
{3,0,"No user is currently signed on.",-1},
{4,0,"'%s' disabled in NCL system.",-1},
{5,0,"'%s' not supported on this display device.",-1},
{6,0,"'%s' not enabled while in drawing management",-1},
{7,0,"Drawing Management currently active",-1},
{8,0,"'%s' is only available in drawing management",-1},
{9,0,"'%s' disabled in UIMS System.",-1},
{10,0,"'%s' disabled while record and playback is active",-1},
{11,5,"The system has run out of dynamic memory",335},
{12,0,"'%s' disabled in DDC System.",-1},
{13,5,"The system has run out of choice devices for menus",336},
{14,5,"The system has run out of choice devices for popup menus.",337},
{15,0,"'%s' is only available when CADD is enabled.",-1},
{16,0,"'%s' is only available when CAM is enabled.",-1},
{1,-1,"Enter reference point",-1},
{2,-1,"Exit Drawing Management?",-1},
{3,-1,"- NCL - Copyright 1991-2018 Numerical Control Computer Sciences, Windows 64-bit",-1},
{1,1,"Class initialization failure - class number disparity, row = %d, class = %d",338},
{1,0,"Can't create %s from given constraints.",-1},
{2,0,"Only 2D curves may be swept into surface.",-1},
{3,0,"Unable to calcuate fillet curve.",-1},
{4,0,"%s was not created.",-1},
{5,0,"Unable to %s shells",-1},
{6,0,"Unable to project curve onto surface.",-1},
{1,-1,"Pick curve to convert to bspline.",-1},
{2,-1,"Lower left point on plane.",-1},
{3,-1,"Lower right point on plane.",-1},
{4,-1,"Upper left point on plane.",-1},
{5,-1,"Center coordinate of sphere.",-1},
{6,-1,"Radius of sphere.",-1},
{7,-1,"Top point of cylinder axis.",-1},
{8,-1,"Direction (and length) of cylinder axis.",-1},
{9,-1,"Radius of cylinder.",-1},
{10,-1,"Point defining start of surface segment.",-1},
{11,-1,"Starting angle of surface segment.",-1},
{12,-1,"Ending angle of surface segment.",-1},
{13,-1,"Top point of cone axis.",-1},
{14,-1,"Radius at top of cone.",-1},
{15,-1,"Direction (and length) of cone axis.",-1},
{16,-1,"Radius at bottom of cone.",-1},
{17,-1,"Pick curve to revolve.",-1},
{18,-1,"Point defining axis of revolution.",-1},
{19,-1,"Vector defining axis of revolution.",-1},
{20,-1,"Vector defining axis of revolution.",-1},
{21,-1,"Vector defining axis of revolution.",-1},
{22,-1,"Vector defining axis of revolution.",-1},
{23,-1,"Vector defining axis of revolution.",-1},
{24,-1,"Vector defining axis of revolution.",-1},
{25,-1,"Enter point for cubic.",-1},
{26,-1,"Direction of curve at start point.",-1},
{27,-1,"Direction of curve at end point.",-1},
{28,-1,"Radius of fillet.",-1},
{29,-1,"Pick curve near start point of fillet.",-1},
{30,-1,"Pick curve near end point of fillet.",-1},
{31,-1,"Pick first curve or point of ruled surface.",-1},
{32,-1,"Pick second curve or point of ruled surface.",-1},
{33,-1,"Pick center curve of swept surface.",-1},
{34,-1,"Pick rail curve of swept surface.",-1},
{35,-1,"Pick 2D cross-section curve of swept surface.",-1},
{36,-1,"Pick surface to reverse normal.",-1},
{37,-1,"Enter coordinate of corner of box.",-1},
{38,-1,"Enter diagonal vector of box.",-1},
{39,-1,"Enter center point of torus.",-1},
{40,-1,"Enter axial vector of torus.",-1},
{41,-1,"Enter axial radius of torus.",-1},
{42,-1,"Enter circular radius of torus.",-1},
{43,-1,"Union this shell",-1},
{44,-1,"Subtract this shell",-1},
{45,-1,"Intersect this shell",-1},
{46,-1,"with this shell",-1},
{47,-1,"from this shell",-1},
{48,-1,"Enter new tolerance value.",-1},
{49,-1,"Pick curve to offset.",-1},
{50,-1,"Enter offset distance.",-1},
{51,-1,"Pick surface to tessellate.",-1},
{52,-1,"Pick curve to reverse parameterization.",-1},
{53,-1,"Pick base curve of tabulated cylinder.",-1},
{54,-1,"Enter direction of tabulated cylinder.",-1},
{55,-1,"Pick next curve to fit surface through.",-1},
{56,-1,"Enter vector defining lift direction.",-1},
{57,-1,"Pick curve to lift into shell.",-1},
{58,-1,"Pick first curve defining sum shell.",-1},
{59,-1,"Pick second curve defining sum shell.",-1},
{60,-1,"Pick surface to project curves onto.",-1},
{61,-1,"Enter vector to project along (optional).",-1},
{62,-1,"Pick curves to project onto surface.",-1},
{63,-1,"Pick entity to extract curve.",-1},
{64,-1,"Enter parameter value.",-1},
{65,-1,"Pick closed, planar curve to define face boundary.",-1},
{66,-1,"Pick first (U0) boundary curve.",-1},
{67,-1,"Pick second (V1) boundary curve.",-1},
{68,-1,"Pick third (U1) boundary curve.",-1},
{69,-1,"Pick fourth (V0) boundary curve.",-1},
{70,-1,"Pick face to trim outer boundary.",-1},
{71,-1,"Pick trimming curve.",-1},
{72,-1,"Pick face inner boundary lies on.",-1},
{73,-1,"Pick interior curve.",-1},
{74,-1,"Pick edge curve to extract.",-1},
{75,-1,"Bottom point of cylinder axis.",-1},
{76,-1,"Bottom point of cone axis.",-1},
{77,-1,"End point of surface segment.",-1},
{78,-1,"Pick surfaces for analysis.",-1},
{79,-1,"Pick bspline to change shape.",-1},
{80,-1,"Pick control point to move.",-1},
{81,-1,"Enter coordinates to move it to.",-1},
{82,-1,"Pick curves to reparameterize.",-1},
{83,-1,"Pick curve(s) to convert to curve with one bspline. ",-1},
{84,-1,"Pick face on which to define outer boundary.",-1},
{85,-1,"Pick entity to extract face from.",-1},
{86,-1,"Pick surface(s) to convert into face(s).",-1},
{87,-1,"Pick composite face(s) to convert into simple faces.",-1},
{88,-1,"Pick first face to join.",-1},
{89,-1,"Pick second face to join.",-1},
{90,-1,"Pick closed curve known to lie on face.",-1},
{91,-1,"Enter up vector.",-1},
{92,-1,"Enter angle of twist.",-1},
{93,-1,"Constant up vector (0) or propagate up vector (1);",-1},
{94,-1,"Pick a surface of revolution.",-1},
{1,0,"'%s' not enabled while in NCLCAM",-1},
{2,0,"Must pick points.",-1},
{3,0,"error message goes here [More info in HELP.]",339},
{4,0,"Must select INVERS before selecting CONST",-1},
{5,0,"Can't open file %s",-1},
{6,0,"Picked geometry does not use a subscripted (RESERV) variable.",-1},
{7,0,"Wireframe geometry not allowed at this time.",-1},
{8,0,"Z coordinate not allowed when entering X,Y coord.",-1},
{9,0,"Incorrect length must be 1 to 6 character. ",-1},
{10,0,"Incorrect Prefix must be 1 to 20 alphabetic characters.  No blanks. ",-1},
{11,0,"Prefix must contain at least 1 alphabetic character and no blanks. ",-1},
{12,0,"Valid 2 character vocabulary words are not allowed as Prefix. ",-1},
{13,0,"Unibase file has incorrect version number. Use UNIBASE VERSION CONVERTER in NIS.",-1},
{14,0,"Unibase file is a different machine type.  Use PLATFORM CONVERTER in NIS.",-1},
{15,0,"No TRACUT is in effect.  No change to view made.",-1},
{16,0,"No REFSYS is in effect.  No change to view made.",-1},
{17,0,"Illegal geometry picked.  Picking limited to %s.",-1},
{18,0,"Post-processor commands are not allowed on last entity.",-1},
{19,0,"Nested Line is too long. Please break it up.",-1},
{1,-1,"Label of new entity",-1},
{2,-1,"X,Y coordinate of point",-1},
{3,-1,"Z coordinate",-1},
{4,-1,"Label of circle new point is on",-1},
{5,-1,"Angle (from x axis) of new point",-1},
{6,-1,"Label of circle to intersect",-1},
{7,-1,"Label of curve to intersect",-1},
{8,-1,"Label of point near intersection",-1},
{9,-1,"Point label",-1},
{10,-1,"Circle label",-1},
{11,-1,"Angle",-1},
{12,-1,"Label of point to offset",-1},
{13,-1,"X,Y,Z offset values",-1},
{14,-1,"Label of line/circle to get endpoint of",-1},
{15,-1,"Label of line to intersect",-1},
{16,-1,"Label of plane to intersect",-1},
{17,-1,"Label of circle to define center point",-1},
{18,-1,"Label of first plane to intersect",-1},
{19,-1,"Label of second plane to intersect",-1},
{20,-1,"Label of third plane to intersect",-1},
{21,-1,"Label of line to intersect",-1},
{22,-1,"Label of circle/arc to intersect",-1},
{23,-1,"Label of first circle to intersect",-1},
{24,-1,"Label of second circle to intersect",-1},
{25,-1,"Label of first line to intersect",-1},
{26,-1,"Label of second line to intersect",-1},
{27,-1,"Label of plane to intersect ",-1},
{28,-1,"Label of curve to intersect",-1},
{29,-1,"Label of point near intersection",-1},
{30,-1,"Label of first point of line",-1},
{31,-1,"Label of second point of line",-1},
{32,-1,"Label of point line is to go through",-1},
{33,-1,"Label of curve line is to be perpendicular to",-1},
{34,-1,"Label of point near point of intersection",-1},
{35,-1,"Label of point line is to go through",-1},
{36,-1,"Label of curve line is to be tangent to",-1},
{37,-1,"Label of point near point of tangency",-1},
{38,-1,"X,Y coordinate of startpoint ",-1},
{39,-1,"X,Y coordinate of endpoint ",-1},
{40,-1,"Z coordinate of startpoint ",-1},
{41,-1,"Z coordinate of endpoint ",-1},
{42,-1,"Label of first plane to intersect ",-1},
{43,-1,"Label of second plane to intersect ",-1},
{44,-1,"Label of circle new line is tangent to",-1},
{45,-1,"Angle that line is to make",-1},
{46,-1,"Label of line new line is to make angle with",-1},
{47,-1,"Label of line to offset",-1},
{48,-1,"X,Y,Z offset",-1},
{49,-1,"Label of point line is to go through",-1},
{50,-1,"Label of line new line is parallel to",-1},
{51,-1,"Label of point line is to go through",-1},
{52,-1,"Label of line or curve new line is perpendicular to",-1},
{53,-1,"Label of line new line is parallel to",-1},
{54,-1,"Distance from line",-1},
{55,-1,"Label of point line is to go through",-1},
{56,-1,"Label of circle line is tangent to",-1},
{57,-1,"Label of first circle line is tangent to",-1},
{58,-1,"Label of second circle line is tangent to",-1},
{59,-1,"Distance from axis",-1},
{60,-1,"Label of point new line is to go through",-1},
{61,-1,"Angle of new line",-1},
{62,-1,"Label of line ",-1},
{63,-1,"X,Y coordinate of point in string line",-1},
{64,-1,"Label of new point in string line",-1},
{65,-1,"X,Y coordinate of the center",-1},
{66,-1,"Radius of a circle",-1},
{67,-1,"X,Y coordinate of the center",-1},
{68,-1,"Radius of a circle",-1},
{69,-1,"Label of the center point of a circle",-1},
{70,-1,"Label of a line the new circle is tangent to",-1},
{71,-1,"Label of the center point of a circle",-1},
{72,-1,"Radius of a circle",-1},
{73,-1,"Label of the center point of a circle",-1},
{74,-1,"Label of a circumference point on a circle",-1},
{75,-1,"Label of the center point of a circle",-1},
{76,-1,"Label of a circle the new circle is tangent to ",-1},
{77,-1,"Label of the first circumference point of a circle",-1},
{78,-1,"Label of the second circumference point of a circle",-1},
{79,-1,"Radius of a circle",-1},
{80,-1,"Pick first entity (point/line/circle/curve) new circle is tangent to",-1},
{81,-1,"Pick second entity (point/line/circle/curve) new circle is tangent to",-1},
{82,-1,"Pick entity (line/circle/curve) new circle is tangent to",-1},
{-1},
{84,-1,"Radius of circle ",-1},
{85,-1,"Label of circle to offset",-1},
{86,-1,"X,Y,Z offset",-1},
{87,-1,"Label of first point on circle",-1},
{88,-1,"Label of second point on circle",-1},
{89,-1,"Label of third point on circle",-1},
{90,-1,"Label of a circumference point on a circle",-1},
{91,-1,"Label of tangent vector at first point or of another circumference point ",-1},
{92,-1,"Label of tangent vector at second point",-1},
{93,-1,"Label of line/circle new circle is tangent to",-1},
{94,-1,"Label of point on circumference of circle",-1},
{95,-1,"Radius of circle",-1},
{96,-1,"Label of first line circle is tangent to",-1},
{97,-1,"Label of second line circle is tangent to",-1},
{98,-1,"Radius of circle",-1},
{99,-1,"Label of first circle new circle is tangent to ",-1},
{100,-1,"Label of second circle new circle is tangent to ",-1},
{101,-1,"Radius of new circle",-1},
{102,-1,"Label of point new circle is to go through",-1},
{103,-1,"Label of circle new circle is tangent to",-1},
{104,-1,"Radius of new circle",-1},
{105,-1,"Label of line new circle is tangent to",-1},
{106,-1,"Label of circle new circle is tangent to",-1},
{107,-1,"Radius of new circle",-1},
{108,-1,"Label of first line circle is tangent to",-1},
{109,-1,"Label of second line circle is tangent to",-1},
{110,-1,"Label of third line circle is tangent to",-1},
{111,-1,"X,Y,Z of center point",-1},
{112,-1,"i,j,k of normal vector ",-1},
{113,-1,"Optional: I,J,K of limit plane",-1},
{114,-1,"distance of limit plane",-1},
{115,-1,"Label of center point of circle",-1},
{116,-1,"Label of normal vector of circle",-1},
{117,-1,"Radius of circle",-1},
{118,-1,"Label of limit plane",-1},
{119,-1,"I,J,K coordinates of vector",-1},
{120,-1,"Label of point vector starts at",-1},
{121,-1,"Label of surface vector ends at",-1},
{122,-1,"Label of start point of vector",-1},
{123,-1,"Label of end point of vector",-1},
{124,-1,"Label of plane vector is perpendicular to",-1},
{125,-1,"Label of vector to unitize",-1},
{126,-1,"Label of first vector",-1},
{127,-1,"Label of second vector",-1},
{128,-1,"Label of first plane to intersect",-1},
{129,-1,"Label of second plane to intersect",-1},
{130,-1,"I,J,K coodinates of plane normal",-1},
{131,-1,"Distance of plane from origin",-1},
{132,-1,"Label of first point defining plane",-1},
{133,-1,"Label of second point defining plane",-1},
{134,-1,"Label of third point defining plane",-1},
{135,-1,"Label of point new plane is to go through",-1},
{136,-1,"Label of plane new plane is parallel to",-1},
{137,-1,"Label of plane new plane is parallel to",-1},
{138,-1,"Distance of new plane",-1},
{139,-1,"Pick display point.",-1},
{140,-1,"Label of vector new plane is perpendicular to",-1},
{141,-1,"Label of first point new plane is to go through",-1},
{142,-1,"Label of second point new plane is to go through",-1},
{143,-1,"Label of plane new plane is perpendicular to",-1},
{144,-1,"Label of point new plane is to go through",-1},
{145,-1,"Label of first plane new plane is perpendicular to",-1},
{146,-1,"Label of second plane new plane is perpendicular to",-1},
{147,-1,"Label of line lying on new plane",-1},
{148,-1,"Label of plane new plane is perpendicular to",-1},
{149,-1,"Twelve parameter values defining matrix",-1},
{150,-1,"Label of matrix to invert",-1},
{151,-1,"Label of first matrix to multiply",-1},
{152,-1,"Label of second matrix to multiply",-1},
{153,-1,"Label of point defining origin",-1},
{154,-1,"Label of xaxis vector",-1},
{155,-1,"Label of vector giving approximate yaxis",-1},
{156,-1,"Label of plane to define mirror transformation",-1},
{157,-1,"Scale factor: x [,y [,z]]",-1},
{158,-1,"Angle of rotation about chosen axis",-1},
{159,-1,"X,Y,Z values to translate by",-1},
{160,-1,"Label of point defining curve",-1},
{161,-1,"Label of vector defining curve",-1},
{162,-1,"Label of point defining curve",-1},
{163,-1,"Label of vector defining curve",-1},
{164,-1,"Label of first boundary curve of ruled surface",-1},
{165,-1,"Label of second boundary curve of ruled surface",-1},
{166,-1,"Label of boundary curve",-1},
{167,-1,"Label of slope curve",-1},
{168,-1,"Label of boundary curve",-1},
{169,-1,"Label of slope curve",-1},
{170,-1,"Label of first plane of fillet surface",-1},
{171,-1,"Label of second plane of fillet surface",-1},
{172,-1,"Radius of fillet surface",-1},
{173,-1,"Name of file containing mesh surface definition",-1},
{174,-1,"Label of mesh surface",-1},
{175,-1,"Number of starting patch of mesh surface",-1},
{176,-1,"Number of ending patch of mesh surface",-1},
{177,-1,"Name of file containing quilt surface",-1},
{178,-1,"Number of patch",-1},
{179,-1,"Z value",-1},
{180,-1,"Label of Z surf plane",-1},
{181,-1,"Label of refsys matrix",-1},
{182,-1,"Label of modsys matrix",-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{190,-1,"Macro name",-1},
{191,-1,"Complete macro parameter list",-1},
{192,-1,"Expression to evaluate",-1},
{193,-1,"Label to jumpto if expression less than 0 ",-1},
{194,-1,"Label to jumpto if expression equal to 0 ",-1},
{195,-1,"Label to jumpto if expression greater than 0 ",-1},
{196,-1,"Label to jumpto",-1},
{197,-1,"Enter NCL language statement",-1},
{198,-1,"Label of geometry to redefine ",-1},
{199,-1,"Label of geometry to trim/extend to",-1},
{200,-1,"Label of geometry to remove",-1},
{201,-1,"Scale factor:",-1},
{202,-1,"Pick geometry to show",-1},
{203,-1,"Enter source line number",-1},
{204,-1,"Name of file containing mesh surface",-1},
{205,-1,"Label of surface",-1},
{206,-1,"Number of starting patch",-1},
{207,-1,"Number of ending patch",-1},
{208,-1,"Name of file containing quilt surface",-1},
{209,-1,"Next patch number",-1},
{210,-1,"Enter increment value",-1},
{211,-1,"Enter column number",-1},
{212,-1,"Enter maximum length of tape (ft)",-1},
{213,-1,"Enter source line number(s) to be deleted (L1, Ln)",-1},
{214,-1,"Enter character string to search for",-1},
{215,-1,"Enter TOKEN to search for",-1},
{216,-1,"Pick a point",-1},
{217,-1,"Pick a line",-1},
{218,-1,"Pick a vector",-1},
{219,-1,"Pick a plane",-1},
{220,-1,"Pick a circle",-1},
{221,-1,"Pick a curve",-1},
{222,-1,"Pick a surface",-1},
{223,-1,"Enter a scalar value",-1},
{224,-1,"Enter distance along curve",-1},
{225,-1,"Pick optional starting point",-1},
{226,-1,"Enter number of segments for circle displays",-1},
{227,-1,"Enter number of U segments for surface displays",-1},
{228,-1,"Enter number of V segments for surface displays",-1},
{229,-1,"Enter DO loop label",-1},
{230,-1,"Enter name of loop counter",-1},
{231,-1,"Enter initial value for loop counter",-1},
{232,-1,"Enter the limit value for loop counter",-1},
{233,-1,"Enter increment size for loop counter (optional)",-1},
{234,-1,"Enter a scalar variable name.",-1},
{235,-1,"Enter a reserved point array name.",-1},
{236,-1,"Enter optional forward vector array name.",-1},
{237,-1,"Enter optional tool axis vector array name.",-1},
{238,-1,"Pick geometry",-1},
{239,-1,"Pick subscripted geometry (RESERV-VARIABLE geometry)",-1},
{240,-1,"Pick a point vector",-1},
{241,-1,"Label of point-vector defining plane",-1},
{242,-1,"Label of geometry to reverse ",-1},
{243,-1,"Enter DONE to accept pick or REJECT OP to continue picking.",-1},
{244,-1,"Enter geometry to open/close",-1},
{245,-1,"Enter number of calls to reset. Default [ALL]",-1},
{246,-1,"Enter a reserved point vector array name.",-1},
{247,-1,"Pick a surface or plane to calculate projection vectors (optional)",-1},
{248,-1,"Pick a surface/curve.",-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{390,-1,"Enter distance of plane parallel to starting tool axis (leave-dist)",-1},
{391,-1,"Enter distance of plane parallel to final tool axis (approach-dist)",-1},
{392,-1,"Pick drive surface vector.",-1},
{393,-1,"Pick check surface vector.",-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{400,-1,"Pick Retract plane, point or By Text for distance",-1},
{401,-1,"Enter NCL command:",-1},
{402,-1,"Pick point.",-1},
{403,-1,"Pick boundary curve.",-1},
{404,-1,"Pick slope control curve.",-1},
{405,-1,"Enter: diameter [radius, height, angle/BARREL radius, z-height, flt angle]",-1},
{406,-1,"Enter: i, j, k",-1},
{407,-1,"Enter: vector label",-1},
{408,-1,"Enter: angle(degrees)",-1},
{409,-1,"Enter: cutter height",-1},
{410,-1,"Enter: MAXANG",-1},
{411,-1,"Enter: MAXDP",-1},
{412,-1,"Enter: NUMPTS",-1},
{413,-1,"Enter: THICK",-1},
{414,-1,"Enter: TOLER",-1},
{415,-1,"Pick part surface",-1},
{416,-1,"Enter: matrix label",-1},
{417,-1,"Enter: index value",-1},
{418,-1,"Enter: number of times",-1},
{419,-1,"Enter: x, y, z offsets",-1},
{420,-1,"Enter: index value",-1},
{421,-1,"Enter: x, y ",-1},
{422,-1,"Enter: x, y, z ",-1},
{423,-1,"Pick drive surface",-1},
{424,-1,"Pick check surface",-1},
{425,-1,"Edit line if required",-1},
{426,-1,"Enter: z or x,y,z or a plane label",-1},
{427,-1,"Enter part program file name",-1},
{428,-1,"Pick surface to scrub",-1},
{429,-1,"Enter number of passes, points per pass",-1},
{430,-1,"Enter clfile file name",-1},
{431,-1,"Enter the feed rate value",-1},
{432,-1,"Enter the distance from the check surface",-1},
{433,-1,"Enter the feed rate scaling factor",-1},
{434,-1,"Enter the offset factor",-1},
{435,-1,"Pick boundary points",-1},
{436,-1,"Pick curve (spline) to offset",-1},
{437,-1,"Enter layer number",-1},
{438,-1,"Pick a viewport for motion to be displayed in",-1},
{439,-1,"Pick circle to trim/extend",-1},
{440,-1,"Pick 1st line to trim/extend circle to",-1},
{441,-1,"Pick 2nd line to trim/extend circle to",-1},
{442,-1,"Pick surface to rmill",-1},
{443,-1,"Pick 1st check surface, line, circle, curve, or plane",-1},
{444,-1,"Pick 2nd check surface, line, circle, curve, or plane",-1},
{445,-1,"Pick 1st drive_surface line, or plane ",-1},
{446,-1,"Pick 2nd drive_surface line, or plane ",-1},
{447,-1,"Pick Clearance option plane or By Text for distance",-1},
{448,-1,"Enter height up cutter to contact drive surface",-1},
{449,-1,"Enter X, Y, Z delta values",-1},
{450,-1,"Pick a patern",-1},
{451,-1,"Pick patern or point",-1},
{452,-1,"Enter points to retain",-1},
{453,-1,"Enter points to omit",-1},
{454,-1,"Enter height to raise tool",-1},
{455,-1,"Enter points to avoid",-1},
{456,-1,"Enter a point in the patern",-1},
{457,-1,"Pick a point or vector",-1},
{458,-1,"Enter number of points in patern",-1},
{459,-1,"Enter start angle of patern",-1},
{460,-1,"Enter end angle of patern",-1},
{461,-1,"Enter increment distance(s) between patern(s) ",-1},
{462,-1,"Enter number of points",-1},
{463,-1,"Enter distance between patern(s) ",-1},
{464,-1,"Enter number of parlel paterns including original patern ",-1},
{465,-1,"Pick a point",-1},
{466,-1,"Enter number of paterns to increment",-1},
{467,-1,"Enter increment angle(s) between points ",-1},
{468,-1,"Enter number of points to increment",-1},
{469,-1,"Enter angle(s) between point(s) ",-1},
{470,-1,"Enter increment distance(s) between point(s) ",-1},
{471,-1,"Enter distance(s) between point(s) ",-1},
{472,-1,"Pick Clearance Plane or Planar Surface",-1},
{-1},
{474,-1,"Enter APT source file name ",-1},
{475,-1,"Pick first surface of net surface",-1},
{476,-1,"Pick last surface of net surface",-1},
{477,-1,"Pick surface(s) of net surface",-1},
{478,-1,"Pick surface(s)",-1},
{479,-1,"Pick line, circle, or curve",-1},
{480,-1,"Pick starting line",-1},
{481,-1,"Pick line or circle",-1},
{482,-1,"Pick ending line",-1},
{483,-1,"Pick a surface or plane",-1},
{484,-1,"Pick a near point (optional)",-1},
{485,-1,"Label of line or circle defining curve",-1},
{486,-1,"Enter edge number (1:U=0, 2:U=1, 3:V=0, 4:V=1)",-1},
{487,-1,"Enter number of points to use in generating curve",-1},
{488,-1,"Enter amount to offset from edge (0 to 1)",-1},
{489,-1,"Pick point or point-vector.",-1},
{490,-1,"Pick the material shape",-1},
{491,-1,"Pick the part shape",-1},
{492,-1,"Pick the shape to modify",-1},
{493,-1,"Pick plane or surface for bottom of pocket",-1},
{494,-1,"Pick plane or planar surface for top of pocket",-1},
{495,-1,"Pick perimeter geometry: (surface,curve,patern,circle or subscripted points)",-1},
{496,-1,"Pick island geometry: (surface,curve,patern,circle or subscripted points)",-1},
{497,-1,"Pick point for ending location",-1},
{498,-1,"Enter number of loops around pocket periphery: (return for full pocket)",-1},
{499,-1,"Pick surface(s) to analyze",-1},
{500,-1,"Pick first surface ",-1},
{501,-1,"Pick last surface ",-1},
{502,-1,"Enter percentange to offset from edge (0 to 100)",-1},
{503,-1,"Pick points for entry locations",-1},
{504,-1,"Pick surface to pocket:",-1},
{505,-1,"Pick curve to use as pocket perimeter:",-1},
{506,-1,"Pick start component of open side(s):",-1},
{507,-1,"Pick end component of open side(s):",-1},
{508,-1,"Pick surface",-1},
{-1},
{510,-1,"Label of  entity to blank",-1},
{511,-1,"Label of entity to remove",-1},
{512,-1,"Select new label location (DONE for the default location)",-1},
{513,-1,"Select end location of leader line on the entity(DONE for the default location)",-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{520,-1,"Label of surface to offset",-1},
{521,-1,"Label of first surface fillet is tangent to",-1},
{522,-1,"Label of second surface fillet is tangent to",-1},
{523,-1,"Label of near point to indicate position of fillet",-1},
{524,-1,"Starting radius of fillet surface",-1},
{525,-1,"Ending radius of fillet surface (optional)",-1},
{526,-1,"Label of curve defining variable fillet radius values",-1},
{527,-1,"Label of curve defining end limits of fillet surface (optional)",-1},
{528,-1,"Enter trimmed surface to extract from",-1},
{529,-1,"Enter boundary curve on a trimmed surface to extract from",-1},
{530,-1,"Select surface to trim or planar outer boundary curve",-1},
{531,-1,"Enter outer boundary curve",-1},
{532,-1,"Enter inner boundary curve (optional)",-1},
{533,-1,"Enter boundary curve on a trimmed surface to remove",-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{543,-1,"Select 1st bounding point",-1},
{544,-1,"Select 2nd bounding point",-1},
{545,-1,"Enter angle to tilt forward from normal",-1},
{546,-1,"Enter heel clearance distance",-1},
{547,-1,"Pick 4-axis control vector",-1},
{548,-1,"Enter start angle [, end angle] (degrees)",-1},
{-1},
{550,-1,"Label of curve that vector is to be tangent to",-1},
{551,-1,"Label of point on/near curve where vector is to be tangent to",-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{577,-1,"Pick first THRU component of composite curve.",-1},
{578,-1,"Pick last THRU component of composite curve.",-1},
{579,-1,"Pick curve (spline) to translate",-1},
{580,-1,"Label of line, circle or curve defining composite curve",-1},
{581,-1,"Label of 1st point or point-vector on CONIC curve",-1},
{582,-1,"Label of 2nd point on CONIC curve or 1st vector",-1},
{583,-1,"Label of 3rd point on CONIC curve",-1},
{584,-1,"Label of 4th point or point-vector on CONIC curve",-1},
{585,-1,"Label of 5th point on CONIC curve or 2nd vector",-1},
{586,-1,"Label of 2nd point on CONIC curve",-1},
{587,-1,"Label of 3rd point or point-vector on CONIC curve",-1},
{588,-1,"Label of 4th point or vector on CONIC curve or 2nd vector",-1},
{-1},
{590,-1,"Pick curve to revolve or surface of revolution.",-1},
{591,-1,"Pick surface(s) to compare",-1},
{592,-1,"Pick shape to compare",-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{600,-1,"Enter interpolation type (1 = Linear, 2 = Cubic)",-1},
{601,-1,"Pick first (U0) boundary curve.",-1},
{602,-1,"Pick second (V1) boundary curve.",-1},
{603,-1,"Pick third (U1) boundary curve.",-1},
{604,-1,"Pick fourth (V0) boundary curve.",-1},
{605,-1,"Pick curve to revolve.",-1},
{606,-1,"Point (point-vector) defining axis of revolution.",-1},
{607,-1,"Vector defining axis of revolution.",-1},
{608,-1,"Starting angle (between 0 and 360 degrees).",-1},
{609,-1,"Ending angle (between 0 and 360 degrees).",-1},
{610,-1,"Pick the first pointvector.",-1},
{611,-1,"Pick the second pointvector.",-1},
{612,-1,"Pick pointvector.",-1},
{613,-1,"Enter Perpto Vector (optional).",-1},
{614,-1,"Enter Thru Point.",-1},
{615,-1,"Enter Thru Curve.",-1},
{616,-1,"Enter Curve Distance (optional).",-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{-1},
{649,-1,"Pick motion segment to step back to",-1},
{650,-1,"Pick check surface near point",-1},
{651,-1,"Please enter fillet radius.  ",-1},
{652,-1,"Enter distance along tool axis",-1},
{653,-1,"Pick first entity for distance",-1},
{654,-1,"Pick second entity for distance",-1},
{655,-1,"Pick entity for radius",-1},
{656,-1,"Pick surface to fmill",-1},
{657,-1,"Pick optional limit curve",-1},
{658,-1,"Pick start point",-1},
{659,-1,"Pick rapto plane or surface",-1},
{660,-1,"Pick retrct plane or surface",-1},
{661,-1,"Enter Geometry to Get:",-1},
{662,-1,"Pick Beginning Geometry to Get:",-1},
{663,-1,"Pick End Geometry to Get:",-1},
{664,-1,"Pick Beginning Geometry to Put:",-1},
{665,-1,"Pick End Geometry to Put:",-1},
{666,-1,"Enter new name of the get geometry:",-1},
{667,-1,"Enter Geometry to Put:",-1},
{668,-1,"Pick tool axis control surface:",-1},
{669,-1,"Pick plane or planar surface for top of profile:",-1},
{670,-1,"Pick tool geometry",-1},
{671,-1,"Pick point-vector defining axis of revolution:",-1},
{672,-1,"Pick annotation(s) to engrave",-1},
{673,-1,"Enter scalar to receive number of decomposed entities",-1},
{674,-1,"Pick 1st corner of box",-1},
{675,-1,"Pick 2nd corner of box",-1},
{676,-1,"Enter upper and lower limits of box",-1},
{677,-1,"Pick center of box",-1},
{678,-1,"Enter length, width, and height of box [x,y,z]",-1},
{679,-1,"Pick 1st point of cone",-1},
{680,-1,"Pick 2nd point of cone",-1},
{681,-1,"Enter bottom and top radii of cone [bot,top]",-1},
{682,-1,"Enter height of cone",-1},
{683,-1,"Pick center of cone",-1},
{684,-1,"Pick axis of cone",-1},
{685,-1,"Pick circle defining cylinder",-1},
{686,-1,"Enter height of cylinder",-1},
{687,-1,"Pick 1st point of cylinder",-1},
{688,-1,"Pick 2nd point of cylinder",-1},
{689,-1,"Enter radius of cylinder",-1},
{690,-1,"Pick center of cylinder",-1},
{691,-1,"Pick axis of cylinder",-1},
{692,-1,"Pick circle defining sphere",-1},
{693,-1,"Enter radius of sphere",-1},
{694,-1,"Pick center of sphere",-1},
{695,-1,"Pick inner radius circle of torus",-1},
{696,-1,"Pick outer radius circle of torus",-1},
{697,-1,"Enter circular radius of torus",-1},
{698,-1,"Enter axial radius of torus",-1},
{699,-1,"Pick center of torus",-1},
{700,-1,"Pick axis of torus",-1},
{701,-1,"Pick axial radius circle of torus",-1},
{702,-1,"Pick curve to extrude",-1},
{703,-1,"Pick extrusion vector",-1},
{704,-1,"Enter extrusion distance",-1},
{705,-1,"Pick solid(s)",-1},
{706,-1,"Enter minimum and maximum Z-values [min,max]",-1},
{707,-1,"Pick the first drive plane",-1},
{708,-1,"Pick the second drive plane",-1},
{709,-1,"Pick direction vector",-1},
{710,-1,"Pick motion for distance",-1},
{711,-1,"Pick a solid, surface or plane",-1},
{712,-1,"Pick a solid or surface",-1},
{713,-1,"Pick first component of composite curve",-1},
{714,-1,"Pick second component of composite curve",-1},
{715,-1,"Pick geometry to set initial direction",-1},
{716,-1,"Select position on geometry where tool will be positioned",-1},
{717,-1,"Select geometry to position tool at",-1},
};


static int	choicept[] = {
0, -2};

static char *choicemsg[] = {
""};

#define	MSGSIZE		81
static UU_LOGICAL	uinit = UU_TRUE;
static FILE *erfd;
static UU_LOGICAL to_graphic=UU_TRUE;
static UU_LOGICAL to_file=UU_FALSE;

/*********************************************************************
**    I_FUNCTION     :  uu_ugerror0 (subid, erid, errtxt)
**       Unicad error message without argument
**    PARAMETERS   
**       INPUT  : 
**          subid : subsystem number
**          erid  : error number
**       OUTPUT :  
**          errtxt : Text of error message
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_ugerror0 (int subid, int erid, char* errtxt)
//int  subid, erid;
//char *errtxt;

{
	int	index;

/*	-- remember the error numbers for help -- */

	UD_errorsys = subid;
	UD_errornum = erid;
	UD_errorstate = UD_UNISTATE;

	if (subid <= UMAXSUB)
	if (hdmsg[subid].erleng >= erid)
	{
	 index = hdmsg[subid].erbase + erid - 1;
	 if (pemsg[index].ernum == erid)
	  {
		strcpy(errtxt,pemsg[index].msg);
		return;
	  }
	}
	sprintf (errtxt,"ERROR INFORMATION - Subsystem %d, error number %d",subid,erid);
}	/* uu_ugerror0 */



/*********************************************************************
**    I_FUNCTION     :  uu_outputerr(msg)
**       output the error message to the user desired io
**    PARAMETERS   
**       INPUT  : 
**          subid : subsystem number
**          erid  : prompt number
**       OUTPUT :  
**          output
**    RETURNS      : pointer to a string
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_outputerr(char* msg)
//char	*msg;
{
	uu_denter(-1,(us,"ERROR MESSAGE -- %s",msg));
	if (to_graphic)
		ud_wrerr(msg);
	if (to_file)
		fprintf(erfd, "%s\n", msg);
	uu_dexit;
}	/* uu_outputerr */


/*********************************************************************
**    I_FUNCTION     :  uu_uerror0 (subid, erid)
**       Unicad error message without argument
**    PARAMETERS   
**       INPUT  : 
**          subid : subsystem number
**          erid  : error number
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_uerror0 (int subid, int erid)
//int  subid, erid;

{
	int	index;
	char	ebuf[80];

/*	-- remember the error numbers for help -- */

	UD_errorsys = subid;
	UD_errornum = erid;
	UD_errorstate = UD_UNISTATE;

	if (subid <= UMAXSUB)
	if (hdmsg[subid].erleng >= erid)
	{
	 index = hdmsg[subid].erbase + erid - 1;
	 if (pemsg[index].ernum == erid)
	  {
		uu_outputerr(pemsg[index].msg);
		return;
	  }
	}
	sprintf (ebuf,"ERROR INFORMATION - Subsystem %d, error number %d",subid,erid);
	uu_outputerr (ebuf);
}	/* uu_uerror0 */


/*********************************************************************
**    I_FUNCTION     :  uui_msgtemp(m, temp, contrch, msg, pp)
**       concatenate the user specified information into the string
**       contains the control format
**    PARAMETERS   
**       INPUT  : 
**          m : an index to the control format
**          contrch : the control character
**          msg : the string contains the control format
**          p1 : an union contains the user's information
**       OUTPUT :  
**          temp : the concatenated string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uui_msgtemp(int m, char* temp, char contrch, char* msg, int	pp)
//char  temp[], msg[], contrch;
//int  pp, m;

{
	char yy[12];
	UU_REAL qq;

	switch (contrch)
	{
	 case  's' :
			sprintf (temp, msg, (char *) pp);
			break;

	 case  'd' :
			sprintf (temp, msg, pp);
			break;

	 case  'g':
			sprintf (yy,"%f", (UU_REAL *) pp);
			sscanf (yy, "%f", &qq);
			/*   printf ("pp=%g, %f \n", (UU_REAL *) pp, qq);  */
			if (qq < 10000.0)
				sprintf (temp, msg, (UU_REAL *)pp);
			else
				{
				 msg[m] = 'e';
				 sprintf (temp, msg, (UU_REAL *)pp);
				 msg[m] = 'g';
				}
			break;

	 case  'e':
			sprintf (temp, msg, (UU_REAL *) pp);
			break;

	 default : strcpy (temp,msg);
			break;
	}
}	/* uui_msgtemp */





/********************************************************************
**    I_FUNCTION     :  uu_ugerror1(subid, erid, p1)
**       Unicad error message with one argument
**    PARAMETERS   
**       INPUT  : 
**          subid : subsystem number
**          erid  : error number
**          p1    : error information
**       OUTPUT :  
**          errtxt : Error message text
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_ugerror1(int subid, int erid, int p1, char* errtxt)
/*int	subid, erid;
int	p1;  
char *errtxt; */ 

{
	int	index, leng, i;
	char  temp[200];

/*	-- remember the error numbers for help -- */

	UD_errorsys = subid;
	UD_errornum = erid;
	UD_errorstate = UD_UNISTATE;

	if (subid <= UMAXSUB)
	if (hdmsg[subid].erleng >= erid)
	{
	index = hdmsg[subid].erbase + erid -1;
   if (pemsg[index].ernum == erid)
   {
	for (i=0; (pemsg[index].msg[i]!='%')&&(i<MSGSIZE); i++);
	if (pemsg[index].msg[i]=='%')
		{
		 uui_msgtemp (i+1, temp, pemsg[index].msg[i+1], pemsg[index].msg, p1);
		 leng = strlen(temp);
		 if (leng > MSGSIZE)
			temp[MSGSIZE] = '\0';
		 strcpy(errtxt,temp);
		}
	else
		strcpy(errtxt,pemsg[index].msg);
	return;
	}
	}
	sprintf (errtxt,"ERROR INFORMATION - Subsystem %d, error number %d",subid,erid);
}	/* uu_ugerror1 */





/********************************************************************
**    I_FUNCTION     :  uu_uerror1(subid, erid, p1)
**       Unicad error message with one argument
**    PARAMETERS   
**       INPUT  : 
**          subid : subsystem number
**          erid  : error number
**          p1    : error information
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_uerror1(int subid, int erid, int p1)
//int	subid, erid;
//int	p1;  

{
	int	index, leng, i;
	char  temp[200], ebuf[80];

/*	-- remember the error numbers for help -- */

	UD_errorsys = subid;
	UD_errornum = erid;
	UD_errorstate = UD_UNISTATE;

	if (subid <= UMAXSUB)
	if (hdmsg[subid].erleng >= erid)
	{
	index = hdmsg[subid].erbase + erid -1;
   if (pemsg[index].ernum == erid)
   {
	for (i=0; (pemsg[index].msg[i]!='%')&&(i<MSGSIZE); i++);
	if (pemsg[index].msg[i]=='%')
		{
		 uui_msgtemp (i+1, temp, pemsg[index].msg[i+1], pemsg[index].msg, p1);
		 leng = strlen(temp);
		 if (leng > MSGSIZE)
			temp[MSGSIZE] = '\0';
		 uu_outputerr(temp);
		}
	else
		uu_outputerr(pemsg[index].msg);
	return;
	}
	}
	sprintf (ebuf,"ERROR INFORMATION - Subsystem %d, error number %d",subid,erid);
	uu_outputerr (ebuf);
}	/* uu_uerror1 */





/*********************************************************************
**    I_FUNCTION     :  uu_uerror2 (subid, erid, p1, p2)
**       Unicad error message with one argument
**    PARAMETERS   
**       INPUT  : 
**          subid : subsystem number
**          erid  : error number
**          p1    : first error information
**          p2    : second error information
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_uerror2 (int subid, int erid, int p1, int p2)
//int	subid, erid;
//int	p1, p2;

{
	int	index, i, k, j;
	char  temp1[200], temp2[200], temp[200];
	char	ebuf[80];

/*	-- remember the error numbers for help -- */

	UD_errorsys = subid;
	UD_errornum = erid;
	UD_errorstate = UD_UNISTATE;

	if (subid <= UMAXSUB)
	if (hdmsg[subid].erleng >= erid)
	{
	index = hdmsg[subid].erbase + erid -1;
   if (pemsg[index].ernum == erid)
   {
	for (i=0; (pemsg[index].msg[i]!='%')&&(i<MSGSIZE); i++)
		temp[i] = pemsg[index].msg[i];
	if (pemsg[index].msg[i]=='%')
		{
		 temp[i] = pemsg[index].msg[i];
		 temp[i+1] = pemsg[index].msg[i+1];
		 temp[i+2] = '\0';
		 uui_msgtemp (i+1, temp1, pemsg[index].msg[i+1], temp, p1);
		 for (k=i+2,j=0; (pemsg[index].msg[k]!='%')&&(k<MSGSIZE);j++,k++);
		 if (pemsg[index].msg[k]=='%')
			{
			 uui_msgtemp(j+1,temp2,pemsg[index].msg[k+1],&pemsg[index].msg[i+2],p2);
			 sprintf(temp,"%s%s",temp1,temp2);
			}
		 else
			 sprintf(temp,"%s%s", temp1, &pemsg[index].msg[i+2]);
		 uu_outputerr (temp);
		}
	else
		uu_outputerr (pemsg[index].msg);
	return;
	}
	}
	sprintf (ebuf,"ERROR INFORMATION - Subsystem %d, error number %d",subid,erid);
	uu_outputerr (ebuf);
}	/* uu_uerror2 */





/*********************************************************************
**    I_FUNCTION     :  uu_uerror3 (subid, erid, p1, p2, p3)
**       Unicad error message with three argument
**    PARAMETERS   
**       INPUT  : 
**          subid : subsystem number
**          erid  : error number
**          p1    : first error information
**          p2    : second error information
**          p3    : third error information
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_uerror3 (int	subid, int	erid,int	 p1, int	p2, int	p3)
//int	subid, erid;
//int	p1, p2, p3;

{
	int	index, i, k, j;
	char  temp1[200], temp2[200], temp3[200], temp[200];
	char	ebuf[80];

/*	-- remember the error numbers for help -- */

	UD_errorsys = subid;
	UD_errornum = erid;
	UD_errorstate = UD_UNISTATE;

	if (subid <= UMAXSUB)
	if (hdmsg[subid].erleng >= erid)
	{
	index = hdmsg[subid].erbase + erid -1;
   if (pemsg[index].ernum == erid)
   {
	for (i=0; (pemsg[index].msg[i]!='%')&&(i<MSGSIZE); i++)
		temp[i] = pemsg[index].msg[i];
	if (pemsg[index].msg[i]=='%')
		{
		 temp[i] = pemsg[index].msg[i];
		 temp[i+1] = pemsg[index].msg[i+1];
		 temp[i+2] = '\0';
		 uui_msgtemp (i+1, temp1, pemsg[index].msg[i+1], temp, p1);
		 for (k=i+2,j=0; (pemsg[index].msg[k]!='%')&&(k<MSGSIZE);j++,k++)
			temp[j] = pemsg[index].msg[k];
		 if (pemsg[index].msg[k]=='%')
			{
		 	 temp[j] = pemsg[index].msg[k];
		 	 temp[j+1] = pemsg[index].msg[k+1];
		 	 temp[j+2] = '\0';
		 	 uui_msgtemp (j+1, temp2, pemsg[index].msg[k+1], temp, p2);
		 	 for (i=k+2,j=0; (pemsg[index].msg[i]!='%')&&(i<MSGSIZE);j++,i++);
		 	 if (pemsg[index].msg[i]=='%')
			 {
			  uui_msgtemp(j+1,temp3,pemsg[index].msg[i+1],&pemsg[index].msg[k+2],p3);
			  sprintf(temp,"%s%s%s",temp1,temp2,temp3);
			 }
			else
			 	sprintf(temp,"%s%s%s", temp1, temp2, &pemsg[index].msg[i+2]);
			}
		  else
			 sprintf(temp,"%s%s", temp1, &pemsg[index].msg[i+2]);
		 uu_outputerr (temp);
		}
	else
		uu_outputerr (pemsg[index].msg);
	return;
	}
	}
	sprintf (ebuf,"ERROR INFORMATION - Subsystem %d, error number %d",subid,erid);
	uu_outputerr (ebuf);
}	/* uu_uerror3 */








/*********************************************************************
**    I_FUNCTION     :  char *uu_uprompt0 (subid, pmtid)
**       Unicad prompt message without argument
**    PARAMETERS   
**       INPUT  : 
**          subid : subsystem number
**          erid  : prompt number
**       OUTPUT :  
**          output
**    RETURNS      : pointer to a string
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_uprompt0 (int subid, int pmtid)
//int  subid, pmtid;

{
	int	index;
	static char temp[200];
	char	ebuf[80];

/*	-- remember the prompt numbers for help -- */

	UD_promptsys = subid;
	UD_promptnum = pmtid;
	UD_promptstate = UD_UNISTATE;
	temp[0] = '\0';

	if (subid <= UMAXSUB)
	if (hdmsg[subid].pmtleng >= pmtid)
	{
	index = hdmsg[subid].pmtbase + pmtid - 1;
	if (pemsg[index].ernum == pmtid)
	{
	sprintf (temp,"%s", pemsg[index].msg);
	return(temp);
	}
	}
	sprintf (ebuf,"ERROR INFORMATION - Subsystem %d, error number %d",subid,pmtid);
	uu_outputerr (ebuf);
	return(temp);
}	/* uu_uprompt0 */




/*********************************************************************
**    I_FUNCTION     :  char *uu_uprompt1(subid, pmtid, p1)
**       Unicad prompt message with one argument
**    PARAMETERS   
**       INPUT  : 
**          subid : subsystem number
**          erid  : prompt number
**          p1    : first promt information
**       OUTPUT :  
**          output
**    RETURNS      : pointer to a string
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_uprompt1(int subid, int pmtid, int p1)
//int	subid, pmtid;
//int	p1;  

{
	int	index, leng, i;
	static char  temp[200];
	char	ebuf[80];

/*	-- remember the prompt numbers for help -- */

	UD_promptsys = subid;
	UD_promptnum = pmtid;
	UD_promptstate = UD_UNISTATE;
	temp[0] = '\0';

	if (subid <= UMAXSUB)
	if (hdmsg[subid].pmtleng >= pmtid)
	{
	index = hdmsg[subid].pmtbase + pmtid -1;
	if (pemsg[index].ernum == pmtid)
	{
	for (i=0; (pemsg[index].msg[i]!='%')&&(i<MSGSIZE); i++);
	if (pemsg[index].msg[i]=='%')
		{
		 uui_msgtemp (i+1, temp, pemsg[index].msg[i+1], pemsg[index].msg, p1);
		 leng = strlen(temp);
		 if (leng > MSGSIZE)
			temp[MSGSIZE] = '\0';
		}
	else
		sprintf (temp, "%s", pemsg[index].msg);
   return (temp);
	}
	}
	sprintf (ebuf,"ERROR INFORMATION - Subsystem %d, error number %d",subid,pmtid);
	uu_outputerr (ebuf);
   return (temp);
}	/* uu_uprompt1 */





/*********************************************************************
**    I_FUNCTION     :  char *uu_uprompt2 (subid, pmtid, p1, p2)
**       Unicad prompt message with two argument
**    PARAMETERS   
**       INPUT  : 
**          subid : subsystem number
**          erid  : prompt number
**          p1    : first promt information
**          p2    : second promt information
**       OUTPUT :  
**          output
**    RETURNS      : pointer to a string
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_uprompt2 (int subid, int pmtid, int p1, int p2)
//int	subid, pmtid;
//int	p1, p2;

{
	int	index, i, k, j;
	char  temp1[200], temp2[200], ebuf[80]; 
	static char temp[200];

/*	-- remember the prompt numbers for help -- */

	UD_promptsys = subid;
	UD_promptnum = pmtid;
	UD_promptstate = UD_UNISTATE;
	temp[0] = '\0';

	if (subid <= UMAXSUB)
	if (hdmsg[subid].pmtleng >= pmtid)
	{
	index = hdmsg[subid].pmtbase + pmtid -1;
	if (pemsg[index].ernum == pmtid)
	{
	for (i=0; (pemsg[index].msg[i]!='%')&&(i<MSGSIZE); i++)
		temp[i] = pemsg[index].msg[i];
	temp[i] = pemsg[index].msg[i];
	temp[i+1] = pemsg[index].msg[i+1];
	temp[i+2] = '\0';
	if (pemsg[index].msg[i]=='%')
		{
		 uui_msgtemp (i+1, temp1, pemsg[index].msg[i+1], temp, p1);
		 for (k=i+2,j=0; (pemsg[index].msg[k]!='%')&&(k<MSGSIZE);j++,k++);
		 if (pemsg[index].msg[k]=='%')
			{
			 uui_msgtemp(j+1,temp2,pemsg[index].msg[k+1],&pemsg[index].msg[i+2],p2);
			 sprintf(temp,"%s%s",temp1,temp2);
			}
		 else
			 sprintf(temp,"%s%s", temp1, &pemsg[index].msg[i+2]);
		}
	else
		sprintf (temp, "%s", pemsg[index].msg);
	return(temp);
	}
	}
	sprintf (ebuf,"ERROR INFORMATION - Subsystem %d, error number %d",subid,pmtid);
	uu_outputerr (ebuf);
	return(temp);
}	/* uu_uprompt2 */





/*********************************************************************
**    I_FUNCTION     :  uu_initerr()
**       Check the symbol in the .init file to get the 
**       output error message io information
**       the prompt message
**    PARAMETERS   
**       INPUT  : 
**          subid : subsystem number
**          erid  : prompt number
**       OUTPUT :  
**          output
**    RETURNS      : pointer to a string
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_initerr()
{
char	*charptr;
int		fdt;
UX_pathname	pathname;
UU_LOGICAL	found;

	charptr = (char*)UU_NULL;
	if (strcmp("FALSE",ux_getenv("UU_DERROR",UX_NPRTERRS))==0)
		to_graphic = UU_FALSE;
}	/* uu_initerr */


//FILE*	fopen();

static FILE  *fd1 = UU_NULL;

/*********************************************************************
**    I_FUNCTION     :  uu_inithep()
**       initialization routine for the Unicad and Application help
**       message file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_inithep()
{
char	*charptr;
UX_pathname	pathname;
UU_LOGICAL	found;
int	fdt;

	uinit = UU_FALSE;
	charptr = UU_NULL;
	if (ux_get_syspath("UU_FHELP",&charptr,pathname,&found,UX_NPRTERRS)==UU_SUCCESS)
	  {
		if (ux_open(pathname,"r","STREAM","TEXT",&fdt,UX_PRTERRS)==UU_SUCCESS)
    {	/*	ux_open_to_data();*/
		ux_get_os_filedesc(fdt,&fd1,UX_PRTERRS);
		uu_lsdel(charptr);
    }
	  }
}	/* uu_inithep */



/*********************************************************************
**    I_FUNCTION     :  uui_ugetbuf (buf,temp1,ptr,leng,length)
**       put the certain length of characters from one array to another
**       buffer
**    PARAMETERS   
**       INPUT  : 
**          temp - input array
**          length - maxi length of characters can be put into buffer
**       OUTPUT :  
**          ptr - pointer to the first unconverted character in the array
**          leng - total unconvered chars
**          buf - array of converted characters
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uui_ugetbuf (char* buf,char* temp1,int* ptr,int* leng,int length)
//int	*ptr, *leng, length;
//char	buf[], temp1[];

{
	int	i, j, k, ind;

	ind = *ptr + length;
 	if ((temp1[ind-1] != ' ')&&(temp1[ind] != ' '))
	  {
		for (i=ind-1;(temp1[i]!=' ')&&(i>=*ptr); i--);
		for (k=0,j= *ptr; j<i; buf[k++]=temp1[j++]);
	   *ptr = i++;
	  }
	else
	  {
		for (k=0,j= *ptr; j<ind; buf[k++]=temp1[j++]);
		*ptr = ind;
	  }	
	buf[k++] = '\n';
   buf[k] = '\0';
	*leng = strlen (&temp1[*ptr]);
}	/* uui_ugetbuf */




/*********************************************************************
**    I_FUNCTION     :  char *uu_uerhep (subid, erid, first, status, length )
**       Unicad error help message 
**    PARAMETERS   
**       INPUT  : 
**          subid : subsystem number
**          erid  : error number
**          first : flag for first time
**          status : flag to tell whether a message is returned
**       OUTPUT :  
**          output
**    RETURNS      : a pointer to a string
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_uerhep (int subid, int erid, int first, int* status, int length )
//int  subid, erid, length ;
//int  first, *status;

{
	int	index, hpno;
	static char temp[200], buf[200] ;
	static  int  ptr;
	static  int  leng = 0;
	char	ebuf[80];
	static char	nomsg[]={"no help message for this error "};

 if (uinit)		uu_inithep();
	*status = UU_FALSE;
 buf[0] = '\0';
 if (fd1 == UU_NULL) {
		uu_outputerr ("ERROR - Can't open help file");
		return(buf); }
	if (first)
	{
		if (subid <= UMAXSUB)
		if (hdmsg[subid].erleng >= erid)
		{
	   *status = UU_TRUE;
		index = hdmsg[subid].erbase + erid - 1;
		rewind (fd1);
		if ((pemsg[index].ernum == erid) &&
			 ((hpno=pemsg[index].hmsgnum) > -1))
		{
		 while (hpno > -1)
			{
 			 fgets(temp,200,fd1);
			 if (temp[0]=='#')	hpno--;
			}
			 ptr = 1;
			 leng = strlen (temp);
			 if (leng > length)
				{
				 uui_ugetbuf (buf,temp,&ptr,&leng,length);
				 return (buf);
			   }
			 else
			   {
				 leng = 0;
				 return (&temp[1]);
			   }
		}
		else
		   return(nomsg);
		}
		sprintf (ebuf,"ERROR INFORMATION - Subsystem %d, error number %d",subid,erid);
		uu_outputerr (ebuf);
	}
	else
		{
		 if (leng > 0)
			{
			 *status = UU_TRUE;
			 if (leng > length)
				{
				 uui_ugetbuf(buf,temp,&ptr,&leng,length);
				 return (buf);
				}
			 else
			   {
				 leng = 0;
				 return (&temp[ptr]);
			   }
			}
		 else
			{
		 	 if (fgets(temp,200,fd1) != NULL)
		 	 if ((temp[0] != '@') && (temp[0] != '#'))
				{
				 ptr = 1;
			 	 *status = UU_TRUE;
			 	 leng = strlen (temp);
			 	 if (leng > length)
					{
				 	 uui_ugetbuf (buf,temp,&ptr,&leng,length);
				 	 return (buf);
			   	}
			 	 else
			   	{
				 	 leng = 0;
				 	 return (&temp[1]);
			   	}
				}
			}
		}
}	/* uu_uerhep */




/*********************************************************************
**    I_FUNCTION     :  char *uu_upmthep (subid, pmtid, first, status, length )
**       Unicad prompt help message 
**    PARAMETERS   
**       INPUT  : 
**          subid : subsystem number
**          pmtid  : error number
**          first : flag for first time
**          status : flag to tell whether a message is returned
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_upmthep (int subid, int pmtid, int first, int* status, int length )
//int  subid, pmtid, length ;
//int  first, *status;

{
	int	index, hpno;
	static char temp[200], buf[200];
	static int  ptr;
	static int  leng = 0;
	char	ebuf[80];
	static char	nohmsg[]={"no help message for this prompt"};

 if (uinit)		uu_inithep();
	*status = UU_FALSE;
 buf[0] = '\0';
 if (fd1 == UU_NULL) {
		uu_outputerr ("ERROR - Can't open help file");
		return(buf); }
	if (first)
	{
		if (subid <= UMAXSUB)
		if (hdmsg[subid].pmtleng >= pmtid)
		{
	   *status = UU_TRUE;
		index = hdmsg[subid].pmtbase + pmtid - 1;
	   rewind (fd1);
		if ((pemsg[index].ernum == pmtid) &&
			 ((hpno=pemsg[index].hmsgnum) > -1))
		{
		 while (hpno > -1)
			{
 			 fgets(temp,200,fd1);
			 if (temp[0]=='#')	hpno--;
			}
			 ptr = 1;
			 leng = strlen (temp);
			 if (leng > length)
				{
				 uui_ugetbuf (buf,temp,&ptr,&leng,length);
				 return (buf);
			   }
			 else
			   {
				 leng = 0;
				 return (&temp[1]);
			   }
		}
		else  return(nohmsg);
		}
		sprintf (ebuf,"ERROR INFORMATION - Subsystem %d, prompt number %d",subid,pmtid);
		uu_outputerr (ebuf);
	}
	else
		{
		 if (leng > 0)
			{
			 *status = UU_TRUE;
			 if (leng > length)
				{
				 uui_ugetbuf(buf,temp,&ptr,&leng,length);
				 return (buf);
				}
			 else
			   {
				 leng = 0;
				 return (&temp[ptr]);
			   }
			}
		 else
			{
		 	 if (fgets(temp,200,fd1) != NULL)
		 	 if ((temp[0] != '@') && (temp[0] != '#'))
				{
				 ptr = 1;
			 	 *status = UU_TRUE;
			 	 leng = strlen (temp);
			 	 if (leng > length)
					{
				 	 uui_ugetbuf (buf,temp,&ptr,&leng,length);
				 	 return (buf);
			   	}
			 	 else
			   	{
				 	 leng = 0;
				 	 return (&temp[1]);
			   	}
				}
			}
		}
}	/* uu_upmthep */




/*********************************************************************
**    I_FUNCTION     :  char *uu_usysinfor (subid,first,status,length)
**       get a line of system information
**    PARAMETERS   
**       INPUT  : 
**          subid : subsystem number
**          first : flag for first time
**          status : flag to tell whether a message is returned
**          length : length of line to be returned
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char *uu_usysinfor (int subid,int first,int* status,int length)
//int	subid, first, *status, length;

{
	static	int	leng, ptr;
	static	char	temp[200], buf[200];
	char	ebuf[80];

 if (uinit)		uu_inithep();
	*status = UU_FALSE;
	if (first)
		{
		 if (subid <= UMAXSUB)
		 {
		 rewind(fd1);
		 while (subid > -1)
			{
			 fgets (temp,200,fd1);
			 if (temp[0] == '@') 
				subid--;
			}
		 if (fgets(temp,200,fd1) != NULL)
		 if ((temp[0] != '@') && (temp[0] != '#'))
			{
			 ptr = 1;
			 *status = UU_TRUE;
			 leng = strlen (temp);
			 if (leng > length)
				{
				 uui_ugetbuf (buf,temp,&ptr,&leng,length);
				 return (buf);
			   }
			 else
			   {
				 leng = 0;
				 return (&temp[1]);
			   }
			}
		}
	  else
		{
		 sprintf (ebuf,"Illegal subsystem number = %d",subid);
		 uu_outputerr (ebuf);
		}
	 }
	else			/* not the first time */
		{
		 if (leng > 0)
			{
			 *status = UU_TRUE;
			 if (leng > length)
				{
				 uui_ugetbuf(buf,temp,&ptr,&leng,length);
				 return (buf);
				}
			 else
			   {
				 leng = 0;
				 return (&temp[ptr]);
			   }
			}
		 else
			{
		 	 if (fgets(temp,200,fd1) != NULL)
		 	 if ((temp[0] != '@') && (temp[0] != '#'))
				{
				 ptr = 1;
			 	 *status = UU_TRUE;
			 	 leng = strlen (temp);
			 	 if (leng > length)
					{
				 	 uui_ugetbuf (buf,temp,&ptr,&leng,length);
				 	 return (buf);
			   	}
			 	 else
			   	{
				 	 leng = 0;
				 	 return (&temp[1]);
			   	}
				}
			}
	}
}		/* uu_usysinfor */







 /*********************************************************************
 **    E_FUNCTION :  uu_uchoicemsg(subid, choid, choicenum)
 **       This routine returns a pointer to the choice string array and
 **			the number of choices at the item specified by subid and choid.
 **    PARAMETERS   
 **       INPUT  : 
 **          subid - subsystem id
 **				choid - choice id
 **       OUTPUT :  
 **				choicenum - number of choices
 **    RETURNS      : pointer to the character string, null otherwise
 **    SIDE EFFECTS : none
 **    WARNINGS     : none
 *********************************************************************/
 
 char **uu_uchoicemsg(int subid,int choid, int* choicenum)
 /*int	subid, choid, *choicenum;
 */
 {
  	int	ind, k;
  	char	erbuf[80];
    char	**choptr;
 
 	if (subid <= UMAXSUB)
 	if (hdmsg[subid].choleng >= choid)
 	  {
 		ind = hdmsg[subid].chobase + choid - 1;	/* index to choice array */
 		if (choicept[ind] > -1)
 		  {
 			choptr = &(choicemsg[choicept[ind]]);
 			k = ind;
 			while (choicept[++k] == -1);
 			*choicenum = choicept[k] - choicept[ind];
 			return (choptr);
 		  }
 	  }
  	 sprintf (erbuf,"ERROR INFORMATION - subsystem %d, error number %d",subid,choid);
 	 uu_outputerr(erbuf);
 	 return (NULL);
 }	/* uu_uchoicemsg */
 

