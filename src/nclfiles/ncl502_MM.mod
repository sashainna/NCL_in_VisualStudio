#INCLUDE#
/FILE/ ncl_color.mod
/FILE/ ncl_graphic.mod
/FILE/ ncl_interface.mod
/FILE/ ncl_material.mod
/FILE/ ncl_lights.mod
/FILE/ ncl_pick.mod
/FILE/ ncl_cmdline.mod
/FILE/ ncl_chain.mod
/FILE/ ncl_unibase.mod
/FILE/ ncl_labels.mod
/FILE/ ncl_playfeed_MM.mod
/FILE/ ncl_playinterp.mod
/FILE/ ncl_macro.mod
/FILE/ ncl_colors.mod
/FILE/ ncl_view.mod
/FILE/ ncl_background.mod
/FILE/ ncl_srfattr.mod
/FILE/ ncl_source.mod
/FILE/ ncl_motion.mod
/FILE/ ncl_autosave.mod
/FILE/ ncl_preview.mod

#SIGNON#
/Cam/ 		*yes
/Cad/ 		*yes
/Levels/	*no

#DISPLAY#
/Wrap/ *off
/Page/ *off

#PLOT#
/CCPLT1/ *1043
/HPPLT1/ *7470
/TKPLT1/ *4109
/SAVE/ *NO
/USE_SAVED/ *YES
/ABS_INCR/ *ABS
/BYPASS/ *OFF
/FP_TYPE/ *LZS
/FP_LEFT/ 3
/FP_RIGHT/ 4
/UNITS/ *INCH

#PUNCH#
/Greco/ *NO
/Display/ *NO
/Rec_len/ 72
/Pch_on/ 0
/Pch_off/ 0
/Trans_file/ ASC
/User_trans/ *NO
/Leader/ 42
/Speed/ 73
/Partno/ *YES
/Start/
/Comment/

#DNC#
/Mode/ *SEND
/Comm_port/
/Send_pr/
/Rcv_pr/
/Echo_pr/
/Start/
/Xon/ *YES
/Display/ *NO
/Rec_len/ 72
/Parity/ *YES
/Char_wait/ 0
/Rec_wait/ 0
/eof/
/eot/ M02
/trans_file/ ASC

#NCL_QUE#
/Clfile/ *NO
/APT_Source/ *NO
/Print/ *YES
/Update/ *NO
/#_Lines/ 60
/Priority/ *C
/Post_Proc/ *NO
 
#NCL#
/Load_pp/ *YES
/Clfile/ *NO
/Apt_source/ *NO
/Save_pp/ *NO
/Run_pp/ *NO

#IPV#
/Delete_IPV/ *NO
/Delete_WIP/ *NO

#MOTION#
/CUTTER_COLOR/ *GREEN
/CUTTER_PEN/ 1
/SHANK_COLOR/ *ORANGE
/SHANK_PEN/ 1
/HOLDER_COLOR/ *PURPLE
/HOLDER_PEN/ 1
/MOTION_COLOR/ *WHITE
/MOTION_STYLE/ *SOLID
/MOTION_PEN/ 1
/RAPID_COLOR/ *RED
/RAPID_STYLE/ *DASHED
/RAPID_PEN/ 1
/CUTTER_STEP/ 999
/TRACUT/ *IGNORE
/CUTTER_ITERATE/ *NO
/STACK_SIZE/ 10
/COMMANDS/ *NO

#VIEW#
/MS_PAN_GAIN/ 0.01
/MS_ROTATE_GAIN/ 3.0
/MS_ZOOM_GAIN/ 0.03
/KB_PAN_GAIN/ 0.01
/KB_ROTATE_GAIN/ 3.0
/KB_ZOOM_GAIN/ 0.03
/SM_PAN_GAIN/ 0.0005
/SM_ROTATE_GAIN/ 0.0005
/SM_ZOOM_GAIN/ 0.0005
