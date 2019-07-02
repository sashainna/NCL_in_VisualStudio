/*********************************************************************
**    NAME         :  openvx_uif.h
**       CONTAINS: VX structures.
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**        openvx_uif.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:06:40
*********************************************************************/
/* ID:obi * DATE::961213103959 * PROJECT: OVX fixes and enhancements */
/* ID:obi * DATE::950428082525 * PROJECT: Fixes to OPENVX */
/*    Copyright Varimetrix Corporation
*/

#ifndef __openvx_uif_h
#define __openvx_uif_h
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* Defines for relative positioning */
#define VX_UIF_BUTTON			1
#define VX_UIF_TEXT_IN			3
#define VX_UIF_TEXT_OUT			4
#define VX_UIF_SCRL_LIST		5
#define VX_UIF_TTY				6
#define VX_UIF_TEXT_EDIT		7
#define VX_UIF_CHECK_BOX		8
#define VX_UIF_DECOR_BTN		101
#define VX_UIF_DECOR_LINE		102

extern int TASK_NAME;
extern int TASK_FROM;

typedef struct _uif_data_struct
	{
	int	flg[4];
	char	str[512];
	} uif_data_struct;

extern uif_data_struct Uif_data;

typedef struct _ipnt_2d
   {
   int         x;
   int         y;
   } ipnt_2d;

/********** Function calls ****************************************************/

#define vx_uif_asgn_cmd_key			u_asgn_cmd_key
#define vx_uif_btn_inq					u_btn_inq
#define vx_uif_btn_inq_state			u_btn_inq_state
#define vx_uif_btn_set_code			u_btn_set_code
#define vx_uif_btn_set_disable		u_btn_set_disable
#define vx_uif_btn_set_icon			u_btn_set_icon
#define vx_uif_btn_set_status			u_btn_set_status
#define vx_uif_btn_set_text			u_btn_set_text
#define vx_uif_btns_inq_state			u_btns_inq_state
#define vx_uif_btns_set_status		u_btns_set_status
#define vx_uif_btns_set_text			u_btns_set_text
#define vx_uif_busy_tool_def			u_busy_tool_def
#define vx_uif_busy_tool_off			u_busy_tool_off
#define vx_uif_busy_tool_on			u_busy_tool_on
#define vx_uif_cb_inq_state			u_cb_inq_state
#define vx_uif_cb_set_state			u_cb_set_state
#define vx_uif_chk_box_inq				u_chk_box_inq
#define vx_uif_chk_box_set				u_chk_box_set
#define vx_uif_decor_btn				u_decor_btn
#define vx_uif_decor_line				u_decor_line
#define vx_uif_disable_wnds			u_disable_wnds
#define vx_uif_draw_pixel				u_draw_pixel
#define vx_uif_get_max_win_id			u_get_max_win_id
#define vx_uif_get_win_scale			u_get_win_scale
#define vx_uif_help_add					u_help_add
#define vx_uif_help_del					u_help_del
#define vx_uif_help_init_cmd			u_help_init_cmd
#define vx_uif_help_win_def			u_help_win_def
#define vx_uif_inq_colr_num			u_inq_colr_num
#define vx_uif_list_add					u_list_add
#define vx_uif_list_asgn_cmd			u_list_asgn_cmd
#define vx_uif_list_del					u_list_del
#define vx_uif_list_empty				u_list_empty
#define vx_uif_list_get_idx			u_list_get_idx
#define vx_uif_list_get_str			u_list_get_str
#define vx_uif_list_inq					u_list_inq
#define vx_uif_list_itm_cnt			u_list_itm_cnt
#define vx_uif_list_mlt_add			u_list_mlt_add
#define vx_uif_list_mod					u_list_mod
#define vx_uif_list_reset				u_list_reset
#define vx_uif_list_set_status		u_list_set_status
#define vx_uif_list_show_idx			u_list_show_idx
#define vx_uif_load_icon				u_load_icon
#define vx_uif_load_menu				u_load_menu
#define vx_uif_menu_vx					u_menu_vx
#define vx_uif_move_cursor				u_move_cursor
#define vx_uif_pnl_inq_avail			u_pnl_inq_avail
#define vx_uif_pos_cursor				u_pos_cursor
#define vx_uif_reg_esc_cmd				u_reg_esc_cmd
#define vx_uif_reg_opn_cls_cmd		u_reg_opn_cls_cmd
#define vx_uif_reg_quit_cmd			u_reg_quit_cmd
#define vx_uif_replay_event_uif		u_replay_event_uif
#define vx_uif_replay_state			u_replay_state
#define vx_uif_restore_wnds			u_restore_wnds
#define vx_uif_rm_decor					u_rm_decor
#define vx_uif_rm_pnl					u_rm_pnl
#define vx_uif_set_act_pnl_level		u_set_act_pnl_level
#define vx_uif_set_base_win_id		u_set_base_win_id
#define vx_uif_set_btn_colr			u_set_btn_colr
#define vx_uif_set_cms					u_set_cms
#define vx_uif_set_colr_num			u_set_colr_num
#define vx_uif_set_highlight			u_set_highlight
#define vx_uif_set_pnl_state			u_set_pnl_state
#define vx_uif_set_txt_out				u_set_txt_out
#define vx_uif_set_win_colrs			u_set_win_colrs
#define vx_uif_set_win_side			u_set_win_side
#define vx_uif_tty_clr					u_tty_clr
#define vx_uif_tty_erase				u_tty_erase
#define vx_uif_tty_input				u_tty_input
#define vx_uif_tty_mode					u_tty_mode
#define vx_uif_tty_print				u_tty_print
#define vx_uif_tty_state				u_tty_state
#define vx_uif_tty_trigger				u_tty_trigger
#define vx_uif_txt_edit_asgn_cmd		u_txt_edit_asgn_cmd
#define vx_uif_txt_edit_clr			u_txt_edit_clr
#define vx_uif_txt_edit_inq			u_txt_edit_inq
#define vx_uif_txt_edit_ins			u_txt_edit_ins
#define vx_uif_txt_edit_pos			u_txt_edit_pos
#define vx_uif_txt_edit_set			u_txt_edit_set
#define vx_uif_txt_in_add_field		u_txt_in_add_field
#define vx_uif_txt_in_inq_field		u_txt_in_inq_field
#define vx_uif_txt_in_mode				u_txt_in_mode
#define vx_uif_txt_in_prmpt			u_txt_in_prmpt
#define vx_uif_txt_in_value			u_txt_in_value
#define vx_uif_unasgn_cmd_key			u_unasgn_cmd_key
#define vx_uif_unload_icon				u_unload_icon
#define vx_uif_win_add_btns			u_win_add_btns
#define vx_uif_win_add_chk_box		u_win_add_chk_box
#define vx_uif_win_add_list			u_win_add_list
#define vx_uif_win_add_list2			u_win_add_list2
#define vx_uif_win_add_tty				u_win_add_tty
#define vx_uif_win_add_txt_edit		u_win_add_txt_edit
#define vx_uif_win_add_txt_in			u_win_add_txt_in
#define vx_uif_win_add_txt_out		u_win_add_txt_out
#define vx_uif_win_create				u_win_create
#define vx_uif_win_destroy				u_win_destroy
#define vx_uif_win_inq_avail			u_win_inq_avail
#define vx_uif_win_inq_state			u_win_inq_state
#define vx_uif_win_loc_pos				u_win_loc_pos
#define vx_uif_win_set_rel				u_win_set_rel
#define vx_uif_win_show					u_win_show
#define vx_uif_win_shutdown			u_win_shutdown
#define vx_uif_win_title				u_win_title

/* function prototypes */
#ifdef __STDC__
int u_asgn_cmd_key(int owner, int trig_type, int trig_value, int cmd_type, int code);
int u_btn_inq(int win_id, int btn_id, int index, unsigned char *press, unsigned char *cmd_repeat, unsigned char *disable, unsigned char *cmd_type, int *code, unsigned char *label_type, int *icon_id, char *text);
int u_btn_inq_state(int win_id, int btn_id, int index, unsigned char *press, unsigned char *disable);
int u_btn_set_code(int win_id, int btn_id, int index, int type, int code);
int u_btn_set_disable(int win_id, int btn_id, int index, int disable);
int u_btn_set_icon(int win_id, int btn_id, int index, int press, int cmd_repeat, int disable, int type, int code, int icon_id, char *icon_name);
int u_btn_set_status(int win_id, int btn_id, int index, int state);
int u_btn_set_text(int win_id, int btn_id, int index, int press, int cmd_repeat, int disable, int type, int code, char *text);
int u_btns_inq_state(int win_id, int btn_id, int beg_idx, int end_idx, unsigned char press[], unsigned char disable[]);
int u_btns_set_status(int win_id, int btn_id, int beg_idx, int end_idx, unsigned char state[]);
int u_btns_set_text(int win_id, int btn_id, int beg_idx, int end_idx, unsigned char press[], unsigned char cmd_repeat[], unsigned char disable[], unsigned char type[], int code[], char *text[]);
int u_busy_tool_def(int pos_x, int pos_y, int bevel, int bkgd_colr, int frgd_colr);
int u_busy_tool_off(void);
int u_busy_tool_on(int init_timeout);
int u_cb_inq_state(int win_id, int pnl_id, int beg_idx, int end_idx, unsigned char *state);
int u_cb_set_state(int win_id, int pnl_id, int beg_idx, int end_idx, unsigned char *state);
int u_chk_box_inq(int win_id, int pnl_id, int index, unsigned char *disable, unsigned char *state);
int u_chk_box_set(int win_id, int pnl_id, int index, int disable, int state);
int u_decor_btn(int win_id, int ent_id, int position, int pnl_type, int pnl_id, int itm_idx, int ofst_x, int ofst_y, int btn_hght, int btn_wdth, int bevel, int pressed, int btn_off, int btn_on, int bev_high, int bev_low);
int u_decor_line(int win_id, int ent_id, int position, int pnl_type, int pnl_id, int itm_idx, int ofst_x, int ofst_y, int color, int line_width, int close, int num_pnts, ipnt_2d *pnts);
int u_disable_wnds(int inp_opt, int win_id, int mod_cursor);
int u_draw_pixel(int win_id, int x_min, int y_min, int x_max, int y_max, char *pixels);
int u_get_max_win_id(int *win_id);
int u_get_win_scale(double *x_scale, double *y_scale);
int u_help_add(int code, int level, char *file_path, char *file_prfx);
int u_help_del(int code);
int u_help_init_cmd(int cmd_code, int cmd_type);
int u_help_win_def(int chars_x, int chars_y, int top_mrgn, int btm_mrgn, int side_mrgn, int title_ofst, int pos_x, int pos_y, int dcr_btn_hgt, int bevel, int btn_hgt, int bdr_off_colr, int bdr_on_colr, int bdr_bev_high, int bdr_bev_low, int btn_txt_off, int btn_txt_on, int btn_off_colr, int btn_on_colr, int btn_bev_high, int btn_bev_low, int text_colr, int key_word_colr, int bkgd_colr, char *win_font, char *btn_font, char *hdr_font, char *txt_font, int idx_btn_hgt, char *idx_font);
int u_inq_colr_num(int num, unsigned char *red, unsigned char *green, unsigned char *blue);
int u_list_add(int win_id, int list_id, char *value, int selected);
int u_list_asgn_cmd(int win_id, int list_id, int cmd_code);
int u_list_del(int win_id, int list_id, int index);
int u_list_empty(int win_id, int list_id);
int u_list_get_idx(int win_id, int list_id, char *string, int *index);
int u_list_get_str(int win_id, int list_id, int index, char *string);
int u_list_inq(int win_id, int list_id, int *count, int *list[]);
int u_list_itm_cnt(int win_id, int list_id, int *count);
int u_list_mlt_add(int win_id, int list_id, int add_cnt, char **value, unsigned char *selected);
int u_list_mod(int win_id, int list_id, int index, char *value, int selected);
int u_list_reset(int win_id, int list_id);
int u_list_set_cmd(int win_id, int list_id, int cmd_code);
int u_list_set_status(int win_id, int list_id, int index, int selected);
int u_list_show_idx(int win_id, int list_id, int index);
int u_load_icon(char *filename, int group_id);
int u_load_menu(char *name, int offset);
int u_menu_vx(int disp_menu);
int u_move_cursor(int src_x, int src_y, int dst_x, int dst_y, int delay);
int u_pnl_inq_avail(int win_id, int pnl_id, int pnl_type, int *avail);
int u_pos_cursor(int pos_x, int pos_y);
int u_reg_esc_cmd(int tid, int pid, int cmd, int sig, int send_cmd);
int u_reg_opn_cls_cmd(int task_id, int cmd_code);
int u_reg_quit_cmd(int task_id, int cmd_type, int code);
int u_replay_event_uif(int win_id, int win_type, int chld_id1, int chld_id2, int ie_code, int ie_flags, int ie_shiftmask, int ie_locx, int ie_locy);
int u_replay_state(char *filename, int state);
int u_restore_wnds(int mod_cursor);
int u_rm_decor(int win_id, int dec_type, int start_id, int end_id);
int u_rm_pnl(int win_id, int pnl_type, int pnl_id);
int u_set_act_pnl_level(int level, int act_opt);
int u_set_base_win_id(int base_win);
int u_set_btn_colr(int btn_off, int btn_on, int bev_high, int bev_low, int red, int green, int blue);
int u_set_cms(int cms_idx, int ofst, int size, unsigned char *r, unsigned char *g, unsigned char *b);
int u_set_colr_num(int num, int red, int green, int blue);
int u_set_highlight(int state, int color);
int u_set_pnl_state(int win_id, int pnl_id, int pnl_type, int act_opt);
int u_set_txt_out(int win_id, int out_id, char *string);
int u_set_win_colrs(int fg_red, int fg_green, int fg_blue, int bg_red, int bg_green, int bg_blue);
int u_set_win_side(int side);
int u_tty_clr(int win_id, int tty_id);
int u_tty_erase(int win_id, int tty_id, int unit, int count, int opt);
int u_tty_input(int win_id, int tty_id, char *string);
int u_tty_mode(int win_id, int tty_id, int trigger, int inp_mask);
int u_tty_print(int win_id, int tty_id, char *string);
int u_tty_state(int win_id, int tty_id, int enable);
int u_tty_trigger(int win_id, int tty_id);
int u_txt_edit_asgn_cmd(int win_id, int txt_id, int cmd_code);
int u_txt_edit_clr(int win_id, int txt_id, int start, int end);
int u_txt_edit_inq(int win_id, int txt_id, int start, int end, char **buf, int *len);
int u_txt_edit_ins(int win_id, int txt_id, int pos, char *buf);
int u_txt_edit_pos(int win_id, int txt_id, int pos);
int u_txt_edit_set(int win_id, int txt_id, int start, int end, char *buf);
int u_txt_in_add_field(int win_id, int txt_id, int field_id, int position, int itm_idx, int ofst_x, int ofst_y, char *prompt, char *value, int max_len, int disp_len, int inp_mask, int out_mask, char *mask_char);
int u_txt_in_inq_field(int win_id, int inp_id, int field_id, char *value);
int u_txt_in_mode(int win_id, int inp_id, int field_id, int inp_mask, int out_mask, char *mask_char);
int u_txt_in_prmpt(int win_id, int inp_id, int field_id, char *prompt);
int u_txt_in_value(int win_id, int inp_id, int field_id, char *value);
int u_unasgn_cmd_key(int trig_type, int trig_value);
int u_unload_icon(char *filename);
int u_win_add_btns(int win_id, int btn_id, int level, int type, int row_cnt, int col_cnt, int position, int pnl_type, int pnl_id, int itm_idx, int ofst_x, int ofst_y, int btn_hght, int btn_wdth, int bevel, int horz_spc, int vert_spc, int btn_off, int btn_on, int bev_high, int bev_low, int txt_on_colr, int txt_off_colr, int icon_bkgd, char *font);
int u_win_add_chk_box(int win_id, int cb_id, int level, int exclusive, int row_cnt, int col_cnt, int position, int pnl_type, int pnl_id, int itm_idx, int ofst_x, int ofst_y, int btn_hght, int btn_wdth, int bevel, int horz_spc, int vert_spc, int line_wdth, int btn_off, int btn_on, int bev_high, int bev_low, int line_colr);
int u_win_add_list(int win_id, int list_id, int level, int type, int position, int pnl_type, int pnl_id, int itm_idx, int ofst_x, int ofst_y, int height, int width, int max_cnt, int txt_len, int bevel, int vert_gap, int border, char *font, int bg_colr, int bg_in_colr, int hgh_colr, int low_colr, int txt_colr, int txt_in_colr);
int u_win_add_list2(int win_id, int list_id, int level, int type, int position, int pnl_type, int pnl_id, int itm_idx, int ofst_x, int ofst_y, int height, int width, int row_cnt, int col_cnt, int txt_len, int bevel, int vert_gap, int horz_gap, char *font, int bg_colr, int bg_in_colr, int hgh_colr, int low_colr, int txt_colr, int txt_in_colr);
int u_win_add_tty(int win_id, int tty_id, int cmd_code, int level, int trigger, int inp_mask, int scrollbar, int position, int pnl_type, int pnl_id, int itm_idx, int ofst_x, int ofst_y, int height, int width, char *font);
int u_win_add_txt_edit(int win_id, int txt_id, int level, int scrollbar, int position, int pnl_type, int pnl_id, int itm_idx, int offset_x, int offset_y, int height, int width, char *font);
int u_win_add_txt_in(int win_id, int txt_id, int level, int position, int pnl_type, int pnl_id, int itm_idx, int ofst_x, int ofst_y, int height, int width, char *font);
int u_win_add_txt_out(int win_id, int txt_id, int position, int pnl_type, int pnl_id, int itm_idx, int ofst_x, int ofst_y, int height, int width, char *string, char *font, int fg_colr, int bg_colr);
int u_win_create(int owner, int win_id, int height, int width, int position, int pos_opt, int corner, int pos_x, int pos_y, int display, int mem_res, int type, int opt_flag, int jump_cursor, int label, char *font, char *title);
int u_win_destroy(int win_id);
int u_win_inq_avail(int win_id, int *avail);
int u_win_inq_state(int win_id, int *state);
int u_win_loc_pos(int win_id, int pos_x, int pos_y);
int u_win_set_rel(int win_id, int position, int pos_opt, int corner, int pos_x, int pos_y);
int u_win_show(int inp_id, int disp_opt);
int u_win_shutdown(int task);
int u_win_title(int win_id, char *title);
#endif

#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* __openvx_uif_h */
