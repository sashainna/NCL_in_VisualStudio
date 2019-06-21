
create table displst
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	dpt_0	number(80,40),
	dpt_1	number(80,40),
	dpt_2	number(80,40)
);

create table tesslst
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	tpt	number(80,40)
);

create table boxlst
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	bx	number(80,40)
);

create table xyzbylst
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	xyzbndy	number(80,40)
);

create table uvbylst
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	uvbndy	number(80,40)
);

create table uvboxlst
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	uvbx	number(80,40)
);

create table point
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	markertype	number(12),
	snap_node	number(2),
	pt_0	number(80,40),
	pt_1	number(80,40),
	pt_2	number(80,40)
);

create table line
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	spt_0	number(80,40),
	spt_1	number(80,40),
	spt_2	number(80,40),
	ept_0	number(80,40),
	ept_1	number(80,40),
	ept_2	number(80,40)
);

create table circle
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	radius	number(80,40),
	dang	number(80,40),
	center_0	number(80,40),
	center_1	number(80,40),
	center_2	number(80,40),
	svec_0	number(80,40),
	svec_1	number(80,40),
	svec_2	number(80,40),
	nvec_0	number(80,40),
	nvec_1	number(80,40)
);
alter table circle add
(
	nvec_2	number(80,40)
);

create table conic
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	type	number(12),
	invariants_0	number(80,40),
	invariants_1	number(80,40),
	tfmat_0_0	number(80,40),
	tfmat_0_1	number(80,40),
	tfmat_0_2	number(80,40),
	tfmat_1_0	number(80,40),
	tfmat_1_1	number(80,40),
	tfmat_1_2	number(80,40),
	tfmat_2_0	number(80,40)
);
alter table conic add
(
	tfmat_2_1	number(80,40),
	tfmat_2_2	number(80,40),
	tfmat_3_0	number(80,40),
	tfmat_3_1	number(80,40),
	tfmat_3_2	number(80,40),
	t0	number(80,40),
	t1	number(80,40)
);

create table cid
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	crvid_rel	number(8) not null,
	crvid_key	number(8) not null,
	reverse	number(2),
	endparam	number(80,40)
);

create table compcrv
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	closdinu	number(12),
	arclen	number(80,40),
	planar	number(2),
	open	number(2),
	continuity	number(12),
	fcolor	number(12),
	t0	number(80,40),
	t1	number(80,40),
	addflg	number(12)
);

create table pt
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	pt_0	number(80,40),
	pt_1	number(80,40),
	pt_2	number(80,40)
);

create table wt
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	wt	number(80,40)
);

create table t
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	t	number(80,40)
);

create table bsplcrv
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	inverted	number(2),
	planar	number(2),
	open	number(2),
	k	number(12),
	n	number(12),
	t0	number(80,40),
	t1	number(80,40)
);

create table rbsplcrv
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	planar	number(2),
	open	number(2),
	closdinu	number(12),
	k	number(12),
	n	number(12),
	t0	number(80,40),
	t1	number(80,40)
);

create table uvcvonsf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	bskey_rel	number(8) not null,
	bskey_key	number(8) not null,
	dummy	number(12),
	planar	number(2),
	open	number(2),
	closdinu	number(12),
	k	number(12),
	n	number(12),
	t0	number(80,40),
	t1	number(80,40)
);

create table agcrv
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(8),
	subscr	number(12),
	crvaddr	number(12),
	closdinu	number(12)
);

create table poly
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	fcolor	number(12),
	numvtx	number(12),
	vertex_0_0	number(80,40),
	vertex_0_1	number(80,40),
	vertex_0_2	number(80,40),
	vertex_1_0	number(80,40),
	vertex_1_1	number(80,40),
	vertex_1_2	number(80,40),
	vertex_2_0	number(80,40),
	vertex_2_1	number(80,40)
);
alter table poly add
(
	vertex_2_2	number(80,40),
	vertex_3_0	number(80,40),
	vertex_3_1	number(80,40),
	vertex_3_2	number(80,40),
	vertex_4_0	number(80,40),
	vertex_4_1	number(80,40),
	vertex_4_2	number(80,40),
	vertex_5_0	number(80,40),
	vertex_5_1	number(80,40),
	vertex_5_2	number(80,40),
	vertex_6_0	number(80,40),
	vertex_6_1	number(80,40),
	vertex_6_2	number(80,40),
	vertex_7_0	number(80,40),
	vertex_7_1	number(80,40),
	vertex_7_2	number(80,40),
	vertex_8_0	number(80,40),
	vertex_8_1	number(80,40),
	vertex_8_2	number(80,40),
	vertex_9_0	number(80,40),
	vertex_9_1	number(80,40)
);
alter table poly add
(
	vertex_9_2	number(80,40),
	vertex_10_0	number(80,40),
	vertex_10_1	number(80,40),
	vertex_10_2	number(80,40),
	vertex_11_0	number(80,40),
	vertex_11_1	number(80,40),
	vertex_11_2	number(80,40),
	vertex_12_0	number(80,40),
	vertex_12_1	number(80,40),
	vertex_12_2	number(80,40),
	vertex_13_0	number(80,40),
	vertex_13_1	number(80,40),
	vertex_13_2	number(80,40),
	vertex_14_0	number(80,40),
	vertex_14_1	number(80,40),
	vertex_14_2	number(80,40),
	vertex_15_0	number(80,40),
	vertex_15_1	number(80,40),
	vertex_15_2	number(80,40),
	vertex_16_0	number(80,40),
	vertex_16_1	number(80,40)
);
alter table poly add
(
	vertex_16_2	number(80,40),
	vertex_17_0	number(80,40),
	vertex_17_1	number(80,40),
	vertex_17_2	number(80,40),
	vertex_18_0	number(80,40),
	vertex_18_1	number(80,40),
	vertex_18_2	number(80,40),
	vertex_19_0	number(80,40),
	vertex_19_1	number(80,40),
	vertex_19_2	number(80,40),
	vertex_20_0	number(80,40),
	vertex_20_1	number(80,40),
	vertex_20_2	number(80,40),
	vertex_21_0	number(80,40),
	vertex_21_1	number(80,40),
	vertex_21_2	number(80,40),
	vertex_22_0	number(80,40),
	vertex_22_1	number(80,40),
	vertex_22_2	number(80,40),
	vertex_23_0	number(80,40),
	vertex_23_1	number(80,40)
);
alter table poly add
(
	vertex_23_2	number(80,40),
	vertex_24_0	number(80,40),
	vertex_24_1	number(80,40),
	vertex_24_2	number(80,40),
	vertex_25_0	number(80,40),
	vertex_25_1	number(80,40),
	vertex_25_2	number(80,40),
	vertex_26_0	number(80,40),
	vertex_26_1	number(80,40),
	vertex_26_2	number(80,40),
	vertex_27_0	number(80,40),
	vertex_27_1	number(80,40),
	vertex_27_2	number(80,40),
	vertex_28_0	number(80,40),
	vertex_28_1	number(80,40),
	vertex_28_2	number(80,40),
	vertex_29_0	number(80,40),
	vertex_29_1	number(80,40),
	vertex_29_2	number(80,40),
	vertex_30_0	number(80,40),
	vertex_30_1	number(80,40)
);
alter table poly add
(
	vertex_30_2	number(80,40),
	vertex_31_0	number(80,40),
	vertex_31_1	number(80,40),
	vertex_31_2	number(80,40),
	vertex_32_0	number(80,40),
	vertex_32_1	number(80,40),
	vertex_32_2	number(80,40),
	vertex_33_0	number(80,40),
	vertex_33_1	number(80,40),
	vertex_33_2	number(80,40),
	vertex_34_0	number(80,40),
	vertex_34_1	number(80,40),
	vertex_34_2	number(80,40),
	vertex_35_0	number(80,40),
	vertex_35_1	number(80,40),
	vertex_35_2	number(80,40),
	vertex_36_0	number(80,40),
	vertex_36_1	number(80,40),
	vertex_36_2	number(80,40),
	vertex_37_0	number(80,40),
	vertex_37_1	number(80,40)
);
alter table poly add
(
	vertex_37_2	number(80,40),
	vertex_38_0	number(80,40),
	vertex_38_1	number(80,40),
	vertex_38_2	number(80,40),
	vertex_39_0	number(80,40),
	vertex_39_1	number(80,40),
	vertex_39_2	number(80,40),
	vertex_40_0	number(80,40),
	vertex_40_1	number(80,40),
	vertex_40_2	number(80,40),
	vertex_41_0	number(80,40),
	vertex_41_1	number(80,40),
	vertex_41_2	number(80,40),
	vertex_42_0	number(80,40),
	vertex_42_1	number(80,40),
	vertex_42_2	number(80,40),
	vertex_43_0	number(80,40),
	vertex_43_1	number(80,40),
	vertex_43_2	number(80,40),
	vertex_44_0	number(80,40),
	vertex_44_1	number(80,40)
);
alter table poly add
(
	vertex_44_2	number(80,40),
	vertex_45_0	number(80,40),
	vertex_45_1	number(80,40),
	vertex_45_2	number(80,40),
	vertex_46_0	number(80,40),
	vertex_46_1	number(80,40),
	vertex_46_2	number(80,40),
	vertex_47_0	number(80,40),
	vertex_47_1	number(80,40),
	vertex_47_2	number(80,40),
	vertex_48_0	number(80,40),
	vertex_48_1	number(80,40),
	vertex_48_2	number(80,40),
	vertex_49_0	number(80,40),
	vertex_49_1	number(80,40),
	vertex_49_2	number(80,40),
	vertex_50_0	number(80,40),
	vertex_50_1	number(80,40),
	vertex_50_2	number(80,40),
	vertex_51_0	number(80,40),
	vertex_51_1	number(80,40)
);
alter table poly add
(
	vertex_51_2	number(80,40),
	vertex_52_0	number(80,40),
	vertex_52_1	number(80,40),
	vertex_52_2	number(80,40),
	vertex_53_0	number(80,40),
	vertex_53_1	number(80,40),
	vertex_53_2	number(80,40),
	vertex_54_0	number(80,40),
	vertex_54_1	number(80,40),
	vertex_54_2	number(80,40),
	vertex_55_0	number(80,40),
	vertex_55_1	number(80,40),
	vertex_55_2	number(80,40),
	vertex_56_0	number(80,40),
	vertex_56_1	number(80,40),
	vertex_56_2	number(80,40),
	vertex_57_0	number(80,40),
	vertex_57_1	number(80,40),
	vertex_57_2	number(80,40),
	vertex_58_0	number(80,40),
	vertex_58_1	number(80,40)
);
alter table poly add
(
	vertex_58_2	number(80,40),
	vertex_59_0	number(80,40),
	vertex_59_1	number(80,40),
	vertex_59_2	number(80,40),
	vertex_60_0	number(80,40),
	vertex_60_1	number(80,40),
	vertex_60_2	number(80,40),
	vertex_61_0	number(80,40),
	vertex_61_1	number(80,40),
	vertex_61_2	number(80,40),
	vertex_62_0	number(80,40),
	vertex_62_1	number(80,40),
	vertex_62_2	number(80,40),
	vertex_63_0	number(80,40),
	vertex_63_1	number(80,40),
	vertex_63_2	number(80,40),
	vertex_64_0	number(80,40),
	vertex_64_1	number(80,40),
	vertex_64_2	number(80,40),
	vertex_65_0	number(80,40),
	vertex_65_1	number(80,40)
);
alter table poly add
(
	vertex_65_2	number(80,40),
	vertex_66_0	number(80,40),
	vertex_66_1	number(80,40),
	vertex_66_2	number(80,40),
	vertex_67_0	number(80,40),
	vertex_67_1	number(80,40),
	vertex_67_2	number(80,40),
	vertex_68_0	number(80,40),
	vertex_68_1	number(80,40),
	vertex_68_2	number(80,40),
	vertex_69_0	number(80,40),
	vertex_69_1	number(80,40),
	vertex_69_2	number(80,40),
	vertex_70_0	number(80,40),
	vertex_70_1	number(80,40),
	vertex_70_2	number(80,40),
	vertex_71_0	number(80,40),
	vertex_71_1	number(80,40),
	vertex_71_2	number(80,40),
	vertex_72_0	number(80,40),
	vertex_72_1	number(80,40)
);
alter table poly add
(
	vertex_72_2	number(80,40),
	vertex_73_0	number(80,40),
	vertex_73_1	number(80,40),
	vertex_73_2	number(80,40),
	vertex_74_0	number(80,40),
	vertex_74_1	number(80,40),
	vertex_74_2	number(80,40),
	vertex_75_0	number(80,40),
	vertex_75_1	number(80,40),
	vertex_75_2	number(80,40),
	vertex_76_0	number(80,40),
	vertex_76_1	number(80,40),
	vertex_76_2	number(80,40),
	vertex_77_0	number(80,40),
	vertex_77_1	number(80,40),
	vertex_77_2	number(80,40),
	vertex_78_0	number(80,40),
	vertex_78_1	number(80,40),
	vertex_78_2	number(80,40),
	vertex_79_0	number(80,40),
	vertex_79_1	number(80,40)
);
alter table poly add
(
	vertex_79_2	number(80,40),
	vertex_80_0	number(80,40),
	vertex_80_1	number(80,40),
	vertex_80_2	number(80,40),
	vertex_81_0	number(80,40),
	vertex_81_1	number(80,40),
	vertex_81_2	number(80,40),
	vertex_82_0	number(80,40),
	vertex_82_1	number(80,40),
	vertex_82_2	number(80,40),
	vertex_83_0	number(80,40),
	vertex_83_1	number(80,40),
	vertex_83_2	number(80,40),
	vertex_84_0	number(80,40),
	vertex_84_1	number(80,40),
	vertex_84_2	number(80,40),
	vertex_85_0	number(80,40),
	vertex_85_1	number(80,40),
	vertex_85_2	number(80,40),
	vertex_86_0	number(80,40),
	vertex_86_1	number(80,40)
);
alter table poly add
(
	vertex_86_2	number(80,40),
	vertex_87_0	number(80,40),
	vertex_87_1	number(80,40),
	vertex_87_2	number(80,40),
	vertex_88_0	number(80,40),
	vertex_88_1	number(80,40),
	vertex_88_2	number(80,40),
	vertex_89_0	number(80,40),
	vertex_89_1	number(80,40),
	vertex_89_2	number(80,40),
	vertex_90_0	number(80,40),
	vertex_90_1	number(80,40),
	vertex_90_2	number(80,40),
	vertex_91_0	number(80,40),
	vertex_91_1	number(80,40),
	vertex_91_2	number(80,40),
	vertex_92_0	number(80,40),
	vertex_92_1	number(80,40),
	vertex_92_2	number(80,40),
	vertex_93_0	number(80,40),
	vertex_93_1	number(80,40)
);
alter table poly add
(
	vertex_93_2	number(80,40),
	vertex_94_0	number(80,40),
	vertex_94_1	number(80,40),
	vertex_94_2	number(80,40),
	vertex_95_0	number(80,40),
	vertex_95_1	number(80,40),
	vertex_95_2	number(80,40),
	vertex_96_0	number(80,40),
	vertex_96_1	number(80,40),
	vertex_96_2	number(80,40),
	vertex_97_0	number(80,40),
	vertex_97_1	number(80,40),
	vertex_97_2	number(80,40),
	vertex_98_0	number(80,40),
	vertex_98_1	number(80,40),
	vertex_98_2	number(80,40),
	vertex_99_0	number(80,40),
	vertex_99_1	number(80,40),
	vertex_99_2	number(80,40),
	vertex_100_0	number(80,40),
	vertex_100_1	number(80,40)
);
alter table poly add
(
	vertex_100_2	number(80,40),
	vertex_101_0	number(80,40),
	vertex_101_1	number(80,40),
	vertex_101_2	number(80,40),
	vertex_102_0	number(80,40),
	vertex_102_1	number(80,40),
	vertex_102_2	number(80,40),
	vertex_103_0	number(80,40),
	vertex_103_1	number(80,40),
	vertex_103_2	number(80,40),
	vertex_104_0	number(80,40),
	vertex_104_1	number(80,40),
	vertex_104_2	number(80,40),
	vertex_105_0	number(80,40),
	vertex_105_1	number(80,40),
	vertex_105_2	number(80,40),
	vertex_106_0	number(80,40),
	vertex_106_1	number(80,40),
	vertex_106_2	number(80,40),
	vertex_107_0	number(80,40),
	vertex_107_1	number(80,40)
);
alter table poly add
(
	vertex_107_2	number(80,40),
	vertex_108_0	number(80,40),
	vertex_108_1	number(80,40),
	vertex_108_2	number(80,40),
	vertex_109_0	number(80,40),
	vertex_109_1	number(80,40),
	vertex_109_2	number(80,40),
	vertex_110_0	number(80,40),
	vertex_110_1	number(80,40),
	vertex_110_2	number(80,40),
	vertex_111_0	number(80,40),
	vertex_111_1	number(80,40),
	vertex_111_2	number(80,40),
	vertex_112_0	number(80,40),
	vertex_112_1	number(80,40),
	vertex_112_2	number(80,40),
	vertex_113_0	number(80,40),
	vertex_113_1	number(80,40),
	vertex_113_2	number(80,40),
	vertex_114_0	number(80,40),
	vertex_114_1	number(80,40)
);
alter table poly add
(
	vertex_114_2	number(80,40),
	vertex_115_0	number(80,40),
	vertex_115_1	number(80,40),
	vertex_115_2	number(80,40),
	vertex_116_0	number(80,40),
	vertex_116_1	number(80,40),
	vertex_116_2	number(80,40),
	vertex_117_0	number(80,40),
	vertex_117_1	number(80,40),
	vertex_117_2	number(80,40),
	vertex_118_0	number(80,40),
	vertex_118_1	number(80,40),
	vertex_118_2	number(80,40),
	vertex_119_0	number(80,40),
	vertex_119_1	number(80,40),
	vertex_119_2	number(80,40),
	vertex_120_0	number(80,40),
	vertex_120_1	number(80,40),
	vertex_120_2	number(80,40),
	vertex_121_0	number(80,40),
	vertex_121_1	number(80,40)
);
alter table poly add
(
	vertex_121_2	number(80,40),
	vertex_122_0	number(80,40),
	vertex_122_1	number(80,40),
	vertex_122_2	number(80,40),
	vertex_123_0	number(80,40),
	vertex_123_1	number(80,40),
	vertex_123_2	number(80,40),
	vertex_124_0	number(80,40),
	vertex_124_1	number(80,40),
	vertex_124_2	number(80,40),
	vertex_125_0	number(80,40),
	vertex_125_1	number(80,40),
	vertex_125_2	number(80,40),
	vertex_126_0	number(80,40),
	vertex_126_1	number(80,40),
	vertex_126_2	number(80,40),
	vertex_127_0	number(80,40),
	vertex_127_1	number(80,40),
	vertex_127_2	number(80,40),
	vertex_128_0	number(80,40),
	vertex_128_1	number(80,40)
);
alter table poly add
(
	vertex_128_2	number(80,40),
	vertex_129_0	number(80,40),
	vertex_129_1	number(80,40),
	vertex_129_2	number(80,40),
	vertex_130_0	number(80,40),
	vertex_130_1	number(80,40),
	vertex_130_2	number(80,40),
	vertex_131_0	number(80,40),
	vertex_131_1	number(80,40),
	vertex_131_2	number(80,40),
	vertex_132_0	number(80,40),
	vertex_132_1	number(80,40),
	vertex_132_2	number(80,40),
	vertex_133_0	number(80,40),
	vertex_133_1	number(80,40),
	vertex_133_2	number(80,40),
	vertex_134_0	number(80,40),
	vertex_134_1	number(80,40),
	vertex_134_2	number(80,40),
	vertex_135_0	number(80,40),
	vertex_135_1	number(80,40)
);
alter table poly add
(
	vertex_135_2	number(80,40),
	vertex_136_0	number(80,40),
	vertex_136_1	number(80,40),
	vertex_136_2	number(80,40),
	vertex_137_0	number(80,40),
	vertex_137_1	number(80,40),
	vertex_137_2	number(80,40),
	vertex_138_0	number(80,40),
	vertex_138_1	number(80,40),
	vertex_138_2	number(80,40),
	vertex_139_0	number(80,40),
	vertex_139_1	number(80,40),
	vertex_139_2	number(80,40),
	vertex_140_0	number(80,40),
	vertex_140_1	number(80,40),
	vertex_140_2	number(80,40),
	vertex_141_0	number(80,40),
	vertex_141_1	number(80,40),
	vertex_141_2	number(80,40),
	vertex_142_0	number(80,40),
	vertex_142_1	number(80,40)
);
alter table poly add
(
	vertex_142_2	number(80,40),
	vertex_143_0	number(80,40),
	vertex_143_1	number(80,40),
	vertex_143_2	number(80,40),
	vertex_144_0	number(80,40),
	vertex_144_1	number(80,40),
	vertex_144_2	number(80,40),
	vertex_145_0	number(80,40),
	vertex_145_1	number(80,40),
	vertex_145_2	number(80,40),
	vertex_146_0	number(80,40),
	vertex_146_1	number(80,40),
	vertex_146_2	number(80,40),
	vertex_147_0	number(80,40),
	vertex_147_1	number(80,40),
	vertex_147_2	number(80,40),
	vertex_148_0	number(80,40),
	vertex_148_1	number(80,40),
	vertex_148_2	number(80,40),
	vertex_149_0	number(80,40),
	vertex_149_1	number(80,40)
);
alter table poly add
(
	vertex_149_2	number(80,40),
	vertex_150_0	number(80,40),
	vertex_150_1	number(80,40),
	vertex_150_2	number(80,40),
	vertex_151_0	number(80,40),
	vertex_151_1	number(80,40),
	vertex_151_2	number(80,40),
	vertex_152_0	number(80,40),
	vertex_152_1	number(80,40),
	vertex_152_2	number(80,40),
	vertex_153_0	number(80,40),
	vertex_153_1	number(80,40),
	vertex_153_2	number(80,40),
	vertex_154_0	number(80,40),
	vertex_154_1	number(80,40),
	vertex_154_2	number(80,40),
	vertex_155_0	number(80,40),
	vertex_155_1	number(80,40),
	vertex_155_2	number(80,40),
	vertex_156_0	number(80,40),
	vertex_156_1	number(80,40)
);
alter table poly add
(
	vertex_156_2	number(80,40),
	vertex_157_0	number(80,40),
	vertex_157_1	number(80,40),
	vertex_157_2	number(80,40),
	vertex_158_0	number(80,40),
	vertex_158_1	number(80,40),
	vertex_158_2	number(80,40),
	vertex_159_0	number(80,40),
	vertex_159_1	number(80,40),
	vertex_159_2	number(80,40),
	vertex_160_0	number(80,40),
	vertex_160_1	number(80,40),
	vertex_160_2	number(80,40),
	vertex_161_0	number(80,40),
	vertex_161_1	number(80,40),
	vertex_161_2	number(80,40),
	vertex_162_0	number(80,40),
	vertex_162_1	number(80,40),
	vertex_162_2	number(80,40),
	vertex_163_0	number(80,40),
	vertex_163_1	number(80,40)
);
alter table poly add
(
	vertex_163_2	number(80,40),
	vertex_164_0	number(80,40),
	vertex_164_1	number(80,40),
	vertex_164_2	number(80,40),
	vertex_165_0	number(80,40),
	vertex_165_1	number(80,40),
	vertex_165_2	number(80,40),
	vertex_166_0	number(80,40),
	vertex_166_1	number(80,40),
	vertex_166_2	number(80,40),
	vertex_167_0	number(80,40),
	vertex_167_1	number(80,40),
	vertex_167_2	number(80,40),
	vertex_168_0	number(80,40),
	vertex_168_1	number(80,40),
	vertex_168_2	number(80,40),
	vertex_169_0	number(80,40),
	vertex_169_1	number(80,40),
	vertex_169_2	number(80,40),
	vertex_170_0	number(80,40),
	vertex_170_1	number(80,40)
);
alter table poly add
(
	vertex_170_2	number(80,40),
	vertex_171_0	number(80,40),
	vertex_171_1	number(80,40),
	vertex_171_2	number(80,40),
	vertex_172_0	number(80,40),
	vertex_172_1	number(80,40),
	vertex_172_2	number(80,40),
	vertex_173_0	number(80,40),
	vertex_173_1	number(80,40),
	vertex_173_2	number(80,40),
	vertex_174_0	number(80,40),
	vertex_174_1	number(80,40),
	vertex_174_2	number(80,40),
	vertex_175_0	number(80,40),
	vertex_175_1	number(80,40),
	vertex_175_2	number(80,40),
	vertex_176_0	number(80,40),
	vertex_176_1	number(80,40),
	vertex_176_2	number(80,40),
	vertex_177_0	number(80,40),
	vertex_177_1	number(80,40)
);
alter table poly add
(
	vertex_177_2	number(80,40),
	vertex_178_0	number(80,40),
	vertex_178_1	number(80,40),
	vertex_178_2	number(80,40),
	vertex_179_0	number(80,40),
	vertex_179_1	number(80,40),
	vertex_179_2	number(80,40),
	vertex_180_0	number(80,40),
	vertex_180_1	number(80,40),
	vertex_180_2	number(80,40),
	vertex_181_0	number(80,40),
	vertex_181_1	number(80,40),
	vertex_181_2	number(80,40),
	vertex_182_0	number(80,40),
	vertex_182_1	number(80,40),
	vertex_182_2	number(80,40),
	vertex_183_0	number(80,40),
	vertex_183_1	number(80,40),
	vertex_183_2	number(80,40),
	vertex_184_0	number(80,40),
	vertex_184_1	number(80,40)
);
alter table poly add
(
	vertex_184_2	number(80,40),
	vertex_185_0	number(80,40),
	vertex_185_1	number(80,40),
	vertex_185_2	number(80,40),
	vertex_186_0	number(80,40),
	vertex_186_1	number(80,40),
	vertex_186_2	number(80,40),
	vertex_187_0	number(80,40),
	vertex_187_1	number(80,40),
	vertex_187_2	number(80,40),
	vertex_188_0	number(80,40),
	vertex_188_1	number(80,40),
	vertex_188_2	number(80,40),
	vertex_189_0	number(80,40),
	vertex_189_1	number(80,40),
	vertex_189_2	number(80,40),
	vertex_190_0	number(80,40),
	vertex_190_1	number(80,40),
	vertex_190_2	number(80,40),
	vertex_191_0	number(80,40),
	vertex_191_1	number(80,40)
);
alter table poly add
(
	vertex_191_2	number(80,40),
	vertex_192_0	number(80,40),
	vertex_192_1	number(80,40),
	vertex_192_2	number(80,40),
	vertex_193_0	number(80,40),
	vertex_193_1	number(80,40),
	vertex_193_2	number(80,40),
	vertex_194_0	number(80,40),
	vertex_194_1	number(80,40),
	vertex_194_2	number(80,40),
	vertex_195_0	number(80,40),
	vertex_195_1	number(80,40),
	vertex_195_2	number(80,40),
	vertex_196_0	number(80,40),
	vertex_196_1	number(80,40),
	vertex_196_2	number(80,40),
	vertex_197_0	number(80,40),
	vertex_197_1	number(80,40),
	vertex_197_2	number(80,40),
	vertex_198_0	number(80,40),
	vertex_198_1	number(80,40)
);
alter table poly add
(
	vertex_198_2	number(80,40),
	vertex_199_0	number(80,40),
	vertex_199_1	number(80,40),
	vertex_199_2	number(80,40)
);

create table polyline
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	dummy	number(12)
);

create table surfattr
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	use_count	number(12),
	color	number(12),
	layer	number(12),
	pen	number(12),
	line_style	number(12),
	line_weight	number(80,40),
	line_width	number(80,40),
	displayable	number(12),
	selectable	number(2),
	label_on	number(12),
	material	number(12),
	numupaths	number(12),
	numvpaths	number(12),
	ptsperucrv	number(12),
	ptspervcrv	number(12),
	ecolor	number(12),
	shaded	number(2),
	lucency	number(12)
);

create table tu
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	t	number(80,40)
);

create table tv
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	t	number(80,40)
);

create table sskey
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	ssid_rel	number(8) not null,
	ssid_key	number(8) not null
);

create table rbsplsrf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	rldnu	number(12),
	swapuv	number(12),
	rev_normal	number(2),
	closdinu	number(12),
	closdinv	number(12),
	offdist	number(12,4),
	ku	number(12),
	kv	number(12),
	nu	number(12),
	nv	number(12)
);
alter table rbsplsrf add
(
	primitive	number(12),
	prim_param_0	number(80,40),
	prim_param_1	number(80,40),
	prim_param_2	number(80,40),
	prim_param_3	number(80,40),
	prim_param_4	number(80,40),
	prim_param_5	number(80,40),
	prim_param_6	number(80,40),
	prim_param_7	number(80,40),
	prim_param_8	number(80,40),
	prim_param_9	number(80,40),
	prim_param_10	number(80,40),
	prim_param_11	number(80,40),
	prim_param_12	number(80,40),
	prim_param_13	number(80,40),
	prim_param_14	number(80,40),
	prim_param_15	number(80,40)
);

create table agsrf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(8),
	subscr	number(12),
	material	number(12),
	numupaths	number(12),
	numvpaths	number(12),
	ptsperucrv	number(12),
	ptspervcrv	number(12),
	rldnu	number(12),
	rldnv	number(12),
	rev_normal	number(2),
	srfaddr	number(12),
	closdinu	number(12),
	closdinv	number(12)
);

create table sdata
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	sdata	number(80,40)
);

create table netkey
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	id_rel	number(8) not null,
	id_key	number(8) not null
);

create table solid
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	type	number(12),
	closed	number(12),
	box_0	number(80,40),
	box_1	number(80,40),
	box_2	number(80,40),
	box_3	number(80,40),
	box_4	number(80,40),
	box_5	number(80,40)
);

create table agshell
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(8),
	subscr	number(12),
	material	number(12),
	shelladdr	number(12)
);

create table edge
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	edge	number(12)
);

create table body
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(8),
	subscr	number(12),
	pitch	number(80,40),
	name	char(16),
	id	number(12),
	body_number	number(12),
	color	number(12)
);

create table coordsys
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	type	number(12),
	xaxis_0	number(80,40),
	xaxis_1	number(80,40),
	xaxis_2	number(80,40),
	yaxis_0	number(80,40),
	yaxis_1	number(80,40),
	yaxis_2	number(80,40),
	zaxis_0	number(80,40),
	zaxis_1	number(80,40),
	zaxis_2	number(80,40),
	origin_0	number(80,40),
	origin_1	number(80,40),
	origin_2	number(80,40),
	z_depth	number(80,40),
	name	char(16)
);

create table member
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	id_rel	number(8) not null,
	id_key	number(8) not null
);

create table grouper
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	name	char(16)
);

create table drawing
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	name	char(16),
	drwsize	number(12),
	drwscale	number(80,40),
	drwunits	number(12),
	modscale	number(80,40),
	modunits	number(12),
	plotprec	number(80,40)
);

create table layers
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	layer	number(12)
);

create table layer
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	name	char(80),
	num	number(12),
	displayable	number(2),
	selectable	number(2)
);

create table light
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	index	number(12),
	type	number(12),
	intens	number(12),
	position_0	number(80,40),
	position_1	number(80,40),
	position_2	number(80,40),
	direction_0	number(80,40),
	direction_1	number(80,40),
	direction_2	number(80,40),
	attenuation_0	number(80,40),
	attenuation_1	number(80,40),
	attenuation_2	number(80,40),
	cone_angle	number(80,40),
	scale	number(80,40),
	space	number(12),
	exp	number(80,40),
	ambient_0	number(80,40),
	ambient_1	number(80,40)
);
alter table light add
(
	ambient_2	number(80,40),
	ambient_3	number(80,40)
);

create table attrdata
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	use_count	number(12),
	color	number(12),
	layer	number(12),
	pen	number(12),
	line_style	number(12),
	line_weight	number(80,40),
	line_width	number(80,40),
	displayable	number(12),
	selectable	number(2),
	label_on	number(12)
);

create table transf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	use_count	number(12),
	tfmat_0_0	number(80,40),
	tfmat_0_1	number(80,40),
	tfmat_0_2	number(80,40),
	tfmat_1_0	number(80,40),
	tfmat_1_1	number(80,40),
	tfmat_1_2	number(80,40),
	tfmat_2_0	number(80,40),
	tfmat_2_1	number(80,40),
	tfmat_2_2	number(80,40),
	tfmat_3_0	number(80,40),
	tfmat_3_1	number(80,40),
	tfmat_3_2	number(80,40)
);

create table attrmdl
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	use_count	number(12),
	color	number(12),
	layer	number(12),
	pen	number(12),
	line_style	number(12),
	line_weight	number(80,40),
	line_width	number(80,40),
	displayable	number(12),
	selectable	number(2),
	label_on	number(12)
);

create table mtrlmdl
(
	part_id	number(8),
	index	number(12),
	name_0	char(20),
	name_1	char(20),
	name_2	char(20),
	name_3	char(20),
	name_4	char(20),
	name_5	char(20),
	name_6	char(20),
	name_7	char(20),
	name_8	char(20),
	name_9	char(20),
	name_10	char(20),
	name_11	char(20),
	name_12	char(20),
	name_13	char(20),
	name_14	char(20),
	name_15	char(20),
	name_16	char(20),
	name_17	char(20),
	name_18	char(20),
	name_19	char(20)
);
alter table mtrlmdl add
(
	name_20	char(20),
	name_21	char(20),
	name_22	char(20),
	name_23	char(20),
	name_24	char(20),
	name_25	char(20),
	name_26	char(20),
	name_27	char(20),
	name_28	char(20),
	name_29	char(20),
	name_30	char(20),
	name_31	char(20),
	name_32	char(20),
	name_33	char(20),
	name_34	char(20),
	name_35	char(20),
	name_36	char(20),
	name_37	char(20),
	name_38	char(20),
	name_39	char(20),
	name_40	char(20),
	name_41	char(20)
);
alter table mtrlmdl add
(
	name_42	char(20),
	name_43	char(20),
	name_44	char(20),
	name_45	char(20),
	name_46	char(20),
	name_47	char(20),
	name_48	char(20),
	name_49	char(20),
	name_50	char(20),
	name_51	char(20),
	name_52	char(20),
	name_53	char(20),
	name_54	char(20),
	name_55	char(20),
	name_56	char(20),
	name_57	char(20),
	name_58	char(20),
	name_59	char(20),
	name_60	char(20),
	name_61	char(20),
	name_62	char(20),
	name_63	char(20)
);
alter table mtrlmdl add
(
	ka_0	number(80,40),
	ka_1	number(80,40),
	ka_2	number(80,40),
	ka_3	number(80,40),
	ka_4	number(80,40),
	ka_5	number(80,40),
	ka_6	number(80,40),
	ka_7	number(80,40),
	ka_8	number(80,40),
	ka_9	number(80,40),
	ka_10	number(80,40),
	ka_11	number(80,40),
	ka_12	number(80,40),
	ka_13	number(80,40),
	ka_14	number(80,40),
	ka_15	number(80,40),
	ka_16	number(80,40),
	ka_17	number(80,40),
	ka_18	number(80,40),
	ka_19	number(80,40),
	ka_20	number(80,40)
);
alter table mtrlmdl add
(
	ka_21	number(80,40),
	ka_22	number(80,40),
	ka_23	number(80,40),
	ka_24	number(80,40),
	ka_25	number(80,40),
	ka_26	number(80,40),
	ka_27	number(80,40),
	ka_28	number(80,40),
	ka_29	number(80,40),
	ka_30	number(80,40),
	ka_31	number(80,40),
	ka_32	number(80,40),
	ka_33	number(80,40),
	ka_34	number(80,40),
	ka_35	number(80,40),
	ka_36	number(80,40),
	ka_37	number(80,40),
	ka_38	number(80,40),
	ka_39	number(80,40),
	ka_40	number(80,40),
	ka_41	number(80,40)
);
alter table mtrlmdl add
(
	ka_42	number(80,40),
	ka_43	number(80,40),
	ka_44	number(80,40),
	ka_45	number(80,40),
	ka_46	number(80,40),
	ka_47	number(80,40),
	ka_48	number(80,40),
	ka_49	number(80,40),
	ka_50	number(80,40),
	ka_51	number(80,40),
	ka_52	number(80,40),
	ka_53	number(80,40),
	ka_54	number(80,40),
	ka_55	number(80,40),
	ka_56	number(80,40),
	ka_57	number(80,40),
	ka_58	number(80,40),
	ka_59	number(80,40),
	ka_60	number(80,40),
	ka_61	number(80,40),
	ka_62	number(80,40)
);
alter table mtrlmdl add
(
	ka_63	number(80,40),
	kd_0	number(80,40),
	kd_1	number(80,40),
	kd_2	number(80,40),
	kd_3	number(80,40),
	kd_4	number(80,40),
	kd_5	number(80,40),
	kd_6	number(80,40),
	kd_7	number(80,40),
	kd_8	number(80,40),
	kd_9	number(80,40),
	kd_10	number(80,40),
	kd_11	number(80,40),
	kd_12	number(80,40),
	kd_13	number(80,40),
	kd_14	number(80,40),
	kd_15	number(80,40),
	kd_16	number(80,40),
	kd_17	number(80,40),
	kd_18	number(80,40),
	kd_19	number(80,40)
);
alter table mtrlmdl add
(
	kd_20	number(80,40),
	kd_21	number(80,40),
	kd_22	number(80,40),
	kd_23	number(80,40),
	kd_24	number(80,40),
	kd_25	number(80,40),
	kd_26	number(80,40),
	kd_27	number(80,40),
	kd_28	number(80,40),
	kd_29	number(80,40),
	kd_30	number(80,40),
	kd_31	number(80,40),
	kd_32	number(80,40),
	kd_33	number(80,40),
	kd_34	number(80,40),
	kd_35	number(80,40),
	kd_36	number(80,40),
	kd_37	number(80,40),
	kd_38	number(80,40),
	kd_39	number(80,40),
	kd_40	number(80,40)
);
alter table mtrlmdl add
(
	kd_41	number(80,40),
	kd_42	number(80,40),
	kd_43	number(80,40),
	kd_44	number(80,40),
	kd_45	number(80,40),
	kd_46	number(80,40),
	kd_47	number(80,40),
	kd_48	number(80,40),
	kd_49	number(80,40),
	kd_50	number(80,40),
	kd_51	number(80,40),
	kd_52	number(80,40),
	kd_53	number(80,40),
	kd_54	number(80,40),
	kd_55	number(80,40),
	kd_56	number(80,40),
	kd_57	number(80,40),
	kd_58	number(80,40),
	kd_59	number(80,40),
	kd_60	number(80,40),
	kd_61	number(80,40)
);
alter table mtrlmdl add
(
	kd_62	number(80,40),
	kd_63	number(80,40),
	ks_0	number(80,40),
	ks_1	number(80,40),
	ks_2	number(80,40),
	ks_3	number(80,40),
	ks_4	number(80,40),
	ks_5	number(80,40),
	ks_6	number(80,40),
	ks_7	number(80,40),
	ks_8	number(80,40),
	ks_9	number(80,40),
	ks_10	number(80,40),
	ks_11	number(80,40),
	ks_12	number(80,40),
	ks_13	number(80,40),
	ks_14	number(80,40),
	ks_15	number(80,40),
	ks_16	number(80,40),
	ks_17	number(80,40),
	ks_18	number(80,40)
);
alter table mtrlmdl add
(
	ks_19	number(80,40),
	ks_20	number(80,40),
	ks_21	number(80,40),
	ks_22	number(80,40),
	ks_23	number(80,40),
	ks_24	number(80,40),
	ks_25	number(80,40),
	ks_26	number(80,40),
	ks_27	number(80,40),
	ks_28	number(80,40),
	ks_29	number(80,40),
	ks_30	number(80,40),
	ks_31	number(80,40),
	ks_32	number(80,40),
	ks_33	number(80,40),
	ks_34	number(80,40),
	ks_35	number(80,40),
	ks_36	number(80,40),
	ks_37	number(80,40),
	ks_38	number(80,40),
	ks_39	number(80,40)
);
alter table mtrlmdl add
(
	ks_40	number(80,40),
	ks_41	number(80,40),
	ks_42	number(80,40),
	ks_43	number(80,40),
	ks_44	number(80,40),
	ks_45	number(80,40),
	ks_46	number(80,40),
	ks_47	number(80,40),
	ks_48	number(80,40),
	ks_49	number(80,40),
	ks_50	number(80,40),
	ks_51	number(80,40),
	ks_52	number(80,40),
	ks_53	number(80,40),
	ks_54	number(80,40),
	ks_55	number(80,40),
	ks_56	number(80,40),
	ks_57	number(80,40),
	ks_58	number(80,40),
	ks_59	number(80,40),
	ks_60	number(80,40)
);
alter table mtrlmdl add
(
	ks_61	number(80,40),
	ks_62	number(80,40),
	ks_63	number(80,40),
	ks_r_0	number(80,40),
	ks_r_1	number(80,40),
	ks_r_2	number(80,40),
	ks_r_3	number(80,40),
	ks_r_4	number(80,40),
	ks_r_5	number(80,40),
	ks_r_6	number(80,40),
	ks_r_7	number(80,40),
	ks_r_8	number(80,40),
	ks_r_9	number(80,40),
	ks_r_10	number(80,40),
	ks_r_11	number(80,40),
	ks_r_12	number(80,40),
	ks_r_13	number(80,40),
	ks_r_14	number(80,40),
	ks_r_15	number(80,40),
	ks_r_16	number(80,40),
	ks_r_17	number(80,40)
);
alter table mtrlmdl add
(
	ks_r_18	number(80,40),
	ks_r_19	number(80,40),
	ks_r_20	number(80,40),
	ks_r_21	number(80,40),
	ks_r_22	number(80,40),
	ks_r_23	number(80,40),
	ks_r_24	number(80,40),
	ks_r_25	number(80,40),
	ks_r_26	number(80,40),
	ks_r_27	number(80,40),
	ks_r_28	number(80,40),
	ks_r_29	number(80,40),
	ks_r_30	number(80,40),
	ks_r_31	number(80,40),
	ks_r_32	number(80,40),
	ks_r_33	number(80,40),
	ks_r_34	number(80,40),
	ks_r_35	number(80,40),
	ks_r_36	number(80,40),
	ks_r_37	number(80,40),
	ks_r_38	number(80,40)
);
alter table mtrlmdl add
(
	ks_r_39	number(80,40),
	ks_r_40	number(80,40),
	ks_r_41	number(80,40),
	ks_r_42	number(80,40),
	ks_r_43	number(80,40),
	ks_r_44	number(80,40),
	ks_r_45	number(80,40),
	ks_r_46	number(80,40),
	ks_r_47	number(80,40),
	ks_r_48	number(80,40),
	ks_r_49	number(80,40),
	ks_r_50	number(80,40),
	ks_r_51	number(80,40),
	ks_r_52	number(80,40),
	ks_r_53	number(80,40),
	ks_r_54	number(80,40),
	ks_r_55	number(80,40),
	ks_r_56	number(80,40),
	ks_r_57	number(80,40),
	ks_r_58	number(80,40),
	ks_r_59	number(80,40)
);
alter table mtrlmdl add
(
	ks_r_60	number(80,40),
	ks_r_61	number(80,40),
	ks_r_62	number(80,40),
	ks_r_63	number(80,40),
	ks_g_0	number(80,40),
	ks_g_1	number(80,40),
	ks_g_2	number(80,40),
	ks_g_3	number(80,40),
	ks_g_4	number(80,40),
	ks_g_5	number(80,40),
	ks_g_6	number(80,40),
	ks_g_7	number(80,40),
	ks_g_8	number(80,40),
	ks_g_9	number(80,40),
	ks_g_10	number(80,40),
	ks_g_11	number(80,40),
	ks_g_12	number(80,40),
	ks_g_13	number(80,40),
	ks_g_14	number(80,40),
	ks_g_15	number(80,40),
	ks_g_16	number(80,40)
);
alter table mtrlmdl add
(
	ks_g_17	number(80,40),
	ks_g_18	number(80,40),
	ks_g_19	number(80,40),
	ks_g_20	number(80,40),
	ks_g_21	number(80,40),
	ks_g_22	number(80,40),
	ks_g_23	number(80,40),
	ks_g_24	number(80,40),
	ks_g_25	number(80,40),
	ks_g_26	number(80,40),
	ks_g_27	number(80,40),
	ks_g_28	number(80,40),
	ks_g_29	number(80,40),
	ks_g_30	number(80,40),
	ks_g_31	number(80,40),
	ks_g_32	number(80,40),
	ks_g_33	number(80,40),
	ks_g_34	number(80,40),
	ks_g_35	number(80,40),
	ks_g_36	number(80,40),
	ks_g_37	number(80,40)
);
alter table mtrlmdl add
(
	ks_g_38	number(80,40),
	ks_g_39	number(80,40),
	ks_g_40	number(80,40),
	ks_g_41	number(80,40),
	ks_g_42	number(80,40),
	ks_g_43	number(80,40),
	ks_g_44	number(80,40),
	ks_g_45	number(80,40),
	ks_g_46	number(80,40),
	ks_g_47	number(80,40),
	ks_g_48	number(80,40),
	ks_g_49	number(80,40),
	ks_g_50	number(80,40),
	ks_g_51	number(80,40),
	ks_g_52	number(80,40),
	ks_g_53	number(80,40),
	ks_g_54	number(80,40),
	ks_g_55	number(80,40),
	ks_g_56	number(80,40),
	ks_g_57	number(80,40),
	ks_g_58	number(80,40)
);
alter table mtrlmdl add
(
	ks_g_59	number(80,40),
	ks_g_60	number(80,40),
	ks_g_61	number(80,40),
	ks_g_62	number(80,40),
	ks_g_63	number(80,40),
	ks_b_0	number(80,40),
	ks_b_1	number(80,40),
	ks_b_2	number(80,40),
	ks_b_3	number(80,40),
	ks_b_4	number(80,40),
	ks_b_5	number(80,40),
	ks_b_6	number(80,40),
	ks_b_7	number(80,40),
	ks_b_8	number(80,40),
	ks_b_9	number(80,40),
	ks_b_10	number(80,40),
	ks_b_11	number(80,40),
	ks_b_12	number(80,40),
	ks_b_13	number(80,40),
	ks_b_14	number(80,40),
	ks_b_15	number(80,40)
);
alter table mtrlmdl add
(
	ks_b_16	number(80,40),
	ks_b_17	number(80,40),
	ks_b_18	number(80,40),
	ks_b_19	number(80,40),
	ks_b_20	number(80,40),
	ks_b_21	number(80,40),
	ks_b_22	number(80,40),
	ks_b_23	number(80,40),
	ks_b_24	number(80,40),
	ks_b_25	number(80,40),
	ks_b_26	number(80,40),
	ks_b_27	number(80,40),
	ks_b_28	number(80,40),
	ks_b_29	number(80,40),
	ks_b_30	number(80,40),
	ks_b_31	number(80,40),
	ks_b_32	number(80,40),
	ks_b_33	number(80,40),
	ks_b_34	number(80,40),
	ks_b_35	number(80,40),
	ks_b_36	number(80,40)
);
alter table mtrlmdl add
(
	ks_b_37	number(80,40),
	ks_b_38	number(80,40),
	ks_b_39	number(80,40),
	ks_b_40	number(80,40),
	ks_b_41	number(80,40),
	ks_b_42	number(80,40),
	ks_b_43	number(80,40),
	ks_b_44	number(80,40),
	ks_b_45	number(80,40),
	ks_b_46	number(80,40),
	ks_b_47	number(80,40),
	ks_b_48	number(80,40),
	ks_b_49	number(80,40),
	ks_b_50	number(80,40),
	ks_b_51	number(80,40),
	ks_b_52	number(80,40),
	ks_b_53	number(80,40),
	ks_b_54	number(80,40),
	ks_b_55	number(80,40),
	ks_b_56	number(80,40),
	ks_b_57	number(80,40)
);
alter table mtrlmdl add
(
	ks_b_58	number(80,40),
	ks_b_59	number(80,40),
	ks_b_60	number(80,40),
	ks_b_61	number(80,40),
	ks_b_62	number(80,40),
	ks_b_63	number(80,40),
	spec_exp_0	number(80,40),
	spec_exp_1	number(80,40),
	spec_exp_2	number(80,40),
	spec_exp_3	number(80,40),
	spec_exp_4	number(80,40),
	spec_exp_5	number(80,40),
	spec_exp_6	number(80,40),
	spec_exp_7	number(80,40),
	spec_exp_8	number(80,40),
	spec_exp_9	number(80,40),
	spec_exp_10	number(80,40),
	spec_exp_11	number(80,40),
	spec_exp_12	number(80,40),
	spec_exp_13	number(80,40),
	spec_exp_14	number(80,40)
);
alter table mtrlmdl add
(
	spec_exp_15	number(80,40),
	spec_exp_16	number(80,40),
	spec_exp_17	number(80,40),
	spec_exp_18	number(80,40),
	spec_exp_19	number(80,40),
	spec_exp_20	number(80,40),
	spec_exp_21	number(80,40),
	spec_exp_22	number(80,40),
	spec_exp_23	number(80,40),
	spec_exp_24	number(80,40),
	spec_exp_25	number(80,40),
	spec_exp_26	number(80,40),
	spec_exp_27	number(80,40),
	spec_exp_28	number(80,40),
	spec_exp_29	number(80,40),
	spec_exp_30	number(80,40),
	spec_exp_31	number(80,40),
	spec_exp_32	number(80,40),
	spec_exp_33	number(80,40),
	spec_exp_34	number(80,40),
	spec_exp_35	number(80,40)
);
alter table mtrlmdl add
(
	spec_exp_36	number(80,40),
	spec_exp_37	number(80,40),
	spec_exp_38	number(80,40),
	spec_exp_39	number(80,40),
	spec_exp_40	number(80,40),
	spec_exp_41	number(80,40),
	spec_exp_42	number(80,40),
	spec_exp_43	number(80,40),
	spec_exp_44	number(80,40),
	spec_exp_45	number(80,40),
	spec_exp_46	number(80,40),
	spec_exp_47	number(80,40),
	spec_exp_48	number(80,40),
	spec_exp_49	number(80,40),
	spec_exp_50	number(80,40),
	spec_exp_51	number(80,40),
	spec_exp_52	number(80,40),
	spec_exp_53	number(80,40),
	spec_exp_54	number(80,40),
	spec_exp_55	number(80,40),
	spec_exp_56	number(80,40)
);
alter table mtrlmdl add
(
	spec_exp_57	number(80,40),
	spec_exp_58	number(80,40),
	spec_exp_59	number(80,40),
	spec_exp_60	number(80,40),
	spec_exp_61	number(80,40),
	spec_exp_62	number(80,40),
	spec_exp_63	number(80,40)
);

create table dispattr
(
	part_id	number(8),
	consclr	number(12),
	conslnstyl	number(12),
	conswght	number(80,40),
	featclr	number(12),
	fea_decpl	number(12),
	fillclr	number(12),
	hidn_lines	number(2),
	view_in_rel	number(8) not null,
	view_in_key	number(8) not null,
	cpln_key_rel	number(8) not null,
	cpln_key_key	number(8) not null
);

create table drwmdl
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	use_count	number(12),
	drwsn_rel	number(8) not null,
	drwsn_key	number(8) not null,
	drwvp_rel	number(8) not null,
	drwvp_key	number(8) not null,
	drwvw_rel	number(8) not null,
	drwvw_key	number(8) not null,
	curdrw_rel	number(8) not null,
	curdrw_key	number(8) not null,
	modsname	char(16),
	aspect	number(80,40),
	unitsstr	char(100),
	drwsize	number(12),
	drwscale	number(80,40),
	drwunits	number(12),
	modscale	number(80,40),
	modunits	number(12),
	plotprec	number(80,40)
);

create table labelmdl
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	use_count	number(12),
	max	number(12),
	num	number(12),
	pf_0	char(21),
	pf_1	char(21),
	pf_2	char(21),
	pf_3	char(21),
	pf_4	char(21),
	pf_5	char(21),
	pf_6	char(21),
	pf_7	char(21),
	pf_8	char(21),
	pf_9	char(21),
	pf_10	char(21),
	pf_11	char(21),
	pf_12	char(21),
	pf_13	char(21),
	pf_14	char(21),
	pf_15	char(21)
);
alter table labelmdl add
(
	pf_16	char(21),
	pf_17	char(21),
	pf_18	char(21),
	pf_19	char(21),
	pf_20	char(21),
	pf_21	char(21),
	pf_22	char(21),
	pf_23	char(21),
	pf_24	char(21),
	pf_25	char(21),
	pf_26	char(21),
	pf_27	char(21),
	pf_28	char(21),
	pf_29	char(21),
	pf_30	char(21),
	pf_31	char(21),
	pf_32	char(21),
	pf_33	char(21),
	pf_34	char(21),
	pf_35	char(21),
	pf_36	char(21),
	pf_37	char(21)
);
alter table labelmdl add
(
	pf_38	char(21),
	pf_39	char(21),
	next_0	number(12),
	next_1	number(12),
	next_2	number(12),
	next_3	number(12),
	next_4	number(12),
	next_5	number(12),
	next_6	number(12),
	next_7	number(12),
	next_8	number(12),
	next_9	number(12),
	next_10	number(12),
	next_11	number(12),
	next_12	number(12),
	next_13	number(12),
	next_14	number(12),
	next_15	number(12),
	next_16	number(12),
	next_17	number(12),
	next_18	number(12)
);
alter table labelmdl add
(
	next_19	number(12),
	next_20	number(12),
	next_21	number(12),
	next_22	number(12),
	next_23	number(12),
	next_24	number(12),
	next_25	number(12),
	next_26	number(12),
	next_27	number(12),
	next_28	number(12),
	next_29	number(12),
	next_30	number(12),
	next_31	number(12),
	next_32	number(12),
	next_33	number(12),
	next_34	number(12),
	next_35	number(12),
	next_36	number(12),
	next_37	number(12),
	next_38	number(12),
	next_39	number(12),
	pfs_0	char(21)
);
alter table labelmdl add
(
	pfs_1	char(21),
	pfs_2	char(21),
	pfs_3	char(21),
	pfs_4	char(21),
	pfs_5	char(21),
	pfs_6	char(21),
	pfs_7	char(21),
	pfs_8	char(21),
	pfs_9	char(21),
	pfs_10	char(21),
	pfs_11	char(21),
	pfs_12	char(21),
	pfs_13	char(21),
	pfs_14	char(21),
	pfs_15	char(21),
	pfs_16	char(21),
	pfs_17	char(21),
	pfs_18	char(21),
	pfs_19	char(21),
	pfs_20	char(21),
	pfs_21	char(21),
	pfs_22	char(21)
);
alter table labelmdl add
(
	pfs_23	char(21),
	pfs_24	char(21),
	pfs_25	char(21),
	pfs_26	char(21),
	pfs_27	char(21),
	pfs_28	char(21),
	pfs_29	char(21),
	pfs_30	char(21),
	pfs_31	char(21),
	pfs_32	char(21),
	pfs_33	char(21),
	pfs_34	char(21),
	pfs_35	char(21),
	pfs_36	char(21),
	pfs_37	char(21),
	pfs_38	char(21),
	pfs_39	char(21),
	subscr_0	number(12),
	subscr_1	number(12),
	subscr_2	number(12),
	subscr_3	number(12)
);
alter table labelmdl add
(
	subscr_4	number(12),
	subscr_5	number(12),
	subscr_6	number(12),
	subscr_7	number(12),
	subscr_8	number(12),
	subscr_9	number(12),
	subscr_10	number(12),
	subscr_11	number(12),
	subscr_12	number(12),
	subscr_13	number(12),
	subscr_14	number(12),
	subscr_15	number(12),
	subscr_16	number(12),
	subscr_17	number(12),
	subscr_18	number(12),
	subscr_19	number(12),
	subscr_20	number(12),
	subscr_21	number(12),
	subscr_22	number(12),
	subscr_23	number(12),
	subscr_24	number(12)
);
alter table labelmdl add
(
	subscr_25	number(12),
	subscr_26	number(12),
	subscr_27	number(12),
	subscr_28	number(12),
	subscr_29	number(12),
	subscr_30	number(12),
	subscr_31	number(12),
	subscr_32	number(12),
	subscr_33	number(12),
	subscr_34	number(12),
	subscr_35	number(12),
	subscr_36	number(12),
	subscr_37	number(12),
	subscr_38	number(12),
	subscr_39	number(12),
	issub_0	number(12),
	issub_1	number(12),
	issub_2	number(12),
	issub_3	number(12),
	issub_4	number(12),
	issub_5	number(12)
);
alter table labelmdl add
(
	issub_6	number(12),
	issub_7	number(12),
	issub_8	number(12),
	issub_9	number(12),
	issub_10	number(12),
	issub_11	number(12),
	issub_12	number(12),
	issub_13	number(12),
	issub_14	number(12),
	issub_15	number(12),
	issub_16	number(12),
	issub_17	number(12),
	issub_18	number(12),
	issub_19	number(12),
	issub_20	number(12),
	issub_21	number(12),
	issub_22	number(12),
	issub_23	number(12),
	issub_24	number(12),
	issub_25	number(12),
	issub_26	number(12)
);
alter table labelmdl add
(
	issub_27	number(12),
	issub_28	number(12),
	issub_29	number(12),
	issub_30	number(12),
	issub_31	number(12),
	issub_32	number(12),
	issub_33	number(12),
	issub_34	number(12),
	issub_35	number(12),
	issub_36	number(12),
	issub_37	number(12),
	issub_38	number(12),
	issub_39	number(12),
	rel_0	number(12),
	rel_1	number(12),
	rel_2	number(12),
	rel_3	number(12),
	rel_4	number(12),
	rel_5	number(12),
	rel_6	number(12),
	rel_7	number(12)
);
alter table labelmdl add
(
	rel_8	number(12),
	rel_9	number(12),
	rel_10	number(12),
	rel_11	number(12),
	rel_12	number(12),
	rel_13	number(12),
	rel_14	number(12),
	rel_15	number(12),
	rel_16	number(12),
	rel_17	number(12),
	rel_18	number(12),
	rel_19	number(12),
	rel_20	number(12),
	rel_21	number(12),
	rel_22	number(12),
	rel_23	number(12),
	rel_24	number(12),
	rel_25	number(12),
	rel_26	number(12),
	rel_27	number(12),
	rel_28	number(12)
);
alter table labelmdl add
(
	rel_29	number(12),
	rel_30	number(12),
	rel_31	number(12),
	rel_32	number(12),
	rel_33	number(12),
	rel_34	number(12),
	rel_35	number(12),
	rel_36	number(12),
	rel_37	number(12),
	rel_38	number(12),
	rel_39	number(12),
	rel_40	number(12),
	rel_41	number(12),
	rel_42	number(12),
	rel_43	number(12),
	rel_44	number(12),
	rel_45	number(12),
	rel_46	number(12),
	rel_47	number(12),
	rel_48	number(12),
	rel_49	number(12)
);
alter table labelmdl add
(
	rel_50	number(12),
	rel_51	number(12),
	rel_52	number(12),
	rel_53	number(12),
	rel_54	number(12),
	rel_55	number(12),
	rel_56	number(12),
	rel_57	number(12),
	rel_58	number(12),
	rel_59	number(12),
	rel_60	number(12),
	rel_61	number(12),
	rel_62	number(12),
	rel_63	number(12),
	rel_64	number(12),
	rel_65	number(12),
	rel_66	number(12),
	rel_67	number(12),
	rel_68	number(12),
	rel_69	number(12),
	rel_70	number(12)
);
alter table labelmdl add
(
	rel_71	number(12),
	rel_72	number(12),
	rel_73	number(12),
	rel_74	number(12),
	rel_75	number(12),
	rel_76	number(12),
	rel_77	number(12),
	rel_78	number(12),
	rel_79	number(12),
	rel_80	number(12),
	rel_81	number(12),
	rel_82	number(12),
	rel_83	number(12),
	rel_84	number(12),
	rel_85	number(12),
	rel_86	number(12),
	rel_87	number(12),
	rel_88	number(12),
	rel_89	number(12),
	rel_90	number(12),
	rel_91	number(12)
);
alter table labelmdl add
(
	rel_92	number(12),
	rel_93	number(12),
	rel_94	number(12),
	rel_95	number(12),
	rel_96	number(12),
	rel_97	number(12),
	rel_98	number(12),
	rel_99	number(12),
	rel_100	number(12),
	rel_101	number(12),
	rel_102	number(12),
	rel_103	number(12),
	rel_104	number(12),
	rel_105	number(12),
	rel_106	number(12),
	rel_107	number(12),
	rel_108	number(12),
	rel_109	number(12),
	rel_110	number(12),
	rel_111	number(12),
	rel_112	number(12)
);
alter table labelmdl add
(
	rel_113	number(12),
	rel_114	number(12),
	rel_115	number(12),
	rel_116	number(12),
	rel_117	number(12),
	rel_118	number(12),
	rel_119	number(12),
	rel_120	number(12),
	rel_121	number(12),
	rel_122	number(12),
	rel_123	number(12),
	rel_124	number(12),
	rel_125	number(12),
	rel_126	number(12),
	rel_127	number(12),
	rel_128	number(12),
	rel_129	number(12),
	rel_130	number(12),
	rel_131	number(12),
	rel_132	number(12),
	rel_133	number(12)
);
alter table labelmdl add
(
	rel_134	number(12),
	rel_135	number(12),
	rel_136	number(12),
	rel_137	number(12),
	rel_138	number(12),
	rel_139	number(12),
	rel_140	number(12),
	rel_141	number(12),
	rel_142	number(12),
	rel_143	number(12),
	rel_144	number(12),
	rel_145	number(12),
	rel_146	number(12),
	rel_147	number(12),
	rel_148	number(12),
	rel_149	number(12),
	rel_150	number(12),
	rel_151	number(12),
	rel_152	number(12),
	rel_153	number(12),
	rel_154	number(12)
);
alter table labelmdl add
(
	rel_155	number(12),
	rel_156	number(12),
	rel_157	number(12),
	rel_158	number(12),
	rel_159	number(12),
	rel_160	number(12),
	rel_161	number(12),
	rel_162	number(12),
	rel_163	number(12),
	rel_164	number(12),
	rel_165	number(12),
	rel_166	number(12),
	rel_167	number(12),
	rel_168	number(12),
	rel_169	number(12),
	rel_170	number(12),
	rel_171	number(12),
	rel_172	number(12),
	rel_173	number(12),
	rel_174	number(12),
	rel_175	number(12)
);
alter table labelmdl add
(
	rel_176	number(12),
	rel_177	number(12),
	rel_178	number(12),
	rel_179	number(12),
	rel_180	number(12),
	rel_181	number(12),
	rel_182	number(12),
	rel_183	number(12),
	rel_184	number(12),
	rel_185	number(12),
	rel_186	number(12),
	rel_187	number(12),
	rel_188	number(12),
	rel_189	number(12),
	rel_190	number(12),
	rel_191	number(12),
	rel_192	number(12),
	rel_193	number(12),
	rel_194	number(12),
	rel_195	number(12),
	rel_196	number(12)
);
alter table labelmdl add
(
	rel_197	number(12),
	rel_198	number(12),
	rel_199	number(12),
	rel_200	number(12),
	rel_201	number(12),
	rel_202	number(12),
	rel_203	number(12),
	rel_204	number(12),
	rel_205	number(12),
	rel_206	number(12),
	rel_207	number(12),
	rel_208	number(12),
	rel_209	number(12),
	rel_210	number(12),
	rel_211	number(12),
	rel_212	number(12),
	rel_213	number(12),
	rel_214	number(12),
	rel_215	number(12),
	rel_216	number(12),
	rel_217	number(12)
);
alter table labelmdl add
(
	rel_218	number(12),
	rel_219	number(12),
	rel_220	number(12),
	rel_221	number(12),
	rel_222	number(12),
	rel_223	number(12),
	rel_224	number(12),
	rel_225	number(12),
	rel_226	number(12),
	rel_227	number(12),
	rel_228	number(12),
	rel_229	number(12),
	rel_230	number(12),
	rel_231	number(12),
	rel_232	number(12),
	rel_233	number(12),
	rel_234	number(12),
	rel_235	number(12),
	rel_236	number(12),
	rel_237	number(12),
	rel_238	number(12)
);
alter table labelmdl add
(
	rel_239	number(12),
	rel_240	number(12),
	rel_241	number(12),
	rel_242	number(12),
	rel_243	number(12),
	rel_244	number(12),
	rel_245	number(12),
	rel_246	number(12),
	rel_247	number(12),
	rel_248	number(12),
	rel_249	number(12),
	rel_250	number(12),
	rel_251	number(12),
	rel_252	number(12),
	rel_253	number(12),
	rel_254	number(12),
	rel_255	number(12)
);

create table viewdef
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	name	char(15),
	vtype	number(12),
	can_save	number(2),
	modified	number(2),
	projection	number(12),
	sav_ref_pt_0	number(80,40),
	sav_ref_pt_1	number(80,40),
	sav_ref_pt_2	number(80,40),
	sav_pln_norm_0	number(80,40),
	sav_pln_norm_1	number(80,40),
	sav_pln_norm_2	number(80,40),
	sav_up_vect_0	number(80,40),
	sav_up_vect_1	number(80,40),
	sav_up_vect_2	number(80,40),
	sav_eye_dist	number(80,40),
	sav_aperture	number(80,40),
	sav_front_clip	number(80,40),
	sav_back_clip	number(80,40)
);
alter table viewdef add
(
	sav_do_clip	number(2),
	cur_ref_pt_0	number(80,40),
	cur_ref_pt_1	number(80,40),
	cur_ref_pt_2	number(80,40),
	cur_pln_norm_0	number(80,40),
	cur_pln_norm_1	number(80,40),
	cur_pln_norm_2	number(80,40),
	cur_up_vect_0	number(80,40),
	cur_up_vect_1	number(80,40),
	cur_up_vect_2	number(80,40),
	cur_eye_dist	number(80,40),
	cur_aperture	number(80,40),
	cur_front_clip	number(80,40),
	cur_back_clip	number(80,40),
	cur_do_clip	number(2)
);

create table vport
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	name	char(15),
	xform	number(12),
	llf_0	number(80,40),
	llf_1	number(80,40),
	llf_2	number(80,40),
	urb_0	number(80,40),
	urb_1	number(80,40),
	urb_2	number(80,40),
	cur_view_rel	number(8) not null,
	cur_view_key	number(8) not null,
	disp_prio	number(12),
	input_prio	number(12),
	disp_all	number(2),
	bord_seg	number(12),
	aperture_on	number(2),
	v_axis_on	number(2),
	name_on	number(2),
	bord_on	number(2)
);
alter table vport add
(
	nverts	number(12),
	vertices_0	number(80,40),
	vertices_1	number(80,40),
	vertices_2	number(80,40),
	vertices_3	number(80,40),
	vertices_4	number(80,40),
	vertices_5	number(80,40),
	vertices_6	number(80,40),
	vertices_7	number(80,40),
	vertices_8	number(80,40),
	vertices_9	number(80,40),
	vertices_10	number(80,40),
	vertices_11	number(80,40),
	vertices_12	number(80,40),
	vertices_13	number(80,40),
	vertices_14	number(80,40),
	vertices_15	number(80,40),
	vertices_16	number(80,40),
	vertices_17	number(80,40),
	vertices_18	number(80,40),
	vertices_19	number(80,40)
);
alter table vport add
(
	vertices_20	number(80,40),
	vertices_21	number(80,40),
	vertices_22	number(80,40),
	vertices_23	number(80,40),
	vertices_24	number(80,40),
	vertices_25	number(80,40),
	vertices_26	number(80,40),
	vertices_27	number(80,40),
	vertices_28	number(80,40),
	vertices_29	number(80,40),
	vertices_30	number(80,40),
	vertices_31	number(80,40),
	vertices_32	number(80,40),
	vertices_33	number(80,40),
	vertices_34	number(80,40),
	vertices_35	number(80,40),
	vertices_36	number(80,40),
	vertices_37	number(80,40),
	vertices_38	number(80,40),
	vertices_39	number(80,40),
	vertices_40	number(80,40)
);
alter table vport add
(
	vertices_41	number(80,40),
	vertices_42	number(80,40),
	vertices_43	number(80,40),
	vertices_44	number(80,40),
	vertices_45	number(80,40),
	vertices_46	number(80,40),
	vertices_47	number(80,40),
	vertices_48	number(80,40),
	vertices_49	number(80,40),
	vertices_50	number(80,40),
	vertices_51	number(80,40),
	vertices_52	number(80,40),
	vertices_53	number(80,40),
	vertices_54	number(80,40),
	vertices_55	number(80,40),
	vertices_56	number(80,40),
	vertices_57	number(80,40),
	vertices_58	number(80,40),
	vertices_59	number(80,40),
	grid_seg	number(12),
	grid_on	number(2)
);
alter table vport add
(
	motion	number(2),
	disp_mode	number(12),
	wireframe	number(2)
);

create table screen
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	name	char(15),
	active	number(12),
	wstation	number(12),
	nvports	number(12),
	vports_0_rel	number(8) not null,
	vports_0_key	number(8) not null,
	vports_1_rel	number(8) not null,
	vports_1_key	number(8) not null,
	vports_2_rel	number(8) not null,
	vports_2_key	number(8) not null,
	vports_3_rel	number(8) not null,
	vports_3_key	number(8) not null,
	vports_4_rel	number(8) not null,
	vports_4_key	number(8) not null,
	vports_5_rel	number(8) not null,
	vports_5_key	number(8) not null,
	vports_6_rel	number(8) not null,
	vports_6_key	number(8) not null
);
alter table screen add
(
	vports_7_rel	number(8) not null,
	vports_7_key	number(8) not null,
	vports_8_rel	number(8) not null,
	vports_8_key	number(8) not null,
	vports_9_rel	number(8) not null,
	vports_9_key	number(8) not null,
	vports_10_rel	number(8) not null,
	vports_10_key	number(8) not null,
	vports_11_rel	number(8) not null,
	vports_11_key	number(8) not null,
	vports_12_rel	number(8) not null,
	vports_12_key	number(8) not null,
	vports_13_rel	number(8) not null,
	vports_13_key	number(8) not null,
	vports_14_rel	number(8) not null,
	vports_14_key	number(8) not null,
	vports_15_rel	number(8) not null,
	vports_15_key	number(8) not null,
	vports_16_rel	number(8) not null,
	vports_16_key	number(8) not null,
	vports_17_rel	number(8) not null,
	vports_17_key	number(8) not null
);
alter table screen add
(
	vports_18_rel	number(8) not null,
	vports_18_key	number(8) not null,
	vports_19_rel	number(8) not null,
	vports_19_key	number(8) not null
);

create table sysattr
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	use_count	number(12),
	color	number(12),
	layer	number(12),
	pen	number(12),
	line_style	number(12),
	line_weight	number(80,40),
	line_width	number(80,40),
	displayable	number(12),
	selectable	number(2),
	label_on	number(12)
);

create table geom
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	dummy_rel	number(8) not null,
	dummy_key	number(8) not null
);

create table text_nod
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	prompt	number(12),
	visibility	number(12),
	masterindx	number(12),
	text_key_rel	number(8) not null,
	text_key_key	number(8) not null
);

create table snap_nod
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	snap_key_rel	number(8) not null,
	snap_key_key	number(8) not null,
	nbr	number(12)
);

create table masters
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	mastr_key_rel	number(8) not null,
	mastr_key_key	number(8) not null,
	tfmat_0_0	number(80,40),
	tfmat_0_1	number(80,40),
	tfmat_0_2	number(80,40),
	tfmat_1_0	number(80,40),
	tfmat_1_1	number(80,40),
	tfmat_1_2	number(80,40),
	tfmat_2_0	number(80,40),
	tfmat_2_1	number(80,40),
	tfmat_2_2	number(80,40),
	tfmat_3_0	number(80,40),
	tfmat_3_1	number(80,40),
	tfmat_3_2	number(80,40)
);

create table inst
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	inst_key_rel	number(8) not null,
	inst_key_key	number(8) not null,
	tfmat_0_0	number(80,40),
	tfmat_0_1	number(80,40),
	tfmat_0_2	number(80,40),
	tfmat_1_0	number(80,40),
	tfmat_1_1	number(80,40),
	tfmat_1_2	number(80,40),
	tfmat_2_0	number(80,40),
	tfmat_2_1	number(80,40),
	tfmat_2_2	number(80,40),
	tfmat_3_0	number(80,40),
	tfmat_3_1	number(80,40),
	tfmat_3_2	number(80,40)
);

create table acon
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	con_key_rel	number(8) not null,
	con_key_key	number(8) not null
);

create table symbol
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	version	number(12),
	path	char(200)
);

create table instance
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12)
);

create table symattr
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	use_count	number(12),
	color	number(12),
	layer	number(12),
	pen	number(12),
	line_style	number(12),
	line_weight	number(80,40),
	line_width	number(80,40),
	displayable	number(12),
	selectable	number(2),
	label_on	number(12),
	see_snod	number(2),
	see_tnod	number(12)
);

create table ainst
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	inst_key_rel	number(8) not null,
	inst_key_key	number(8) not null
);

create table conector
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	pline_rel	number(8) not null,
	pline_key	number(8) not null
);

create table part_lis
(
	part_id	number(8),
	part_name	char(16),
	part_date	char(16)
);

create table geometry
(
	part_id	number(8),
	geom_type	char(16),
	geom_id	number(12),
	num_varl	number(12)
);

create table assocs
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	assoc_key_rel	number(8) not null,
	assoc_key_key	number(8) not null
);

create table MTID
(
	part_id	number(8),
	dsegid	number(12),
	save1	number(12),
	view_key_rel	number(8) not null,
	view_key_key	number(8) not null,
	bit_tbl	number(12)
);

create table unimod
(
	part_id	number(8),
	dflt_editable	number(2),
	dflt_blanked	number(2)
);

create table unistat
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	system	char(80),
	processor	char(80),
	author	char(80),
	company	char(80),
	fname_0	char(240),
	fname_1	char(16),
	date	char(40),
	translator	char(80),
	mod_system	char(80),
	mod_author	char(80),
	mod_company	char(80),
	mod_date	char(40),
	notes	long
);

create table txtblk
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	subtype	number(12),
	txt_font	number(12),
	txt_dens	number(80,40),
	color	number(12),
	char_cnt	number(12),
	txt_just	number(12),
	fontname	char(17),
	tstring_0	char(240),
	tstring_1	char(240),
	tstring_2	char(240),
	tstring_3	char(240),
	tstring_4	char(65),
	origin_0	number(80,40),
	origin_1	number(80,40),
	origin_2	number(80,40),
	dx	number(80,40),
	dy	number(80,40),
	slant	number(80,40)
);
alter table txtblk add
(
	tangle	number(80,40),
	txt_size	number(80,40),
	sub_sup	number(80,40),
	char_exp	number(80,40),
	char_spa	number(80,40),
	line_spa	number(80,40)
);

create table arcblk
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	subtype	number(12),
	arc_font	number(12),
	arc_dens	number(80,40),
	color	number(12),
	num_pts	number(12),
	cent_pt_0	number(80,40),
	cent_pt_1	number(80,40),
	cent_pt_2	number(80,40),
	radius	number(80,40),
	angles_0	number(80,40),
	angles_1	number(80,40),
	angles_2	number(80,40),
	angles_3	number(80,40),
	angles_4	number(80,40),
	angles_5	number(80,40),
	angles_6	number(80,40),
	angles_7	number(80,40),
	angles_8	number(80,40)
);
alter table arcblk add
(
	angles_9	number(80,40),
	angles_10	number(80,40),
	angles_11	number(80,40),
	angles_12	number(80,40),
	angles_13	number(80,40),
	angles_14	number(80,40),
	angles_15	number(80,40),
	angles_16	number(80,40),
	angles_17	number(80,40),
	angles_18	number(80,40),
	angles_19	number(80,40),
	angles_20	number(80,40),
	angles_21	number(80,40),
	angles_22	number(80,40),
	angles_23	number(80,40),
	angles_24	number(80,40),
	angles_25	number(80,40),
	angles_26	number(80,40),
	angles_27	number(80,40),
	angles_28	number(80,40),
	angles_29	number(80,40)
);
alter table arcblk add
(
	angles_30	number(80,40),
	angles_31	number(80,40),
	angles_32	number(80,40),
	angles_33	number(80,40),
	angles_34	number(80,40),
	angles_35	number(80,40),
	angles_36	number(80,40),
	angles_37	number(80,40),
	angles_38	number(80,40),
	angles_39	number(80,40),
	angles_40	number(80,40),
	angles_41	number(80,40),
	angles_42	number(80,40),
	angles_43	number(80,40),
	angles_44	number(80,40),
	angles_45	number(80,40),
	angles_46	number(80,40),
	angles_47	number(80,40),
	angles_48	number(80,40),
	angles_49	number(80,40)
);

create table lineblk
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	subtype	number(12),
	line_fon	number(12),
	line_den	number(80,40),
	color	number(12),
	num_pts	number(12),
	line_seg_0_0	number(80,40),
	line_seg_0_1	number(80,40),
	line_seg_0_2	number(80,40),
	line_seg_1_0	number(80,40),
	line_seg_1_1	number(80,40),
	line_seg_1_2	number(80,40),
	line_seg_2_0	number(80,40),
	line_seg_2_1	number(80,40),
	line_seg_2_2	number(80,40),
	line_seg_3_0	number(80,40),
	line_seg_3_1	number(80,40),
	line_seg_3_2	number(80,40),
	line_seg_4_0	number(80,40)
);
alter table lineblk add
(
	line_seg_4_1	number(80,40),
	line_seg_4_2	number(80,40),
	line_seg_5_0	number(80,40),
	line_seg_5_1	number(80,40),
	line_seg_5_2	number(80,40),
	line_seg_6_0	number(80,40),
	line_seg_6_1	number(80,40),
	line_seg_6_2	number(80,40),
	line_seg_7_0	number(80,40),
	line_seg_7_1	number(80,40),
	line_seg_7_2	number(80,40),
	line_seg_8_0	number(80,40),
	line_seg_8_1	number(80,40),
	line_seg_8_2	number(80,40),
	line_seg_9_0	number(80,40),
	line_seg_9_1	number(80,40),
	line_seg_9_2	number(80,40),
	line_seg_10_0	number(80,40),
	line_seg_10_1	number(80,40),
	line_seg_10_2	number(80,40),
	line_seg_11_0	number(80,40)
);
alter table lineblk add
(
	line_seg_11_1	number(80,40),
	line_seg_11_2	number(80,40),
	line_seg_12_0	number(80,40),
	line_seg_12_1	number(80,40),
	line_seg_12_2	number(80,40),
	line_seg_13_0	number(80,40),
	line_seg_13_1	number(80,40),
	line_seg_13_2	number(80,40),
	line_seg_14_0	number(80,40),
	line_seg_14_1	number(80,40),
	line_seg_14_2	number(80,40),
	line_seg_15_0	number(80,40),
	line_seg_15_1	number(80,40),
	line_seg_15_2	number(80,40),
	line_seg_16_0	number(80,40),
	line_seg_16_1	number(80,40),
	line_seg_16_2	number(80,40),
	line_seg_17_0	number(80,40),
	line_seg_17_1	number(80,40),
	line_seg_17_2	number(80,40),
	line_seg_18_0	number(80,40)
);
alter table lineblk add
(
	line_seg_18_1	number(80,40),
	line_seg_18_2	number(80,40),
	line_seg_19_0	number(80,40),
	line_seg_19_1	number(80,40),
	line_seg_19_2	number(80,40),
	line_seg_20_0	number(80,40),
	line_seg_20_1	number(80,40),
	line_seg_20_2	number(80,40),
	line_seg_21_0	number(80,40),
	line_seg_21_1	number(80,40),
	line_seg_21_2	number(80,40),
	line_seg_22_0	number(80,40),
	line_seg_22_1	number(80,40),
	line_seg_22_2	number(80,40),
	line_seg_23_0	number(80,40),
	line_seg_23_1	number(80,40),
	line_seg_23_2	number(80,40),
	line_seg_24_0	number(80,40),
	line_seg_24_1	number(80,40),
	line_seg_24_2	number(80,40),
	line_seg_25_0	number(80,40)
);
alter table lineblk add
(
	line_seg_25_1	number(80,40),
	line_seg_25_2	number(80,40),
	line_seg_26_0	number(80,40),
	line_seg_26_1	number(80,40),
	line_seg_26_2	number(80,40),
	line_seg_27_0	number(80,40),
	line_seg_27_1	number(80,40),
	line_seg_27_2	number(80,40),
	line_seg_28_0	number(80,40),
	line_seg_28_1	number(80,40),
	line_seg_28_2	number(80,40),
	line_seg_29_0	number(80,40),
	line_seg_29_1	number(80,40),
	line_seg_29_2	number(80,40),
	line_seg_30_0	number(80,40),
	line_seg_30_1	number(80,40),
	line_seg_30_2	number(80,40),
	line_seg_31_0	number(80,40),
	line_seg_31_1	number(80,40),
	line_seg_31_2	number(80,40),
	line_seg_32_0	number(80,40)
);
alter table lineblk add
(
	line_seg_32_1	number(80,40),
	line_seg_32_2	number(80,40),
	line_seg_33_0	number(80,40),
	line_seg_33_1	number(80,40),
	line_seg_33_2	number(80,40),
	line_seg_34_0	number(80,40),
	line_seg_34_1	number(80,40),
	line_seg_34_2	number(80,40),
	line_seg_35_0	number(80,40),
	line_seg_35_1	number(80,40),
	line_seg_35_2	number(80,40),
	line_seg_36_0	number(80,40),
	line_seg_36_1	number(80,40),
	line_seg_36_2	number(80,40),
	line_seg_37_0	number(80,40),
	line_seg_37_1	number(80,40),
	line_seg_37_2	number(80,40),
	line_seg_38_0	number(80,40),
	line_seg_38_1	number(80,40),
	line_seg_38_2	number(80,40),
	line_seg_39_0	number(80,40)
);
alter table lineblk add
(
	line_seg_39_1	number(80,40),
	line_seg_39_2	number(80,40),
	line_seg_40_0	number(80,40),
	line_seg_40_1	number(80,40),
	line_seg_40_2	number(80,40),
	line_seg_41_0	number(80,40),
	line_seg_41_1	number(80,40),
	line_seg_41_2	number(80,40),
	line_seg_42_0	number(80,40),
	line_seg_42_1	number(80,40),
	line_seg_42_2	number(80,40),
	line_seg_43_0	number(80,40),
	line_seg_43_1	number(80,40),
	line_seg_43_2	number(80,40),
	line_seg_44_0	number(80,40),
	line_seg_44_1	number(80,40),
	line_seg_44_2	number(80,40),
	line_seg_45_0	number(80,40),
	line_seg_45_1	number(80,40),
	line_seg_45_2	number(80,40),
	line_seg_46_0	number(80,40)
);
alter table lineblk add
(
	line_seg_46_1	number(80,40),
	line_seg_46_2	number(80,40),
	line_seg_47_0	number(80,40),
	line_seg_47_1	number(80,40),
	line_seg_47_2	number(80,40),
	line_seg_48_0	number(80,40),
	line_seg_48_1	number(80,40),
	line_seg_48_2	number(80,40),
	line_seg_49_0	number(80,40),
	line_seg_49_1	number(80,40),
	line_seg_49_2	number(80,40)
);

create table arrowblk
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	arr_type	number(12),
	arr_font	number(12),
	arr_dens	number(80,40),
	color	number(12),
	location_0	number(80,40),
	location_1	number(80,40),
	location_2	number(80,40),
	aangle	number(80,40),
	asize	number(80,40)
);

create table assoblk
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	asso_typ	number(12),
	modifier	number(12),
	asso_key_rel	number(8) not null,
	asso_key_key	number(8) not null,
	location_0	number(80,40),
	location_1	number(80,40),
	location_2	number(80,40)
);

create table draft
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	draf_ati_0	number(12),
	draf_ati_1	number(12),
	draf_ati_2	number(12),
	draf_ati_3	number(12),
	draf_ati_4	number(12),
	draf_ati_5	number(12),
	draf_ati_6	number(12),
	draf_ati_7	number(12),
	draf_ati_8	number(12),
	draf_ati_9	number(12),
	draf_ati_10	number(12),
	draf_ati_11	number(12),
	draf_ati_12	number(12),
	draf_ati_13	number(12),
	draf_ati_14	number(12),
	draf_ati_15	number(12),
	draf_ati_16	number(12),
	draf_ati_17	number(12)
);
alter table draft add
(
	draf_ati_18	number(12),
	draf_ati_19	number(12),
	draf_ati_20	number(12),
	draf_ati_21	number(12),
	draf_ati_22	number(12),
	draf_ati_23	number(12),
	draf_ati_24	number(12),
	draf_ati_25	number(12),
	draf_ati_26	number(12),
	draf_ati_27	number(12),
	draf_ati_28	number(12),
	draf_ati_29	number(12),
	draf_ati_30	number(12),
	draf_ati_31	number(12),
	draf_ati_32	number(12),
	draf_ati_33	number(12),
	draf_ati_34	number(12),
	draf_ati_35	number(12),
	draf_ati_36	number(12),
	draf_ati_37	number(12),
	draf_ati_38	number(12)
);
alter table draft add
(
	draf_ati_39	number(12),
	draf_ati_40	number(12),
	draf_ati_41	number(12),
	draf_ati_42	number(12),
	draf_ati_43	number(12),
	draf_atr_0	number(80,40),
	draf_atr_1	number(80,40),
	draf_atr_2	number(80,40),
	draf_atr_3	number(80,40),
	draf_atr_4	number(80,40),
	draf_atr_5	number(80,40),
	draf_atr_6	number(80,40),
	draf_atr_7	number(80,40),
	draf_atr_8	number(80,40),
	draf_atr_9	number(80,40),
	draf_atr_10	number(80,40),
	draf_atr_11	number(80,40),
	draf_atr_12	number(80,40),
	draf_atr_13	number(80,40),
	draf_atr_14	number(80,40),
	draf_atr_15	number(80,40)
);
alter table draft add
(
	draf_atr_16	number(80,40),
	draf_atr_17	number(80,40),
	draf_atr_18	number(80,40),
	draf_atr_19	number(80,40),
	draf_atr_20	number(80,40),
	draf_atr_21	number(80,40),
	draf_atr_22	number(80,40),
	draf_atr_23	number(80,40),
	draf_atr_24	number(80,40),
	cpln_0	number(80,40),
	cpln_1	number(80,40),
	cpln_2	number(80,40),
	cpln_3	number(80,40),
	cpln_4	number(80,40),
	cpln_5	number(80,40),
	cpln_6	number(80,40),
	cpln_7	number(80,40),
	cpln_8	number(80,40),
	cpln_9	number(80,40),
	cpln_10	number(80,40),
	cpln_11	number(80,40)
);

create table hatchlin
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null
);

create table func
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	arg_0	number(12),
	arg_1	number(12),
	arg_2	number(12),
	arg_3	number(12),
	arg_4	number(12),
	funcbuf	char(200)
);

create table qstb
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	symbol	char(12),
	ttype	number(12),
	val_0	number(80,40),
	val_1	number(80,40),
	val_2	number(80,40)
);

create table txt
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	subtype	number(12),
	arckey_rel	number(8) not null,
	arckey_key	number(8) not null,
	dx	number(80,40),
	dy	number(80,40),
	tangle	number(80,40),
	position_0	number(80,40),
	position_1	number(80,40),
	position_2	number(80,40),
	tchar	long
);

create table txtattr
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	use_count	number(12),
	color	number(12),
	layer	number(12),
	pen	number(12),
	line_style	number(12),
	line_weight	number(80,40),
	line_width	number(80,40),
	displayable	number(12),
	selectable	number(2),
	label_on	number(12),
	font	number(12),
	prec	number(12),
	expn	number(80,40),
	spacing	number(80,40),
	height	number(80,40),
	up_0	number(80,40),
	up_1	number(80,40),
	up_2	number(80,40)
);
alter table txtattr add
(
	plane_0	number(80,40),
	plane_1	number(80,40),
	plane_2	number(80,40),
	path	number(12),
	align_hor	number(12),
	align_ver	number(12),
	txt_dens	number(12),
	slant	number(80,40),
	sub_sup	number(80,40),
	line_spacing	number(80,40),
	entity_site	number(12)
);

create table nclattr
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	use_count	number(12),
	color	number(12),
	layer	number(12),
	pen	number(12),
	line_style	number(12),
	line_weight	number(80,40),
	line_width	number(80,40),
	displayable	number(12),
	selectable	number(2),
	label_on	number(12)
);

create table nclpt
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	markertype	number(12),
	snap_node	number(2),
	pt_0	number(80,40),
	pt_1	number(80,40),
	pt_2	number(80,40)
);

create table nclln
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	spt_0	number(80,40),
	spt_1	number(80,40),
	spt_2	number(80,40),
	ept_0	number(80,40),
	ept_1	number(80,40),
	ept_2	number(80,40)
);

create table nclci
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	radius	number(80,40),
	dang	number(80,40),
	center_0	number(80,40),
	center_1	number(80,40),
	center_2	number(80,40),
	svec_0	number(80,40),
	svec_1	number(80,40),
	svec_2	number(80,40),
	nvec_0	number(80,40),
	nvec_1	number(80,40)
);
alter table nclci add
(
	nvec_2	number(80,40)
);

create table vector
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	vec_0	number(80,40),
	vec_1	number(80,40),
	vec_2	number(80,40)
);

create table nclpl
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	radius	number(80,40),
	pt_0	number(80,40),
	pt_1	number(80,40),
	pt_2	number(80,40),
	nvec_0	number(80,40),
	nvec_1	number(80,40),
	nvec_2	number(80,40)
);

create table matrix
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	dalen	number(80,40),
	dbox_0	number(80,40),
	dbox_1	number(80,40),
	dbox_2	number(80,40),
	mat_0_0	number(80,40),
	mat_0_1	number(80,40),
	mat_0_2	number(80,40),
	mat_0_3	number(80,40),
	mat_1_0	number(80,40),
	mat_1_1	number(80,40)
);
alter table matrix add
(
	mat_1_2	number(80,40),
	mat_1_3	number(80,40),
	mat_2_0	number(80,40),
	mat_2_1	number(80,40),
	mat_2_2	number(80,40),
	mat_2_3	number(80,40)
);

create table param
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	s	number(12,4)
);

create table segment
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	point_0	number(80,40),
	point_1	number(80,40),
	point_2	number(80,40),
	delta_0	number(12,4),
	delta_1	number(12,4),
	delta_2	number(12,4),
	duds0	number(12,4),
	duds1	number(12,4),
	rho	number(12,4)
);

create table curve
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	closdinu	number(12),
	t0	number(80,40),
	t1	number(80,40),
	t_end	number(80,40)
);

create table patch
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	pt_0	number(80,40),
	pt_1	number(80,40),
	pt_2	number(80,40),
	delta_0_0	number(12,4),
	delta_0_1	number(12,4),
	delta_0_2	number(12,4),
	delta_1_0	number(12,4),
	delta_1_1	number(12,4),
	delta_1_2	number(12,4),
	delta_2_0	number(12,4),
	delta_2_1	number(12,4),
	delta_2_2	number(12,4),
	delta_3_0	number(12,4),
	delta_3_1	number(12,4),
	delta_3_2	number(12,4),
	delta_4_0	number(12,4),
	delta_4_1	number(12,4),
	delta_4_2	number(12,4)
);
alter table patch add
(
	delta_5_0	number(12,4),
	delta_5_1	number(12,4),
	delta_5_2	number(12,4),
	delta_6_0	number(12,4),
	delta_6_1	number(12,4),
	delta_6_2	number(12,4),
	rho	number(12,4)
);

create table panel
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(8),
	subscr	number(12),
	type	number(12),
	no_param	number(12),
	param_0	number(12,4),
	param_1	number(12,4),
	param_2	number(12,4),
	param_3	number(12,4),
	param_4	number(12,4),
	param_5	number(12,4),
	param_6	number(12,4),
	param_7	number(12,4),
	param_8	number(12,4),
	param_9	number(12,4),
	param_10	number(12,4),
	param_11	number(12,4),
	param_12	number(12,4),
	param_13	number(12,4)
);
alter table panel add
(
	param_14	number(12,4),
	param_15	number(12,4),
	param_16	number(12,4),
	param_17	number(12,4),
	param_18	number(12,4),
	param_19	number(12,4),
	param_20	number(12,4),
	param_21	number(12,4),
	param_22	number(12,4),
	param_23	number(12,4),
	param_24	number(12,4),
	param_25	number(12,4),
	param_26	number(12,4),
	param_27	number(12,4),
	param_28	number(12,4),
	param_29	number(12,4),
	param_30	number(12,4),
	param_31	number(12,4),
	param_32	number(12,4),
	param_33	number(12,4),
	param_34	number(12,4)
);
alter table panel add
(
	param_35	number(12,4),
	param_36	number(12,4),
	param_37	number(12,4),
	param_38	number(12,4),
	param_39	number(12,4),
	param_40	number(12,4),
	param_41	number(12,4),
	param_42	number(12,4),
	param_43	number(12,4),
	param_44	number(12,4),
	param_45	number(12,4),
	param_46	number(12,4),
	param_47	number(12,4),
	param_48	number(12,4),
	param_49	number(12,4)
);

create table panelkey
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	id_rel	number(8) not null,
	id_key	number(8) not null
);

create table surface
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	rldnu	number(12),
	swapuv	number(12),
	rev_normal	number(2),
	closdinu	number(12),
	closdinv	number(12),
	offset	number(12),
	offdist	number(12,4),
	surf_type	number(12),
	primitive	number(12),
	prim_param_0	number(80,40)
);
alter table surface add
(
	prim_param_1	number(80,40),
	prim_param_2	number(80,40),
	prim_param_3	number(80,40),
	prim_param_4	number(80,40),
	prim_param_5	number(80,40),
	prim_param_6	number(80,40),
	prim_param_7	number(80,40),
	prim_param_8	number(80,40),
	prim_param_9	number(80,40),
	prim_param_10	number(80,40),
	prim_param_11	number(80,40),
	prim_param_12	number(80,40),
	prim_param_13	number(80,40),
	prim_param_14	number(80,40),
	prim_param_15	number(80,40)
);

create table revsurf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	dummy	number(12),
	rldnu	number(12),
	swapuv	number(12),
	rev_normal	number(2),
	closdinu	number(12),
	closdinv	number(12),
	offdist	number(12,4),
	cvkey_rel	number(8) not null,
	cvkey_key	number(8) not null,
	primitive	number(12)
);
alter table revsurf add
(
	prim_param_0	number(80,40),
	prim_param_1	number(80,40),
	prim_param_2	number(80,40),
	prim_param_3	number(80,40),
	prim_param_4	number(80,40),
	prim_param_5	number(80,40),
	prim_param_6	number(80,40),
	prim_param_7	number(80,40),
	prim_param_8	number(80,40),
	prim_param_9	number(80,40),
	prim_param_10	number(80,40),
	prim_param_11	number(80,40),
	prim_param_12	number(80,40),
	prim_param_13	number(80,40),
	prim_param_14	number(80,40),
	prim_param_15	number(80,40),
	pta_0	number(80,40),
	pta_1	number(80,40),
	pta_2	number(80,40),
	vca_0	number(80,40),
	vca_1	number(80,40)
);
alter table revsurf add
(
	vca_2	number(80,40),
	sa	number(80,40),
	ta	number(80,40)
);

create table mpatch
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	pt_0	number(80,40),
	pt_1	number(80,40),
	pt_2	number(80,40),
	delta_0_0	number(80,40),
	delta_0_1	number(80,40),
	delta_0_2	number(80,40),
	delta_1_0	number(80,40),
	delta_1_1	number(80,40),
	delta_1_2	number(80,40),
	delta_2_0	number(80,40),
	delta_2_1	number(80,40),
	delta_2_2	number(80,40),
	delta_3_0	number(80,40),
	delta_3_1	number(80,40),
	delta_3_2	number(80,40),
	delta_4_0	number(80,40),
	delta_4_1	number(80,40),
	delta_4_2	number(80,40)
);
alter table mpatch add
(
	delta_5_0	number(80,40),
	delta_5_1	number(80,40),
	delta_5_2	number(80,40),
	delta_6_0	number(80,40),
	delta_6_1	number(80,40),
	delta_6_2	number(80,40),
	delta_7_0	number(80,40),
	delta_7_1	number(80,40),
	delta_7_2	number(80,40),
	delta_8_0	number(80,40),
	delta_8_1	number(80,40),
	delta_8_2	number(80,40),
	delta_9_0	number(80,40),
	delta_9_1	number(80,40),
	delta_9_2	number(80,40),
	delta_10_0	number(80,40),
	delta_10_1	number(80,40),
	delta_10_2	number(80,40),
	delta_11_0	number(80,40),
	delta_11_1	number(80,40),
	delta_11_2	number(80,40)
);
alter table mpatch add
(
	delta_12_0	number(80,40),
	delta_12_1	number(80,40),
	delta_12_2	number(80,40),
	delta_13_0	number(80,40),
	delta_13_1	number(80,40),
	delta_13_2	number(80,40),
	delta_14_0	number(80,40),
	delta_14_1	number(80,40),
	delta_14_2	number(80,40)
);

create table meshsf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	dummy	number(12),
	rldnu	number(12),
	swapuv	number(12),
	rev_normal	number(2),
	closdinu	number(12),
	closdinv	number(12),
	offset	number(12),
	offdist	number(12,4),
	surf_type	number(12),
	m	number(12)
);
alter table meshsf add
(
	n	number(12)
);

create table qpatch
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	bpatchnum_0	number(12),
	bpatchnum_1	number(12),
	bpatchnum_2	number(12),
	bpatchnum_3	number(12),
	origin_0	number(12,4),
	origin_1	number(12,4),
	origin_2	number(12,4),
	xvalues_0	number(12,4),
	xvalues_1	number(12,4),
	xvalues_2	number(12,4),
	xvalues_3	number(12,4),
	xvalues_4	number(12,4),
	xvalues_5	number(12,4),
	xvalues_6	number(12,4),
	xvalues_7	number(12,4),
	xvalues_8	number(12,4),
	xvalues_9	number(12,4),
	xvalues_10	number(12,4)
);
alter table qpatch add
(
	xvalues_11	number(12,4),
	xvalues_12	number(12,4),
	xvalues_13	number(12,4),
	xvalues_14	number(12,4),
	xvalues_15	number(12,4),
	xvalues_16	number(12,4),
	xvalues_17	number(12,4),
	xvalues_18	number(12,4),
	xvalues_19	number(12,4),
	xvalues_20	number(12,4),
	xvalues_21	number(12,4),
	xvalues_22	number(12,4),
	xvalues_23	number(12,4),
	xvalues_24	number(12,4),
	yvalues_0	number(12,4),
	yvalues_1	number(12,4),
	yvalues_2	number(12,4),
	yvalues_3	number(12,4),
	yvalues_4	number(12,4),
	yvalues_5	number(12,4),
	yvalues_6	number(12,4)
);
alter table qpatch add
(
	yvalues_7	number(12,4),
	yvalues_8	number(12,4),
	yvalues_9	number(12,4),
	yvalues_10	number(12,4),
	yvalues_11	number(12,4),
	yvalues_12	number(12,4),
	yvalues_13	number(12,4),
	yvalues_14	number(12,4),
	yvalues_15	number(12,4),
	yvalues_16	number(12,4),
	yvalues_17	number(12,4),
	yvalues_18	number(12,4),
	yvalues_19	number(12,4),
	yvalues_20	number(12,4),
	yvalues_21	number(12,4),
	yvalues_22	number(12,4),
	yvalues_23	number(12,4),
	yvalues_24	number(12,4),
	zvalues_0	number(12,4),
	zvalues_1	number(12,4),
	zvalues_2	number(12,4)
);
alter table qpatch add
(
	zvalues_3	number(12,4),
	zvalues_4	number(12,4),
	zvalues_5	number(12,4),
	zvalues_6	number(12,4),
	zvalues_7	number(12,4),
	zvalues_8	number(12,4),
	zvalues_9	number(12,4),
	zvalues_10	number(12,4),
	zvalues_11	number(12,4),
	zvalues_12	number(12,4),
	zvalues_13	number(12,4),
	zvalues_14	number(12,4),
	zvalues_15	number(12,4),
	zvalues_16	number(12,4),
	zvalues_17	number(12,4),
	zvalues_18	number(12,4),
	zvalues_19	number(12,4),
	zvalues_20	number(12,4),
	zvalues_21	number(12,4),
	zvalues_22	number(12,4),
	zvalues_23	number(12,4)
);
alter table qpatch add
(
	zvalues_24	number(12,4)
);

create table quiltsf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	material	number(12),
	surf_type	number(12),
	numpatches	number(12),
	rldnu	number(12),
	rldnv	number(12),
	offset	number(12),
	offdist	number(12,4),
	midpt_0_0	number(12,4),
	midpt_0_1	number(12,4),
	midpt_0_2	number(12,4)
);
alter table quiltsf add
(
	midpt_1_0	number(12,4),
	midpt_1_1	number(12,4),
	midpt_1_2	number(12,4),
	midpt_2_0	number(12,4),
	midpt_2_1	number(12,4),
	midpt_2_2	number(12,4),
	midpt_3_0	number(12,4),
	midpt_3_1	number(12,4),
	midpt_3_2	number(12,4),
	midpt_4_0	number(12,4),
	midpt_4_1	number(12,4),
	midpt_4_2	number(12,4),
	midpt_5_0	number(12,4),
	midpt_5_1	number(12,4),
	midpt_5_2	number(12,4),
	midpt_6_0	number(12,4),
	midpt_6_1	number(12,4),
	midpt_6_2	number(12,4),
	midpt_7_0	number(12,4),
	midpt_7_1	number(12,4),
	midpt_7_2	number(12,4)
);
alter table quiltsf add
(
	midpt_8_0	number(12,4),
	midpt_8_1	number(12,4),
	midpt_8_2	number(12,4),
	midpt_9_0	number(12,4),
	midpt_9_1	number(12,4),
	midpt_9_2	number(12,4),
	midpt_10_0	number(12,4),
	midpt_10_1	number(12,4),
	midpt_10_2	number(12,4),
	midpt_11_0	number(12,4),
	midpt_11_1	number(12,4),
	midpt_11_2	number(12,4),
	shaded	number(2),
	lucency	number(12),
	dummy	number(12)
);

create table patpnt
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	pnpt	number(80,40)
);

create table patern
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	markertype	number(12),
	pntype	number(12),
	dummy	number(12)
);

create table netsf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	surf_type	number(12),
	bndsfs_0_0	number(12),
	bndsfs_0_1	number(12),
	bndsfs_0_2	number(12),
	bndsfs_0_3	number(12),
	bndsfs_1_0	number(12),
	bndsfs_1_1	number(12),
	bndsfs_1_2	number(12),
	bndsfs_1_3	number(12),
	bndsfs_2_0	number(12)
);
alter table netsf add
(
	bndsfs_2_1	number(12),
	bndsfs_2_2	number(12),
	bndsfs_2_3	number(12),
	bndsfs_3_0	number(12),
	bndsfs_3_1	number(12),
	bndsfs_3_2	number(12),
	bndsfs_3_3	number(12),
	bndsfs_4_0	number(12),
	bndsfs_4_1	number(12),
	bndsfs_4_2	number(12),
	bndsfs_4_3	number(12),
	bndsfs_5_0	number(12),
	bndsfs_5_1	number(12),
	bndsfs_5_2	number(12),
	bndsfs_5_3	number(12),
	bndsfs_6_0	number(12),
	bndsfs_6_1	number(12),
	bndsfs_6_2	number(12),
	bndsfs_6_3	number(12),
	bndsfs_7_0	number(12),
	bndsfs_7_1	number(12)
);
alter table netsf add
(
	bndsfs_7_2	number(12),
	bndsfs_7_3	number(12),
	bndsfs_8_0	number(12),
	bndsfs_8_1	number(12),
	bndsfs_8_2	number(12),
	bndsfs_8_3	number(12),
	bndsfs_9_0	number(12),
	bndsfs_9_1	number(12),
	bndsfs_9_2	number(12),
	bndsfs_9_3	number(12),
	bndsfs_10_0	number(12),
	bndsfs_10_1	number(12),
	bndsfs_10_2	number(12),
	bndsfs_10_3	number(12),
	bndsfs_11_0	number(12),
	bndsfs_11_1	number(12),
	bndsfs_11_2	number(12),
	bndsfs_11_3	number(12),
	bndsfs_12_0	number(12),
	bndsfs_12_1	number(12),
	bndsfs_12_2	number(12)
);
alter table netsf add
(
	bndsfs_12_3	number(12),
	bndsfs_13_0	number(12),
	bndsfs_13_1	number(12),
	bndsfs_13_2	number(12),
	bndsfs_13_3	number(12),
	bndsfs_14_0	number(12),
	bndsfs_14_1	number(12),
	bndsfs_14_2	number(12),
	bndsfs_14_3	number(12),
	bndsfs_15_0	number(12),
	bndsfs_15_1	number(12),
	bndsfs_15_2	number(12),
	bndsfs_15_3	number(12),
	bndsfs_16_0	number(12),
	bndsfs_16_1	number(12),
	bndsfs_16_2	number(12),
	bndsfs_16_3	number(12),
	bndsfs_17_0	number(12),
	bndsfs_17_1	number(12),
	bndsfs_17_2	number(12),
	bndsfs_17_3	number(12)
);
alter table netsf add
(
	bndsfs_18_0	number(12),
	bndsfs_18_1	number(12),
	bndsfs_18_2	number(12),
	bndsfs_18_3	number(12),
	bndsfs_19_0	number(12),
	bndsfs_19_1	number(12),
	bndsfs_19_2	number(12),
	bndsfs_19_3	number(12),
	bndsfs_20_0	number(12),
	bndsfs_20_1	number(12),
	bndsfs_20_2	number(12),
	bndsfs_20_3	number(12),
	bndsfs_21_0	number(12),
	bndsfs_21_1	number(12),
	bndsfs_21_2	number(12),
	bndsfs_21_3	number(12),
	bndsfs_22_0	number(12),
	bndsfs_22_1	number(12),
	bndsfs_22_2	number(12),
	bndsfs_22_3	number(12),
	bndsfs_23_0	number(12)
);
alter table netsf add
(
	bndsfs_23_1	number(12),
	bndsfs_23_2	number(12),
	bndsfs_23_3	number(12),
	bndsfs_24_0	number(12),
	bndsfs_24_1	number(12),
	bndsfs_24_2	number(12),
	bndsfs_24_3	number(12),
	bndsfs_25_0	number(12),
	bndsfs_25_1	number(12),
	bndsfs_25_2	number(12),
	bndsfs_25_3	number(12),
	bndsfs_26_0	number(12),
	bndsfs_26_1	number(12),
	bndsfs_26_2	number(12),
	bndsfs_26_3	number(12),
	bndsfs_27_0	number(12),
	bndsfs_27_1	number(12),
	bndsfs_27_2	number(12),
	bndsfs_27_3	number(12),
	bndsfs_28_0	number(12),
	bndsfs_28_1	number(12)
);
alter table netsf add
(
	bndsfs_28_2	number(12),
	bndsfs_28_3	number(12),
	bndsfs_29_0	number(12),
	bndsfs_29_1	number(12),
	bndsfs_29_2	number(12),
	bndsfs_29_3	number(12),
	bndsfs_30_0	number(12),
	bndsfs_30_1	number(12),
	bndsfs_30_2	number(12),
	bndsfs_30_3	number(12),
	bndsfs_31_0	number(12),
	bndsfs_31_1	number(12),
	bndsfs_31_2	number(12),
	bndsfs_31_3	number(12),
	bndsfs_32_0	number(12),
	bndsfs_32_1	number(12),
	bndsfs_32_2	number(12),
	bndsfs_32_3	number(12),
	bndsfs_33_0	number(12),
	bndsfs_33_1	number(12),
	bndsfs_33_2	number(12)
);
alter table netsf add
(
	bndsfs_33_3	number(12),
	bndsfs_34_0	number(12),
	bndsfs_34_1	number(12),
	bndsfs_34_2	number(12),
	bndsfs_34_3	number(12),
	bndsfs_35_0	number(12),
	bndsfs_35_1	number(12),
	bndsfs_35_2	number(12),
	bndsfs_35_3	number(12),
	bndsfs_36_0	number(12),
	bndsfs_36_1	number(12),
	bndsfs_36_2	number(12),
	bndsfs_36_3	number(12),
	bndsfs_37_0	number(12),
	bndsfs_37_1	number(12),
	bndsfs_37_2	number(12),
	bndsfs_37_3	number(12),
	bndsfs_38_0	number(12),
	bndsfs_38_1	number(12),
	bndsfs_38_2	number(12),
	bndsfs_38_3	number(12)
);
alter table netsf add
(
	bndsfs_39_0	number(12),
	bndsfs_39_1	number(12),
	bndsfs_39_2	number(12),
	bndsfs_39_3	number(12)
);

create table shapwd
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	wd	number(80,40)
);

create table shape
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	f2d3d	number(12),
	dummy	number(12)
);

create table scalar
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	subscr	number(12),
	scalar_value	number(80,40),
	classnm	char(21),
	descript	char(64),
	modified	number(80,40)
);

create table labloc
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	del_0	number(80,40),
	del_1	number(80,40),
	del_2	number(80,40),
	index	number(12)
);

create table labtbl
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null
);

create table evwd
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	wd	number(80,40)
);

create table evalcv
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	curve_type	number(12)
);

create table evalsf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	material	number(12),
	numupaths	number(12),
	numvpaths	number(12),
	ptsperucrv	number(12),
	ptspervcrv	number(12),
	rldnu	number(12),
	rldnv	number(12),
	closdinu	number(12),
	closdinv	number(12),
	surf_type	number(12)
);
alter table evalsf add
(
	offset	number(12),
	shaded	number(2),
	lucency	number(12),
	offdist	number(12,4)
);

create table nclpv
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	pt_0	number(80,40),
	pt_1	number(80,40),
	pt_2	number(80,40),
	ve_0	number(80,40),
	ve_1	number(80,40),
	ve_2	number(80,40)
);

create table ibndykey
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	id_rel	number(8) not null,
	id_key	number(8) not null
);

create table trimsf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	labloc_0	number(80,40),
	labloc_1	number(80,40),
	labloc_2	number(80,40),
	ldrloc_0	number(80,40),
	ldrloc_1	number(80,40),
	ldrloc_2	number(80,40),
	subscr	number(12),
	closdinu	number(12),
	closdinv	number(12),
	offdist	number(12,4),
	uv_key_rel	number(8) not null,
	uv_key_key	number(8) not null,
	cv_key_rel	number(8) not null,
	cv_key_key	number(8) not null,
	bs_key_rel	number(8) not null,
	bs_key_key	number(8) not null,
	ub_min	number(12,4)
);
alter table trimsf add
(
	ub_max	number(12,4),
	vb_min	number(12,4),
	vb_max	number(12,4),
	u_min	number(12,4),
	u_max	number(12,4),
	v_min	number(12,4),
	v_max	number(12,4),
	drive_type	number(12)
);

create table datael
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	value	number(80,40),
	label	char(64),
	isub	number(12),
	type	number(12),
	delim	number(12),
	dum	number(12)
);

create table datast
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	subscr	number(12),
	nargs	number(12)
);

create table textvar
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	label	char(64),
	subscr	number(12),
	text_0	char(240),
	text_1	char(16)
);

create table color
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	color_name_0	char(96),
	color_name_1	char(96),
	color_name_2	char(96),
	color_name_3	char(96),
	color_name_4	char(96),
	color_name_5	char(96),
	color_name_6	char(96),
	color_name_7	char(96),
	color_name_8	char(96),
	color_name_9	char(96),
	color_name_10	char(96),
	color_name_11	char(96),
	color_name_12	char(96),
	color_name_13	char(96),
	color_name_14	char(96),
	color_name_15	char(96),
	color_name_16	char(96),
	color_name_17	char(96),
	color_name_18	char(96)
);
alter table color add
(
	color_name_19	char(96),
	color_name_20	char(96),
	color_name_21	char(96),
	color_name_22	char(96),
	color_name_23	char(96),
	color_name_24	char(96),
	color_name_25	char(96),
	color_name_26	char(96),
	color_name_27	char(96),
	color_name_28	char(96),
	color_name_29	char(96),
	color_name_30	char(96),
	color_name_31	char(96),
	color_name_32	char(96),
	color_name_33	char(96),
	color_name_34	char(96),
	color_name_35	char(96),
	color_name_36	char(96),
	color_name_37	char(96),
	color_name_38	char(96),
	color_name_39	char(96),
	color_name_40	char(96)
);
alter table color add
(
	color_name_41	char(96),
	color_name_42	char(96),
	color_name_43	char(96),
	color_name_44	char(96),
	color_name_45	char(96),
	color_name_46	char(96),
	color_name_47	char(96),
	color_name_48	char(96),
	color_name_49	char(96),
	color_name_50	char(96),
	color_name_51	char(96),
	color_name_52	char(96),
	color_name_53	char(96),
	color_name_54	char(96),
	color_name_55	char(96),
	color_name_56	char(96),
	color_name_57	char(96),
	color_name_58	char(96),
	color_name_59	char(96),
	color_name_60	char(96),
	color_name_61	char(96),
	color_name_62	char(96)
);
alter table color add
(
	color_name_63	char(96),
	color_value_0_0	number(12),
	color_value_0_1	number(12),
	color_value_0_2	number(12),
	color_value_1_0	number(12),
	color_value_1_1	number(12),
	color_value_1_2	number(12),
	color_value_2_0	number(12),
	color_value_2_1	number(12),
	color_value_2_2	number(12),
	color_value_3_0	number(12),
	color_value_3_1	number(12),
	color_value_3_2	number(12),
	color_value_4_0	number(12),
	color_value_4_1	number(12),
	color_value_4_2	number(12),
	color_value_5_0	number(12),
	color_value_5_1	number(12),
	color_value_5_2	number(12),
	color_value_6_0	number(12),
	color_value_6_1	number(12)
);
alter table color add
(
	color_value_6_2	number(12),
	color_value_7_0	number(12),
	color_value_7_1	number(12),
	color_value_7_2	number(12),
	color_value_8_0	number(12),
	color_value_8_1	number(12),
	color_value_8_2	number(12),
	color_value_9_0	number(12),
	color_value_9_1	number(12),
	color_value_9_2	number(12),
	color_value_10_0	number(12),
	color_value_10_1	number(12),
	color_value_10_2	number(12),
	color_value_11_0	number(12),
	color_value_11_1	number(12),
	color_value_11_2	number(12),
	color_value_12_0	number(12),
	color_value_12_1	number(12),
	color_value_12_2	number(12),
	color_value_13_0	number(12),
	color_value_13_1	number(12)
);
alter table color add
(
	color_value_13_2	number(12),
	color_value_14_0	number(12),
	color_value_14_1	number(12),
	color_value_14_2	number(12),
	color_value_15_0	number(12),
	color_value_15_1	number(12),
	color_value_15_2	number(12),
	color_value_16_0	number(12),
	color_value_16_1	number(12),
	color_value_16_2	number(12),
	color_value_17_0	number(12),
	color_value_17_1	number(12),
	color_value_17_2	number(12),
	color_value_18_0	number(12),
	color_value_18_1	number(12),
	color_value_18_2	number(12),
	color_value_19_0	number(12),
	color_value_19_1	number(12),
	color_value_19_2	number(12),
	color_value_20_0	number(12),
	color_value_20_1	number(12)
);
alter table color add
(
	color_value_20_2	number(12),
	color_value_21_0	number(12),
	color_value_21_1	number(12),
	color_value_21_2	number(12),
	color_value_22_0	number(12),
	color_value_22_1	number(12),
	color_value_22_2	number(12),
	color_value_23_0	number(12),
	color_value_23_1	number(12),
	color_value_23_2	number(12),
	color_value_24_0	number(12),
	color_value_24_1	number(12),
	color_value_24_2	number(12),
	color_value_25_0	number(12),
	color_value_25_1	number(12),
	color_value_25_2	number(12),
	color_value_26_0	number(12),
	color_value_26_1	number(12),
	color_value_26_2	number(12),
	color_value_27_0	number(12),
	color_value_27_1	number(12)
);
alter table color add
(
	color_value_27_2	number(12),
	color_value_28_0	number(12),
	color_value_28_1	number(12),
	color_value_28_2	number(12),
	color_value_29_0	number(12),
	color_value_29_1	number(12),
	color_value_29_2	number(12),
	color_value_30_0	number(12),
	color_value_30_1	number(12),
	color_value_30_2	number(12),
	color_value_31_0	number(12),
	color_value_31_1	number(12),
	color_value_31_2	number(12),
	color_value_32_0	number(12),
	color_value_32_1	number(12),
	color_value_32_2	number(12),
	color_value_33_0	number(12),
	color_value_33_1	number(12),
	color_value_33_2	number(12),
	color_value_34_0	number(12),
	color_value_34_1	number(12)
);
alter table color add
(
	color_value_34_2	number(12),
	color_value_35_0	number(12),
	color_value_35_1	number(12),
	color_value_35_2	number(12),
	color_value_36_0	number(12),
	color_value_36_1	number(12),
	color_value_36_2	number(12),
	color_value_37_0	number(12),
	color_value_37_1	number(12),
	color_value_37_2	number(12),
	color_value_38_0	number(12),
	color_value_38_1	number(12),
	color_value_38_2	number(12),
	color_value_39_0	number(12),
	color_value_39_1	number(12),
	color_value_39_2	number(12),
	color_value_40_0	number(12),
	color_value_40_1	number(12),
	color_value_40_2	number(12),
	color_value_41_0	number(12),
	color_value_41_1	number(12)
);
alter table color add
(
	color_value_41_2	number(12),
	color_value_42_0	number(12),
	color_value_42_1	number(12),
	color_value_42_2	number(12),
	color_value_43_0	number(12),
	color_value_43_1	number(12),
	color_value_43_2	number(12),
	color_value_44_0	number(12),
	color_value_44_1	number(12),
	color_value_44_2	number(12),
	color_value_45_0	number(12),
	color_value_45_1	number(12),
	color_value_45_2	number(12),
	color_value_46_0	number(12),
	color_value_46_1	number(12),
	color_value_46_2	number(12),
	color_value_47_0	number(12),
	color_value_47_1	number(12),
	color_value_47_2	number(12),
	color_value_48_0	number(12),
	color_value_48_1	number(12)
);
alter table color add
(
	color_value_48_2	number(12),
	color_value_49_0	number(12),
	color_value_49_1	number(12),
	color_value_49_2	number(12),
	color_value_50_0	number(12),
	color_value_50_1	number(12),
	color_value_50_2	number(12),
	color_value_51_0	number(12),
	color_value_51_1	number(12),
	color_value_51_2	number(12),
	color_value_52_0	number(12),
	color_value_52_1	number(12),
	color_value_52_2	number(12),
	color_value_53_0	number(12),
	color_value_53_1	number(12),
	color_value_53_2	number(12),
	color_value_54_0	number(12),
	color_value_54_1	number(12),
	color_value_54_2	number(12),
	color_value_55_0	number(12),
	color_value_55_1	number(12)
);
alter table color add
(
	color_value_55_2	number(12),
	color_value_56_0	number(12),
	color_value_56_1	number(12),
	color_value_56_2	number(12),
	color_value_57_0	number(12),
	color_value_57_1	number(12),
	color_value_57_2	number(12),
	color_value_58_0	number(12),
	color_value_58_1	number(12),
	color_value_58_2	number(12),
	color_value_59_0	number(12),
	color_value_59_1	number(12),
	color_value_59_2	number(12),
	color_value_60_0	number(12),
	color_value_60_1	number(12),
	color_value_60_2	number(12),
	color_value_61_0	number(12),
	color_value_61_1	number(12),
	color_value_61_2	number(12),
	color_value_62_0	number(12),
	color_value_62_1	number(12)
);
alter table color add
(
	color_value_62_2	number(12),
	color_value_63_0	number(12),
	color_value_63_1	number(12),
	color_value_63_2	number(12)
);
insert into geometry values(0, 'displst', -1, 0);
insert into geometry values(0, 'tesslst', -1, 0);
insert into geometry values(0, 'boxlst', -1, 0);
insert into geometry values(0, 'xyzbylst', -1, 0);
insert into geometry values(0, 'uvbylst', -1, 0);
insert into geometry values(0, 'uvboxlst', -1, 0);
insert into geometry values(0, 'point', 0, 1);
insert into geometry values(0, 'line', 1, 1);
insert into geometry values(0, 'circle', 2, 1);
insert into geometry values(0, 'conic', 3, 1);
insert into geometry values(0, 'cid', -1, 0);
insert into geometry values(0, 'compcrv', 4, 2);
insert into geometry values(0, 'pt', -1, 0);
insert into geometry values(0, 'wt', -1, 0);
insert into geometry values(0, 't', -1, 0);
insert into geometry values(0, 'bsplcrv', 5, 2);
insert into geometry values(0, 'rbsplcrv', 6, 4);
insert into geometry values(0, 'uvcvonsf', 7, 4);
insert into geometry values(0, 'agcrv', 8, 0);
insert into geometry values(0, 'poly', 9, 1);
insert into geometry values(0, 'polyline', 10, 2);
insert into geometry values(0, 'surfattr', 11, 0);
insert into geometry values(0, 'tu', -1, 0);
insert into geometry values(0, 'tv', -1, 0);
insert into geometry values(0, 'sskey', -1, 0);
insert into geometry values(0, 'rbsplsrf', 12, 9);
insert into geometry values(0, 'agsrf', 13, 0);
insert into geometry values(0, 'sdata', -1, 0);
insert into geometry values(0, 'netkey', -1, 0);
insert into geometry values(0, 'solid', 14, 4);
insert into geometry values(0, 'agshell', 15, 0);
insert into geometry values(0, 'edge', -1, 0);
insert into geometry values(0, 'body', 16, 1);
insert into geometry values(0, 'coordsys', 17, 0);
insert into geometry values(0, 'member', -1, 0);
insert into geometry values(0, 'grouper', 18, 1);
insert into geometry values(0, 'drawing', 19, 1);
insert into geometry values(0, 'layers', -1, 0);
insert into geometry values(0, 'layer', 20, 1);
insert into geometry values(0, 'light', 21, 0);
insert into geometry values(0, 'attrdata', 22, 0);
insert into geometry values(0, 'transf', 23, 0);
insert into geometry values(0, 'attrmdl', 24, 0);
insert into geometry values(0, 'mtrlmdl', 25, 0);
insert into geometry values(0, 'dispattr', 26, 0);
insert into geometry values(0, 'drwmdl', 27, 0);
insert into geometry values(0, 'labelmdl', 28, 0);
insert into geometry values(0, 'viewdef', 29, 0);
insert into geometry values(0, 'vport', 30, 0);
insert into geometry values(0, 'screen', 31, 0);
insert into geometry values(0, 'sysattr', 32, 0);
insert into geometry values(0, 'geom', -1, 0);
insert into geometry values(0, 'text_nod', -1, 0);
insert into geometry values(0, 'snap_nod', -1, 0);
insert into geometry values(0, 'masters', -1, 0);
insert into geometry values(0, 'inst', -1, 0);
insert into geometry values(0, 'acon', 33, 0);
insert into geometry values(0, 'symbol', 34, 5);
insert into geometry values(0, 'instance', 35, 3);
insert into geometry values(0, 'symattr', 36, 0);
insert into geometry values(0, 'ainst', -1, 0);
insert into geometry values(0, 'conector', 37, 1);
insert into geometry values(0, 'part_lis', 38, 0);
insert into geometry values(0, 'geometry', 39, 0);
insert into geometry values(0, 'assocs', -1, 0);
insert into geometry values(0, 'MTID', 40, 1);
insert into geometry values(0, 'unimod', 41, 0);
insert into geometry values(0, 'unistat', 42, 0);
insert into geometry values(0, 'txtblk', -1, 0);
insert into geometry values(0, 'arcblk', -1, 0);
insert into geometry values(0, 'lineblk', -1, 0);
insert into geometry values(0, 'arrowblk', -1, 0);
insert into geometry values(0, 'assoblk', -1, 0);
insert into geometry values(0, 'draft', 43, 5);
insert into geometry values(0, 'hatchlin', 44, 1);
insert into geometry values(0, 'func', -1, 0);
insert into geometry values(0, 'qstb', 45, 1);
insert into geometry values(0, 'txt', 46, 1);
insert into geometry values(0, 'txtattr', 47, 0);
insert into geometry values(0, 'nclattr', 48, 0);
insert into geometry values(0, 'nclpt', 49, 1);
insert into geometry values(0, 'nclln', 50, 1);
insert into geometry values(0, 'nclci', 51, 1);
insert into geometry values(0, 'vector', 52, 1);
insert into geometry values(0, 'nclpl', 53, 1);
insert into geometry values(0, 'matrix', 54, 1);
insert into geometry values(0, 'param', -1, 0);
insert into geometry values(0, 'segment', -1, 0);
insert into geometry values(0, 'curve', 55, 3);
insert into geometry values(0, 'patch', -1, 0);
insert into geometry values(0, 'panel', 56, 1);
insert into geometry values(0, 'panelkey', -1, 0);
insert into geometry values(0, 'surface', 57, 6);
insert into geometry values(0, 'revsurf', 58, 5);
insert into geometry values(0, 'mpatch', -1, 0);
insert into geometry values(0, 'meshsf', 59, 5);
insert into geometry values(0, 'qpatch', -1, 0);
insert into geometry values(0, 'quiltsf', 60, 5);
insert into geometry values(0, 'patpnt', -1, 0);
insert into geometry values(0, 'patern', 61, 2);
insert into geometry values(0, 'netsf', 62, 4);
insert into geometry values(0, 'shapwd', -1, 0);
insert into geometry values(0, 'shape', 63, 3);
insert into geometry values(0, 'scalar', 64, 0);
insert into geometry values(0, 'labloc', -1, 0);
insert into geometry values(0, 'labtbl', 65, 1);
insert into geometry values(0, 'evwd', -1, 0);
insert into geometry values(0, 'evalcv', 66, 2);
insert into geometry values(0, 'evalsf', 67, 3);
insert into geometry values(0, 'nclpv', 68, 1);
insert into geometry values(0, 'ibndykey', -1, 0);
insert into geometry values(0, 'trimsf', 69, 7);
insert into geometry values(0, 'datael', -1, 0);
insert into geometry values(0, 'datast', 70, 1);
insert into geometry values(0, 'textvar', 71, 0);
insert into geometry values(0, 'color', 72, 0);
