data _null_;
	sleeptime='9feb2016:08:00'dt-datetime();
	call sleep(sleeptime,1);
run;


/*连接数据库*/
/***********************************************/
*目录创建; 
%LET FIL_PTH=Q:\MIS\zhangyj\mis需求\;

data _null_;
	call symput('_dte',put(today()-2,yymmddn8.));
	call symput('_mth',put(intnx('month',today(),-1,'end'),yymmddn8.));
	call symput('_mth6',put(intnx('month',today(),-1,'end'),yymmn6.));
	call symput('stt_Dte','01JAN2014'd);
	call symput('end_Dte','10DEC2014'd); 
run;
%Put &_dte &_mth &_mth6. &_Bdate. &_Bdate_Int ;

%libname(D&_dte.,ADHDB,db441); 
%libname(NTR,ODSDB,NTR);
%libname(CLB,ODSDB,CLB); 
LIBNAME mysas "&FIL_PTH.\data";
options user=mysas;
options mprint mlogic;
/*察看日志*/
options sastrace=',,,d' sastraceloc=saslog nostsuffix; 
proc printto log="&FIL_PTH.\log.log" new;
run;


/*导入原始数据集*/
proc import out = scgw.org
  datafile = 'Q:\MIS\zhangyj\Mis需求\PB\PBD01120141215D00064199 四川省分行公务卡审核数据\04 数据集\or_data.xls'
  dbms = excel2000 replace;
  getnames = yes;
run;
/*导入txt*/
data org;
	infile "&FIL_PTH.\04 数据集\cobrand.txt" dsd Truncover lrecl=30000;
	input mem_nbr_txt $1-40;
run;
/*导入csv*/ 
data sm;
	infile "Z:\MIS\zhangyj\mis需求\MKT\MKT01120150604D00065633 承保客户手机号变更统计\04 数据集\生命.csv" firstobs=2 dlm=',' dsd Truncover lrecl=30000;
	informat VAR1 $50.;
	informat VAR2 $50.;
	informat VAR3 $50.;
	format VAR1 $50.;
	format VAR2 $50.;
	format VAR3 $50.;

	input VAR1 VAR2 VAR3;
run;

/*申请表*/
proc sql;
create table evt
as
select t.app_inr_id
       ,t.app_id
       ,t.bch_id
       ,t.ful_nme
       ,t.Man_Crd_Pan_Txt
       ,t.Fnl_Apr_Rst_Cde
       ,rt.pan_txt
       ,rt.Pla_Sts_Cde
       ,rt.Pdt_Id
       ,rt.Rmb_Act_Typ_Cde
 from db441.EVT_APP_NEW t
 left join db441.CRD_CRD_CRT rt
 on rt.App_Id = t.app_id
quit;


/*标记手机号分类：三大运营商 家用电话 86打头*/
data tesco.csr_detail;
 	set db441.CSR_CSR;      
    Mob_phe=COMPRESS(Mob_Phe,'0123456789','K'); 
	IF LENGTHN(mob_phe)=14 and substr(mob_phe,1,3)='086' then mob_phe=substr(mob_phe,4,11);
	ELSE IF LENGTHN(Mob_Phe)=12 AND SUBSTR(Mob_Phe,1,1)='0' THEN Mob_Phe=SUBSTR(Mob_Phe,2,11); 
  	IF LENGTHN(Mob_Phe)=11 THEN Mob_Typ = PUT(SUBSTR(Mob_Phe,1,3),$Des_Mob_Typ_Cde.);
	ELSE Mob_Typ='其他';		 
	IF Mob_Typ IN ('移动','联通','电信') then flag_mob = 1;
	else flag_mob = 0;
 	keep Csr_ID Csr_Rfe_Nbr_Txt Mob_phe flag_mob;
run;

/*同一手机号码对应多个客户，则多个客户均进行沟通屏蔽*/
proc sql;
	create table tesco.Csr_Mobile
	as
	select Csr_ID
	from tesco.csr_detail
	where Mob_phe in(
		select Mob_phe
		from csr_detail 
		where flag_mob=1
		group by Mob_phe
		having count(distinct Csr_ID)>1
		)
	;
quit;

/*新的去手机号方式*/
/*标记手机号分类：三大运营商 家用电话 86打头*/
proc sql;
create table csr_detail
as
select Csr_ID
       ,Csr_Rfe_Nbr_Txt
       ,Mob_phe
       ,MOB_OPR_CDE
	   ,case when MOB_OPR_CDE in('CM','CU','CT') then 1 
	    else 0 end as flag_mob
	   from db441.CSR_CSR
;
quit; 

/*取白金卡*/
proc sql;
	create table bjk
	as
	select a.top_act_id
	from db441.Crd_Crd_Crt as a
	inner join db441.Prd_prd_csf as b
	on a.PDT_ID=b.PDT_ID
		and a.rmb_act_typ_cde=b.act_typ_cde
		and b.CRD_LVL_CDE='3'
	where FNL_PLA_STS_CDE IN('A','N');
quit;
/*公务卡*/
proc sql;
create table gwk
as
	select a.top_act_id
	from db441.Crd_Crd_Crt as a
	inner join db441.Prd_prd_csf as b
	on a.PDT_ID=b.PDT_ID
		and a.rmb_act_typ_cde=b.act_typ_cde
		and b.pdt_typ_cde in('12','13','46','72')
	where a.FNL_PLA_STS_CDE IN('A','N')
quit;


/* boss卡(北京市分行)*/
proc sql;
create table crd_bs
as
	select a.top_act_id
           ,a.Pan_Txt
           ,a.Rmb_Act_Id
           ,a.Bch_Id
           ,b.pdt_typ_cde
           ,a.Csr_Id
           ,a.Rmb_Slt_Act_Id
           ,a.FNL_PLA_STS_CDE
           ,a.Man_Act_Ind
	from db441.Crd_Crd_Crt as a
	inner join db441.Prd_prd_csf as b
	on a.PDT_ID=b.PDT_ID
		and a.rmb_act_typ_cde=b.act_typ_cde
		and b.pdt_typ_cde in('62')
	where a.FNL_PLA_STS_CDE IN('A','N')
          and a.bch_id = '091';
quit;


/*proc noprint*/
proc sql;
	create table Bch1
	as
	select a.*,b.Bch_ID
	from Bch as a
	left join db441.dim_bch as b
	on a.bch_nme=b.bch_nme;
quit;
proc sql noprint;
	select compress(Trim(Bch_ID)) into:Bch_list separated by '","' 
	from Bch1 
	;
quit;
%put &Bch_list;


/*输出*/

DATA _NULL_;                                                                                                   
SET mob2;
FILE  "&FIL_PTH.\output\春节乐购华东区活动手机号.del"  dlm=',' DROPOVER  LRECL=32767; 
PUT CSR_RFE_NBR_TXT mob_phe;       
RUN;  
DATA _NULL_;                                                                                                   
SET mob2_test;
FILE  "&FIL_PTH.\output\春节乐购华东区活动抽检.del"  dlm=',' DROPOVER  LRECL=32767; 
PUT pan_txt mob_phe Bch_Nme typnme;       
RUN; 

/*分包输出 */
%macro outdel(OutPutFilePath,datasetname,ColNmes,Nums);
proc sql noprint;
  	select count(*) into:nbr
    from &datasetname.;
quit;
%put nbr.;

%let idx=%SYSFUNC(CEIL(&nbr./&Nums.));
		%do ii=1 %to &idx.;
			%let i= %sysfunc(int(&ii.-1));
			%let str_obs=%sysfunc(int(&Nums.*&i.+1));
			%let end_obs=%sysfunc(min(%eval(&str_obs.+&Nums.)-1,&nbr.));

			DATA _NULL_;                                                                                                   
				SET &datasetname.(firstobs=&str_obs. obs=&end_obs.);
				FILE  "&OutPutFilePath.&ii..del"  dlm=',' DROPOVER  LRECL=32767;  
				PUT &ColNmes.;       
			RUN; 
		%end; 
%mend;
%outdel(&FIL_PTH.\output\沃尔玛门店_非白金卡_&_dte._,mob2_5,CSR_RFE_NBR_TXT mob_phe,499999); 

libname xls excel "&FIL_PTH.\output\上海地区2011年各月有境外消费的客户数.xls";
data xls.sheet1(dblabel=yes);
	set R2;
    title '上海地区2011年各月有境外消费的客户数';
	label _LABEL_='月份(上海地区)';
	drop Test ;
run;
libname xls clear;



/***********************************************/
/*分多个sheet输出*/ 
proc sql noprint;
  	select count(distinct Des_Tye) into:nbr
    from R;
quit;
proc sql noprint;
	select distinct Des_Tye into:Des_Tye1-:Des_Tye%left(&nbr.)
	from R;
quit;
*%put _all_;
%macro outxls;
libname xls excel "Z:MIS\kuangdp\mis\pbd\PBD01120120514D00066074_商户交易流水\output\交易流水.xls";

%do t= 1 %to &nbr;
data temp;
	set R1;
	where Des_Tye = "&&Des_Tye&t.";
	drop Des_Tye;
run;
proc sql noprint;
	select sum(cnt) as cnt into:cnt_&t.
	from R2
	where Des_Tye = "&&Des_Tye&t.";
quit;

	%let idx=%SYSFUNC(CEIL(&&cnt_&t./65535));
		%do ii=1 %to &idx.;
		%let i= %sysfunc(int(&ii.-1));
		%let str_obs=%sysfunc(int(65535*&i.+1));
		%let end_obs=%sysfunc(min(%eval(&str_obs.+65534),&&cnt_&t.));

		data xls.&&Des_Tye&t.._&&ii.(dblabel=yes);
		    set temp(firstobs=&str_obs. obs=&end_obs.);
		run;
		%end;
%end;

libname xls clear;
%mend;
%outxls;

/*分多个sheet输出*/
libname xls excel "&FIL_PTH.\output\沃尔玛店员推荐手工导入刷卡金结果_测试文件.xls";
	data xls.一号1人(dblabel=yes);
		set Tmp6(keep=_COL0 _COL1 _COL2 _COL3 _COL4 Pan_Txt _COL6 FNL_PLA_STS_CDE); 
		label PanTxt="主卡号";
	run;
	data xls.一号多人(dblabel=yes);
		set split_b(drop=Cnt_Csr); 
	run;
	data xls.一号0人(dblabel=yes);
		set split_c(drop=Cnt_Csr); 
	run;
libname xls clear;


/*一般消费，一般取现，转账*/
proc sql noprint;
	select a.txn_cde into:txn_cde_list separated by '","'
	from db441.Dim_txn_cde_csf as a 
	where a.Txn_Sub_Csf_Cde IN('21','31','03') /*一般消费，一般取现，转账 */
	;
quit;

/*汇总交易金额*/
%macro fun;
%do ii = 201101 %to 201201;
	%if %sysfunc(substr(&ii.,5,2))=13 %then %let ii = %eval(&ii. + 88);
proc sql;
	create Table Txn1_&ii
	as
	select e.Act_ID
			,PUT(Txn_Dte,yymmn6.) as Txn_YMT
			,count(*) as txn_cnt
			,sum(case when ptg_cry_cde = '840' 
					then 7 * ptg_amt * 0.01 
					else ptg_amt * 0.01 end) as Amt 
	from db441.evt_txn_&ii as e 
	where e.txn_cde in ("&txn_cde_list.") 
		and Txn_Dte between '01JAN2011'd and '31DEC2011'd
	group by 1,2
	;
quit;
%end;

data Txn1_all;
	set %do jj = 201101 %to 201201;
		%if %sysfunc(substr(&jj.,5,2))=13 %then %let jj = %eval(&jj. + 88);
		Txn1_&jj.
		%end;;
run;

%mend; 
%fun;
 
 
/*查找在某一商户是否有消费记录*/
*店内一般消费;
proc sql noprint;
	select txn_cde into: txn_cde separated by '","'
	from pdm.dim_txn_cde_csf
	where TXN_SUB_CSF_CDE='21';
quit;

%macro txn;
%do m=%sysfunc(putn(&stt_Dte.,yymmn6.)) %to %sysfunc(putn(&end_Dte.,yymmn6.));
%if %sysfunc(substr(&m.,5,2))=13 %then %let ii = %eval(&m. + 88);
proc sql;
	create table dn_txn_&m. as
	select a.rmb_ptg_act_id
		  ,a.rmb_slt_act_id
		  ,a.crd_acr_id
		  ,a.crd_acr_tml_txt
		  ,a.txn_dte
		  ,a.txn_tme
		  ,a.txn_cde
		  ,a.ptg_amt
		  ,a.ptg_cry_cde
		  ,a.pan_txt
		  ,b.acr_typ
	from pdm.evt_txn_&m.(where=(txn_dte>=&stt_Dte and txn_dte<=&end_Dte. and txn_cde in ("&txn_cde."))) as a
	inner join (select distinct acr_typ,crd_acr_id from mer_n) as b on a.crd_acr_id=b.crd_acr_id
	;
quit;
%end;
data dn_txn;
	set %do m=%sysfunc(putn(&stt_Dte.,yymmn6.)) %to %sysfunc(putn(&end_Dte.,yymmn6.));
		dn_txn_&m.
	%end;;
run;
%mend;
%txn;


%macro aaa;
%do mth =201501 %to 201507;
%if %substr(&mth.,5,2)=13 %then %let mth = %eval(&mth.+88);
data _NULL_;
	set cde_ecg_rat_mth end = last;
	retain ECG_RAT_VAL;
	if sta_mth = "&MTH." THEN CALL SYMPUT("rat",ECG_RAT_VAL);
	else if sta_mth < "&MTH." and last then  CALL SYMPUT("rat",ECG_RAT_VAL);
RUN;
PROC SQL;
	CREATE TABLE TXN_&MTH. AS
	SELECT
		 rmb_ptg_act_id
		,txn_dte
		,(case when Dbt_Cdt_Cde="DEBT" and ptg_cry_cde="156" then Ptg_Amt
		 		  when Dbt_Cdt_Cde="CRED" and ptg_cry_cde="156" then -Ptg_Amt
			      when Dbt_Cdt_Cde="DEBT" and ptg_cry_cde="840" then Ptg_Amt*(&rat)
			      when Dbt_Cdt_Cde="CRED" and ptg_cry_cde="840" then -Ptg_Amt*(&rat) else 0 end)*0.01 as amt
	FROM PDM.Evt_txn_&MTH.
	where txn_cde in ("&gnr_cns_cde.") and crd_acr_id in ("&aim_acr.")
	; 
QUIT;
%end;
data TXN;
set
%do mth =201501 %to 201507;
%if %substr(&mth.,5,2)=13 %then %let mth = %eval(&mth.+88);
	TXN_&MTH.
%end;
;
run;
%mend;

%aaa;


/*数据集比对*/
proc sql;
	create table t1_k2z
	as
	select distinct CSR_RFE_NBR_TXT, csr_id,mob_phe ,pan_txt
	from kdp.mob2
	except
	select distinct CSR_RFE_NBR_TXT, csr_id, mob_phe ,pan_txt
	from Final_wemk_normal_nbj;
quit;
proc sql;
	create table t1_z2k
	as 
	select distinct CSR_RFE_NBR_TXT, csr_id, mob_phe ,pan_txt
	from Final_wemk_normal_nbj
	except
	select distinct CSR_RFE_NBR_TXT, csr_id, mob_phe ,pan_txt
	from kdp.mob2
;
quit;


/*分段输出*/
proc sql;
	create table crd3
	as 
	select case when a.IsSMDN=1 and Bch_ID in ("&Bch_list_sm.") and IsBjk=1 then '1.sm_bj'
				when a.IsSMDN=1 and Bch_ID in ("&Bch_list_sm.") and IsBjk=0 then '2.sm_fbj'
				when Bch_ID in ("&Bch_list.") and IsBjk=1 then '3.wrm_bj'
				when Bch_ID='061' and IsBjk=0 then '4.wrm_wh'
				when Bch_ID in ("&Bch_list.") and IsBjk=0 then '5.wrm_fbj'
				else 'oth' end as Type 
		,*
	from crd2 as a
	where a.top_act_id not in(select top_act_id from tx_top)
		and a.csr_id not in(select csr_id from tx_csr)
		and a.csr_id not in(select csr_id from Csr_MobUp1)
		and a.Rmb_Act_Id not in(select rmb_ptg_act_id from tx_ptg) 
	;
quit;


/*格式化文本*/
data scgw.app_card_chi;
  set scgw.app_card;
  length app_status card_status $ 10;
  if fnl_apr_rst_cde = 'A' then app_status = '通过';
  else if fnl_apr_rst_cde in('N','C') then app_status = '未通过';
  if Fnl_Pla_Sts_Cde = 'A' then card_status = '已激活';
  else if Fnl_Pla_Sts_Cde = 'N' then card_status = '待激活';
  else if Fnl_Pla_Sts_Cde = 'C' then card_status = '销卡';
run;


/******临时表使用案例*******/
%libname(mis,adhdb,mis);
data null;
	call symput("sasglbid",substr(cats("&SYSJOBID.",compress("&SYSPROCESSID.",'123456789','K')),1,15));
run;
%put &sasglbid.;

proc sql;
	%connect(adhdb);
	execute (delete from mis.zhangyunjie where num1 = &sasglbid.) by db2;
	disconnect from db2;
quit;

proc sql;
	insert into mis.zhangyunjie(num1, num2, num3)
	select distinct &sasglbid., top_act_id, typ_cnt
	from t03_csr_crd
	;
quit;

proc sql;
	create table t04_crd as
	select a.rmb_act_id as rmb_ptg_act_id
		, a.rmb_slt_act_id
		, a.top_act_id
		, a.csr_id
		, a.pan_txt
		, a.fnl_pla_sts_cde
		, a.ope_dte
		, c.hex_act_id
		, d.pdt_id || d.rmb_act_typ_cde as tpk		
		, e.ful_nme
		, e.mob_phe
		, e.csr_rfe_nbr_txt
		, t.num3 as typ_cnt 
	from mis.tianhy t
	join pdm.crd_crd_crt a on t.num2 = a.top_act_id and a.fnl_pla_sts_cde in ('A','N','C') and a.man_act_ind = '1'
	join pdm.prd_prd_csf b on a.pdt_id = b.pdt_id and a.rmb_act_typ_cde = b.act_typ_cde and b.pdt_bnd_cde = '1'
	join pdm.act_act c on a.rmb_slt_act_id = c.act_id
	join pdm.act_slt_act_crt d on a.rmb_slt_act_id = d.rmb_act_id	
	join pdm.csr_csr e on a.csr_id = e.csr_id
	where t.num1 = &sasglbid.
	;
quit;



/*增加标签并删去多余变量*/
Data mis.SglRcd01;
    set mis.SingleRecord01(keep=_COL6 Txn_Des_Txt Tot_Amt Txn_Tms); 
	Label Txn_Des_Txt='交易描述'
          Tot_Amt='交易金额'
          txn_tms='交易笔数';
run;

Data mis.mtplRcd01;
    set mis.multipleRecord01(keep=_COL6 Txn_Des_Txt Tot_Amt Txn_Tms); 
	Label Txn_Des_Txt='交易描述'
          Tot_Amt='交易金额'
		  txn_tms='交易笔数';
run;	


/*周期末拖欠账户*/
%macro evt(ii);
Proc sql;
	Create Table Act_&ii
	as
	select e.Rmb_Act_Id 
	from pdm.ACT_SLT_ACT_EOC_&ii as e 
	where (put(e.rmb_atc_sts_cde,$fmt_dqt_sts_cde.)>'M0'
			Or put(e.usd_atc_sts_cde,$fmt_dqt_sts_cde.)>'M0')
	;
quit;
%mend;
%macro fun;
%do ii = 201101 %to 201112;
	%if %sysfunc(substr(&ii.,5,2))=13 %then %let ii = %eval(&ii. + 88);
	%evt(&ii.);
%end;
%mend;
%fun;

/*问题*/
proc sql;
	create table Crd4
	as
	select distinct a.Bch_Id
		,a.Csr_Id
	from Crd3 as a
	inner join pdm.ACT_SLT_ACT_CRT as b 
	on a.Rmb_Slt_Act_Id=b.Rmb_Act_Id 
	where put(b.rmb_atc_sts_cde,$fmt_dqt_sts_cde.)='M0'
		and put(b.usd_atc_sts_cde,$fmt_dqt_sts_cde.)='M0'
		and b.rmb_mnl_sts_cde in ('','VIP')
		and b.usd_mnl_sts_cde in ('','VIP')
	;
quit;


/*分sheet页统计，页码不一致
a)	关联到一个客户的，单独列到一个sheet里，名为"1号1人"；
b)	如果关联出多个客户，单独列到一个sheet里，名为"1号多人"，该批客户暂不生成导刷卡金文件，返回PB确认，在下次需导入时使用；
c)	如果关联不到客户，单独列到一个sheet里，名为"1号0人"，该批客户暂不生成导刷卡金文件，返回PB确认，在下次需导入时使用；
*/
* 
proc sql;
	create table tmp1_split
	as
	select t1.*
		,t2.Cnt_Csr "号码对应客户数"
	from import_PB as t1
	inner join (
		select a._COL3 
			,count(distinct Csr_id) as Cnt_Csr
		from tmp1 as a
		group by a._COL3
	) t2
	on t1._COL3=t2._COL3
	where t1._COL5 = ''
	;
quit;

data split_a split_b split_c;
	set tmp1_split; 
	if Cnt_Csr=1 Then Output split_a; 
	else if Cnt_Csr>1 Then Output split_b;
	else if Cnt_Csr=0 Then Output split_c;
	;
run;


/*排序保留重复第一行*/
proc sort data=tmp3_1 out=tmp4;
	by _COL3 DESCENDING Man_Act_Ind DESCENDING IsSameCsr_id DESCENDING Max_Dte1 Pan_Txt;
run;

data tmp5;
	set tmp4;
	by _COL3 DESCENDING Man_Act_Ind DESCENDING IsSameCsr_id DESCENDING Max_Dte1 Pan_Txt;
	if first._COL3;
run;

proc sort data = dm_in.very_crd3 out = dm_in.ss1 dupout = dm_in.ss nodupkey;
by top_act_id;
run;

/*修改日期格式，日期转换字符串*/
data Crd_pan_act_rlt11;
set Crd_pan_act_rlt1;
date1 = put(ORD_STT_APP_DTE,yymmddn8.);
date2 = put(ORD_APP_DTE,yymmddn8.);
format date1 $ 8.;
format date2 $ 8.;
run;

/*字符串转换日期*/
data custom_top1;
set custom_top;
begdate1 = input(begdate,yymmdd10.);
format begdate1 date9.;
run;

/*datepart日期处理*/
data coffee_final1;
set coffee_finals;
used_date = datepart(USED_TIME);
check_date = mdy(substr(checkdate,5,2),substr(checkdate,7,2),substr(checkdate,1,4));
format check_date yymmddn8.;
format used_date yymmddn8.;
run;


/*随机取数据*/
proc surveyselect data=Customers
         method=sys rate=.02
         seed=1234 out=SampleControl;
      strata State;
      control Type Usage;
run;
proc surveyselect data=Mob_R2_1 
	method=sys N=850000 
	out=Samp85W;
run;

/*交易*/
%macro evt(name,begin_date,end_date,amt);
%do m=%sysfunc(putn(&begin_date.,yymmn6.)) %to %sysfunc(putn(&end_date.,yymmn6.));
	%if %sysfunc(substr(&m.,5,2))=13 %then %let m = %eval(&m. + 88);
proc sql;
	create Table Txn_&m._&name.
	as
	select e.Act_ID
			,e.Rmb_ptg_act_id
			,e.Rmb_slt_act_id
			,e.pan_txt
			,e.Txn_Dte 
			,e.Txn_Tme
			,e.Crd_Acr_Id
			,case when ptg_cry_cde = '840' 
					then 7 * ptg_amt * 0.01 
					else ptg_amt * 0.01 end as Amt 
	from db441.evt_txn_&m as e
    join mis.zhangyunjie t on t.num2 = e.Rmb_ptg_act_id
	where e.txn_cde in ("&txn_cde_list.") and 
        e.Txn_Dte between &begin_date and &end_date
	;
quit;
%end;
data dn_txn_&name;
	set %do m=%sysfunc(putn(&begin_date.,yymmn6.)) %to %sysfunc(putn(&end_date.,yymmn6.));
		%if %sysfunc(substr(&m.,5,2))=13 %then %let m = %eval(&m. + 88);
		Txn_&m._&name.
	%end;;
run;
%mend;

/*交易日期：活动期间（2015-1-9~2015-2-10）*/
%evt(20150109,'09JAN2015'd,'10FEB2015'd);
/*活动前交易日期：（2014-12-7~2015-1-8）*/
%evt(20141207,'07DEC2014'd,'08JAN2015'd);
/*去年同期交易日期：（2014-1-9~2014-2-10）*/
%evt(20140109,'09JAN2014'd,'10FEB2014'd);


/*format使用*/
data fmt_rat_val;
	format start $6. label 18.4;
	informat label 18.4;
	set db441.cde_ecg_rat_mth;
	start=sta_mth; 
	label=ecg_rat_val; 
	fmtname="$rat_val"; 
run;

proc format cntlin=fmt_rat_val cntlout=work.tmp;
run;


/**************账户状态正常（清算层 M0/人工状态 ''或者VIP）*****************/
data temp_normal(keep=rmb_act_id);
	set db441.act_slt_act_crt(keep=rmb_act_id rmb_atc_sts_cde usd_atc_sts_cde 
								 rmb_mnl_sts_cde usd_mnl_sts_cde );
	where put(rmb_atc_sts_cde,$FMT_DQT_STS_CDE.) in('M0','M1')
	and put(usd_atc_sts_cde,$FMT_DQT_STS_CDE.) in('M0','M1')
	and rmb_mnl_sts_cde in ('', 'VIP')
	and usd_mnl_sts_cde in ('', 'VIP');
run;


data temp_csr_m2;
	set db441.act_slt_act_crt;
	keep top_act_id;
	where put(rmb_atc_sts_cde,$fmt_dqt_sts_cde.)>='M2'
	or put(usd_atc_sts_cde,$fmt_dqt_sts_cde.)>='M2' 
	or rmb_mnl_sts_cde in ('CLOF','CLOP','CLSK','SFDJ','TRDJ','180B','180L',
			       'CLTS','CACA','FCBC','FQZW','TRQT','YSQZ','HZDJ')
	or usd_mnl_sts_cde in ('CLOF','CLOP','CLSK','SFDJ','TRDJ','180B','180L',
			       'CLTS','CACA','FCBC','FQZW','TRQT','YSQZ','HZDJ')
	;
	Typ='1';
run; 



format使用方法

crd_lvl_cde=put(pdt_id||rmb_act_typ_cde,$fmt_act_crd_lvl_cde.)


*hash 大表查找小表数据

data sms2;
set pdm.csr_csr(keep=csr_id csr_rfe_nbr_txt);
if 0 then set sms;
if _N_=1 then do;
declare hash dd(dataset:'sms');
dd.definekey('csr_rfe_Nbr_Txt');
dd.definedata(all:'yes');
dd.definedone();
end;
if dd.find(Key:csr_rfe_nbr_Txt)=0;
run;


/*转换人民币清算层账号*/
proc sql;
create table cpn_lty_act1
as
select t.*
       ,case when r.Usd_Slt_Act_Id is not null then r.rmb_Slt_Act_Id
        else t.slt_act_id end as rmb_Slt_Act_Id 
	   from CPN_LTY_ACT t
	   left join pdm.ACT_SLT_ACT_CRY_RLT r
	   on t.slt_act_id = r.Usd_Slt_Act_Id
;
quit;
data cpn_lty_act1;
set cpn_lty_act1;
format rmb_Slt_Act_Id 20.;
run;

proc sql;
create table cpn_lty_act2
as
select t.rmb_Slt_Act_Id,sum(t.Avl_Lty_Pts) as Avl_Lty_Pts1
from cpn_lty_act1 t
group by 1
;
quit;



/*奖励积分模板*/
/*奖励文件：
1.	积分奖励文件：加油走红未参与客户EDM调研奖励.txt
奖励类型(JF);HEX_ACT_ID;刷卡金金额;TPK;备注
*/

proc sql;
create table crd4
as
select a.*
       ,c.pdt_id || c.rmb_act_typ_cde as tpk
	   ,'5000' as jf_awd
	   ,'JF' as type
	   ,'' as remark
       from crd3 a 
	   join db441.act_slt_act_crt(dbkey=rmb_act_id) c on a.rmb_slt_act_id = c.rmb_act_id
;
quit;

proc sql;
	create table crd5 as
	select a.*
		, b.hex_act_id
		, d.ful_nme
		, d.mob_phe_nbr as new_mob_phe_nbr
		, d.csr_rfe_nbr_txt as new_csr_rfe_nbr_txt
	from crd4 a 
	join db441.act_act(dbkey=act_id) b on a.rmb_slt_act_id = b.act_id
	join db441.csr_csr(dbkey=csr_id) d on a.csr_id = d.csr_id
	;
quit;
DATA _NULL_;                                                                                                   
SET crd5;
FILE  "&FIL_PTH.\05 提数报表\加油走红未参与客户EDM调研奖励.txt"  dlm=';'; 
PUT type hex_act_id jf_awd tpk remark;       
RUN; 



/*沟通数据
2.	短信沟通文件：加油超红未参与客户EDM调研奖励通SR知.del
客户参考号，手机号，主卡卡号后四位
*/

DATA _NULL_;                                                                                                   
SET crd5;
FILE  "&FIL_PTH.\05 提数报表\加油超红未参与客户EDM调研奖励通知.del"  dlm=',' DROPOVER  LRECL=32767; 
PUT csr_rfe_nbr_txt mob_phe_nbr pan_txt1;       
RUN; 

/*CIM文件
3.奖励结果同时生成CIM文件，活动码：SA15PMU004000001，文件名：MIS_MKTACT_HTTION_ADD_FUELEDMAWD_ yymmdd.csv*/
proc sql;
	create table SKJ_cim_awd
	as
	select '"'||trim(Csr_Rfe_Nbr_Txt)||'"' as Csr_Rfe_Nbr_Txt,
		'"'||trim(pan_txt)||'"' as pan_txt1,
		'"'||'PB15RMU064000001'||'"' as _TEMA001,
		'"'||'AW03'||'"' as _TEMA002,
		'"'||"201512"||'"' as _TEMA003,
		'"'||"20151228"||'"' as _TEMA004,
		'' as _TEMA005,
		'' as _TEMA006,
		'' as _TEMA007,
		'' as _TEMA008,
		'' as _TEMA009,
		'' as _TEMA010,
		'' as _TEMA011,
		'' as _TEMA012,
		'' as _TEMA013,
		'2015-12-28' as _tema014,
		'' as _tema015,
		'' as _TEMA016,
		skj1 as _TEMA017
		from crd5
    ;
quit;

/*hive里面去字段*/
data test;
do x= 1 to 2;
x=x+1;
output;
end;
run;

data test1;
set test;
x1 = input(x,$100.);
x1 = 'index.html?channel=HR37';
run;

data test2;
set test1;
x2 = find(x1,'channel=','i');
x3 = find(x1,'&','i',x2+8);
if x3 ne 0 then x4 = substr(x1,x2+8,x3-x2-8);
else if x3 = 0 then x4 = substr(x1,x2+8);
run;


额度使用率计算公式：（总余额（不含分期）+分期余额-好享贷消费余额-人民币好享贷现金余额）/人民币总信用额度
解释：好享贷额度不占用客户CC01额度，故当期出账的好享贷消费余额需要剔除


data temp.rmc_high_risk_t02;
set pdm.act_slt_act_eoc_201502(keep=sta_mth rmb_act_id top_act_id Rmb_Non_Ilt_Tot_Bal_Amt Rmb_Idt_Cdt_Bal_Amt Rmb_Ilt_Bal_Amt
	Usd_Non_Ilt_Tot_Bal_Amt Usd_Idt_Cdt_Bal_Amt Usd_Ilt_Bal_Amt Rmb_Tot_Cdt_Lmt_Amt Rmb_Csh_Cdt_Lmt_Amt RMB_CSH_IDT_CDT_BAL_AMT);
	final_bal_amt=sum(max(Rmb_Non_Ilt_Tot_Bal_Amt,0),max(Usd_Non_Ilt_Tot_Bal_Amt,0)*input(put('201502',$rat_val.),18.4))/100;
	rmb_no_idt_bal=sum(max(sum(Rmb_Non_Ilt_Tot_Bal_Amt,-Rmb_Idt_Cdt_Bal_Amt,-RMB_CSH_IDT_CDT_BAL_AMT),0),Rmb_Ilt_Bal_Amt);
	usd_no_idt_bal=sum(max(sum(Usd_Non_Ilt_Tot_Bal_Amt,-Usd_Idt_Cdt_Bal_Amt),0),Usd_Ilt_Bal_Amt)*input(put(sta_mth,$rat_val.),18.4);
	no_idt_bal=sum(rmb_no_idt_bal,usd_no_idt_bal)/100;
	Rmb_Tot_Cdt_Lmt_Amt=Rmb_Tot_Cdt_Lmt_Amt/100;
	Rmb_Csh_Cdt_Lmt_Amt=Rmb_Csh_Cdt_Lmt_Amt/100;
run;




*额度使用率大于等于90%*/

proc sql;
create table temp.rmc_high_risk_t03 as
select top_act_id,rmb_act_id,no_idt_bal,Rmb_Tot_Cdt_Lmt_Amt,
no_idt_bal/Rmb_Tot_Cdt_Lmt_Amt as cdt_lmt_ratio
from temp.rmc_high_risk_t02
where no_idt_bal>=0.9*Rmb_Tot_Cdt_Lmt_Amt
and Rmb_Tot_Cdt_Lmt_Amt>0 ;
quit;

proc sort data=temp.rmc_HIGH_RISK_t03 nodupkey;
   by TOP_act_id;
run;


data fmt_rat_val;
	format start $6. label 18.4;
	informat label 18.4;
	set pdm.cde_ecg_rat_mth;
	start=sta_mth; 
	label=ecg_rat_val; 
	fmtname="$rat_val"; 
run;



/*输出 */
%macro outdel(OutPutFilePath,datasetname,ColNmes,Nums);
proc sql noprint;
  	select count(*) into:nbr
    from &datasetname.;
quit;
%put nbr.;
/*del要小写，CSR_RFE_NBR_TXT 不能重复*/
%let idx=%SYSFUNC(CEIL(&nbr./&Nums.));
		%do ii=1 %to &idx.;
			%let i= %sysfunc(int(&ii.-1));
			%let str_obs=%sysfunc(int(&Nums.*&i.+1));
			%let end_obs=%sysfunc(min(%eval(&str_obs.+&Nums.)-1,&nbr.));

			DATA _NULL_;                                                                                                   
				SET &datasetname.(firstobs=&str_obs. obs=&end_obs.);
				FILE  "&OutPutFilePath.&ii..del"  dlm=',' DROPOVER  LRECL=32767; 
				if _N_=1 then do; 
				      put "client_email_id,email";
				  end;
				PUT &ColNmes.;       
			RUN;  
		%end; 
%mend;

%outdel(&FIL_PTH.\05 提数报表\2015年太平洋远东百货年中庆刷卡积分加码EDM,crd5,CSR_RFE_NBR_TXT EML_ADR,499999);