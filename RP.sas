*计算换手率、交易年份、交易周;
data trd;
  set trd.trd_week;
  v=Wnshrtrd/(Wsmvosd/Wclsprc);
  label v="turnover";
  year=substr(Opndt,1,4);
  label year="交易年份";
  week=substr(Trdwnt,6,2);
  label week="交易周数";
run;
*根据股票代码、交易周份排序;
proc sort data = trd;
  by stkcd year week;
run;
*为排序好的数据集trd_sorted增加排序变量;
data trd;
  set trd;
  retain t; *给排序结果增加新的变量;
  by stkcd Trdwnt;
  if first.stkcd then t = 1;
  else t + 1;
  label t="每个股票上市周数";
  if t>265 then do;
    fenzi=0;
	fenmu=0;
    do n=1 to 265;
	  temp1=1;
	  do tao=1 to n-1 while (n>1);
	    temp2=1-lag<n-tao>(v);
		temp1=temp1*temp2;
	  end;
	  temp3=lag<n>(v)*lag<n>(Wclsprc)*temp1;
	  temp4=lag<n>(v)*temp1;
	end;
    fenzi=fenzi+temp3;
	fenmu=fenmu+temp4;
  end;
run;
	  
%macro lagn (n,x)
lagn=lag&i.(x);
if n=&i. then name=name&i.;
drop name&i.;
%end;
%mend
