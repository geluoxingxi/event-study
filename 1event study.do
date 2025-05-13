use eventdata.dta, clear
sort code eventdate 
gen 新变量=_n
*将打开的事件日期文件与证券代码合并
joinby code using "Return data.dta" 
*优先以新变量排序和交易日期排序
sort 新变量  date1 
*= 交易日期转化连续数字
by 新变量: gen date_num=_n
* 变化日期格式
rename date1 trade_date
format trade_date %td
gen event_date=date(eventdate,"YMD") 
format event_date %td
* 部分公司事件公告日不是在交易日 以往后推最近的一个交易日 
gen temp=trade_date-event_date
* 事件日期之前的不算在内，随便赋值一个很高的数值，以便后面剔除
replace temp=99999 if temp<0  
egen min_dif = min(temp),  by(新变量)
drop if min_dif==99999
gen target=date_num if temp==min_dif
egen td=mean(target), by(新变量)
drop temp min_dif target
gen dif=date_num-td
* 事件窗口期[0, +10]  
bys 新变量: gen event_window=1 if dif>=0 & dif<=10
* 估计窗口期
bys 新变量: gen estimation_window=1 if dif>=-60 & dif<0
replace event_window=0 if event_window==.
replace estimation_window=0 if estimation_window==.
drop if event_window==0 & estimation_window==0

* 剔除估计窗口期不足的
bys 新变量: egen estimation_num=sum(estimation_window)
drop if estimation_num<50  
bys 新变量: egen event_num=sum(event_window)
drop if event_num<11   
egen id=group(新变量)
*（形成预期回报率变量并赋值全为.）
gen predicted_return=. 
gen residual=. 
sum id
global MaxID=r(max)
* 计算估计收益率 (循环回归 运行时间可能会久一些)
forvalues i=1(1)$MaxID {
	qui reg log_returnsFTSE log_returnsSP if id==`i' & estimation_window==1 
	predict temp if id==`i' 
	predict temp_residuals, residuals
	replace predicted_return=temp if id==`i' & event_window==1
	replace residual=temp_residuals
	drop temp
	drop temp_residuals
}
*n为估计窗口的长度
bysort id dif: gen sd_estimation = sqrt(sum(residual^2)/(n))
bysort id dif: gen ci_low1=predicted_return - 1.96*sd_estimation/(n)^(1/2) 
bysort id dif: gen ci_high1=predicted_return + 1.96*sd_estimation/(n)^(1/2)
* 异常收益率AR
keep if event_window==1
gen AR=log_returnsFTSE-predicted_return
gen AR_low=log_returnsFTSE-ci_low1
gen AR_high= log_returnsFTSE-ci_high1 
* 计算CAR
sort id dif
by id: egen CAR总和=sum(AR)
by id, sort: generate CAR = sum(AR)
by id, sort: generate CAR_low = sum(AR_low)
by id, sort: generate CAR_high = sum(AR_high)
save temp.dta, replace

*单个T检验
use temp.dta, clear
set matsize 1300
asdoc by id:ttest CAR==0

* 多样本走势图
use temp.dta, clear
*放大100倍数字可读性高，也可不放大
gen CAR1=CAR*100
gen CAR_low1=CAR_low*100
gen CAR_high1=CAR_high*100
collapse (mean) AAR=AR CAR1 CAR_low1 CAR_high1 if dif>=0, by(dif)
label var AAR "AAR"
label var CAR1 "CAAR"
twoway (rarea CAR_low1 CAR_high1 dif, color(gray)) (line CAR1 dif , lcolor(navy ) lpattern(longdash) legend(ring(0) position(11) cols(1)), ytitle(), xtitle("Event Window")), ylabel(#10, format(%7.2f))
graph save Graph "D:\总CAAR.gph"

*CAAR显著性
use 总temp.dta, clear
gen CAR1=CAR*100
bysort dif: ttest CAR1=0
