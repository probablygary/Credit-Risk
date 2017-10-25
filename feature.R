###########################
### Feature Engineering ###
###########################
library('data.table')
library('plyr')

base = './Data/' # Data directory 
load(paste0(base, 'clean.Rdata'))

feat   = data.table(full_d[,c('apprefno', 'c', 'bad_flag_worst6')], key = 'apprefno')
x_a    = data.table(full_a[,c('apprefno', 'c')], key = 'apprefno')
x_e    = data.table(full_d[,c('apprefno', 'c')], key = 'apprefno')
full_e = data.table(full_e, key = 'apprefno')
x_d    = data.table(full_d[,c('apprefno', 'cibil_score', 'approved_credit_limit',
 'existing_credit_limit')], key = 'apprefno')

# Process payment history features for to numeric
pay_text = function(x){
	if (is.na(x)){
		return('XXX')
	}else{
		text = substring(x, seq(1,nchar(x),3), seq(3,nchar(x),3))
		text = gsub('STD', '000', text)
		return(as.numeric(text))
	}
}

hist1  = simplify2array(sapply(full_a[,'paymenthistory1'], pay_text))
hist2  = simplify2array(sapply(full_a[,'paymenthistory2'], pay_text))

# payment_history_avg_dpd_0_29_bucket
hist1_29 = sapply(hist1, function(x)  sum(x<30, na.rm = TRUE))
hist2_29 = sapply(hist2, function(x)  sum(x<30, na.rm = TRUE))
x_a$pay_dpd_0_29_bucket = apply(cbind(hist1_29, hist2_29), 1, sum)

# payment_history_avg_dpd_30_bucket
hist1_30 = sapply(hist1, function(x)  sum(x>30, na.rm = TRUE))
hist2_30 = sapply(hist2, function(x)  sum(x>30, na.rm = TRUE))
x_a$pay_dpd_30_bucket = apply(cbind(hist1_30, hist2_30), 1, sum)

# worst_dpd
hist1_worst = sapply(hist1, function(x)  as.numeric(max(x, na.rm = TRUE)))
hist2_worst = sapply(hist2, function(x)  as.numeric(max(x, na.rm = TRUE)))
x_a$worst_dpd = apply(cbind(hist1_worst, hist2_worst), 1, function(x) max(x, na.rm = TRUE))
x_a$worst_dpd = sapply(x_a$worst_dpd, function(x) ifelse(is.infinite(x), NA, x))

# payment_history_mean_length
hist1_total = sapply(hist1, function(x)  length(x))
hist2_total = sapply(hist2, function(x)  length(x))
x_a$payment_history_mean_length = apply(cbind(hist1_total, hist2_total), 1, sum)

# pay_dpd_ratio
x_a$pay_dpd_ratio = ifelse(x_a[,payment_history_mean_length] == 0, NA, x_a[,pay_dpd_0_29_bucket]/x_a[,payment_history_mean_length])

# Average and merge
x_a = x_a[, lapply(.SD,mean), by = 'apprefno', .SDcols = names(x_a)[3:ncol(x_a)]]

# total_diff_lastpaymt_opened_dt
full_a$total_diff_lastpaymt_opened_dt = as.numeric(full_a[,'last_paymt_dt'] - full_a[,'opened_dt'])
temp = data.table(full_a[,c('apprefno', 'total_diff_lastpaymt_opened_dt')])
temp = temp[, lapply(.SD, sum), by = 'apprefno', .SDcols = c('total_diff_lastpaymt_opened_dt')]
x_a  = merge(x_a, temp, 'apprefno')

# min_months_last_30_plus
months_dpd_30_1 = mapply(function(x,y){if (y>0){return(x)} else{return(NA)}}, hist1_29, hist1_30)
months_dpd_30_2 = mapply(function(x,y){if (y>0){return(x)} else{return(NA)}}, hist2_29, hist2_30)
temp = data.frame('apprefno' = full_a$apprefno)
temp$min_months_last_30_plus = apply(cbind(months_dpd_30_1, months_dpd_30_2), 1, min)
temp = data.table(temp, key = 'apprefno')
temp = temp[, lapply(.SD, min), by = 'apprefno', .SDcols = c('min_months_last_30_plus')]
x_a  = merge(x_a, temp)

# utilisation_trend
temp = full_a[,c('apprefno','cur_balance_amt','creditlimit','cashlimit')]
temp$utilisation_trend = ((ave(temp$cur_balance_amt, temp$apprefno, FUN = sum)/
		ave(temp$creditlimit, temp$apprefno, FUN = sum))/
	(ave(temp$cur_balance_amt, temp$apprefno, FUN = mean)/
		(ave(temp$creditlimit, temp$apprefno, FUN = mean) +
			ave(temp$cashlimit, temp$apprefno, FUN = mean))))
temp[which(is.infinite(temp[,'utilisation_trend'])), 'utilisation_trend'] = NA
temp = data.table(unique(temp[,c(1,ncol(temp))]), key = 'apprefno')
x_a  = merge(x_a, temp)

# ratio_currbalance_creditlimit
temp = full_a[,c('apprefno','cur_balance_amt','creditlimit')]
temp$ratio_currbalance_creditlimit = ave(full_a$cur_balance_amt, full_a$apprefno, FUN = sum) / 
	ave(full_a$creditlimit, full_a$apprefno, FUN = sum)
temp[which(is.infinite(temp[,'ratio_currbalance_creditlimit'])), 'ratio_currbalance_creditlimit'] = NA
temp = data.table(unique(temp[,c(1,ncol(temp))]), key = 'apprefno')
x_a  = merge(x_a, temp)

# count_enquiry_recency_365
temp   = full_e[, lapply(.SD, max), by = 'apprefno', .SDcols = 'enquiry_dt']
temp   = rename(temp, c('enquiry_dt' = 'max_dt'))
full_e = merge(full_e, temp)
full_e$recent_flag = as.numeric((full_e$max_dt - full_e$enquiry_dt) < 366)
temp = full_e[,c('apprefno','recent_flag')]
temp = temp[, lapply(.SD, sum), by = 'apprefno', .SDcols = 'recent_flag']
temp = rename(temp, c('recent_flag' = 'count_enquiry_recency_365'))
x_e  = merge(x_e, temp)
x_e  = x_e[,c(1,3)]

# count_enquiry_recency_90
full_e$recent_flag = as.numeric((full_e$max_dt - full_e$enquiry_dt) < 91)
temp = full_e[,c('apprefno','recent_flag')]
temp = temp[, lapply(.SD, sum), by = 'apprefno', .SDcols = 'recent_flag']
temp = rename(temp, c('recent_flag' = 'count_enquiry_recency_90'))
x_e  = merge(x_e, temp)

# mean_diff_open_enquiry_dt
full_e$diff_oe = as.numeric(full_e$dt_open - full_e$enquiry_dt)
x_e = merge(x_e, full_e[, lapply(.SD, mean), by = 'apprefno', .SDcols = 'diff_oe'])
x_e = rename(x_e, c('diff_oe' = 'mean_diff_open_enquiry_dt'))

# max_freq_enquiry
Mode = function(x) {
  y = unique(x)
  y[which.max(tabulate(match(x, y)))]
}
x_e = merge(x_e, (full_e[, lapply(.SD, Mode), by = 'apprefno', .SDcols = 'enq_purpose']))

# cibil_score
x_d$cibil_bag = ifelse(x_d$cibil_score>749, 'high', ifelse(x_d$cibil_score>599, 'medium', 
	ifelse(x_d$cibil_score>299,  'low', '-1')))
x_d$cibil_bag = factor(x_d$cibil_bag)

# Deal with NA
to_0 = c('pay_dpd_0_29_bucket', 'pay_dpd_30_bucket', 'worst_dpd')
to_mean = c('pay_dpd_ratio', 'payment_history_mean_length', 'total_diff_lastpaymt_opened_dt', 'utilisation_trend',
	'ratio_currbalance_creditlimit','count_enquiry_recency_365','count_enquiry_recency_90',
	 'mean_diff_open_enquiry_dt', 'enquiry_freq')
to_max = c('min_months_last_30_plus')
x_a = data.frame(x_a)
for (x in names(x_a)){
	if (x %in% to_0){
		x_a[which(is.na(x_a[, x])), x] = 0
	}
}
for (x in names(x_a)){
	if (x %in% to_mean){
		x_a[which(is.na(x_a[,x])), x] = mean(x_a[,x], na.rm = TRUE)
	}
}
for (x in names(x_a)){
	if (x %in% to_max){
		x_a[which(is.na(x_a[,x])), x] = max(x_a[,x], na.rm = TRUE)
	}
}
x_e = data.frame(x_e)
for (x in names(x_e)){
	if (x %in% to_mean){
		x_e[which(is.na(x_e[,x])), x] = mean(x_e[,x], na.rm = TRUE)
	}
}

feat = merge(feat, data.table(x_a), key = 'apprefno')
feat = merge(feat, data.table(x_e), key = 'apprefno')
feat = merge(feat, x_d)
feat = feat[,-'apprefno']

fac_feats  = which(sapply(feat, is.factor))

# Set NA to mode
for (x in fac_feats){
	temp = table(as.vector(feat[,x, with = FALSE]))
	feat[which(is.na(feat[, x, with = FALSE])), x] = names(temp)[temp == max(temp)]
}

feat = data.frame(feat)

train   = feat[which(feat$c == 'train'), 2:ncol(feat)]
test    = feat[which(feat$c == 'test'), 2:ncol(feat)]

# Save changes
save(train, test, file = paste0(base, 'feat.Rdata'))
print('Done!')