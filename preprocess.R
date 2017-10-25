#########################################
### Basic Cleaning and Transformation ###
#########################################
print('Running...')
base = './Data/' # Data directory 
load(paste0(base, 'full.Rdata'))


##################################
### Clean Account Segment data ###
##################################

# Delete columns with all NA
full_a = full_a[, colSums(is.na(full_a)) != nrow(full_a)]

# Remove extraneous features
extra = c('id','externalid', 'dt_opened', 'upload_dt', 'member_short_name')
full_a  = full_a[, !(colnames(full_a) %in% extra)]

# Set Numeric features
num = c('high_credit_amt', 'cur_balance_amt', 'amt_past_due', 'valueofcollateral', 
	'creditlimit', 'cashlimit', 'rateofinterest', 'repaymenttenure', 'writtenoffamounttotal', 
	'writtenoffamountprincipal', 'settlementamount', 'actualpaymentamount')
full_a[num] = lapply(full_a[num], function(x) as.numeric(as.character(x))) # also converts non-num values to NA
for (x in num){
	full_a[which(is.na(full_a[,x])), x] = 0
	# full_a[which(is.na(full_a[,x])), x] = mean(full_a[,x], na.rm = TRUE)
}
full_a[which(is.na(full_a[,'creditlimit'])), x] = mean(full_a[,x], na.rm = TRUE)

# Set categorical features
cat = c('owner_indic', 'acct_type', 'writtenoffandsettled', 'typeofcollateral', 'paymentfrequency')
full_a[cat] = lapply(full_a[cat], factor)

# Set char. features
char = c('paymenthistory1', 'paymenthistory2')
# full_a[char] = lapply(full_a[char], function(x) as.character(x))
full_a[char] = lapply(full_a[char], function(x) gsub('\"\"\"', '', x))

# Set date-time features
dt = c('last_paymt_dt', 'opened_dt', 'closed_dt', 'reporting_dt', 'paymt_str_dt', 'paymt_end_dt')
full_a[dt] = lapply(full_a[dt], function(x) as.Date(x, '%d-%B-%y'))


##################################
### Clean Enquiry Segment data ###
##################################

# Delete columns with all NA
full_e = full_e[, colSums(is.na(full_e)) != nrow(full_e)]

# Remove extraneous features
extra = c('id','externalid', 'upload_dt', 'member_short_name')
full_e  = full_e[, !(colnames(full_e) %in% extra)]

# Set Numeric features
num = c('enq_amt')
full_e[num] = lapply(full_e[num], function(x) as.numeric(as.character(x))) # also converts non-num values to NA
for (x in num){
	# full_e[which(is.na(full_e[,x])), x] = 0
	full_e[which(is.na(full_e[,x])), x] = mean(full_e[,x], na.rm = TRUE)
}

# Set categorical features
cat = c('enq_purpose')
full_e[cat] = lapply(full_e[cat], factor)

# Set date-time features
dt = c('enquiry_dt','dt_opened')
full_e[dt] = lapply(full_e[dt], function(x) as.Date(x, '%d-%B-%y'))


###############################
### Clean Data Segment data ###
###############################

# Remove extraneous features
full_d = full_d[,c(1:59,85,86,87)] #Remove all app_* columns (duplicates/not useful)
extra = c('id','ref_code', 'entry_time', 'status_type','app_pan', 'app_disposition',
	'app_mobile', 'res_from_month', 'month_joining', 'months_exp', 'id_proof', 'id_proof_number',
	'rbl_relationship_no', 'status_date', 'cin', 'office_email', 'email')
full_d  = full_d[, !(colnames(full_d) %in% extra)]

# Remove rows with majority NA features
full_d = full_d[-which(is.na(full_d[,'card_name']) & is.na(full_d[,'app_has_card']) & is.na(full_d[,'office_city'])),]

# Set categorical features
cat = c('apprefno', 'bad_flag_worst6', 'c', 'aip_status', 'app_gender', 'marital_status',
 'employment_type', 'app_res_pincode', 'office_pin')
full_d[cat] = lapply(full_d[cat], factor)

# Set Numeric features
num = c('cibil_score', 'approved_credit_limit', 'num_dependents', 'res_from_yr', 'year_joining',
	'years_exp', 'worst_dpd6', 'override_months')
full_d[num] = lapply(full_d[num], function(x) as.numeric(as.character(x))) # also converts non-num values to NA
for (x in num){
	# full_d[which(is.na(full_d[,x])), x] = 0
	full_d[which(is.na(full_d[,x])), x] = mean(full_d[,x], na.rm = TRUE)
}

# Set date-time features
dt = c('app_dob', 'dt_opened')
full_d[dt] = lapply(full_d[dt], function(x) as.Date(x, '%d-%B-%y'))


###############################
### Save cleaned variables ###
###############################

full_d = unique(full_d)
full_a = unique(full_a)
full_e = unique(full_e)


# Build training sets for analysis
train_d = full_d[which(full_d$c == 'train'), -which(names(full_d) == 'c')]
train_a = full_a[which(full_a$c == 'train'), -which(names(full_a) == 'c')]
train_e = full_e[which(full_e$c == 'train'), -which(names(full_e) == 'c')]

# Build test sets
test_d = full_d[which(full_d$c == 'test'), -which(names(full_d) == 'c')]
test_a = full_a[which(full_a$c == 'test'), -which(names(full_a) == 'c')]
test_e = full_e[which(full_e$c == 'test'), -which(names(full_e) == 'c')]

save(full_a, full_e, full_d, file = paste0(base, 'clean.Rdata'))
save(train_a, train_e, train_d, file = paste0(base, 'train.Rdata'))
save(test_a, test_e, test_d, file = paste0(base, 'test.Rdata'))

print('Done!')