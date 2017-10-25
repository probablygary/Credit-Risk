##############################################
### Build Complete DB for initial cleaning ###
##############################################

base = './Data/' # Data directory 

# Build complete DB
train_a = read.csv(paste0(base, 'raw_account_70.csv'), header = TRUE, na.strings = c('', ' ', 'NA', 'NOT DISCLOSED'), stringsAsFactors = TRUE)
train_e = read.csv(paste0(base, 'raw_enquiry_70.csv'), header = TRUE, na.strings = c('', ' ', 'NA', 'NOT DISCLOSED'), stringsAsFactors = TRUE)
train_d = read.csv(paste0(base, 'raw_data_70.csv'), header = TRUE, na.strings = c('', ' ', 'NA', 'NOT DISCLOSED'), stringsAsFactors = TRUE)
test_a  = read.csv(paste0(base, 'raw_account_30.csv'), header = TRUE, na.strings = c('', ' ', 'NA', 'NOT DISCLOSED'), stringsAsFactors = TRUE)
test_e  = read.csv(paste0(base, 'raw_enquiry_30.csv'), header = TRUE, na.strings = c('', ' ', 'NA', 'NOT DISCLOSED'), stringsAsFactors = TRUE)
test_d  = read.csv(paste0(base, 'raw_data_30.csv'), header = TRUE, na.strings = c('', ' ', 'NA', 'NOT DISCLOSED'), stringsAsFactors = TRUE)
train_d$c = as.factor('train')
train_a$c = as.factor('train')
train_e$c = as.factor('train')
test_d$c  = as.factor('test')
test_a$c  = as.factor('test')
test_e$c  = as.factor('test')
full_d = unique(rbind(train_d, test_d))
full_a = unique(rbind(train_a, test_a))
full_e = unique(rbind(train_e, test_e))

save(full_d, full_e, full_a, file = paste0(base, 'full.Rdata'))