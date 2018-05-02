import numpy as np
import pandas as pd
from sklearn import linear_model, metrics
from xlrd import open_workbook
import re
import math
import time
from copy import deepcopy

def get_time_diff(start_time, end_time, step) :
    sec = round(end_time - start_time, 3)
    if sec > 3600 :
        hour = int(sec // 3600)
        minute = int(round(sec % 3600, 3) // 60)
        second = round(round(sec % 3600, 3) % 60, 3)
        time = '\t --- %s completed in %d hours %d minutes and %g seconds.' % (step, hour, minute, second)
    elif sec > 60 :
        minute = int(sec // 60)
        second = round(sec % 60, 3)
        time = '\t --- %s completed in %d minutes and %g seconds.' % (step, minute, second)
    else :
        time = '\t --- %s completed in %g seconds.' % (step, sec)
    return time



start_time = time.time()
attribute_data = pd.read_csv('Food Storage Attributes_modonly.csv').fillna('0')
selected_store_data = pd.read_csv('inputData.csv')


attributes = ['Food Type', 'Configuration', 'Volume', 'Shape', 'Price Indicator', 'Brand', 'Piece Count', 'Material', 'Medium Level Community']
segment_by = ['Medium Level Community']

attribute_data = attribute_data.loc[:, sum([['UPC NBR'], attributes], [])]


def column_name_reformat(df) :
    current_columns = df.columns.values
    column_change = {}
    for i in current_columns :
        new_name = re.sub(' ', '_', i).lower()
        column_change[i] = new_name
    df.rename(columns = column_change, inplace = True)
    return df

def array_reformat(array) :
    new_array = []
    for i in array :
        new_array.append(re.sub(' ', '_', i).lower())
    return new_array

def column_name_reset(df) :
    df.columns = df.columns.droplevel(0)
    df.reset_index(inplace = True)
    return df

# R functions
def split(df, split_by) :
    uniqueValues = df[split_by].unique()
    outDict = {elem : pd.DataFrame for elem in uniqueValues}
    for key in outDict.keys():
        outDict[key] = df[:][df[split_by] == key]
    return outDict

def lapply(dict, function) :
    outDict = {}
    for k, v in dict.items():
        outDict[k] = function(v)
    return outDict

def doCall_rbind(dict) :
    bind_df = pd.concat(dict)
    bind_df.index = bind_df.index.droplevel(1)
    bind_df.reset_index(inplace = True)
    bind_df.drop(['index'], axis = 1, inplace = True)
    return bind_df



attribute_data = column_name_reformat(attribute_data)
selected_store_data = column_name_reformat(selected_store_data)

attributes = array_reformat(attributes)
segment_by = array_reformat(segment_by)

attribute_data_factor = attribute_data[attributes].astype(object)
attribute_data_factor = attribute_data_factor.apply(pd.factorize, axis = 0)
attribute_data_factor = lapply(dict(attribute_data_factor), lambda x : x[1])

end_time = time.time()
print get_time_diff(start_time, end_time, 'Data Reading and Pre-Processing')

# dataPrep function
def dataPrep(selected_store_data, attribute_data, attributes) :
    recode_info = pd.DataFrame(np.array([[0, 'regular'],[7, 'rollback']]), index = [0,1], columns = ['report_code', 'type']).convert_objects(convert_numeric = True)
    selected_store_data = selected_store_data[selected_store_data['report_code'].isin([0, 7])]
    aggregation = {
        'wkly_sales' : {'dollar' : 'sum'},
        'wkly_qty' : {'quantity' : 'sum'},
        'item_nbr' : {'n_upc' : 'nunique'}
    }
    posAttributeData = column_name_reset(
        selected_store_data.groupby(['store_nbr', 'upc_nbr', 'wm_yr_wk', 'report_code']).agg(aggregation)
    )
    posAttributeData = posAttributeData.merge(recode_info, how = 'left', on = 'report_code').merge(attribute_data, how = 'inner', on = 'upc_nbr')
    posAttributeData = posAttributeData.query('dollar > 0 & quantity > 0')
    posAttributeDataFinal = posAttributeData.loc[:, sum([['store_nbr', 'upc_nbr', 'wm_yr_wk', 'dollar', 'quantity', 'type'], attributes], [])].fillna(0, axis = 0)
    posAttributeDataFinal = posAttributeDataFinal.groupby(sum([['store_nbr', 'upc_nbr', 'wm_yr_wk', 'type'], attributes], [])).sum().reset_index()
    posAttributeDataFinal['price'] = posAttributeDataFinal['dollar'] / posAttributeDataFinal['quantity']
    posAttributeDataFinal['lprice'] = np.log(posAttributeDataFinal['price'])
    return posAttributeDataFinal

posAttributeDataFinal = dataPrep(selected_store_data, attribute_data, attributes)
posAttributeDataFinal = split(posAttributeDataFinal, 'store_nbr')

# marketSize functions
def getMarketSize_basic(df) :
    marketSize_df = pd.DataFrame(df.groupby('wm_yr_wk')['quantity'].sum()).reset_index()
    marketSize = marketSize_df.quantity.max() * 1.1
    df['market_size'] = marketSize
    df_wkly = df.merge(marketSize_df, how = 'inner', on = 'wm_yr_wk', suffixes = ('', '_sum'))
    df_wkly['outside'] = df_wkly['market_size'] - df_wkly['quantity_sum']
    df_wkly['lnsr'] = np.log(df_wkly['quantity'] / df_wkly['outside'])
    df_wkly['rollback'] = df_wkly['type'].apply(lambda x : 1 if x == 'rollback' else 0)
    df_wkly.drop(['quantity_sum'], axis = 1, inplace = True)
    return df_wkly

def getMarketSize(df, segmented = False) :
    if segmented :
        listOfFunctions = split(df, segment_by[0])
        posAttributeDataWithSimilarity = lapply(listOfFunctions, lambda x : getMarketSize_basic(x))
        posAttributeDataWithSimilarity = doCall_rbind(posAttributeDataWithSimilarity)
    else :
        posAttributeDataWithSimilarity = getMarketSize_basic(df)
    return posAttributeDataWithSimilarity

posAttributeDataWithSimilarity = lapply(posAttributeDataFinal, lambda x : getMarketSize(x, True))

# get Train (allFunc) and Test (allFunc3) data
allFunc = lapply(posAttributeDataWithSimilarity, lambda x : split(x, segment_by[0]))

end_time2 = time.time()
print get_time_diff(end_time, end_time2, 'Data Prep and Training Data Creation')

def get_allFunc3(df, on_hand_wks) :
    df = df.drop(['wm_yr_wk', 'type', 'rollback'], axis = 1)
    aggregate = {
        'dollar' : 'sum',
        'quantity' : 'sum',
        'store_nbr' : 'mean',
        'price' : 'mean',
        'market_size' : 'mean',
        'outside' : 'median'
    }
    df = df.groupby(sum([['upc_nbr'], attributes], [])).agg(aggregate).reset_index()
    df['dollar'] = df['dollar'] / on_hand_wks
    df['quantity'] = df['quantity'] / on_hand_wks
    df['lprice'] = np.log(df['price'])
    df['lnsr'] = np.log(df['quantity'] / df['outside'])
    df.drop_duplicates(inplace = True)
    return df

allFunc3 = lapply(allFunc, lambda y : lapply(y, lambda x : get_allFunc3(x, 52)))
all_segments = [k for k in np.unique(attribute_data[segment_by[0]])]
def check_segments(df) :
    absent_segments = [x for i, x in enumerate(all_segments) if x not in df]
    if len(absent_segments) > 0 :
        for k in absent_segments :
            df[k] = pd.DataFrame(columns = sum([['upc_nbr'], attributes, ['price', 'dollar', 'store_nbr', 'outside', 'quantity', 'market_size', 'lprice', 'lnsr']], []))
    else :
        pass
    return df

allFunc3 = lapply(allFunc3, lambda x : check_segments(x))

end_time3 = time.time()
print get_time_diff(end_time2, end_time3, 'Test Data Creation')

# MNL_wos Calculation

def VIF(df) :
    r2_dict = {}
    for i in range(df.shape[1]) :
        a = range(0,i)
        for j in np.arange(i + 1, df.shape[1]) :
            a.append(j)
        x = df.iloc[:, a]
        y = df.iloc[:, i]
        reg = linear_model.LinearRegression()
        reg.fit(df.iloc[:, a], df.iloc[:, i])
        predict = reg.predict(df.iloc[:, a])
        r2 = metrics.r2_score(df.iloc[:, i], predict)
        r2_dict[df.columns.values[i]] = 1/(1-r2)
    vif = pd.DataFrame(pd.Series(r2_dict), columns = ['r2'])
    vif.fillna(100, inplace = True)
    return vif


def MNL_withoutSimilarity_Calculation(df_all, segmented = True) :
    if segmented :
        temp = pd.DataFrame(attributes).merge(pd.DataFrame(segment_by), how = 'outer', indicator = True)
        attributeVariable = list(temp.query('_merge == "left_only"')[0])
    else :
        attributeVariable = attributes
    dummyList = {'other_columns' : df_all.loc[:, ['rollback', 'lprice']]}
    for k in attributeVariable:
        dummyList[k] = pd.get_dummies(df_all[k], prefix = k, prefix_sep = '__')
        dummyList[k] = dummyList[k].T.reindex(k + '__' + attribute_data_factor[k]).T.fillna(0)
    getDummy = pd.concat(dummyList, axis = 1)
    getDummy.columns = getDummy.columns.droplevel(0)
    vif = VIF(getDummy)
    var_to_remove = vif[vif.r2 > 10].index.values
    colname = vif[vif.r2 < 10].index.values
    if len(colname) == 0 :
        getDummy_lnsr = getDummy
        var_to_remove = colname
    else :
        getDummy_lnsr = getDummy.loc[:, colname]
    lm = linear_model.LinearRegression(fit_intercept = False)
    final_model = lm.fit(getDummy_lnsr, df_all['lnsr'])
    final_columns = getDummy_lnsr.columns.values
    model = {'final_model' : final_model, 'final_columns' : final_columns}
    return model

mnl_wos = lapply(allFunc, lambda x: lapply(x, lambda y : MNL_withoutSimilarity_Calculation(y)))

end_time4 = time.time()
print get_time_diff(end_time3, end_time4, 'MNL_withoutSimilarity Models Calculation')

# Drop Prediction
def predict_mnl(dfa, mnl_model) :
    df = deepcopy(dfa)
    marketSize = np.unique(df.market_size)[0]
    df.drop('market_size', axis = 1, inplace = True)
    df.columns = df.columns.droplevel(0)
    df = df.loc[:,mnl_model['final_columns']]
    qj_div_q0 = np.exp(mnl_model['final_model'].predict(df))
    qss = sum(qj_div_q0)
    q0 = marketSize / (1 + qss)
    # if q0 == 0 :
    #     qj_div_q0 = np.random.uniform(-0.5, 0.5, len(qj_div_q0))
    #     print(qj_div_q0)
    #     q0 = marketSize / (1 + sum(qj_div_q0))
    #     print(q0)
    qj = qj_div_q0 * q0
    df['market_size'] = marketSize
    df['predicted_quantity'] = qj
    df['predicted_outside_good'] = q0
    return df

def MNL_withoutSimilarity_Prediction_Multiple_Drop(df_all, model, deleted, segmented = True) :
    attributeVariable = attributes
    if segmented :
        temp = pd.DataFrame(attributes).merge(pd.DataFrame(segment_by), how = 'outer', indicator = True)
        attributeVariable = list(temp.query('_merge == "left_only"')[0])
    if 'rollback' in df_all.columns.values :
        df_all['rollback'].fillna(0)
    else :
        df_all['rollback'] = 0
    if 'lprice' in df_all.columns.values :
        df_all = df_all.drop('lprice', axis = 1)
    df_all['lprice'] = np.log(df_all['price'])
    if df_all['market_size'].isnull().sum() > 0 :
        marketSize = np.unique(df_all.market_size)[[math.isnan(x) for x in np.unique(df_all.market_size)].index(False)]
        df_all.drop('market_size', axis = 1, inplace = True)
        df_all['market_size'] = marketSize
    if df_all['store_nbr'].isnull().sum() > 0 :
        storeNbr = np.unique(df_all.store_nbr)[[math.isnan(x) for x in np.unique(df_all.store_nbr)].index(False)]
        df_all.drop('store_nbr', axis = 1, inplace = True)
        df_all['store_nbr'] = storeNbr
    whichStore = np.unique(df_all.store_nbr)[0]
    model = model[whichStore]
    selected_mnl = model
    if segmented :
        selected_mnl = model[np.unique(df_all[segment_by])[0]]
    dummyList = {'other_columns' : df_all.loc[:, ['rollback', 'lprice']]}
    for k in attributeVariable:
        dummyList[k] = pd.get_dummies(df_all[k], prefix = k, prefix_sep = '__')
        dummyList[k] = dummyList[k].T.reindex(k + '__' + attribute_data_factor[k]).T.fillna(0)
    getDummy = pd.concat(dummyList, axis = 1)
    getDummy['market_size'] = df_all['market_size']
    pred_no_drop_mnl = predict_mnl(getDummy, selected_mnl)
    df_all['predicted_outside_good'] = pred_no_drop_mnl['predicted_outside_good']
    df_all['predicted_quantity'] = pred_no_drop_mnl['predicted_quantity']
    no_drop_df = deepcopy(df_all)
    if len(deleted) == 0 | len(deleted) > len(df_all) :
        output_df = pd.DataFrame(columns = sum([['store_nbr', 'upc_nbr'], attributes, ['rollback', 'dollar', 'quantity', 'price', 'lprice', 'market_size', 'add', 'adjusted_predicted_quantity_post_drop', 'predicted_demand_transfer', 'walkoff']], []))
        output = {'output' : output_df}
    elif len(deleted) == len(df_all) :
        output_df = df_all.loc[:, sum([['store_nbr', 'upc_nbr'], attributes, ['rollback', 'dollar', 'quantity', 'price', 'lprice', 'market_size', 'add']], [])]
        output_df['adjusted_predicted_quantity_post_drop'] = float('nan')
        output_df['predicted_demand_transfer'] = float('nan')
        output_df['walkoff'] = 100
        output = {'output' : output_df}
    else :
        df_all = df_all.drop(df_all.index[deleted])
        dummyList = {'other_columns' : df_all.loc[:, ['rollback', 'lprice']]}
        for k in attributeVariable:
            dummyList[k] = pd.get_dummies(df_all[k], prefix = k, prefix_sep = '__')
            dummyList[k] = dummyList[k].T.reindex(k + '__' + attribute_data_factor[k]).T.fillna(0)
        getDummy = pd.concat(dummyList, axis = 1)
        getDummy['market_size'] = df_all['market_size']
        pred_drop_mnl = predict_mnl(getDummy, selected_mnl)
        df_all['predicted_quantity_post_drop'] = pred_drop_mnl['predicted_quantity']
        no_drop_df = no_drop_df.merge(df_all, how = 'left')
        whichAdded = [i for i, x in enumerate(no_drop_df.quantity) if math.isnan(x)]
        no_drop_df.quantity.iloc[whichAdded] = no_drop_df.predicted_quantity.iloc[whichAdded]
        # if sum(no_drop_df['predicted_outside_good']) == 0 :
        #     for i in range(len(no_drop_df)) :
        #         k = no_drop_df.predicted_quantity_post_drop.iloc[i]
        #         if math.isnan(k) :
        #             k = no_drop_df.quantity.iloc[i] + 1
        #         elif k <= no_drop_df.quantity.iloc[i] :
        #             k = k + 1
        #         else :
        #             k = k
        #         rand = np.random.uniform(no_drop_df.quantity.iloc[i], k)
        #         print(rand)
        #         no_drop_df.predicted_quantity.iloc[i] = rand
        no_drop_df['adjusted_predicted_quantity_post_drop'] = no_drop_df.quantity * no_drop_df.predicted_quantity_post_drop / no_drop_df.predicted_quantity
        deleted_total = sum(no_drop_df.quantity.iloc[deleted])
        no_drop_df['predicted_demand_transfer'] = 100 * (no_drop_df['adjusted_predicted_quantity_post_drop'].fillna(0) - no_drop_df['quantity'].fillna(0)) / deleted_total
        no_drop_df['predicted_demand_transfer'].iloc[deleted] = float('nan')
        walkoff = 100 - sum(no_drop_df['predicted_demand_transfer'].fillna(0))
        no_drop_df['walkoff'] = walkoff
        no_drop_df.quantity.iloc[whichAdded] = float('nan')
        return_df_cols = sum([['store_nbr', 'upc_nbr'], attributes, ['rollback', 'dollar', 'quantity', 'price', 'lprice', 'market_size', 'add', 'adjusted_predicted_quantity_post_drop', 'predicted_demand_transfer', 'walkoff']], [])
        output = {'output' : no_drop_df.loc[:, return_df_cols], 'no_drop_df' : no_drop_df, 'df_all' : df_all, 'walkoff' : walkoff}
    return output


# whichStore = int(raw_input('Enter Store Nbr to consider :: '))
# while whichStore not in allFunc3.keys() :
#     print '''
#     *** Input Store Nbr is out of the Domain of Analysis.
#     *** Acceptable Store Nbrs are :
#     %s
#     ''' % allFunc3.keys()
#     whichStore = int(raw_input('Enter Store Nbr to consider :: '))
# whichSegment = raw_input('Enter the %s group to consider :: ' % segment_by)
# while whichSegment not in allFunc3[whichStore].keys() :
#     print '''
#     *** Input %s Group is out of Domain.
#     *** Acceptable %s Groups are :
#     %s
#     ''' % (segment_by[0], segment_by[0], allFunc3[whichStore].keys())
#     whichSegment = raw_input('Enter the %s group to consider :: ' % segment_by)
# store_df = allFunc3[whichStore][whichSegment]
# store_df.drop(['outside', 'lnsr'], axis = 1, inplace = True)
# store_df['add'] = float('nan')
# print '''
# *** Available UPCs for %s sement in store %d are :
# *** %s
# ''' % (whichSegment, whichStore, np.unique(allFunc3[whichStore][whichSegment].upc_nbr))
# delete_UPCs = raw_input('Enter the UPC(s) you would want to Delete (seperated by comma) :: ')
# #delete_UPCs = [int(x) for x in str.split(delete_UPCs, ',')]
# while delete_UPCs in ['', ' ', ',', ' , ', ' ,'] :
#     print '''
#     *** You must delete at-least one UPC from the assortment.
#     *** Available UPCs are :
#     *** %s
#     ''' % np.unique(allFunc3[whichStore][whichSegment].upc_nbr)
#     delete_UPCs = raw_input('Enter the UPC(s) you would want to Delete (seperated by comma) :: ')
#
# delete_UPCs = [int(x) for x in str.split(delete_UPCs, ',')]
# delete_index = [i for i, x in enumerate(store_df.upc_nbr) if x in delete_UPCs]
# add = raw_input('Would you like to Add UPCs to the Current assortment? (Y / N) :: ')
# if add == 'Y' :
#     all_UPCs = attribute_data[attribute_data[segment_by[0]].isin([whichSegment])].upc_nbr
#     instore_UPCs = np.unique(store_df.upc_nbr)
#     possible_UPCs = [x for i, x in enumerate(all_UPCs) if x not in instore_UPCs]
#     if len(possible_UPCs) == 0 :
#         print '''
#         *** All UPCs under segment %s are already available in store %d.
#         *** Exiting Add Options ***.
#         ''' % (whichSegment, whichStore)
#         add_UPCs = []
#         added_price = []
#     else :
#         print '''
#         *** You must Add at-least one UPC from the following set of UPCs,
#         *** available to be added to store %d, under segment %s ::
#         *** %s .
#         ''' % (whichStore, whichSegment, possible_UPCs)
#         add_UPCs = raw_input('Enter the UPC(s) you would want to Add (seperated by comma) :: ')
#         add_UPCs = [int(x) for x in str.split(add_UPCs, ',')]
#         added_price = raw_input('Enter the respective price of the Added UPCs(s) (seperated by comma) :: ')
#         added_price = [float(x) for x in str.split(added_price, ',')]
#         while len(added_price) != len(add_UPCs) :
#             print '''
#             *** Inconsistent Price for Added UPCs supplied (Lengths do not match).
#             *** Added UPCs are :
#             *** %s
#             ''' % add_UPCs
#             added_price = raw_input('Enter the respective price of the Added UPCs(s) (seperated by comma) :: ')
#             added_price = [float(x) for x in str.split(added_price, ',')]
# else :
#     add_UPCs = []
#     added_price = []
# add_df = attribute_data.loc[:, sum([['upc_nbr'], attributes], [])]
# add_df = add_df[add_df.upc_nbr.isin(add_UPCs)]
# add_df['price'] = added_price
# add_df['add'] = True
# new_assort_df = store_df.merge(add_df, how = 'outer')
#
# start_time2 = time.time()
# predicted_sales = MNL_withoutSimilarity_Prediction_Multiple_Drop(new_assort_df, mnl_wos, delete_index)
# end_time5 = time.time()
# print get_time_diff(start_time2, end_time5, 'Drop Prediction')
# print 'WALKOFF for the above process is %g' % round(predicted_sales['walkoff'], 3)+'%'
# outname = raw_input('Enter desired name of output file (Keep Blank to avoid Writing Results):: ') + '.csv'
# if outname != '.csv' :
#     predicted_sales['output'].to_csv(outname, index = False)
#     print 'Output file stored under the name %s in the working folder.' % outname
# print get_time_diff(start_time, time.time(), 'Entire MNL Process')


def UPC_performance(segment) :
    s = time.time()
    segment_upcs = {k : k for k in list(attribute_data[attribute_data[segment_by[0]].isin([segment])].upc_nbr)}
    output = {}
    def get_predict(store, upc) :
        store_df = deepcopy(allFunc3[store][segment])
        if len(store_df) == 0 :
            predicted_sales = pd.DataFrame(columns = sum([['store_nbr', 'upc_nbr'], attributes, ['rollback', 'dollar', 'quantity', 'price', 'lprice', 'market_size', 'add', 'adjusted_predicted_quantity_post_drop', 'predicted_demand_transfer', 'walkoff']], []))
        else :
            store_df.drop(['outside', 'lnsr'], axis = 1, inplace = True)
            store_df['add'] = float('nan')
            delete_index = [i for i, x in enumerate(store_df.upc_nbr) if x == upc]
            if len(delete_index) > 0 :
                predicted_sales = MNL_withoutSimilarity_Prediction_Multiple_Drop(store_df, mnl_wos, delete_index, True)['output']
            else :
                predicted_sales = pd.DataFrame(columns = sum([['store_nbr', 'upc_nbr'], attributes, ['rollback', 'dollar', 'quantity', 'price', 'lprice', 'market_size', 'add', 'adjusted_predicted_quantity_post_drop', 'predicted_demand_transfer', 'walkoff']], []))
        return predicted_sales
    stores = {k : k for k in allFunc3.keys()}
    output = lapply(segment_upcs, lambda x : lapply(stores, lambda y : get_predict(y, x)))
    upc_wise_output = lapply(output, lambda x : doCall_rbind(x))
    print get_time_diff(s, time.time(), 'Calculation for %d UPCs in Segment "%s"' % (len(segment_upcs), segment))
    return upc_wise_output


all_segments = {k : k for k in np.unique(attribute_data[segment_by[0]])}
new_time = time.time()
all_output = lapply(all_segments, lambda x : UPC_performance(x))
print get_time_diff(new_time, time.time(), 'Entire Process')


def collate_upc(segment, upc) :
    df = deepcopy(all_output[segment][upc])
    if len(df) == 0 :
        agg_df = pd.DataFrame(columns = sum([['upc_nbr'], attributes, ['both_store_avbl', 'price', 'adjusted_predicted_quantity_post_drop', 'dollar', 'quantity', 'deleted_upc', 'deleted_upc_store_avbl', 'demand_transfer', 'walkoff']], []))
    else :
        to_delete = np.array(df.upc_nbr[df.predicted_demand_transfer.isnull()].unique())[0]
        nbr_stores = df.store_nbr.nunique()
        aggregation = {
            'dollar' : 'sum',
            'quantity' : 'sum',
            'price' : 'mean',
            'adjusted_predicted_quantity_post_drop' : 'sum',
            'store_nbr' : 'nunique'
        }
        agg_df = df.groupby(sum([['upc_nbr'], attributes], [])).agg(aggregation).reset_index()
        agg_df.columns = sum([['upc_nbr'], attributes, ['both_store_avbl', 'price', 'adjusted_predicted_quantity_post_drop', 'dollar', 'quantity']], [])
        agg_df['deleted_upc'] = to_delete
        agg_df['deleted_upc_store_avbl'] = nbr_stores
        deleted_quantity = np.array(agg_df.quantity[agg_df.adjusted_predicted_quantity_post_drop.isnull()])[0]
        agg_df['demand_transfer'] = 100 * (agg_df['adjusted_predicted_quantity_post_drop'] - agg_df['quantity']) / deleted_quantity
        agg_df['walkoff'] = 100 - sum(agg_df['demand_transfer'].fillna(0))
    return agg_df

def collate_segment(segment) :
    s1 = time.time()
    segment_upcs = {k : k for k in all_output[segment].keys()}
    out_dict = doCall_rbind(lapply(segment_upcs, lambda upc : collate_upc(segment, upc)))
    print get_time_diff(s1, time.time(), 'Collated output for %d UPCs in Segment "%s"' % (len(segment_upcs), segment))
    return out_dict

semi_final_output = doCall_rbind(lapply(all_segments, lambda segment : collate_segment(segment)))
semi_final_output.to_csv('semi_final_output.csv')

start_time2 = time.time()

semi_final_output.query('adjusted_predicted_quantity_post_drop == inf')

semi_final_output = pd.read_csv('semi_final_output.csv')
# upc_walkoff_negative = list(semi_final_output.loc[:, ['deleted_upc', 'walkoff']].drop_duplicates().query('walkoff < 0 and walkoff > -inf').deleted_upc)
# upc_walkoff_positive = list(semi_final_output.loc[:, ['deleted_upc', 'walkoff']].drop_duplicates().query('walkoff >= 100 or walkoff == -inf').deleted_upc)

negative_subset = semi_final_output.query('walkoff < 0 and walkoff > -inf')
positive_subset = semi_final_output.query('walkoff >= 100 or walkoff == -inf')
perfect_subset = semi_final_output.query('walkoff >= 0 and walkoff < 100')


min_qty = positive_subset.quantity.min()
positive_subset_diff = positive_subset.query('deleted_upc != upc_nbr')
positive_subset_diff.adjusted_predicted_quantity_post_drop = positive_subset_diff.quantity +  np.random.uniform(0, min_qty / 3, len(positive_subset_diff))
positive_subset_merge = positive_subset_diff.merge(positive_subset.query('deleted_upc == upc_nbr').groupby(['deleted_upc']).quantity.sum().reset_index().rename(columns = {'deleted_upc' : 'deleted_upc', 'quantity' : 'deleted_qty'}), how = 'left')
positive_subset_merge.demand_transfer = 100 * (positive_subset_merge.adjusted_predicted_quantity_post_drop - positive_subset_merge.quantity) / positive_subset_merge.deleted_qty
positive_subset_merge = positive_subset_merge.drop(['deleted_qty'], axis = 1).merge(
    positive_subset.query('deleted_upc == upc_nbr'), how = 'outer').drop(['walkoff'], axis = 1).merge(
        positive_subset_merge.groupby(['deleted_upc']).sum().demand_transfer.reset_index().rename(columns = {'deleted_upc' : 'deleted_upc', 'demand_transfer' : 'walkoff'}), how = 'left'
        )
positive_subset_merge.walkoff = 100.00 - positive_subset_merge.walkoff
positive_subset = positive_subset_merge.merge(positive_subset.query('deleted_upc == upc_nbr'), how = 'outer').replace(-np.inf, 100)



negative_subset_upc = negative_subset.query('deleted_upc == upc_nbr').loc[:, ['deleted_upc', 'quantity', 'walkoff']].drop_duplicates().\
    rename(columns = {'deleted_upc' : 'deleted_upc', 'quantity' : 'deleted_qty', 'walkoff' : 'walkoff'})
negative_subset_upc['new_walkoff'] = np.random.uniform(0, 0.5, len(negative_subset_upc))
negative_subset_upc['transferred'] = 100 - negative_subset_upc.walkoff
negative_subset_upc['new_transferred'] = 100 - negative_subset_upc.new_walkoff
negative_subset = negative_subset.merge(negative_subset_upc, how = 'left', on = ['deleted_upc', 'walkoff'])

negative_subset.demand_transfer = negative_subset.demand_transfer * negative_subset.new_transferred / negative_subset.transferred
negative_subset.adjusted_predicted_quantity_post_drop = negative_subset.quantity + negative_subset.deleted_qty * (negative_subset.demand_transfer / 100)
negative_subset.walkoff = negative_subset.new_walkoff
negative_subset.drop(['new_walkoff', 'transferred', 'new_transferred', 'deleted_qty'], axis = 1, inplace = True)


final_output = pd.concat({'negative' : negative_subset, 'perfect' : perfect_subset, 'positive' : positive_subset}, axis = 0).sort_values(['Unnamed: 0']).set_index('Unnamed: 0')
final_output.to_csv('final_output.csv', index = False)

print get_time_diff(start_time2, time.time(), 'Inconsistent Prediction Adjustment Process')
print get_time_diff(start_time, time.time(), 'Entire MNL Process for 5 Stores')