#!/usr/bin/env python3

from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import RANSACRegressor
from sklearn.linear_model import LinearRegression
from sklearn.linear_model import LogisticRegression
from sklearn.pipeline import Pipeline
import math
import numpy as np
import sys
import re
import matplotlib.pyplot as plt
import os
import random
# Ressources:
# https://scikit-learn.org/stable/modules/generated/sklearn.pipeline.Pipeline.html
# https://scikit-learn.org/stable/modules/linear_model.html#polynomial-regression-extending-linear-models-with-basis-functions
# https://www.w3schools.com/python/python_ml_polynomial_regression.asp
# https://stackoverflow.com/questions/31301242/generate-the-full-polynomial-regression-formula-not-just-coefficients-using-skl/31303635#31303635
# https://stackoverflow.com/questions/31290976/sklearn-how-to-get-coefficients-of-polynomial-features?rq=1
# https://stackoverflow.com/questions/34869851/how-can-i-see-the-effect-of-sklearn-preprocessing-polynomialfeatures
# https://stackoverflow.com/questions/34373606/scikit-learn-coefficients-polynomialfeatures
# https://scikit-learn.org/stable/auto_examples/inspection/plot_linear_model_coefficient_interpretation.html
# https://stackoverflow.com/questions/18993867/scikit-learn-logistic-regression-model-coefficients-clarification
# https://stackoverflow.com/questions/57924484/finding-coefficients-for-logistic-regression-in-python
# https://stackoverflow.com/questions/49782120/scikit-learn-logistic-regression-equation
# https://stackoverflow.com/questions/18993867/scikit-learn-logistic-regression-model-coefficients-clarification

# Ensure we get a file of events
if len(sys.argv) != 2:
    raise ValueError('You must provide the file from profiling.')

# Load all lines
print('[ACTION] open file: ' + str(sys.argv[1]))
profFile = open(sys.argv[1], 'r') 
allLines = profFile.readlines() 
print('[ACTION] clos file: ' + str(sys.argv[1]))
profFile.close()

# Configuration
cutoff_value        = None
use_logistic        = (cutoff_value is not None)
compact_Y_cutoff    = use_logistic
use_ransac          = False
use_linearreg       = not (use_ransac or use_logistic)
use_weights         = (not use_logistic and cutoff_value is not None)
correlation_score_threshold = 0.3
compact_formula     = True
plotFigs            = (os.environ.get('SAVE_PNG') and os.environ.get('SAVE_PNG').lower() == 'true')
MaxPolyDegree       = 5
nb_event_limit      = 1000
filter_percent      = 0.05

print('[CONFIG] cutoff_value: ' + str(cutoff_value))
print('[CONFIG] compact_Y_cutoff: ' + str(compact_Y_cutoff))
print('[CONFIG] use_logistic: ' + str(use_logistic))
print('[CONFIG] use_ransac: ' + str(use_ransac))
print('[CONFIG] use_linearreg: ' + str(use_linearreg))
print('[CONFIG] use_weights: ' + str(use_weights))
print('[CONFIG] correlation_score_threshold: ' + str(correlation_score_threshold))
print('[CONFIG] compact_formula: ' + str(compact_formula))
print('[CONFIG] plotFigs: ' + str(plotFigs))
print('[CONFIG] MaxPolyDegree: ' + str(MaxPolyDegree))
print('[CONFIG] nb_event_limit: ' + str(nb_event_limit))
print('[CONFIG] filter_percent: ' + str(filter_percent))

# Working variables  
allNames=[]
allX={}
allModes={}
allY={}
allNbParams={}
lineCount = {}
usingTaskCount = {}
totalTaskCount = {}

print('[ACTION] Read task names...')
for line in allLines:
    line = line.strip()
    if not line[0] == '{' or not line[-1] == '}':
        raise ValueError('Invalid line format ' + str(line))
        
    event = line[1:len(line)-1].split(',')
    if len(event) == 0:
        raise ValueError('Invalid line format ' + str(line))
    nbParamsInTask = int(event[1])
    if len(event) != nbParamsInTask + 3:
        raise ValueError('Invalid line format ' + str(line))
    
    taskName = event[0]
    if taskName not in allNames:
        if taskName in allX or taskName in allY or taskName in allModes:
            raise ValueError('Error in names/X/Y consistency ' + + str(allNames) + str(allX) + ' ' + str(allY))
        allNames.append(taskName)
        allX[taskName] = []
        allModes[taskName] = []
        allY[taskName] = []
        lineCount[taskName] = 0
        usingTaskCount[taskName] = 0
        totalTaskCount[taskName] = 0
    
    lineCount[taskName] += 1

print('[ACTION] Generate random indexes...')
randomsBetween0AndLineCount = {}
for names in allNames:
    if lineCount[names] >= nb_event_limit:
        randomsBetween0AndLineCount[names] = random.sample(range(0, lineCount[names]), nb_event_limit)

print('[ACTION] Read lines...')
for line in allLines:
    line = line.strip()
    event = line[1:len(line)-1].split(',')
    taskName = event[0]
    totalTaskCount[taskName] += 1

    if lineCount[taskName] >= nb_event_limit:
        if usingTaskCount[taskName] >= nb_event_limit:
            continue
        if totalTaskCount[taskName] not in randomsBetween0AndLineCount[taskName]:
            continue
    
    nbParamsInTask = int(event[1])
    allX[taskName].append([float(x_mode.split('=')[1]) for x_mode in event[2:len(event)-1]])
    allModes[taskName].append(str(x_mode.split('=')[0].strip()) for x_mode in event[2:len(event)-1])
    allY[taskName].append(float(event[-1]))
    allNbParams[taskName] = int(event[1])
    usingTaskCount[taskName] += 1
    
if use_weights:
    print('[ACTION] Generate weights...')
    allWeights = {}
    diffCoef = 100 # A value at compact_Y_cutoff is 100× more important than extrem values
    for taskName in allNames:
        print('[ACTION] - Weights for taskName ' + taskName)
        Y = allY[taskName]
        allWeights[taskName] = []
        
        minY = min(Y[:])
        maxY = max(Y[:])
        maxStep = max([abs(minY-compact_Y_cutoff), abs(maxY-compact_Y_cutoff)])
        
        for i in range(len(Y)):
            allWeights[taskName].append(diffCoef * (1 - abs(Y[i]-compact_Y_cutoff)/maxStep) + 1)

# Compute the complete duration
print('[ACTION] Compute the complete duration...')
max_task_time = 0
for taskName in allNames:
    Y = allY[taskName]
    max_time_for_task = max(Y)
    max_task_time = max(max_task_time, max_time_for_task)
print('[INFO] Total time: ' + str(max_task_time)) 

filter_time = max_task_time * filter_percent
print('[INFO] Filter time: ' + str(filter_time))

# Compact cutoff
if compact_Y_cutoff:
    print('[ACTION] Compact cutoff...')
    for taskName in allNames:
        print('[ACTION] - Compact for taskName ' + taskName)

        Y = allY[taskName]
        for i in range(len(Y)):
            if Y[i] < compact_Y_cutoff:
                Y[i] = compact_Y_cutoff-1
            else:
                Y[i] = compact_Y_cutoff+1   


# Open model file
modelFile = open(sys.argv[1]+'.model', "w")
modelFile.write('degree=' + str(MaxPolyDegree) + '\n')
print('[CONFIG] Output file: ' + sys.argv[1]+'.model')

# Iterate over the functions
for taskName in allNames:
    print('============================================================')
    print('[ACTION] Regression for taskName ' + taskName)

    X = allX[taskName]
    Y = allY[taskName]
    modes = allModes[taskName]
    nbParams = allNbParams[taskName]
    maxY = max(Y[:])
    
    if maxY < filter_time:
        print('[INFO] - maxY < filter_time')
        print('[INFO]  - maxY : ' + str(maxY))
        formula = "None"
    elif nbParams == 0:
        print('[INFO] - No parameters')
        print('[INFO]  - sum of execution time : ' + str(sum(Y)))
        print('[INFO]  - Average : ' + str(np.average(Y)))
        formula = str(np.average(Y))
    elif (use_logistic == False and max(Y)-min(Y) <= 0.05*np.average(Y)) or (use_logistic and (Y[0] == np.asmatrix(Y)[:]).all()):
        print('[INFO] - Seems constant with average of : ' + str(np.average(Y)))
        formula = str(np.average(Y))
        avY = np.average(Y)

        if cutoff_value is not None:
            percentsSuccess = 100*(sum([(Y[i] <= cutoff_value and avY <= cutoff_value) or (Y[i] > cutoff_value and avY > cutoff_value) for i in range(len(Y))])/float(len(Y)))
            print('[INFO]   - Success for treshold of ' + str(cutoff_value) + ' : ' + str(percentsSuccess))
        
        if plotFigs:                    
            Xorig = np.asmatrix(allX[taskName])
            fig, ax = plt.subplots(nbParams, squeeze=False)
            fig.suptitle(taskName)
            for param in range(nbParams):
                column = Xorig[:,param].tolist()                                                
                ax[param,0].scatter(column, Y, color='blue')
                order = [int(x) for x,y in sorted(enumerate(column), key = lambda x: x[1])] # np.argsort(column)
                ax[param,0].plot([min(column[:]), max(column[:])], [avY,avY], color='red')     
    else:
        # Remove useless data
        X=np.asarray(X)
        # print(X[:,1].reshape(-1, 1).shape)
        X_names=['X'+str(x) for x in range(nbParams)]
        # Remove useless data or data with no correlation
        if not use_logistic:
            for idxCol in range(X.shape[1]-1, -1, -1):
                Xcol = X[:,idxCol].reshape(-1, 1)
                if (X[0,idxCol] == X[:,idxCol]).all() or LinearRegression().fit(Xcol,Y).score(Xcol,Y) < correlation_score_threshold: #, TODO decide what to do with W values
                    X = np.delete(X, idxCol, axis=1)
                    del X_names[idxCol]
                    del modes[idxCol]
        
        if len(X_names) == 0:
            print('[INFO]  - All variables have been deleted use average ' + str(np.average(Y)))
            formula = str(np.average(Y))
            avY = np.average(Y)
            
            if cutoff_value is not None:
                percentsSuccess = 100*(sum([(Y[i] <= cutoff_value and avY <= cutoff_value) or (Y[i] > cutoff_value and avY > cutoff_value) for i in range(len(Y))])/float(len(Y)))
                print('[INFO]    - Success for treshold of ' + str(cutoff_value) + ' : ' + str(percentsSuccess))

        else:        
        
            if use_logistic:
                print('[INFO]  Use logistic')
                linReg = LogisticRegression()
                if use_weights:
                    linReg = linReg.fit(X, Y, sample_weight=allWeights[taskName])
                else:
                    linReg = linReg.fit(X, Y)
                print('[INFO]   - score: ' + str(linReg.score(X, Y)))
                print('[INFO]   - coef: ' + str(linReg.coef_))
                print('[INFO]   - intercept_ : ' + str(linReg.intercept_))
                print('[INFO]   - classes_: ' + str(linReg.classes_))
                if cutoff_value is not None:
                    fY = linReg.predict(X)
                    percentsSuccess = 100*(sum([(Y[i] <= cutoff_value and fY[i] <= cutoff_value) or (Y[i] > cutoff_value and fY[i] > cutoff_value) for i in range(len(Y))])/float(len(Y)))
                    print('[INFO] - Success for treshold of ' + str(cutoff_value) + ' : ' + str(percentsSuccess))
                Xpoly = X
            elif use_ransac or use_linearreg:
                print('[INFO]  Use linear regression')
                if use_ransac:
                    linReg = RANSACRegressor()
                else:
                    linReg = LinearRegression(copy_X=True, n_jobs=-1, fit_intercept=True)
                
                for idxPoly in range(1, min(MaxPolyDegree, len(X))+1):
                    print('[INFO]  Test poly degree: ' + str(idxPoly))
                    polyReg = PolynomialFeatures(degree=idxPoly)
                    Xpoly = polyReg.fit_transform(X)
                    current_X_names = polyReg.get_feature_names_out(X_names)
                        
                    if use_weights:
                        linReg = linReg.fit(Xpoly, Y, sample_weight=allWeights[taskName])
                    else:
                        linReg = linReg.fit(Xpoly, Y)
                    
                    print('[INFO]    - score: ' + str(linReg.score(Xpoly, Y)))
                    if use_ransac:
                        print('[INFO]   - ?')
                    else:
                        print('[INFO]    - coef linear: ' + str(linReg.coef_))
                        print('[INFO]    - get_feature_names : ' + str(current_X_names))
                        print('[INFO]    - coef poly: ' + str(polyReg.powers_))
                        print('[INFO]    - intercept_ : ' + str(linReg.intercept_))
                    
                    if cutoff_value is not None:
                        fY = linReg.predict(Xpoly)
                        percentsSuccess = 100*(sum([(Y[i] <= cutoff_value and fY[i] <= cutoff_value) or (Y[i] > cutoff_value and fY[i] > cutoff_value) for i in range(len(Y))])/float(len(Y)))
                        print('[INFO]   - Success for treshold of ' + str(cutoff_value) + ' : ' + str(percentsSuccess))
                                    
                    if linReg.score(Xpoly, Y) >= 0.9999:
                        print('[INFO] - success, keep this degree')
                        break
                X_names = current_X_names
            else:
                raise ValueError('No regression selected... ')
        
                    
            if plotFigs:
                print('[ACTION] Generate figure...')
                Xorig = np.asmatrix(allX[taskName])
                predict = np.array(linReg.predict(Xpoly))
                
                fig, ax = plt.subplots(nbParams, squeeze=False)
                fig.suptitle(taskName)
                for param in range(nbParams):
                    column = Xorig[:,param].tolist()                                                
                    ax[param,0].scatter(column, Y, color='blue')
                    order = [int(x) for x,y in sorted(enumerate(column), key = lambda x: x[1])] # np.argsort(column)
                    ax[param,0].plot(np.array(column)[order], predict[order], color='red')
                # plt.show()
                print('[INFO] save figure in ' + taskName + '-full.png')
                fig.savefig(taskName + '-full.png', bbox_inches='tight')
                    
            if compact_formula:
                print('[INFO] Compact formula')
                
                Xtrunc = Xpoly                
                current_score = linReg.score(Xtrunc, Y)   
                if cutoff_value is not None:             
                    fY = linReg.predict(Xtrunc)
                    current_success_rate = 100*(sum([(Y[i] <= cutoff_value and fY[i] <= cutoff_value) or (Y[i] > cutoff_value and fY[i] > cutoff_value) for i in range(len(Y))])/float(len(Y)))
            
                hasChanged = True
                while len(X_names) > 1 and hasChanged :
                    hasChanged = False                                        
                    # Find lower coef
                    idx_to_delete = 0
                    if use_logistic:
                        lowest_impact = linReg.coef_.transpose()[idx_to_delete][0] * abs(max(Xpoly[:,idx_to_delete].tolist())[0]-min(Xpoly[:,idx_to_delete].tolist())[0])
                    elif use_ransac:
                        raise ValueError('TODO')
                    else:
                        lowest_impact = linReg.coef_[idx_to_delete] * abs(max(Xpoly[:,idx_to_delete].tolist())-min(Xpoly[:,idx_to_delete].tolist()))
                        
                    for i in range(1,len(X_names)):
                        if use_logistic:
                            current_impact = linReg.coef_.transpose()[i][0] * abs(max(Xpoly[:,i].tolist())[0]-min(Xpoly[:,i].tolist())[0])
                        elif use_ransac:
                            raise ValueError('TODO')
                        else:
                            current_impact = linReg.coef_[i] * abs(max(Xpoly[:,i].tolist())-min(Xpoly[:,i].tolist()))
                        
                        if lowest_impact > current_impact:
                            idx_to_delete = i
                            lowest_impact = current_impact
                    
                    print('[INFO] - idx to delete: ' + str(idx_to_delete) + ' - ' + X_names[idx_to_delete] )
                    # Try to delete it
                    new_Xpoly = np.delete(Xpoly, idx_to_delete, axis=1)
                    
                    if use_logistic:
                        linRegTrunc = LogisticRegression()
                    elif use_ransac:
                        linRegTrunc = RANSACRegressor()
                    else:
                        linRegTrunc = LinearRegression(copy_X=True, n_jobs=-1, fit_intercept=True)
                        
                    if use_weights:
                        linRegTrunc = linRegTrunc.fit(new_Xpoly, Y, sample_weight=allWeights[taskName])
                    else:
                        linRegTrunc = linRegTrunc.fit(new_Xpoly, Y)
                        
                    new_score = linRegTrunc.score(new_Xpoly, Y) 
                    print('[INFO]   - score: ' + str(new_score))
                    print('[INFO]   - coef: ' + str(linRegTrunc.coef_))
                    print('[INFO]   - intercept_ : ' + str(linRegTrunc.intercept_))    
                    if cutoff_value is not None:           
                        fY = linRegTrunc.predict(new_Xpoly)                    
                        new_success_rate = 100*(sum([(Y[i] <= cutoff_value and fY[i] <= cutoff_value) or (Y[i] > cutoff_value and fY[i] > cutoff_value) for i in range(len(Y))])/float(len(Y)))
                        print('[INFO]   - success rate: ' + str(new_success_rate))
                    # Check if that improve things
                    if new_score*1.05 < current_score:
                        print('[INFO]   - Not improved because of the score, stop here')
                        hasChanged = False
                    elif cutoff_value is not None and new_success_rate < current_success_rate:
                        print('[INFO]   - Not improved because of the success rate, stop here')
                        hasChanged = False
                    else:
                        print('[INFO]   - delete ' + str(idx_to_delete))
                        current_score = new_score 
                        hasChanged = True
                        X_names = np.delete(X_names, idx_to_delete) 
                        Xpoly = new_Xpoly
                        linReg = linRegTrunc
                        if cutoff_value is not None:     
                            current_success_rate = new_success_rate
            
            if X_names.size == 0 :
                print('[INFO] - Every variables were delete, will take the average...')
                formula = str(np.average(Y))
                avY = np.average(Y)  
                if cutoff_value is not None:           
                    percentsSuccess = 100*(sum([(Y[i] <= cutoff_value and avY <= cutoff_value) or (Y[i] > cutoff_value and avY > cutoff_value) for i in range(len(Y))])/float(len(Y)))
                    print('[INFO] - Success for treshold of ' + str(cutoff_value) + ' : ' + str(percentsSuccess))
            else:
                print('[INFO] - Generate formula...')
                if use_ransac:
                    formula = "Undefined"
                elif use_logistic:
                    Xcoefs = [str(X_names[i]) + '*' + str(linReg.coef_.transpose()[i][0]) for i in range(len(linReg.coef_.transpose()))]
                    formula = '(1/(1 + std::exp(' + str(linReg.intercept_[0])  + '+' + '+'.join(Xcoefs) + ')))<0.5 ? ' + str(cutoff_value-1) + ':' + str(cutoff_value+1)
                elif use_linearreg:
                    coeficients = linReg.coef_
                    formulaList = []                        
                    for feature, coeficient in zip(X_names, coeficients):
                        if(str(feature) == '1'):
                            formulaList.append(str(coeficient))
                        else:
                            formulaList.append(str(feature).replace(' ', '*') + '*' + str(coeficient))
                    formula = ' + '.join(formulaList)
                    formula = str(linReg.intercept_) + ' + ' + re.sub('([+* ])?(X[0-9])+[\^]([0-9])+', '\\1apac_fpow<\\3>(\\2)', formula)
                else:
                    raise ValueError('No regression selected... ')
                
                if plotFigs:
                    print('[ACTION] Generate figure...')
                    Xorig = np.asmatrix(allX[taskName])
                    predict = np.array(linReg.predict(Xpoly))
                        
                    fig, ax = plt.subplots(nbParams, squeeze=False)
                    fig.suptitle(taskName)
                    for param in range(nbParams):
                        column = Xorig[:,param].tolist()                                                
                        ax[param,0].scatter(column, Y, color='blue')
                        order = [int(x) for x,y in sorted(enumerate(column), key = lambda x: x[1])] # np.argsort(column)
                        ax[param,0].plot(np.array(column)[order], predict[order], color='red')
                    # plt.show()
                    print('[INFO] Generate figure in ' + taskName + '.png')
                    fig.savefig(taskName + '.png', bbox_inches='tight')
    # Show formula
    print('[INFO] => ' + formula)
    # Put it into the file
    modelFile.write(taskName + '(nbparams=' + str(nbParams) + ')' + '=' + formula + '\n')

# The End
print('[INFO] close model file')
modelFile.close()  
