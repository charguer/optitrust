#!/usr/bin/env python3

from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import RANSACRegressor
from sklearn.linear_model import LinearRegression
from sklearn.linear_model import LogisticRegression
from sklearn.pipeline import Pipeline
import math
import os

os.environ['OPENBLAS_NUM_THREADS'] = '1'
import numpy as np
import sys
import re
import matplotlib.pyplot as plt
import random
import subprocess
import pandas as pd
from pathlib import Path
import argparse  # for command line arguments parsing


# Resources:
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

from enum import Enum
class Regrtype(Enum):
	linear = 1
	logistic = 2	
	ransac = 3


def is_linear(s):
  return Regrtype['linear'].name == s


def is_logistic(s):
  return Regrtype['logistic'].name == s


def is_ransac(s):
  return Regrtype['ransac'].name == s


def print_config(args):
    print(f'[CONFIG] cutoff_value: {args.cutoff_value}')
    print(f'[CONFIG] regression type: {args.regression_type}')
    print(f'[CONFIG] use_weights: {args.use_weights}')
    print(f'[CONFIG] correlation_score_threshold: {args.correlation_score_threshold}')
    print(f'[CONFIG] compact_formula: {args.compact}')
    print(f'[CONFIG] plot_figures: {args.plot}')
    print(f'[CONFIG] max_poly_degree: {args.max_poly_degree}')
    print(f'[CONFIG] nb_event_limit: {args.nb_event_limit}')
    print(f'[CONFIG] extremum_percentage: {args.extremum_percentage}')


def load_datafilenames(profile_file_name):
    """
    Load the .apac_prof file contents. Each line is a csv filename named after a task 
    and containing the data pertaining to this task.
    Return a list filenames.
    """
    # Load all lines
    print(f'[ACTION] open file: {profile_file_name}')
    my_file = Path(profile_file_name)
    if not my_file.is_file():
        exit(0)
    profile = open(profile_file_name, 'r')
    all_lines = profile.readlines()
    print(f'[ACTION] close file: {profile_file_name}')
    profile.close()
    return all_lines


def fill_in_tasks(all_lines,profile_file_name):
    """
    """
    # Working variables  
    all_names = []
    all_nb_params_in_task = []
    all_tasks = []
    all_X = {}
    all_modes = {}
    all_Y = {}
    all_nb_params = {}
    line_count = {}

    print('[ACTION] Read task names...')
    is_empty_task = False
    for line in all_lines:
	  ##### TO DO write task name extraction as a function ! ########
        task = line.strip()
        task_splited = task.split('.')
        if len(task_splited) == 0:
            raise ValueError(f'Invalid line format : line {line}')
        print(f'task_splited.length={len(task_splited)}')
        print(f'task_splited[0]={task_splited[0]}')
        all_nb_params_in_task.append(task_splited[-5])
        a = 0
        task_name = ""
        maxa = sum([(True) for i in task_splited[1:-6]])
        for i in task_splited[1:-5]:
            task_name += i
            if a < maxa:
                task_name += "."
            a += 1
        ##### END TO DO #####
        if os.path.isfile(task) :
            task_line_count = sum(1 for _ in open(task)) - 1
            print(f'task_line_count={task_line_count}')
            if task_line_count > 0:
                if task_name not in all_names:
                    if task_name in all_X or task_name in all_Y or task_name in all_modes:
                        raise ValueError(f'Error in names/X/Y consistency {all_names}  {all_X} {all_Y}\n')
                    all_names.append(task_name)
                    all_tasks.append(task)
                    all_X[task_name] = []
                    all_modes[task_name] = []
                    all_Y[task_name] = []
                    line_count[task_name] = task_line_count
            else:
                os.remove(task)    # we remove the file if we can't identify a task name, harmless ??
                is_empty_task = True
        else:
            is_empty_task = True

    if is_empty_task:
        f = open(profile_file_name, 'w')
        task_string = '\n'.join(all_tasks)
        f.write(task_string)
        f.close()
    return all_names, all_nb_params_in_task, all_tasks, all_X, all_modes, all_Y, all_nb_params, line_count


def generate_random_samples(nb_event_limit, extremum_percentage, tasks):
    """
    """
    all_names, all_nb_params_in_task, all_tasks, all_X, all_modes, all_Y, all_nb_params, line_count = tasks
    print('[ACTION] Generate random indices...') 
    randoms_between_0_and_line_count = {}
    for names in all_names:
        if (line_count[names] - 1) > nb_event_limit:
            randoms_between_0_and_line_count[names] = [i for i in
                                                  range(0, int((nb_event_limit * extremum_percentage) / 100))]
            randoms_between_0_and_line_count[names] += \
                random.sample( \
                    range( \
                        int((nb_event_limit * extremum_percentage) / 100), \
                        int(line_count[names] - ((nb_event_limit * extremum_percentage) / 100)) \
                        ), \
                    int((nb_event_limit * (100 - (2 * extremum_percentage))) / 100) \
                    )
            randoms_between_0_and_line_count[names] += [i for i in range(
                int(line_count[names] - ((nb_event_limit * extremum_percentage) / 100)), line_count[names])]
        else:
            randoms_between_0_and_line_count[names] = [i for i in range(0, line_count[names])]
        print(f'names={names}')
    return randoms_between_0_and_line_count


def read_csv(all_lines, randoms_between_0_and_line_count, nb_event_limit, tasks):
    all_names, all_nb_params_in_task, all_tasks, all_X, all_modes, all_Y, all_nb_params, line_count = tasks
    print('[ACTION] Read each csv...')
    for task in all_tasks:
        print(task)
        task_splited = task.split('.')
        a = 0
        task_name = ""
        maxa = sum([(True) for i in task_splited[1:-6]])
        for i in task_splited[1:-5]:
            task_name += i
            if a < maxa:
                task_name += "."
            a += 1
        print(task_name)
        data_frame_task = pd.read_csv(task)
        all_nb_params[task_name] = int(task_splited[-5])
        read_nbr = 0
        write_nbr = 0

        if task_splited[-4][0] == 'R' and task_splited[-3][0] == 'W':
            read_nbr = int(task_splited[-4][1:])
            write_nbr = int(task_splited[-3][1:])
        else:
            raise ValueError('Invalid file name ' + str(task))

        if len(data_frame_task.columns) != all_nb_params[task_name] + 1 or len(data_frame_task.columns) != (
                read_nbr + write_nbr + 1) or all_nb_params[task_name] != (read_nbr + write_nbr):
            raise ValueError('Invalid task format ' + str(task) + '\n' +
                             'Between dataframe : ' + str(len(data_frame_task.columns)) + '\n' +
                             'And Filename var : ' + str(all_nb_params[task_name] + 1) + '\n')
        data_frame_task.sort_values(by=['ET'], inplace=True)
        data_frame_task.to_csv(task, index=False)
        for taskIdx in randoms_between_0_and_line_count[task_name]:
            all_Y[task_name].append(data_frame_task.at[taskIdx, 'ET'])
            all_X[task_name].append([data_frame_task.at[taskIdx, columnName] for columnName in data_frame_task.columns[:-1]])
            all_modes[task_name].append([str(columnName[0].strip()) for columnName in data_frame_task.columns[:-1]])

        tmp_event = nb_event_limit
        if line_count[task_name] <= nb_event_limit:
            nb_event_limit = line_count[task_name]
        if len(all_X[task_name]) != nb_event_limit or len(all_Y[task_name]) != nb_event_limit or len(
                all_modes[task_name]) != nb_event_limit:
            raise ValueError('Invalid event number with task : ' + task_name + '\n' +
                             'event number : ' + str(nb_event_limit) + '\n' +
                             'X : ' + str(len(all_X[task_name])) + '\n' +
                             'Modes : ' + str(len(all_modes[task_name])) + '\n' +
                             'Y : ' + str(len(all_Y[task_name])) + '\n')
        nb_event_limit = tmp_event
    return


def generate_weights(cutoff_value, tasks):
    """
    """
    all_names, all_nb_params_in_task, all_tasks, all_X, all_modes, all_Y, all_nb_params, line_count = tasks

    print('[ACTION] Generate weights...')
    all_weights = {}
    diff_coef = 100  # A value at compact_Y_cutoff is 100× more important than extreme values
    for task_name in all_names:
        print('[ACTION] - Weights for task_name ' + task_name)
        Y = all_Y[task_name]
        all_weights[task_name] = []

        if not Y:
            continue
        min_Y = min(Y[:])
        max_Y = max(Y[:])
        if cutoff_value is not None:
            maxStep = max([abs(min_Y - cutoff_value), abs(max_Y - cutoff_value)])
            for i in range(len(Y)):
                all_weights[task_name].append(diff_coef * (1 - abs(Y[i] - cutoff_value) / maxStep) + 1)
        else:
            raise ValueError('cuttoff_value cannot be None whene using weights !')

    return all_weights


def compact_cutoff(cutoff_value, tasks):
    '''
    assume cutoff_value is not None
    return the list modified all_Y
    '''
    all_names, all_nb_params_in_task, all_tasks, all_X, all_modes, all_Y, all_nb_params, line_count = tasks
    print('[ACTION] Compact cutoff...')
    for task_name in all_names:
        print('[ACTION] - Compact for task_name ' + task_name)
        Y = all_Y[task_name]
        for i in range(len(Y)):
            if Y[i] < cutoff_value:
                Y[i] = cutoff_value - 1
            else:
                Y[i] = cutoff_value + 1

    return Y


def plot_fig(cutoff_value, profile_filename, regression_type_func, all_X, X_poly, nb_params, task_name, Y, end_file_name):
    print('[ACTION] Generate figure...')
    X_orig = np.ma.asarray(all_X[task_name])
    predict = np.array(regression_type_func.predict(X_poly))

    fig, ax = plt.subplots(nb_params, squeeze=False)
    fig.suptitle(task_name)
    for param in range(nb_params):
        column = X_orig[:, param].tolist()
        ax[param, 0].scatter(column, Y, color='blue')
        order = [int(x) for x, y in sorted(enumerate(column), key=lambda x: x[1])]  # np.argsort(column)
        ax[param, 0].plot(np.array(column)[order], predict[order], color='red')
    # plt.show()
    true_cutoff_value = cutoff_value if cutoff_value is not None else ''
    print('[INFO] save figure in ' + profile_filename + '.' + true_cutoff_value + task_name + end_file_name + '.png')
    fig.savefig(profile_filename + '.' + true_cutoff_value + task_name + end_file_name + '.png', bbox_inches='tight')

def calculate_score_ratio_to_stop(new_X_poly):
    size=sum([(True) for i in new_X_poly])
    size_str=str(size)
    digit_length=len(size_str)
    ret=1.+(1./digit_length)
    return ret


def generate_model_linear(profile_filename, regression_type, correlation_score_threshold, use_weights,
                          all_weights, max_poly_degree, cutoff_value, tasks, plot_figures, compact_formula, verbose):
    """
    This is the beast.
    """
    all_names, all_nb_params_in_task, all_tasks, all_X, all_modes, all_Y, all_nb_params, line_count = tasks
    # Create the model file
    model_filename = profile_filename + '.model'
    model_file = open(model_filename, "w")
    model_file.write(f'degree={max_poly_degree}\n')
    print(f'[CONFIG] Output file: {model_filename}')

    # Iterate over the functions
    idx_task=0
    for task_name in all_names:
        print('============================================================')
        print('[ACTION] Regression ' + str(idx_task) + ' for task_name ' + task_name)
        idx_task+=1
        X = all_X[task_name]
        Y = all_Y[task_name]
        modes = all_modes[task_name]
        nb_params = all_nb_params[task_name]
        if not X or not Y or not modes:
            print( '[INFO] - Not find anything ')
            print(f'[INFO] - Inside Y : {Y}')
            print(f'[INFO] - Inside X : {X}')
            print(f'[INFO] - Inside modes : {modes}')
            continue

        average_Y = np.average(Y)
        formula = str(average_Y)
        if nb_params == 0:
            print( '[INFO] - No parameter')
            print(f'[INFO] - Sum of execution time : {sum(Y)}')
            print(f'[INFO] - Average : {average_Y}')
        elif max(Y) - min(Y) <= 0.05 * average_Y :
            print(f'[INFO] - Seems constant with average of {average_Y}')

            if cutoff_value is not None:
                percents_success = 100 * (sum(
                    [(Y[i] <= cutoff_value and average_Y <= cutoff_value) or (Y[i] > cutoff_value and average_Y > cutoff_value) for i in
                    range(len(Y))]) / float(len(Y)))
                print(f'[INFO]   - Success for threshold of {cutoff_value} : {percents_success}')

            if plot_figures:
                X_orig = np.ma.asarray(all_X[task_name])
                fig, ax = plt.subplots(nb_params, squeeze=False)
                fig.suptitle(task_name)
                for param in range(nb_params):
                    column = X_orig[:, param].tolist()
                    ax[param, 0].scatter(column, Y, color='blue')
                    order = [int(x) for x, y in sorted(enumerate(column), key=lambda x: x[1])]  # np.argsort(column)
                    ax[param, 0].plot([min(column[:]), max(column[:])], [average_Y, average_Y], color='red')
                true_cutoff_value = cutoff_value if cutoff_value is not None else ''
                print('[INFO] save figure in ' + profile_filename + '.' + true_cutoff_value + task_name + '-empty.png')
                fig.savefig(profile_filename + '.' + true_cutoff_value + task_name + '-empty.png', bbox_inches='tight')

        else:
            # Remove useless data
            X = np.ma.asarray(X)
            X_names = ['X' + str(x) for x in range(nb_params)]
            # Remove useless data or data with no correlation
            for idx_col in range(X.shape[1] - 1, -1, -1):
                if (X[0, idx_col] == X[:, idx_col]).all():
                    poly_regr = PolynomialFeatures(degree=max_poly_degree)
                    X_poly = poly_regr.fit_transform(X[:, idx_col].reshape(-1, 1))
                    if LinearRegression().fit(X_poly, Y).score(X_poly, Y) < correlation_score_threshold:  # TODO decide what to do with W values
                        X = np.delete(X, idx_col, axis=1)
                        del X_names[idx_col]
                        del modes[idx_col]
            if len(X_names) == 0:
                print(f'[INFO]  - All variables have been deleted use average {average_Y}')
                formula = str(np.average(Y))
                average_Y = np.average(Y)

                if cutoff_value is not None:
                    percents_success = 100 * (sum(
                        [(Y[i] <= cutoff_value and average_Y <= cutoff_value) or (Y[i] > cutoff_value and average_Y > cutoff_value) for
                        i in range(len(Y))]) / float(len(Y)))
                    print(f'[INFO]    - Success for threshold of {cutoff_value} : {percents_success}')
            else:
                print('[INFO]  Use linear regression')
                regression_type_func = LinearRegression(copy_X=True, n_jobs=-1, fit_intercept=True)

                for idxPoly in range(1, min(max_poly_degree, len(X)) + 1):
                    print('[INFO]  Test poly degree: ' + str(idxPoly))
                    poly_regr = PolynomialFeatures(degree=idxPoly)
                    X_poly = poly_regr.fit_transform(X)
                    current_X_names = poly_regr.get_feature_names(X_names)

                    if use_weights:
                        regression_type_func = regression_type_func.fit(X_poly, Y, sample_weight=all_weights[task_name])
                    else:
                        regression_type_func = regression_type_func.fit(X_poly, Y)

                    coef = regression_type_func.coef_
                    intercept = regression_type_func.intercept_
                    print(f'[INFO]    - score: {regression_type_func.score(X_poly, Y)}')
                    if verbose:
                        print(f'[INFO]    - coef linear: {coef}')
                        print(f'[INFO]    - get_feature_names : {current_X_names}')
                        print(f'[INFO]    - coef poly: {poly_regr.powers_}')
                        print(f'[INFO]    - intercept_ : {intercept}')

                    if cutoff_value is not None:
                        f_Y = regression_type_func.predict(X_poly)
                        percents_success = 100 * (sum([(Y[i] <= cutoff_value and f_Y[i] <= cutoff_value) or (
                            Y[i] > cutoff_value and f_Y[i] > cutoff_value) for i in range(len(Y))]) / float(len(Y)))
                        print('[INFO]   - Success for treshold of ' + str(cutoff_value) + ' :' + str(percents_success))

                    if regression_type_func.score(X_poly, Y) >= 0.999:
                        print('[INFO] - success, keep this degree')
                        break
                X_names = current_X_names

            if plot_figures:
                plot_fig(cutoff_value, profile_filename, regression_type_func, all_X, X_poly, nb_params, task_name, Y, '-full')

            if compact_formula:
                print('[INFO] Compact formula')
                X_trunc = X_poly
                Y_classification = []
                Y_classification = Y
                current_score = regression_type_func.score(X_trunc, Y_classification)
                if cutoff_value is not None:
                    f_Y = regression_type_func.predict(X_trunc)
                    current_success_rate = 100 * (sum([(Y[i] <= cutoff_value and f_Y[i] <= cutoff_value) or (
                            Y[i] > cutoff_value and f_Y[i] > cutoff_value) for i in range(len(Y))]) / float(len(Y)))

                has_changed = True
                while len(X_names) > 1 and has_changed:
                    has_changed = False
                    # Find lower coef
                    idx_to_delete = 0
                    lowest_impact = regression_type_func.coef_[idx_to_delete] * max(
                        [abs(max(X_poly[:, idx_to_delete].tolist())), abs(min(X_poly[:, idx_to_delete].tolist()))])

                    for i in range(1, len(X_names)):
                        current_impact = regression_type_func.coef_[i] * max(
                            [abs(max(X_poly[:, i].tolist())), abs(min(X_poly[:, i].tolist()))])

                        if lowest_impact > current_impact:
                            idx_to_delete = i
                            lowest_impact = current_impact

                    if verbose:
                        print('[INFO] - idx to delete: ' + str(idx_to_delete) + ' - ' + X_names[idx_to_delete])
                    # Try to delete it
                    new_X_poly = np.delete(X_poly, idx_to_delete, axis=1)

                    lin_regr_trunc = LinearRegression(copy_X=True, n_jobs=-1, fit_intercept=True)
                    Y_classification = []
                    Y_classification = Y
                    if use_weights:
                        lin_regr_trunc = lin_regr_trunc.fit(new_X_poly, Y_classification, sample_weight=all_weights[task_name])
                    else:
                        lin_regr_trunc = lin_regr_trunc.fit(new_X_poly, Y_classification)

                    new_score = lin_regr_trunc.score(new_X_poly, Y_classification)
                    
                    if verbose:
                        print('[INFO]   - score: ' + str(new_score))
                        print('[INFO]   - coef: ' + str(lin_regr_trunc.coef_))
                        print('[INFO]   - intercept_ : ' + str(lin_regr_trunc.intercept_))
                    if cutoff_value is not None:
                        f_Y = lin_regr_trunc.predict(new_X_poly)
                        new_success_rate = 100 * (sum([(Y[i] <= cutoff_value and f_Y[i] <= cutoff_value) or (
                                Y[i] > cutoff_value and f_Y[i] > cutoff_value) for i in range(len(Y))]) / float(
                            len(Y)))
                        print('[INFO]   - Success rate :' + str(new_success_rate))
                    # Check if that improve things
                    if new_score * calculate_score_ratio_to_stop(new_X_poly) < current_score:
                        print('[INFO]   - Not improved because of the score, stop here: ' + str(new_score))
                        has_changed = False
                    elif cutoff_value is not None and new_success_rate < current_success_rate:
                        print('[INFO]   - Not improved because of the success rate, stop here: ' + str(new_success_rate))
                        has_changed = False
                    else:
                        print('[INFO] compacting formula -> score ' + str(new_score) + ' -> delete ' + str(idx_to_delete) + ' : ' + X_names[idx_to_delete])
                        current_score = new_score
                        has_changed = True
                        del X_names[idx_to_delete]
                        X_poly = new_X_poly
                        regression_type_func = lin_regr_trunc
                        if cutoff_value is not None:
                            current_success_rate = new_success_rate

            if X_names == 0:
                print('[INFO] - Every variables were delete, will take the average...')
                formula = str(np.average(Y))
                average_Y = np.average(Y)
                if cutoff_value is not None:
                    percents_success = 100 * (sum(
                        [(Y[i] <= cutoff_value and average_Y <= cutoff_value) or (Y[i] > cutoff_value and average_Y > cutoff_value)
                         for i in range(len(Y))]) / float(len(Y)))
                    print('[INFO] - Success for treshold of ' + str(cutoff_value) + ' :' + str(percents_success))
            else:
                print('[INFO] - Generate formula...')
                coeficients = regression_type_func.coef_
                formula_list = []
                for feature, coeficient in zip(X_names, coeficients):
                    if (str(feature) == '1'):
                        formula_list.append(str(coeficient))
                    else:
                        formula_list.append(str(feature).replace(' ', '*') + '*' + str(coeficient))
                formula = ' + '.join(formula_list)
                formula = str(regression_type_func.intercept_) + ' + ' + re.sub('([-+* ])?X([0-9])+[\^]([0-9])+',
                                                                    '\\1apac_fpow<\\3>(\\2)', formula)
            if plot_figures:
                plot_fig(cutoff_value, profile_filename, regression_type_func, all_X, X_poly, nb_params, task_name, Y, '-end')
        # Show formula
        print('[INFO] => ' + formula)
        # Put it into the file
        model_file.write(task_name + '(nbparams=' + str(nb_params) + ')' + '=' + formula + '\n')

    print('(For parsing purpose) Regression ' + str(idx_task))

    # The End
    print('[INFO] close model file')
    model_file.close()


def generate_model_logistic(profile_filename, regression_type, correlation_score_threshold, use_weights,
                            all_weights, max_poly_degree, cutoff_value, tasks, plot_figures, compact_formula, verbose):
    """
    This is the beast.
    """
    all_names, all_nb_params_in_task, all_tasks, all_X, all_modes, all_Y, all_nb_params, line_count = tasks
    # Create the model file
    model_filename = profile_filename + '.model'
    model_file = open(model_filename, "w")
    model_file.write(f'degree={max_poly_degree}\n')
    print(f'[CONFIG] Output file: {model_filename}')

    # Iterate over the functions
    idx_task=0
    for task_name in all_names:
        print('============================================================')
        print('[ACTION] Regression ' + str(idx_task) + ' for task_name ' + task_name)
        idx_task+=1
        X = all_X[task_name]
        Y = all_Y[task_name]
        modes = all_modes[task_name]
        nb_params = all_nb_params[task_name]
        if not X or not Y or not modes:
            print( '[INFO] - Not find anything ')
            print(f'[INFO] - Inside Y : {Y}')
            print(f'[INFO] - Inside X : {X}')
            print(f'[INFO] - Inside modes : {modes}')
            continue

        average_Y = np.average(Y)
        formula = str(average_Y)
        if nb_params == 0:
            print( '[INFO] - No parameter')
            print(f'[INFO] - Sum of execution time : {sum(Y)}')
            print(f'[INFO] - Average : {average_Y}')
        elif (Y[0] == np.ma.asarray(Y)[:]).all():
            print(f'[INFO] - Seems constant with average of {average_Y}')

            if cutoff_value is not None:
                percents_success = 100 * (sum(
                    [(Y[i] <= cutoff_value and average_Y <= cutoff_value) or (Y[i] > cutoff_value and average_Y > cutoff_value) for i in
                    range(len(Y))]) / float(len(Y)))
                print(f'[INFO]   - Success for threshold of {cutoff_value} : {percents_success}')

            if plot_figures:
                X_orig = np.ma.asarray(all_X[task_name])
                fig, ax = plt.subplots(nb_params, squeeze=False)
                fig.suptitle(task_name)
                for param in range(nb_params):
                    column = X_orig[:, param].tolist()
                    ax[param, 0].scatter(column, Y, color='blue')
                    order = [int(x) for x, y in sorted(enumerate(column), key=lambda x: x[1])]  # np.argsort(column)
                    ax[param, 0].plot([min(column[:]), max(column[:])], [average_Y, average_Y], color='red')
                true_cutoff_value = cutoff_value if cutoff_value is not None else ''
                print('[INFO] save figure in ' + profile_filename + '.' + true_cutoff_value + task_name + '-empty.png')
                fig.savefig(profile_filename + '.' + true_cutoff_value + task_name + '-empty.png', bbox_inches='tight')

        else:
            # Remove useless data
            X = np.ma.asarray(X)
            X_names = ['X' + str(x) for x in range(nb_params)]
            if len(X_names) == 0:
                print(f'[INFO]  - All variables have been deleted use average {average_Y}')
                formula = str(np.average(Y))
                average_Y = np.average(Y)

                if cutoff_value is not None:
                    percents_success = 100 * (sum(
                        [(Y[i] <= cutoff_value and average_Y <= cutoff_value) or (Y[i] > cutoff_value and average_Y > cutoff_value) for
                        i in range(len(Y))]) / float(len(Y)))
                    print(f'[INFO]    - Success for threshold of {cutoff_value} : {percents_success}')
            else:
                print('[INFO]  Use logistic')
                regression_type_func = LogisticRegression()
                Y_classification = [1 if Y[i] > cutoff_value else 0 for i in range(len(Y))]
                if use_weights:
                    regression_type_func = regression_type_func.fit(X, Y_classification, sample_weight=all_weights[task_name])
                else:
                    regression_type_func = regression_type_func.fit(X, Y_classification)
                print('[INFO]   - score: ' + str(regression_type_func.score(X, Y_classification)))
                print('[INFO]   - coef: ' + str(regression_type_func.coef_))
                print('[INFO]   - intercept_ : ' + str(regression_type_func.intercept_))
                print('[INFO]   - classes_: ' + str(regression_type_func.classes_))
                if cutoff_value is not None:
                    f_Y = regression_type_func.predict(X)
                    percents_success = 100 * (sum([(Y[i] <= cutoff_value and f_Y[i] <= cutoff_value) or (
                        Y[i] > cutoff_value and f_Y[i] > cutoff_value) for i in range(len(Y))]) / float(len(Y)))
                    print(f'[INFO]    - Success for threshold of {cutoff_value} : {percents_success}')
                X_poly = X
            if plot_figures:
                plot_fig(cutoff_value, profile_filename, regression_type_func, all_X, X_poly, nb_params, task_name, Y, '-full')

            if compact_formula:
                print('[INFO] Compact formula')
                X_trunc = X_poly
                Y_classification = []
                Y_classification = [1 if Y[i] > cutoff_value else 0 for i in range(len(Y))]
                current_score = regression_type_func.score(X_trunc, Y_classification)
                if cutoff_value is not None:
                    f_Y = regression_type_func.predict(X_trunc)
                    current_success_rate = 100 * (sum([(Y[i] <= cutoff_value and f_Y[i] <= cutoff_value) or (
                            Y[i] > cutoff_value and f_Y[i] > cutoff_value) for i in range(len(Y))]) / float(len(Y)))

                has_changed = True
                while len(X_names) > 1 and has_changed:
                    has_changed = False
                    # Find lower coef
                    idx_to_delete = 0
                    lowest_impact = regression_type_func.coef_.transpose()[idx_to_delete] * max(
                        [abs(max(X_poly[:, idx_to_delete].tolist())),
                            abs(min(X_poly[:, idx_to_delete].tolist()))])

                    for i in range(1, len(X_names)):
                        current_impact = regression_type_func.coef_.transpose()[i] * max(
                            [abs(max(X_poly[:, i].tolist())), abs(min(X_poly[:, i].tolist()))])

                        if lowest_impact > current_impact:
                            idx_to_delete = i
                            lowest_impact = current_impact
                    if verbose:
                        print('[INFO] - idx to delete: ' + str(idx_to_delete) + ' - ' + X_names[idx_to_delete])
                    # Try to delete it
                    new_X_poly = np.delete(X_poly, idx_to_delete, axis=1)

                    lin_regr_trunc = LogisticRegression()
                    Y_classification = []
                    Y_classification = [1 if Y[i] > cutoff_value else 0 for i in range(len(Y))]
                    if use_weights:
                        lin_regr_trunc = lin_regr_trunc.fit(new_X_poly, Y_classification, sample_weight=all_weights[task_name])
                    else:
                        lin_regr_trunc = lin_regr_trunc.fit(new_X_poly, Y_classification)

                    new_score = lin_regr_trunc.score(new_X_poly, Y_classification)
                    
                    if verbose:
                        print('[INFO]   - score: ' + str(new_score))
                        print('[INFO]   - coef: ' + str(lin_regr_trunc.coef_))
                        print('[INFO]   - intercept_ : ' + str(lin_regr_trunc.intercept_))
                    if cutoff_value is not None:
                        f_Y = lin_regr_trunc.predict(new_X_poly)
                        new_success_rate = 100 * (sum([(Y[i] <= cutoff_value and f_Y[i] <= cutoff_value) or (
                                Y[i] > cutoff_value and f_Y[i] > cutoff_value) for i in range(len(Y))]) / float(
                            len(Y)))
                        if verbose:
                            print('[INFO]   - Success rate :' + str(new_success_rate))
                    # Check if that improve things
                    if new_score * calculate_score_ratio_to_stop(new_X_poly) < current_score:
                        print('[INFO]   - Not improved because of the score, stop here: ' + str(new_score))
                        has_changed = False
                    elif cutoff_value is not None and new_success_rate < current_success_rate:
                        print('[INFO]   - Not improved because of the success rate, stop here: ' + str(new_success_rate))
                        has_changed = False
                    else:
                        print('[INFO] compacting formula -> score ' + str(new_score) + ' -> delete ' + str(idx_to_delete) + ' : ' + X_names[idx_to_delete])
                        current_score = new_score
                        has_changed = True
                        del X_names[idx_to_delete]
                        X_poly = new_X_poly
                        regression_type_func = lin_regr_trunc
                        if cutoff_value is not None:
                            current_success_rate = new_success_rate

            if X_names == 0:
                print('[INFO] - Every variables were delete, will take the average...')
                formula = str(np.average(Y))
                average_Y = np.average(Y)
                if cutoff_value is not None:
                    percents_success = 100 * (sum(
                        [(Y[i] <= cutoff_value and average_Y <= cutoff_value) or (Y[i] > cutoff_value and average_Y > cutoff_value)
                         for i in range(len(Y))]) / float(len(Y)))
                    print('[INFO] - Success for treshold of ' + str(cutoff_value) + ' :' + str(percents_success))
            else:
                print('[INFO] - Generate formula...')
                Xcoefs = [str(X_names[i]) + '*' + str(regression_type_func.coef_.transpose()[i][0]) for i in
                            range(len(regression_type_func.coef_.transpose()))]
                formula = '(1/(1 + std::exp(' + str(regression_type_func.intercept_[0]) + '+' + '+'.join(
                    Xcoefs) + ')))<0.5 ? ' + str(cutoff_value - 1) + ':' + str(cutoff_value + 1)
            if plot_figures:
                plot_fig(cutoff_value, profile_filename, regression_type_func, all_X, X_poly, nb_params, task_name, Y, '-end')
        # Show formula
        print('[INFO] => ' + formula)
        # Put it into the file
        model_file.write(task_name + '(nbparams=' + str(nb_params) + ')' + '=' + formula + '\n')

    print('(For parsing purpose) Regression ' + str(idx_task))


    # The End
    print('[INFO] close model file')
    model_file.close()


def generate_model_ransac(profile_filename, regression_type, correlation_score_threshold, use_weights,
                   all_weights, max_poly_degree, cutoff_value, tasks, plot_figures, compact_formula, verbose):
    """
    This is the beast.
    """
    all_names, all_nb_params_in_task, all_tasks, all_X, all_modes, all_Y, all_nb_params, line_count = tasks
    # Create the model file
    model_filename = profile_filename + '.model'
    model_file = open(model_filename, "w")
    model_file.write(f'degree={max_poly_degree}\n')
    print(f'[CONFIG] Output file: {model_filename}')

    # Iterate over the functions
    idx_task=0
    for task_name in all_names:
        print('============================================================')
        print('[ACTION] Regression ' + str(idx_task) + ' for task_name ' + task_name)
        idx_task+=1
        X = all_X[task_name]
        Y = all_Y[task_name]
        modes = all_modes[task_name]
        nb_params = all_nb_params[task_name]
        if not X or not Y or not modes:
            print( '[INFO] - Not find anything ')
            print(f'[INFO] - Inside Y : {Y}')
            print(f'[INFO] - Inside X : {X}')
            print(f'[INFO] - Inside modes : {modes}')
            continue

        average_Y = np.average(Y)
        formula = str(average_Y)
        if nb_params == 0:
            print( '[INFO] - No parameter')
            print(f'[INFO] - Sum of execution time : {sum(Y)}')
            print(f'[INFO] - Average : {average_Y}')
        elif max(Y) - min(Y) <= 0.05 * average_Y :
            print(f'[INFO] - Seems constant with average of {average_Y}')

            if cutoff_value is not None:
                percents_success = 100 * (sum(
                    [(Y[i] <= cutoff_value and average_Y <= cutoff_value) or (Y[i] > cutoff_value and average_Y > cutoff_value) for i in
                    range(len(Y))]) / float(len(Y)))
                print(f'[INFO]   - Success for threshold of {cutoff_value} : {percents_success}')

            if plot_figures:
                X_orig = np.ma.asarray(all_X[task_name])
                fig, ax = plt.subplots(nb_params, squeeze=False)
                fig.suptitle(task_name)
                for param in range(nb_params):
                    column = X_orig[:, param].tolist()
                    ax[param, 0].scatter(column, Y, color='blue')
                    order = [int(x) for x, y in sorted(enumerate(column), key=lambda x: x[1])]  # np.argsort(column)
                    ax[param, 0].plot([min(column[:]), max(column[:])], [average_Y, average_Y], color='red')
                true_cutoff_value = cutoff_value if cutoff_value is not None else ''
                print('[INFO] save figure in ' + profile_filename + '.' + true_cutoff_value + task_name + '-empty.png')
                fig.savefig(profile_filename + '.' + true_cutoff_value + task_name + '-empty.png', bbox_inches='tight')

        else:
            # Remove useless data
            X = np.ma.asarray(X)
            X_names = ['X' + str(x) for x in range(nb_params)]
            # Remove useless data or data with no correlation
            for idx_col in range(X.shape[1] - 1, -1, -1):
                if (X[0, idx_col] == X[:, idx_col]).all():
                    poly_regr = PolynomialFeatures(degree=max_poly_degree)
                    X_poly = poly_regr.fit_transform(X[:, idx_col].reshape(-1, 1))
                    if LinearRegression().fit(X_poly, Y).score(X_poly, Y) < correlation_score_threshold:  # TODO decide what to do with W values
                        X = np.delete(X, idx_col, axis=1)
                        del X_names[idx_col]
                        del modes[idx_col]
            if len(X_names) == 0:
                print(f'[INFO]  - All variables have been deleted use average {average_Y}')
                formula = str(np.average(Y))
                average_Y = np.average(Y)

                if cutoff_value is not None:
                    percents_success = 100 * (sum(
                        [(Y[i] <= cutoff_value and average_Y <= cutoff_value) or (Y[i] > cutoff_value and average_Y > cutoff_value) for
                        i in range(len(Y))]) / float(len(Y)))
                    print(f'[INFO]    - Success for threshold of {cutoff_value} : {percents_success}')
            else:
                print('[INFO]  Use ransac regression')
                regression_type_func = RANSACRegressor(max_trials=300, random_state=0, min_samples=X.shape[1] + 1, residual_threshold=np.abs(np.mean(Y)))

                for idxPoly in range(1, min(max_poly_degree, len(X)) + 1):
                    print('[INFO]  Test poly degree: ' + str(idxPoly))
                    poly_regr = PolynomialFeatures(degree=idxPoly)
                    X_poly = poly_regr.fit_transform(X)
                    current_X_names = poly_regr.get_feature_names(X_names)

                    if use_weights:
                        regression_type_func = regression_type_func.fit(X_poly, Y, sample_weight=all_weights[task_name])
                    else:
                        regression_type_func = regression_type_func.fit(X_poly, Y)

                    coef = regression_type_func.estimator_.coef_
                    intercept = regression_type_func.estimator_.intercept_
                    print(f'[INFO]    - score: {regression_type_func.score(X_poly, Y)}')
                    if verbose:
                        print(f'[INFO]    - coef linear: {coef}')
                        print(f'[INFO]    - get_feature_names : {current_X_names}')
                        print(f'[INFO]    - coef poly: {poly_regr.powers_}')
                        print(f'[INFO]    - intercept_ : {intercept}')

                    if cutoff_value is not None:
                        f_Y = regression_type_func.predict(X_poly)
                        percents_success = 100 * (sum([(Y[i] <= cutoff_value and f_Y[i] <= cutoff_value) or (
                            Y[i] > cutoff_value and f_Y[i] > cutoff_value) for i in range(len(Y))]) / float(len(Y)))
                        print('[INFO]   - Success for treshold of ' + str(cutoff_value) + ' :' + str(percents_success))

                    if regression_type_func.score(X_poly, Y) >= 0.999:
                        print('[INFO] - success, keep this degree')
                        break
                X_names = current_X_names

            if plot_figures:
                plot_fig(cutoff_value, profile_filename, regression_type_func, all_X, X_poly, nb_params, task_name, Y, '-full')

            if compact_formula:
                print('[INFO] Compact formula')
                X_trunc = X_poly
                Y_classification = []
                Y_classification = Y
                current_score = regression_type_func.score(X_trunc, Y_classification)
                if cutoff_value is not None:
                    f_Y = regression_type_func.predict(X_trunc)
                    current_success_rate = 100 * (sum([(Y[i] <= cutoff_value and f_Y[i] <= cutoff_value) or (
                            Y[i] > cutoff_value and f_Y[i] > cutoff_value) for i in range(len(Y))]) / float(len(Y)))

                has_changed = True
                while len(X_names) > 1 and has_changed:
                    has_changed = False
                    # Find lower coef
                    idx_to_delete = 0
                    lowest_impact = regression_type_func.estimator_.coef_[idx_to_delete] * max(
                        [abs(max(X_poly[:, idx_to_delete].tolist())), abs(min(X_poly[:, idx_to_delete].tolist()))])

                    for i in range(1, len(X_names)):
                        current_impact = regression_type_func.estimator_.coef_[i] * max(
                            [abs(max(X_poly[:, i].tolist())), abs(min(X_poly[:, i].tolist()))])

                        if lowest_impact > current_impact:
                            idx_to_delete = i
                            lowest_impact = current_impact
                    if verbose:
                        print('[INFO] - idx to delete: ' + str(idx_to_delete) + ' - ' + X_names[idx_to_delete])
                    # Try to delete it
                    new_X_poly = np.delete(X_poly, idx_to_delete, axis=1)

                    lin_regr_trunc = RANSACRegressor(max_trials=300, random_state=0)
                    Y_classification = []
                    Y_classification = Y
                    if use_weights:
                        lin_regr_trunc = lin_regr_trunc.fit(new_X_poly, Y_classification, sample_weight=all_weights[task_name])
                    else:
                        lin_regr_trunc = lin_regr_trunc.fit(new_X_poly, Y_classification)

                    new_score = lin_regr_trunc.score(new_X_poly, Y_classification)
                    
                    if verbose:
                        print('[INFO]   - score: ' + str(new_score))
                        print('[INFO]   - coef: ' + str(lin_regr_trunc.estimator_.coef_))
                        print('[INFO]   - intercept_ : ' + str(lin_regr_trunc.estimator_.intercept_))
                    if cutoff_value is not None:
                        f_Y = lin_regr_trunc.predict(new_X_poly)
                        new_success_rate = 100 * (sum([(Y[i] <= cutoff_value and f_Y[i] <= cutoff_value) or (
                                Y[i] > cutoff_value and f_Y[i] > cutoff_value) for i in range(len(Y))]) / float(
                            len(Y)))
                        if verbose:
                            print('[INFO]   - Success rate :' + str(new_success_rate))
                    # Check if that improve things
                    if new_score * calculate_score_ratio_to_stop(new_X_poly) < current_score:
                        print('[INFO]   - Not improved because of the score, stop here: ' + str(new_score))
                        has_changed = False
                    elif cutoff_value is not None and new_success_rate < current_success_rate:
                        print('[INFO]   - Not improved because of the success rate, stop here: ' + str(new_success_rate))
                        has_changed = False
                    else:
                        print('[INFO] compacting formula -> score ' + str(new_score) + ' -> delete ' + str(idx_to_delete) + ' : ' + X_names[idx_to_delete])
                        current_score = new_score
                        has_changed = True
                        del X_names[idx_to_delete]
                        X_poly = new_X_poly
                        regression_type_func = lin_regr_trunc
                        if cutoff_value is not None:
                            current_success_rate = new_success_rate

            if X_names == 0:
                print('[INFO] - Every variables were delete, will take the average...')
                formula = str(np.average(Y))
                average_Y = np.average(Y)
                if cutoff_value is not None:
                    percents_success = 100 * (sum(
                        [(Y[i] <= cutoff_value and average_Y <= cutoff_value) or (Y[i] > cutoff_value and average_Y > cutoff_value)
                         for i in range(len(Y))]) / float(len(Y)))
                    print('[INFO] - Success for treshold of ' + str(cutoff_value) + ' :' + str(percents_success))
            else:
                print('[INFO] - Generate formula...')
                coeficients = regression_type_func.estimator_.coef_
                formula_list = []
                for feature, coeficient in zip(X_names, coeficients):
                    if (str(feature) == '1'):
                        formula_list.append(str(coeficient))
                    else:
                        formula_list.append(str(feature).replace(' ', '*') + '*' + str(coeficient))
                formula = ' + '.join(formula_list)
                formula = str(regression_type_func.estimator_.intercept_) + ' + ' + re.sub('([-+* ])?X([0-9])+[\^]([0-9])+',
                                                                                '\\1apac_fpow<\\3>(\\2)', formula)
            if plot_figures:
                plot_fig(cutoff_value, profile_filename, regression_type_func, all_X, X_poly, nb_params, task_name, Y, '-end')
        # Show formula
        print('[INFO] => ' + formula)
        # Put it into the file
        model_file.write(task_name + '(nbparams=' + str(nb_params) + ')' + '=' + formula + '\n')

    print('(For parsing purpose) Regression ' + str(idx_task))


    # The End
    print('[INFO] close model file')
    model_file.close()


def main():
    parser = argparse.ArgumentParser(description="Build an execution model based on profiling values.")
    parser.add_argument('--profile', type=str, required=True, help="profiling data filename")
    parser.add_argument('--cutoff_value', action='store', required=False, dest='cutoff_value', type=float, help="cutoff_value")
    parser.add_argument('--regr', action='store', required=True, dest='regression_type', choices=[i.name for i in Regrtype], help="type of regression")
    parser.add_argument('--use-weights', help="Use weights for regression", action="store_true")
    parser.add_argument('--do-not-compact', help="Do not try to compact the formula", dest="compact", action="store_false", default=True)
    parser.add_argument('--max-poly-degree', action='store', required=False, dest='max_poly_degree', type=int, default=5, 
     				help="set the maxium degree of polynom")
    parser.add_argument('--nb-event-limit', action='store', required=False, dest='nb_event_limit', type=int, default=1000,
                        help="set the upper limit on number of events")
    parser.add_argument('--extremum_percentage', action='store', required=False, dest='extremum_percentage', type=int, default=10, 
    				help="set the extremum percentage")
    parser.add_argument('--min-corr', action='store', required=False, dest='correlation_score_threshold', default=0.8,
                        type=float, help="mininum correlation coefficient to attain [0;1]")
    parser.add_argument('--plot', required=False, dest='plot', action="store_false" if (os.environ.get('SAVE_PNG') and os.environ.get('SAVE_PNG').lower() == 'false') else "store_true", help="triggers figures plot")
       
    parser.add_argument('--verbose', required=False, dest='verbose', action="store_true", help="prints everything !", default=False)
    args = parser.parse_args()
    if args.verbose:
        print(args)

        print_config(args)
    all_lines = load_datafilenames(args.profile)
    tasks = fill_in_tasks(all_lines, args.profile)
    rand_samples = generate_random_samples(args.nb_event_limit, args.extremum_percentage, tasks)
    read_csv(all_lines, rand_samples, args.nb_event_limit, tasks)
    all_weights = generate_weights(args.cutoff_value, tasks) if args.use_weights else {}
    if args.cutoff_value is not None:
        compact_cutoff(args.cutoff_value, tasks)
        # TODO : check that all_Y has actually been modified by side-effect
    if is_linear(args.regression_type) :
        generate_model_linear(args.profile, args.regression_type, args.correlation_score_threshold, args.use_weights,
                                all_weights, args.max_poly_degree, args.cutoff_value, tasks, args.plot, args.compact, args.verbose)
    elif is_logistic(args.regression_type) and args.cutoff_value is not None :
        generate_model_logistic(args.profile, args.regression_type, args.correlation_score_threshold, args.use_weights,
                                all_weights, args.max_poly_degree, args.cutoff_value, tasks, args.plot, args.compact, args.verbose)
    elif is_ransac(args.regression_type) :
        generate_model_ransac(args.profile, args.regression_type, args.correlation_score_threshold, args.use_weights,
                                all_weights, args.max_poly_degree, args.cutoff_value, tasks, args.plot, args.compact, args.verbose)
    else :
        raise ValueError('No regression selected... ')


if __name__ == '__main__':
    main()
