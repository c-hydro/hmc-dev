#!/usr/bin/python3
"""
HMC Debugger - Debug variable 2d

__date__ = '20220525'
__version__ = '1.5.0'
__author__ =
        'Fabio Delogu (fabio.delogu@cimafoundation.org'

__library__ = 'HMC Debugger'

General command line:
python3 debug_2dVar.py -path_data path_string

Version(s):
20220525 (1.5.0) --> First Python release
20151012 (1.0.2) --> Latest MatLab release
"""

# -------------------------------------------------------------------------------------
# Complete library
import logging
import os
import time
import pickle
import pandas as pd
import numpy as np

from datetime import datetime
from argparse import ArgumentParser
from copy import deepcopy

import matplotlib.pylab as plt
# -------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------
# Algorithm information
alg_version = '1.5.0'
alg_release = '2022-05-25'
alg_name = 'HMC Debugger - debug2dVar'
# Algorithm parameter(s)
time_format = '%Y-%m-%d %H:%M'

# Algorithm logger
logger = logging.getLogger(__name__)
logger_format = "[%(filename)s:%(lineno)s - %(funcName)20s() ] %(message)s"
logger.setLevel(logging.DEBUG)

logging.basicConfig(
    level=logging.INFO,
    format=logger_format,
    handlers=[logging.FileHandler("debug_2dVar.log"), logging.StreamHandler()]
)

# Menu options
menu_options_type_1 = {
    'f': 'Debug the first 2d variable with [id=1]',
    'r': 'Debug the n 2d variable [id=1,2,3 ...]',
    'q': 'Quit debug',
}

menu_options_type_2 = {
    'n': 'Debug the next 2d variable',
    'o': 'Debug the same 2d variable',
    'p': 'Debug the previous 2d variable',
    'r': 'Debug the n 2d variable [id=1,2,3 ...]',
    'i': 'Re-initialize the debugger',
    'sf': 'Save figures',
    'cf': 'Close figures',
    'sd': 'Save datasets',
    'q': 'Quit debug',
}

list_choice_figure = ['f', 'r', 'n', 'o', 'p']
# -------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Script Main
def main():

    # -------------------------------------------------------------------------------------
    # algorithm information
    logging.info(' ============================================================================ ')
    logging.info(' ==> ' + alg_name + ' (Version: ' + alg_version + ' Release_Date: ' + alg_release + ')')
    logging.info(' ==> START ... ')
    logging.info(' ')

    # time information
    start_time = time.time()
    # -------------------------------------------------------------------------------------

    # -------------------------------------------------------------------------------------
    # get debugger info
    file_folder = get_args()
    file_name_data_template = 'data_file_{:}.txt'
    file_name_ws_template = 'data_file_{:}_{:}.workspace'
    file_name_figure_template = 'figure_file_{:}_{:}.png'
    # -------------------------------------------------------------------------------------

    # -------------------------------------------------------------------------------------
    # start debugger
    file_id, debug_option = None, None
    file_dict_data_tag, file_dict_ws_tag, file_dict_figure_tag = {}, {}, {}
    while True:

        # get time now
        now_date_time = datetime.now()
        now_date_str = now_date_time.strftime("%Y%m%d_%H%M%S")

        # select file id
        print('')
        print('=========================================')
        print(' DEBUG OPTION(S)')

        if file_id is None:
            print_menu(menu_options_type_1)
            debug_option = 'f'
            debug_option = input('ENTER YOUR CHOICE: ')
        elif (file_id is not None) and (debug_option == 'i'):
            print_menu(menu_options_type_1)
            debug_option = 'f'
            debug_option = input('ENTER YOUR CHOICE: ')
        else:
            print_menu(menu_options_type_2)
            debug_option = 'n'
            debug_option = input('ENTER YOUR CHOICE: ')

        file_id = set_file_id(file_id=file_id, debug_option=debug_option)

        print('')
        print(' ===> HANDLE OPTION: "' + debug_option + '"')
        print(' ===> FILE_ID: "' + str(file_id) + '"')
        print('=========================================')
        print('')

        # Check the file_id variable
        if file_id is not None:

            # define file name(s)
            file_tag = '{0:02d}'.format(file_id)
            file_name_data_tag = file_name_data_template.format(file_tag)
            file_name_ws_tag = file_name_ws_template.format(file_tag, now_date_str)
            file_name_figure_tag = file_name_figure_template.format(file_tag, now_date_str)

            file_path_data_tag = os.path.join(file_folder, file_name_data_tag)
            file_path_ws_tag = os.path.join(file_folder, file_name_ws_tag)
            file_path_figure_tag = os.path.join(file_folder, file_name_figure_tag)

            if debug_option in sorted(list_choice_figure):  # choices to collect and plot datasets

                if os.path.exists(file_path_data_tag):
                    file_var_data, file_rows_2d, file_cols_2d = get_file_data(file_path_data_tag)
                    file_var_stats = compute_var_data(file_var_data)
                    file_var_info = organize_var_info(file_id, file_time=now_date_str, file_stats=file_var_stats)
                    file_var_figure = create_figure(file_var_data, figure_title=file_var_info, figure_cmap=None)
                    show_figure(file_var_figure)

                    file_dict_data_tag[file_id] = file_var_data
                    file_dict_ws_tag[file_id] = file_path_ws_tag
                    file_dict_figure_tag[file_id] = file_path_figure_tag

                else:
                    logging.warning(' ===> File "' + file_path_data_tag + '" does not exists.')
                    logging.info(' ---> Check your choice according with the available datasets')

            elif debug_option in ['i']:  # choice to re-init debug

                logging.info(' ---> Close all figures ... ')
                close_figures()
                logging.info(' ---> Close all figures ... DONE')

            elif debug_option in ['sf']:  # choice to save figures

                logging.info(' ---> Save all figures ... ')
                save_figures(file_dict_figure_tag)
                logging.info(' ---> Save all figures ... DONE')

            elif debug_option in ['cf']:  # choice to close figures

                logging.info(' ---> Close all figures ... ')
                close_figures()
                logging.info(' ---> Close all figures ... DONE')

            elif debug_option in ['sd']:  # choice to save datasets

                logging.info(' ---> Save all datasets ... ')
                save_var_obj(file_dict_ws_tag, file_dict_data_tag)
                logging.info(' ---> Save all datasets ... ')

            elif debug_option in ['q']:  # exit debug

                logging.info(' ---> Close debug -- Thanks message before exiting ')
                break

            else:  # choice not supported

                logging.warning(' ===> Your selection "' + debug_option + '" is not supported')
                logging.info(' ---> Check your choice and select a valid option')

            '''
            # DEBUG
            if file_id == 3:
                save_figures(file_dict_figure_tag)
                close_figures()
                save_var_obj(file_dict_ws_tag, file_dict_data_tag)
                print('test')
            '''

        else:  # file_id == NoneType

            logging.warning(' ===> Your file_id is NoneType')
            logging.info(' ---> Check your choice and select a valid option')
    # -------------------------------------------------------------------------------------

    # -------------------------------------------------------------------------------------
    # Info algorithm
    time_elapsed = round(time.time() - start_time, 1)

    logging.info(' ')
    logging.info(' ==> ' + alg_name + ' (Version: ' + alg_version + ' Release_Date: ' + alg_release + ')')
    logging.info(' ==> TIME ELAPSED: ' + str(time_elapsed) + ' seconds')
    logging.info(' ==> ... END')
    logging.info(' ==> Bye, Bye')
    logging.info(' ============================================================================ ')
    # -------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Method to set file_id
def set_file_id(file_id=None, debug_option='n'):

    if file_id is not None:
        if debug_option == 'n':
            file_id += 1
        elif debug_option == 'o':
            pass
        elif debug_option == 'i':
            file_id = 1
        elif debug_option == 'p':
            file_id -= 1
        elif debug_option == 'sf':
            pass
        elif debug_option == 'cf':
            pass
        elif debug_option == 'sd':
            pass
        elif debug_option == 'r':
            file_id = enter_file_id()
        elif debug_option == 'q':
            pass
        else:
            logging.warning(' ===> Choice "' + debug_option + '" is not allowed in the debugger.')
    elif (file_id is None) and (debug_option != 'r'):
        file_id = 1
    elif (file_id is None) and (debug_option == 'r'):
        file_id = enter_file_id()
    else:
        logging.warning(' ===> Choice "' + str(debug_option) + '" is not allowed in the debugger.')

    if file_id < 1:
        file_id = 1
    return file_id
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Method to enter file id
def enter_file_id():
    file_id_user = input('ENTER FILE ID: ')
    if isinstance(file_id_user, str):
        file_id = int(file_id_user)
    else:
        logging.warning(' ===> FILE ID "' + str(file_id_user) + '" format is not supported. Set to NoneType')
        file_id = None
    return file_id
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Method to write file data
def write_file_data(file_path, file_data):
    file_folder, file_name = os.path.split(file_path)
    if not os.path.exists(file_folder):
        os.makedirs(file_folder)
    if os.path.exists(file_path):
        os.remove(file_path)

    with open(file_path, 'w') as file_handle:
        for file_item in file_data:
            file_handle.write("%s\n" % file_item)
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Method to read file obj
def read_var_obj(file_path_dict):
    file_ws = {}
    for file_key, file_path in file_path_dict.items():
        if os.path.exists(file_path):
            file_data = pickle.load(open(file_path, "rb"))
            file_ws[file_key] = file_data
    return file_ws
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Method to save file obj
def save_var_obj(file_path_dict, file_data_dict):

    for (file_key, file_path), (data_key, file_data) in zip(file_path_dict.items(), file_data_dict.items()):

        if os.path.exists(file_path):
            os.remove(file_path)
        folder_name, file_name = os.path.split(file_path)
        make_folder(folder_name)

        with open(file_path, 'wb') as file_handle:
            pickle.dump(file_data, file_handle, protocol=pickle.HIGHEST_PROTOCOL)
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Method to save figure(s)
def save_figures(figure_file_name):

    figures_file = list(figure_file_name.values())
    figures_handle = list(map(plt.figure, plt.get_fignums()))

    for figure_handle, figure_file in zip(figures_handle, figures_file):

        figure_folder, figure_name = os.path.split(figure_file)
        make_folder(figure_folder)

        figure_handle.savefig(figure_file, bbox_inches='tight')
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Method to close figure(s)
def close_figures():
    fig_nums = plt.get_fignums()
    for fig_num in fig_nums:
        plt.close(fig_num)
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Method to show figure
def show_figure(figure_handle):
    figure_handle.show()
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Method to plot file data
def create_figure(file_var_data,
                  figure_title='plot', figure_x_label='axis_x', figure_y_label='axis_y',
                  figure_cmap=None):

    figure_handle = plt.figure()  # create a figure object
    ax = figure_handle.add_subplot(1, 1, 1)  # create an axes object in the figure
    if figure_cmap is None:
        image = ax.imshow(file_var_data, interpolation=None)
    else:
        image = ax.imshow(file_var_data, cmap='gist_earth', interpolation=None)
    ax.set_ylabel(figure_y_label)
    ax.set_xlabel(figure_x_label)
    ax.set_title(figure_title, fontsize=10)  # set the font size via a keyword argument
    figure_handle.colorbar(image)  # note that color_bar is a method of the figure, not the axes

    return figure_handle
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Method to organize variable information
def organize_var_info(file_id, file_time=None, file_stats=None):

    string_id = str(file_id)

    file_var_min, file_var_max, file_var_avg, file_var_time = '-9999.0', '-9999.0', '-9999.0', 'NA'
    if file_stats is not None:
        if 'var_min' in list(file_stats.keys()):
            # file_var_min = str(file_stats['var_min'])
            file_var_min = '{:0.3f}'.format(file_stats['var_min'])
        if 'var_max' in list(file_stats.keys()):
            # file_var_max = str(file_stats['var_max'])
            file_var_max = '{:0.3f}'.format(file_stats['var_max'])
        if 'var_avg' in list(file_stats.keys()):
            # file_var_avg = str(file_stats['var_avg'])
            file_var_avg = '{:0.3f}'.format(file_stats['var_avg'])

    if file_time is not None:
        file_var_time = deepcopy(file_time)

    var_info = ' variable info \n' + \
               ' id: "' + string_id + '" - time: "' + file_var_time + '" \n ' + \
               ' min: "' + file_var_min + '" max: "' + file_var_max + '" avg: "' + file_var_avg + '"'

    return var_info
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Method to compute var data
def compute_var_data(file_var_data, file_no_data=-9999.0):

    file_var_data[file_var_data == file_no_data] = np.nan
    file_var_max = np.nanmax(file_var_data)
    file_var_min = np.nanmin(file_var_data)
    file_var_avg = np.nanmean(file_var_data)

    file_var_stats = {'var_min': file_var_min, 'var_max': file_var_max, 'var_avg': file_var_avg}

    return file_var_stats
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Method to get file data
def get_file_data(file_name, file_delimiter=',', file_header=None, file_no_data=-9999,
                  file_name_idx_cols='idx_cols', file_name_idx_rows='idx_rows', file_name_data='data',
                  file_view='geographic'):

    file_columns = [file_name_idx_rows, file_name_idx_cols, file_name_data]

    file_dframe = pd.read_table(
        file_name, delimiter=file_delimiter, header=file_header, names=file_columns)

    file_cols_n = int(np.nanmax(file_dframe[file_name_idx_cols].values))
    file_rows_n = int(np.nanmax(file_dframe[file_name_idx_rows].values))
    file_var_1d = file_dframe[file_name_data].values.astype(np.float)
    file_var_2d = np.reshape(file_var_1d, (file_cols_n, file_rows_n))

    file_var_2d[file_var_2d == file_no_data] = np.nan

    file_rows_min, file_cols_min, file_step = 0, 0, 1
    file_rows_max, file_cols_max = deepcopy(file_rows_n), deepcopy(file_cols_n)
    file_rows_1d = np.arange(file_rows_min, file_rows_max, file_step)
    file_cols_1d = np.arange(file_cols_min, file_cols_max, file_step)

    file_rows_2d, file_cols_2d = np.meshgrid(file_rows_1d, file_cols_1d)

    if file_view == 'fortran':
        file_var_data = np.flipud(file_var_2d)
    elif file_view == 'geographic':
        file_var_data = np.flipud(np.transpose(file_var_2d))
        file_rows_2d = np.flipud(np.transpose(file_rows_2d))
        file_cols_2d = np.flipud(np.transpose(file_cols_2d))

    else:
        logging.error(' ===> File view "' + file_view + '" is not supported')
        raise NotImplementedError('File view name is not allowed')

    # debug
    # plt.figure()
    # plt.imshow(file_var_data)
    # plt.colorbar()
    # plt.show()

    return file_var_data, file_rows_2d, file_cols_2d
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Method to print menu
def print_menu(menu_options):
    for key in menu_options.keys():
        menu_string = str(key) + ' -- ' + str(menu_options[key])
        print(menu_string)
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Method to make folder
def make_folder(folder_name):
    if not os.path.exists(folder_name):
        os.makedirs(folder_name)
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Method to get script argument(s)
def get_args():
    parser_handle = ArgumentParser()
    parser_handle.add_argument('-path_data', action="store", dest="path_data")
    parser_values = parser_handle.parse_args()

    alg_path_data = os.path.split(os.path.abspath(__file__))[0]
    if parser_values.path_data:
        alg_path_data = parser_values.path_data

    return alg_path_data
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Call script from external library
if __name__ == '__main__':
    main()
# ----------------------------------------------------------------------------

