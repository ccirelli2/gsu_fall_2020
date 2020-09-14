# -*- coding: utf-8 -*-
"""
This dataset was obtained from the StatLib repository.\nhttp://lib.stat.cmu.edu/datasets/\n\nThe target variable is the median house value for California districts.\n\nThis dataset was derived from the 1990 U.S. census, using one row per census\nblock group. A block group is the smallest geographical unit for which the U.S.\nCensus Bureau publishes sample data (a block group typically has a population\nof 600 to 3,000 people).\n\nIt can be downloaded/loaded using the\n:func:`sklearn.datasets.fetch_california_housing` function.\n\n.. topic:: References\n\n    - Pace, R. Kelley and Ronald Barry, Sparse Spatial Autoregressions,\n      Statistics and Probability Letters, 33 (1997) 291-297\n'}
"""

from sklearn.datasets import fetch_california_housing
import numpy as np

data = fetch_california_housing()
dir_output = r'C:\Users\chris.cirelli\Desktop\repositories\gsu_fall_2020\Visualizations\hw1'

df = pd.DataFrame(data= np.c_[data['data'], data['target']],
                     columns= data['feature_names'] + ['target'])


#df.to_excel(dir_output + '/' + 'california_housing_data.xlsx')


df_head = df.head(1).transpose()
df_head.to_excel(dir_output + '/' + 'data_dict.xlsx')
