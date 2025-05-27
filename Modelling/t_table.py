#%% Create the treshold table
import pandas as pd
import numpy as np
#%% Load the data
df = pd.read_csv('thresholds.csv')
#%% Create the tableù
# number of disctinct values

s1_rows = df['S1.point'].nunique()
s2_rows = df['S2.point'].nunique()
s3_rows = df['S3.point'].nunique()
# replace values == 99 to 12 in S1.point, 14 in S2.point and 7 in S3.point
df['S1.point'] = df['S1.point'].replace(99, 12)
df['S2.point'] = df['S2.point'].replace(99, 14)
df['S3.point'] = df['S3.point'].replace(99, 7)

sigma_ = np.zeros((s1_rows, s2_rows, s3_rows))
lambda_ = np.zeros((s1_rows, s2_rows, s3_rows))
gamma_ = np.zeros((s1_rows, s2_rows, s3_rows))
#%%
    
for k in range(s3_rows):
    for j in range(s2_rows):
        for i in range(s1_rows):
            sigma_[i,j,k] = (df['sigma'][df['S1.point'] == i][df['S2.point'] == j][df['S3.point'] == k ].max() - df['sigma'][df['S1.point'] == i][df['S2.point'] == j][df['S3.point'] == k ].min()) / 2 + df['sigma'][df['S1.point'] == i][df['S2.point'] == j][df['S3.point'] == k ].min()
            lambda_[i,j,k] = (df['lambda'][df['S1.point'] == i][df['S2.point'] == j][df['S3.point'] == k ].max() - df['lambda'][df['S1.point'] == i][df['S2.point'] == j][df['S3.point'] == k ].min()) / 2 + df['lambda'][df['S1.point'] == i][df['S2.point'] == j][df['S3.point'] == k ].min()
            gamma_[i,j,k] = (df['gamma'][df['S1.point'] == i][df['S2.point'] == j][df['S3.point'] == k ].max() - df['gamma'][df['S1.point'] == i][df['S2.point'] == j][df['S3.point'] == k ].min()) / 2 + df['gamma'][df['S1.point'] == i][df['S2.point'] == j][df['S3.point'] == k ].min()


# %%
# Create a new dataframe with the results
t_table = pd.DataFrame({
    'S1.point': np.repeat(np.arange(s1_rows), s2_rows * s3_rows),
    'S2.point': np.tile(np.repeat(np.arange(s2_rows), s3_rows), s1_rows),
    'S3.point': np.tile(np.arange(s3_rows), s1_rows * s2_rows),
    'sigma': sigma_.flatten(),
    'lambda': lambda_.flatten(),
    'gamma': gamma_.flatten()
})
# %%

# %% Save the t_table to a csv file
t_table.to_csv('t_table.csv')

# %%
