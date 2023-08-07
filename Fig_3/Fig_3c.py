# In[1]: Fig_3c
    
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="ticks")

f, ax = plt.subplots(figsize=(8, 12))

# Please modify the path
df = pd.read_csv("G:/paper2/7_code/main_fig_code/Fig_3/data/06_mEDM_best_delta.csv")

g = sns.catplot(
     data = df, x="Driver", y="delta",
    capsize=.1, 
    kind="point"
)

g.ax.set_xlabel("Driver",fontsize=12)
g.ax.set_ylabel("Effect size",fontsize=12)
g.ax.tick_params(axis='y',labelsize=12)
g.ax.tick_params(axis='x',labelsize=12)

# Please modify the path
g.savefig("G:/paper2/7_code/main_fig_code/Fig_3/output/Fig_3c.pdf")

