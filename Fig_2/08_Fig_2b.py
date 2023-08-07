# In[1]: Fig.2b
    
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="ticks")

# Initialize the figure with a logarithmic x axis
f, ax = plt.subplots(figsize=(8, 12))

df = pd.read_csv("G:/paper2/7_code/main_fig_code/Fig_2/00_Fig_2b_scatter.csv")

g = sns.lmplot(
    data = df, x="IOBW", y="T_detrend", hue="Hemisphere" ,
    height=5
)

g.ax.set_xlabel("IOBW index (detrended, standardised)",fontsize=12)
g.ax.set_ylabel("Temperature (detrended, standardised)",fontsize=12)
g.ax.tick_params(axis='y',labelsize=12)
g.ax.tick_params(axis='x',labelsize=12)
# Please modify the path
g.savefig("G:/paper2/7_code/main_fig_code/Fig_2/Fig_2b.pdf")

