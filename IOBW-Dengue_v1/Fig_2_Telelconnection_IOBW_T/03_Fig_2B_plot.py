# In[1]: Fig.2B
    
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="ticks")

# Initialize the figure with a logarithmic x axis
f, ax = plt.subplots(figsize=(8, 12))

df = pd.read_csv("02_Fig_2B_IOBW_T.csv")

g = sns.lmplot(
    data = df, x="spring_IOBW_detrend", y="summer_T_detrend", hue="Hemisphere" ,
    height=5
)

g.ax.set_xlabel("Sping IOBW index (detrended, standardised)",fontsize=12)
g.ax.set_ylabel("Summer Temperature (detrended, standardised)",fontsize=12)
g.ax.tick_params(axis='y',labelsize=12)
g.ax.tick_params(axis='x',labelsize=12)
# Please modify the path
g.savefig("Fig_2B.pdf")

