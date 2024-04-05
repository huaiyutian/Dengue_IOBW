# In[1]: Fig. 4b
    
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="ticks")

f, ax = plt.subplots(figsize=(8, 12))

# Please modify the path
df = pd.read_csv("02_output/01_Fig_4B_N-S_obs-sim.csv")

g = sns.lmplot(
    data = df, x="Month", y="Cases",
    hue="facet" , 
    scatter = False, palette="Set1",truncate=False, order=3
)

g.ax.set_xlabel("Month",fontsize=12)
g.ax.set_ylabel("Dengue incidence (detrended, standardised)",fontsize=12)
g.ax.tick_params(axis='y',labelsize=12)
g.ax.tick_params(axis='x',labelsize=12)
g.ax.set_xticks(ticks=[1,2,3,4,5,6,7,8,9,10,11,12])
# Please modify the path
g.savefig("02_output/Fig_4b.pdf", bbox_inches='tight')
