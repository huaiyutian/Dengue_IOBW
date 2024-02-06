# In[1]: Fig. 1b
    
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="ticks")

f, ax = plt.subplots(figsize=(8, 12))

# Please modify the path
df = pd.read_csv("G:/paper2/7_code/main_fig_code/Fig_1/Fig_1b.csv")

g = sns.lmplot(
    data = df, x="Month", y="IR_detrend",
    hue="facet" , 
    scatter = False, palette="Set1",truncate=False, order=3
)

g.ax.set_xlabel("Month",fontsize=12)
g.ax.set_ylabel("Dengue incidence (detrended, standardised)",fontsize=12)
g.ax.tick_params(axis='y',labelsize=12)
g.ax.tick_params(axis='x',labelsize=12)
g.ax.set_xticks(ticks=[1,2,3,4,5,6,7,8,9,10,11,12])

# Please modify the path
g.savefig("G:/paper2/7_code/main_fig_code/Fig_1/Fig_1b.pdf")


# In[2]: Fig. 1c
    
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="ticks")

f, ax = plt.subplots(figsize=(8, 12))

# Please modify the path
df = pd.read_csv("G:/paper2/7_code/main_fig_code/Fig_1/Fig_1c_scatter.csv")

g = sns.lmplot(
    data = df, x="IOBW", y="IR_detrend", hue="Hemisphere" ,
    height=5
)

g.ax.set_xlabel("IOBW index (detrended, standardised)",fontsize=12)
g.ax.set_ylabel("Dengue incidence (detrended, standardised)",fontsize=12)

# Please modify the path
g.savefig("G:/paper2/7_code/main_fig_code/Fig_1/Fig_1c.pdf")




# In[3]: Fig. 1d
    
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="ticks")

f, ax = plt.subplots(figsize=(8, 12))

# Please modify the path
df = pd.read_csv("G:/paper2/7_code/main_fig_code/Fig_1/Fig_1d_scatter.csv")

g = sns.lmplot(
    data = df, x="cor_iobw_ir_N", y="cor_iobw_ir_S", 
    height=5
)

g.ax.set_xlabel("Northern Hemisphere",fontsize=12)
g.ax.set_ylabel("Southern Hemisphere",fontsize=12)
g.ax.tick_params(axis='y',labelsize=12)
g.ax.tick_params(axis='x',labelsize=12)
g.ax.set_xticks(ticks=[-0.6,-0.3,0,0.3,0.6])
g.ax.set_yticks(ticks=[-0.6,-0.3,0,0.3,0.6])

# Please modify the path
g.savefig("G:/paper2/7_code/main_fig_code/Fig_1/Fig_1d.pdf")

