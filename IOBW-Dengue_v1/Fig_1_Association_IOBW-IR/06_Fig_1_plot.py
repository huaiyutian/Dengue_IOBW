# In[1]: Fig. 1B
    
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="ticks")

f, ax = plt.subplots(figsize=(8, 12))

# Please modify the path
df = pd.read_csv("02_Fig_1B_IOBW_monthly-IR.csv")

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
g.savefig("Fig_1B.pdf")


# In[2]: Fig. 1C
    
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="ticks")

f, ax = plt.subplots(figsize=(8, 12))

# Please modify the path
df = pd.read_csv("03_Fig_1C_IOBW_annual-IR.csv")

g = sns.lmplot(
    data = df, x="IOBW_spring_detrend", y="IR_detrend", hue="Hemisphere" ,
    height=5
)

g.ax.set_xlabel("IOBW index (detrended, standardised)",fontsize=12)
g.ax.set_ylabel("Dengue incidence (detrended, standardised)",fontsize=12)

# Please modify the path
g.savefig("Fig_1C.pdf")




# In[3]: Fig. 1D
    
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="ticks")

f, ax = plt.subplots(figsize=(8, 12))

# Please modify the path
df = pd.read_csv("04_Fig_1D_30climate-index_IR.csv")

g = sns.lmplot(
    data = df, x="r_IOBW_IR_North", y="r_IOBW_IR_South", 
    height=5
)

g.ax.set_xlabel("Northern Hemisphere",fontsize=12)
g.ax.set_ylabel("Southern Hemisphere",fontsize=12)
g.ax.tick_params(axis='y',labelsize=12)
g.ax.tick_params(axis='x',labelsize=12)
g.ax.set_xticks(ticks=[-0.6,-0.3,0,0.3,0.6])
g.ax.set_yticks(ticks=[-0.6,-0.3,0,0.3,0.6])

# Please modify the path
g.savefig("Fig_1D.pdf")

