# In[1]: Fig. 4b
    
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="ticks")

f, ax = plt.subplots(figsize=(8, 12))

# Please modify the path
df = pd.read_csv("../01_data/05_N-S_obs-sim.csv")

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
g.savefig("../02_output figure/Fig_4b.pdf", bbox_inches='tight')

# In[2]: Fig. 4c_1
from string import ascii_letters
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="white")
sns.set(font_scale=2.5)
df = pd.read_csv("../01_data/06-1_obs_std.csv",index_col=0)
# Set up the matplotlib figure
f, ax = plt.subplots(figsize=(40, 22))
# Generate a custom diverging colormap
cmap = sns.diverging_palette(240, 10, as_cmap=True)
#cmap = sns.color_palette("vlag", as_cmap=True)
# Draw the heatmap with the mask and correct aspect ratio
g= sns.heatmap(df,  cmap="RdBu", vmax=7 ,vmin=-7  , square=True, cbar_kws={"shrink": .5})
fig = g.get_figure()
fig.savefig("../02_output figure/Fig_4c_1.pdf",dpi=300)

cmap = sns.diverging_palette(230, 20, as_cmap=True)

# In[3]: Fig. 4c_2
from string import ascii_letters
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="white")
sns.set(font_scale=2.5)
df = pd.read_csv("../01_data/06-2_pre_std_iobw.csv",index_col=0)
# Set up the matplotlib figure
f, ax = plt.subplots(figsize=(40, 22))
# Generate a custom diverging colormap
cmap = sns.diverging_palette(240, 10, as_cmap=True)
#cmap = sns.color_palette("vlag", as_cmap=True)
# Draw the heatmap with the mask and correct aspect ratio
g= sns.heatmap(df,  cmap="RdBu", vmax=7 ,vmin=-7  , square=True, cbar_kws={"shrink": .5})
fig = g.get_figure()
fig.savefig("../02_output fugure/Fig_4c_2.pdf",dpi=300)

cmap = sns.diverging_palette(230, 20, as_cmap=True)

# In[4]: Fig. 4c_3
from string import ascii_letters
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="white")
sns.set(font_scale=2.5)
df = pd.read_csv("../01_data/06-3_pre_std_no_iobw.csv",index_col=0)
# Set up the matplotlib figure
f, ax = plt.subplots(figsize=(40, 22))
# Generate a custom diverging colormap
cmap = sns.diverging_palette(240, 10, as_cmap=True)
#cmap = sns.color_palette("vlag", as_cmap=True)
# Draw the heatmap with the mask and correct aspect ratio
g= sns.heatmap(df,  cmap="RdBu", vmax=7 ,vmin=-7  , square=True, cbar_kws={"shrink": .5})
fig = g.get_figure()
fig.savefig("../02_output figure/Fig_4c_3.pdf",dpi=300)

cmap = sns.diverging_palette(230, 20, as_cmap=True)

