# In[1]: Fig. 4c_1
from string import ascii_letters
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="white")
sns.set(font_scale=2.5)
df = pd.read_csv("02_output/02_Fig_4C_1_obs_std.csv",index_col=0)
# Set up the matplotlib figure
f, ax = plt.subplots(figsize=(40, 22))
# Generate a custom diverging colormap
cmap = sns.diverging_palette(240, 10, as_cmap=True)
#cmap = sns.color_palette("vlag", as_cmap=True)
# Draw the heatmap with the mask and correct aspect ratio
g= sns.heatmap(df,  cmap="RdBu", vmax=7 ,vmin=-7  , square=True, cbar_kws={"shrink": .5})
fig = g.get_figure()
fig.savefig("02_output/Fig_4c_1.pdf",dpi=300)

cmap = sns.diverging_palette(230, 20, as_cmap=True)

# In[2]: Fig. 4c_2
from string import ascii_letters
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="white")
sns.set(font_scale=2.5)
df = pd.read_csv("02_output/03_Fig_4C_2_pre_std_iobw.csv",index_col=0)
# Set up the matplotlib figure
f, ax = plt.subplots(figsize=(40, 22))
# Generate a custom diverging colormap
cmap = sns.diverging_palette(240, 10, as_cmap=True)
#cmap = sns.color_palette("vlag", as_cmap=True)
# Draw the heatmap with the mask and correct aspect ratio
g= sns.heatmap(df,  cmap="RdBu", vmax=7 ,vmin=-7  , square=True, cbar_kws={"shrink": .5})
fig = g.get_figure()
fig.savefig("02_output/Fig_4c_2.pdf",dpi=300)

cmap = sns.diverging_palette(230, 20, as_cmap=True)

# In[3]: Fig. 4c_3
from string import ascii_letters
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_theme(style="white")
sns.set(font_scale=2.5)
df = pd.read_csv("02_output/04_Fig_4C_3_pre_std_no_iobw.csv",index_col=0)
# Set up the matplotlib figure
f, ax = plt.subplots(figsize=(40, 22))
# Generate a custom diverging colormap
cmap = sns.diverging_palette(240, 10, as_cmap=True)
#cmap = sns.color_palette("vlag", as_cmap=True)
# Draw the heatmap with the mask and correct aspect ratio
g= sns.heatmap(df,  cmap="RdBu", vmax=7 ,vmin=-7  , square=True, cbar_kws={"shrink": .5})
fig = g.get_figure()
fig.savefig("02_output/Fig_4c_3.pdf",dpi=300)

cmap = sns.diverging_palette(230, 20, as_cmap=True)

