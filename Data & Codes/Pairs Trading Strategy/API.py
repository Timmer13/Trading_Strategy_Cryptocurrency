%load_ext autoreload
%autoreload 2

import numpy as np
import pandas as pd
from joblib import Parallel, delayed
import operator
import matplotlib.pyplot as plt

from crycompare import *

from ClusterLib.clusterlib import *
from ClusterLib.distlib import *

%matplotlib inline

p = Price()

coinList = p.coinList()
coins = sorted(list( coinList['Data'].keys() ))

h = History()

df_dict = {}
for coin in coins:
    histo = h.histoDay(coin,'USD',allData=True)
    if histo['Data']:
        df_histo = pd.DataFrame(histo['Data'])
        df_histo['time'] = pd.to_datetime(df_histo['time'],unit='s')
        df_histo.index = df_histo['time']
        del df_histo['time']
        del df_histo['volumefrom']
        del df_histo['volumeto']
        
        df_dict[coin] = df_histo

crypto_histo = pd.concat(df_dict.values(), axis=1, keys=df_dict.keys())
histo_coins = [elem for elem in crypto_histo.columns.levels[0] if not elem == 'MYC']


histo_length = {}
for coin in histo_coins:
    histo_length[coin] = np.sum( ~np.isnan(crypto_histo[coin]['close'].values) )
    
sorted_length = sorted(histo_length.items(), key=operator.itemgetter(1),reverse=True)


sub_coins = [sorted_length[i][0] for i in range(300)]

sub_crypto_histo = crypto_histo[sub_coins]
sub_crypto_histo.tail()


N = len(sub_coins)
recent_histo = sub_crypto_histo[-1000:]


returns_dict = {}
for coin in sub_coins:
    coin_histo = recent_histo[coin]
    coin_returns = pd.DataFrame(np.diff(np.log(coin_histo.get_values()),axis=0))
    returns_dict[coin] = coin_returns

recent_returns = pd.concat(returns_dict.values(),axis=1,keys=returns_dict.keys())
recent_returns.index = recent_histo.index[1:]


recent_returns = recent_returns.replace([np.inf, -np.inf], np.nan)
recent_returns = recent_returns.fillna(value=0)

recent_returns.isnull().values.any()

False

plt.figure(figsize=(40,10))
for coin in sub_coins:
    plt.plot(recent_returns[coin])
#plt.legend(sub_coins,loc='upper left')
plt.xlabel('time',fontsize=18)
plt.ylabel('returns \'X/USD\'',fontsize=18)
plt.show()


dist_mat = np.zeros((N,N))
a,b = np.triu_indices(N,k=1)
dist_mat[a,b] = Parallel(n_jobs=-2,verbose=1) (delayed(distcorr)(recent_returns[sub_coins[a[i]]],recent_returns[sub_coins[b[i]]]) for i in range(len(a)))
dist_mat[b,a] = dist_mat[a,b]


[Parallel(n_jobs=-2)]: Done 39186 tasks      | elapsed: 19.3min
[Parallel(n_jobs=-2)]: Done 42036 tasks      | elapsed: 20.7min
[Parallel(n_jobs=-2)]: Done 44850 out of 44850 | elapsed: 22.1min finished


seriated_dist_mat, res_order, res_linkage = compute_serial_matrix(dist_mat)
ordered_coins = [sub_coins[res_order[i]] for i in range(len(res_order))]


plt.figure(figsize=(80,80))
plt.pcolormesh(seriated_dist_mat)
#plt.colorbar()
plt.xlim([0,N])
plt.ylim([0,N])
plt.xticks( np.arange(N)+0.5, ordered_coins, rotation=90, fontsize=25 )
plt.yticks( np.arange(N)+0.5, ordered_coins, fontsize=25 )
plt.show()


nb_clusters = 30

cluster_map = pd.DataFrame(scipy.cluster.hierarchy.fcluster(res_linkage,nb_clusters,'maxclust'),index=ordered_coins)
clusters = []
k = 0
for i in range(0,nb_clusters):
    compo = cluster_map[cluster_map[0]==(i+1)].index.values
    clusters.append(compo)
    k = k + len(compo)
   
for cluster in clusters:
    print(cluster)


plt.figure(figsize=(40,10))
alt_coins = ['GRC', 'XTC', 'EAC', 'CINNI', 'RYC', 'COOL', 'MN']
for coin in alt_coins:
    plt.plot(np.log(sub_crypto_histo[coin]['close']))
plt.xlabel('time',fontsize=18)
plt.ylabel('log \'X/USD\'',fontsize=18)
plt.legend(alt_coins,loc='upper left')
plt.show() 
    

