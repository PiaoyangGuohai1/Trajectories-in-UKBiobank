import pandas as pd
import seaborn as sns
import numpy as np
import networkx as nx
import matplotlib.pyplot as plt
from scipy.sparse import csr_matrix

def matrix_power(m, power):
    result = m.copy()
    for _ in range(power - 1):
        result = result.dot(m)
    return result

def markov_clustering(matrix, expansion=2, inflation=2, loop_value=1.0, iterations=100):
    m = csr_matrix(matrix)
    
    m.setdiag(loop_value)
    
    row_sum = m.sum(axis=1).A1
    row_sum[row_sum == 0] = 1
    m = m.multiply(1 / row_sum).tocsr()
    
    clustering = np.full(m.shape[0], -1)
    
    for _ in range(iterations):
        m = matrix_power(m, expansion)
        m.data = np.power(m.data, inflation)
        row_sum = m.sum(axis=1).A1
        row_sum[row_sum == 0] = 1
        m = m.multiply(1 / row_sum).tocsr()
        
    attractors = m.nonzero()[1]
    clustering = {}
    for idx, cluster_id in enumerate(attractors):
        if cluster_id not in clustering:
            clustering[cluster_id] = []
        clustering[cluster_id].append(idx)
    
    labels = np.zeros(m.shape[0], dtype=int)
    for cluster_id, nodes in clustering.items():
        for node in nodes:
            labels[node] = cluster_id
    
    return labels

dt_jaccard = pd.read_csv("data/trajectories_cluster/dt_jaccard.csv", index_col=0)
jaccard_matrix = dt_jaccard.values

expansion_values = [2]
inflation_values = [1.2, 1.4, 1.6, 1.8, 2.0]

results = {}

for expansion in expansion_values:
    for inflation in inflation_values:
        clustering_result = markov_clustering(jaccard_matrix, expansion=expansion, inflation=inflation)
        
        key = f'expansion_{expansion}_inflation_{inflation}'
        results[key] = clustering_result

data = []
for key, clustering in results.items():
    expansion = int(key.split('_')[1])
    inflation = float(key.split('_')[3])
    clusters_count = len(np.unique(clustering))
    data.append({'expansion': expansion, 'inflation': inflation, 'clusters': clusters_count})

df = pd.DataFrame(data)

pivot_table = df.pivot(index='inflation', columns='expansion', values='clusters')

plt.figure(figsize=(8, 6))
sns.heatmap(pivot_table, annot=True, fmt='g', cmap='viridis', cbar_kws={'label': 'Number of Clusters'})
plt.title('Number of Clusters by Expansion and Inflation Parameters')
plt.xlabel('Expansion')
plt.ylabel('Inflation')
plt.xticks(rotation=45)
plt.yticks(rotation=0)
plt.savefig('plot/clusters_parameter_heatmap.png', dpi=300, bbox_inches='tight')
plt.show()

    
    
clustering_result = markov_clustering(jaccard_matrix, expansion=2, inflation = 1.6)
clustering_result


unique, counts = np.unique(clustering_result, return_counts=True)
cluster_counts = dict(zip(unique, counts))
print("Cluster Counts:")
for cluster, count in cluster_counts.items():
    print(f"Cluster {cluster}: {count} items")


cluster_counts_df = pd.DataFrame(list(cluster_counts.items()), columns=['Cluster', 'Count'])
plt.figure(figsize=(10, 6))
plt.bar(cluster_counts_df['Cluster'], cluster_counts_df['Count'], color='skyblue')
plt.xlabel('Cluster')
plt.ylabel('Count')
plt.title('Number of Items in Each Cluster')
plt.xticks(rotation=45)
plt.grid(axis='y')
plt.show()

clustering_result_df = pd.DataFrame(clustering_result, columns=['Cluster'])

clustering_result_df.to_csv("data/trajectories_cluster/clustering_results.csv", index=False)
