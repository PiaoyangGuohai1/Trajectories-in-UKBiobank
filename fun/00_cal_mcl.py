import pandas as pd
import numpy as np
import markov_clustering as mc
import networkx as nx
import matplotlib.pyplot as plt
from scipy.sparse import csr_matrix

dt_jaccard = pd.read_csv("result/data/04_trajectories_cluster/dt_jaccard.csv", index_col=0)

jaccard_matrix = dt_jaccard.values

jaccard_matrix = (jaccard_matrix + jaccard_matrix.T) / 2

matrix = csr_matrix(jaccard_matrix)

np.random.seed(42)
result = mc.run_mcl(jaccard_matrix, inflation=1.2)

clusters = mc.get_clusters(result)

diagnoses = dt_jaccard.index.tolist()

cluster_dict_mcl = {}
for cluster_id, cluster in enumerate(clusters):
    cluster_diagnoses = [diagnoses[i] for i in cluster]
    cluster_dict_mcl[f'Cluster_{cluster_id+1}'] = cluster_diagnoses

for cluster_name, cluster_diagnoses in cluster_dict_mcl.items():
    print(f"{cluster_name}: {cluster_diagnoses}")


G = nx.from_scipy_sparse_array(matrix)


mapping = dict(zip(range(len(diagnoses)), diagnoses))
G = nx.relabel_nodes(G, mapping)

cluster_membership = {}
for cluster_id, cluster in enumerate(clusters):
    for node_id in cluster:
        node_name = diagnoses[node_id]
        cluster_membership[node_name] = cluster_id

colors = [cluster_membership.get(node) for node in G.nodes()]

plt.figure(figsize=(12, 12))
pos = nx.spring_layout(G, k=0.15, seed=42)
nx.draw_networkx_nodes(G, pos, node_color=colors, cmap=plt.cm.Set3, node_size=200)
nx.draw_networkx_edges(G, pos, alpha=0.5)
nx.draw_networkx_labels(G, pos, font_size=8)
plt.title('Markov Clustering of Diagnoses')
plt.axis('off')
plt.show()
