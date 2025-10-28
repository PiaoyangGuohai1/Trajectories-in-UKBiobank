import community

dt_jaccard = pd.read_csv("result/data/04_trajectories_cluster/dt_jaccard.csv", index_col=0)

G = nx.from_pandas_adjacency(dt_jaccard)

np.random.seed(42)
partition = community.best_partition(G, weight='weight')

cluster_dict_louvain = {}
for node, cluster_id in partition.items():
  cluster_dict_louvain.setdefault(cluster_id, []).append(node)

for cluster_id, cluster_nodes in cluster_dict_louvain.items():
  print(f"Cluster {cluster_id + 1}: {cluster_nodes}")


colors = [partition.get(node) for node in G.nodes()]

plt.figure(figsize=(12, 12))
pos = nx.spring_layout(G, k=0.15)
nx.draw_networkx_nodes(G, pos, node_color=colors, cmap=plt.cm.Set3, node_size=200)
nx.draw_networkx_edges(G, pos, alpha=0.5)
nx.draw_networkx_labels(G, pos, font_size=8)
plt.title('Community Detection Clustering of Diagnoses')
plt.axis('off')
plt.show()
