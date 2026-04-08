# Core Network Visualization
install.packages("igraph")       # The engine for network analysis and basic plotting [cite: 29]
install.packages("ggraph")       # The 'ggplot2' equivalent for networks (cleaner, more professional)
install.packages("visNetwork")   # For interactive, searchable HTML-based network maps

# General Plotting & Aesthetics
install.packages("ggplot2")      # Industry standard for charts and data distributions
install.packages("RColorBrewer") # High-quality color palettes for distinguishing communities [cite: 32, 41]
install.packages("gridExtra")    # To arrange multiple plots (e.g., Degree vs. Betweenness)


# 1. Load the Dataset
# out.actor-collaboration contains: from_node, to_node, weight [cite: 49]
# We'll read it and treat it as an undirected network [cite: 24]
raw_data <- read.table("out.actor-collaboration", header = FALSE)
colnames(raw_data) <- c("from", "to", "weight")

# 2. Data Subsampling for Feasibility 
# The full network is too large for most local machines.
# We will take a sample of the first 50,000 edges to demonstrate the method.
data_sample <- raw_data[1:50000, ]

# 3. Construct the Graph [cite: 30]
# We use the 'weight' column to represent repeated collaborations [cite: 20, 48]
g <- graph_from_data_frame(d = data_sample, directed = FALSE)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

# 4. Community Detection [cite: 32, 33]
# Using the Louvain method to find collaboration groups
communities <- cluster_louvain(g)
V(g)$community <- communities$membership

# 5. Centrality Analysis [cite: 34]
# Degree Centrality: Identify actors with many collaborators [cite: 35]
V(g)$degree <- degree(g)

# Betweenness Centrality: Identify brokers/bridges [cite: 36, 37]
# This measures how often a node acts as a bridge between others
V(g)$betweenness <- betweenness(g, normalized = TRUE)

# 6. Answering Research Questions
# RQ1 & RQ2: Identifying Broker Actors [cite: 14, 16]
# We look for nodes with high betweenness but potentially lower degree
top_brokers <- head(sort(V(g)$betweenness, decreasing = TRUE), 10)
print("Top Potential Broker IDs (High Betweenness):")
print(top_brokers)

# RQ3: Community Structure [cite: 17, 41]
print(paste("Number of communities detected:", length(unique(V(g)$community))))
print(paste("Modularity score:", modularity(communities)))

# 7. Visualization (Subset for clarity) [cite: 30, 31]
# Visualizing a small neighborhood to see the clusters and bridges
sub_g <- induction_subgraph(g, V(g)[1:100])
plot(sub_g, 
     vertex.color = V(sub_g)$community, 
     vertex.size = log(V(sub_g)$degree + 1) * 3,
     main = "Actor Collaboration Subnetwork: Communities and Bridges")