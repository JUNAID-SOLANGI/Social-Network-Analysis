# Importing R-packages
library(readxl)
library(igraph)

#loading Dataset 

data <-read_excel('C:/Users/JUNAID/Documents/EDGES.xlsx', sheet = 'Sheet1')

#dataset

head(data)

#Creating indirected two mode Graph

g <- graph_from_data_frame(data[, c("source", "target")], directed = FALSE)

# adding Weights 
# Add edge weights
E(g)$weight <- data$weight


# Mark nodes as faculty (TRUE) or batch (FALSE)
V(g)$type <- !(V(g)$name %in% unique(data$target))


install.packages("visNetwork")
library(visNetwork)

# Convert to data frames
nodes <- data.frame(id = V(g)$name, label = V(g)$name, group = ifelse(V(g)$type, "Faculty", "Batch"))
edges <- as_data_frame(g, what = "edges")

# Plot
visNetwork(nodes, edges) %>% 
  visEdges(smooth = FALSE) %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

install.packages("dplyr") 
library(dplyr)

edges <- as_data_frame(g, what = "edges") %>%
  mutate(value = weight,
         title = paste("Weight:", weight))

# Insights
# Faculty involvement per Batch
library(readxl)
library(dplyr)
library(ggplot2)

# Load data (replace path with your file path)
data <- read_excel('C:/Users/JUNAID/Documents/EDGES.xlsx', sheet = 'Sheet1')

# Total weight per faculty (source)
faculty_weights <- data %>%
  group_by(source) %>%
  summarise(total_weight = sum(weight)) %>%
  arrange(desc(total_weight))

# Plot faculty total weights
ggplot(faculty_weights, aes(x = reorder(source, total_weight), y = total_weight)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  coord_flip() + # horizontal bars for readability
  labs(title = 'Total Teaching Load per Faculty',
       x = 'Faculty',
       y = 'Sum of Weights') +
  theme_minimal()

# Total weight per batch (target)
batch_weights <- data %>%
  group_by(target) %>%
  summarise(total_weight = sum(weight)) %>%
  arrange(desc(total_weight))

# Plot batch total weights
ggplot(batch_weights, aes(x = reorder(target, total_weight), y = total_weight)) +
  geom_bar(stat = 'identity', fill = 'coral') +
  coord_flip() +
  labs(title = 'Total Faculty Involvement per Batch',
       x = 'Batch',
       y = 'Sum of Weights') +
  theme_minimal()
##


# Calculate total teaching load per faculty
faculty_load <- data %>%
  group_by(source) %>%
  summarise(total_weight = sum(weight)) %>%
  arrange(desc(total_weight))

# 1. Bar graph of total teaching load per faculty
ggplot(faculty_load, aes(x = reorder(source, total_weight), y = total_weight)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  coord_flip() +
  labs(title = 'Faculty Workload: Total Teaching Load',
       x = 'Faculty',
       y = 'Total Weight (Load)') +
  theme_minimal()

# 2. Histogram of faculty workload distribution
ggplot(faculty_load, aes(x = total_weight)) +
  geom_histogram(binwidth = 1, fill = 'skyblue', color = 'black') +
  labs(title = 'Distribution of Faculty Workloads',
       x = 'Total Teaching Load',
       y = 'Number of Faculty') +
  theme_minimal()

# 3. Boxplot for faculty workload to spot outliers
ggplot(faculty_load, aes(y = total_weight)) +
  geom_boxplot(fill = 'lightgreen') +
  labs(title = 'Boxplot of Faculty Workloads',
       y = 'Total Teaching Load') +
  theme_minimal()

###Faculty coverage


# 1. Count how many faculty are assigned to each batch
batch_coverage <- data %>%
  group_by(target) %>%
  summarise(faculty_count = n_distinct(source)) %>%
  arrange(desc(faculty_count))

# 2. Bar plot: Faculty coverage per batch
ggplot(batch_coverage, aes(x = reorder(target, faculty_count), y = faculty_count)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Faculty Coverage per Batch",
       x = "Batch",
       y = "Number of Faculty") +
  theme_minimal()

##Network Matrics
# Create the graph (undirected)
g <- graph_from_data_frame(data[, c("source", "target")], directed = FALSE)

# Add edge weights
E(g)$weight <- data$weight

# Set node types: TRUE for faculty, FALSE for batch
V(g)$type <- !(V(g)$name %in% unique(data$target))

# 1. Degree Centrality (unweighted)
deg_centrality <- degree(g, mode = "all")

# 2. Weighted Degree Centrality
wtd_degree <- strength(g, mode = "all", weights = E(g)$weight)

# Combine into a dataframe
centrality_df <- data.frame(
  name = V(g)$name,
  type = ifelse(V(g)$type, "Faculty", "Batch"),
  degree = deg_centrality,
  weighted_degree = wtd_degree
)

# View top nodes by weighted degree
centrality_df %>% arrange(desc(weighted_degree)) %>% head()

# 3. Bar plot of weighted degree by faculty
faculty_df <- centrality_df %>% filter(type == "Faculty")

ggplot(faculty_df, aes(x = reorder(name, weighted_degree), y = weighted_degree)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Faculty: Weighted Degree Centrality (Teaching Load)",
       x = "Faculty",
       y = "Weighted Degree") +
  theme_minimal()

# 4. Bar plot of unweighted degree by batch (how many faculty connected)
batch_df <- centrality_df %>% filter(type == "Batch")

ggplot(batch_df, aes(x = reorder(name, degree), y = degree)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Batch: Degree Centrality (Faculty Connections)",
       x = "Batch",
       y = "Degree (Number of Faculty)") +
  theme_minimal()

####
# Create bipartite graph
g <- graph_from_data_frame(data[, c("source", "target")], directed = FALSE)

# Assign edge weights
E(g)$weight <- data$weight

# Assign node types: TRUE = faculty, FALSE = batch
V(g)$type <- !(V(g)$name %in% unique(data$target))

# Project bipartite graph
projections <- bipartite_projection(g, which = "both", multiplicity = TRUE)

faculty_proj <- projections[[1]]  # Faculty–Faculty network
batch_proj   <- projections[[2]]  # Batch–Batch network

# Add weights (number of shared batches/faculty)
E(faculty_proj)$weight <- count_multiple(faculty_proj)
E(batch_proj)$weight <- count_multiple(batch_proj)

# Optional: simplify
faculty_proj <- simplify(faculty_proj, edge.attr.comb = list(weight = "sum"))
batch_proj   <- simplify(batch_proj, edge.attr.comb = list(weight = "sum"))

# Faculty–Faculty plot
plot(faculty_proj,
     layout = layout_with_fr(faculty_proj),
     edge.width = E(faculty_proj)$weight,
     vertex.label.cex = 0.8,
     vertex.color = "lightgreen",
     main = "Faculty–Faculty Network (Shared Batches)")

# Batch–Batch plot
plot(batch_proj,
     layout = layout_with_kk(batch_proj),
     edge.width = E(batch_proj)$weight,
     vertex.label.cex = 0.8,
     vertex.color = "skyblue",
     main = "Batch–Batch Network (Shared Faculty - KK Layout)")

## interactive
library(visNetwork)

# --- Convert batch_proj to nodes and edges ---
batch_nodes <- data.frame(id = V(batch_proj)$name,
                          label = V(batch_proj)$name,
                          group = "Batch")
batch_edges <- get.data.frame(batch_proj, what = "edges")

# Now mutate safely
batch_edges <- batch_edges %>%
  mutate(value = weight,
         title = paste("Shared Faculty:", weight))



# Ensure edges include weight
batch_edges <- get.data.frame(batch_proj, what = "edges") %>%
  mutate(value = weight,
         title = paste("Shared Faculty:", weight))

# Define nodes
batch_nodes <- data.frame(id = V(batch_proj)$name,
                          label = V(batch_proj)$name,
                          group = "Batch")

# Interactive Network
visNetwork(batch_nodes, batch_edges) %>%
  visEdges(smooth = FALSE, scaling = list(min = 1, max = 10)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visPhysics(solver = "forceAtlas2Based",
             forceAtlas2Based = list(gravitationalConstant = -50)) %>%
  visInteraction(navigationButtons = TRUE)

##betweenness
betweenness_vals <- betweenness(g, normalized = TRUE)

## Eigenvector centrality 
eigenvector_vals <- eigen_centrality(g, directed = FALSE, weights = E(g)$weight)$vector

# adding
V(g)$betweenness <- betweenness_vals
V(g)$eigenvector <- eigenvector_vals

# Visualisation
library(RColorBrewer)

# Create color palettes
betweenness_colors <- colorRampPalette(brewer.pal(9, "YlGnBu"))(100)
eigenvector_colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(100)

# Normalize centralities for plotting (0 to 1 scale)
bet_norm <- (betweenness_vals - min(betweenness_vals)) / (max(betweenness_vals) - min(betweenness_vals))
eig_norm <- (eigenvector_vals - min(eigenvector_vals)) / (max(eigenvector_vals) - min(eigenvector_vals))

# Plot Betweenness Centrality
plot(g, 
     vertex.size = 10 + bet_norm * 20,
     vertex.color = betweenness_colors[ceiling(bet_norm * 99) + 1],
     vertex.label.cex = 0.8,
     edge.width = E(g)$weight,
     main = "Faculty-Batch Network: Betweenness Centrality")

# Plot Eigenvector Centrality
plot(g, 
     vertex.size = 10 + eig_norm * 20,
     vertex.color = eigenvector_colors[ceiling(eig_norm * 99) + 1],
     vertex.label.cex = 0.8,
     edge.width = E(g)$weight,
     main = "Faculty-Batch Network: Eigenvector Centrality")
## interctive 
nodes$title <- paste0("<p><b>", nodes$label, "</b><br>",
                      "Betweenness: ", nodes$betweenness, "<br>",
                      "Eigenvector: ", nodes$eigenvector, "</p>")


nodes <- data.frame(id = V(g)$name,
                    label = V(g)$name,
                    group = ifelse(V(g)$type, "Faculty", "Batch"),
                    betweenness = round(betweenness_vals, 2),
                    eigenvector = round(eigenvector_vals, 2),
                    size = 10 + eig_norm * 20)  # size by eigenvector centrality

edges <- get.data.frame(g, what = "edges") %>%
  mutate(value = weight,
         title = paste("Weight:", weight))

nodes$title <- paste0(
  "<b>", nodes$label, "</b><br>",
  "Betweenness: ", round(nodes$betweenness, 2), "<br>",
  "Eigenvector: ", round(nodes$eigenvector, 2))
  
visNetwork(nodes, edges) %>%
    visEdges(smooth = FALSE, scaling = list(min = 1, max = 10)) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visLegend() %>%
    visPhysics(solver = "forceAtlas2Based") %>%
    visInteraction(navigationButtons = TRUE)
nodes$title <- paste0(
  "<b>", nodes$label, "</b><br>",
  "Betweenness: ", round(nodes$betweenness, 2), "<br>",
  "Eigenvector: ", round(nodes$eigenvector, 2)
)


##
# Recalculate centrality measures safely
btw <- betweenness(g, normalized = TRUE)
eig <- eigen_centrality(g, directed = FALSE, weights = E(g)$weight)$vector

# Check lengths
length(btw)
length(eig)
length(V(g))  # All should be same

centrality_df <- data.frame(
  name = V(g)$name,
  group = ifelse(V(g)$type, "Faculty", "Batch"),
  betweenness = btw,
  eigenvector = eig
)



# Betweenness
ggplot(centrality_df, aes(x = reorder(name, betweenness), y = betweenness, fill = group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Betweenness Centrality", x = "Node", y = "Centrality") +
  theme_minimal()

# Eigenvector
ggplot(centrality_df, aes(x = reorder(name, eigenvector), y = eigenvector, fill = group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Eigenvector Centrality", x = "Node", y = "Centrality") +
  theme_minimal()

