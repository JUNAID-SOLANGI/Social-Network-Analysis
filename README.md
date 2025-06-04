# Social Network Analysis of Faculty Teaching Load
Faculty of Engineering and Technology, University of Sindh

# 📌 Purpose
This study aimed to analyze the social structure and workload distribution of faculty members by examining only the official teaching timetables. Using Social Network Analysis (SNA), we explored how faculty are connected through shared teaching responsibilities and how teaching loads are distributed across individuals and batches.


# 📊 Dataset
Source: FET Timetables 2021

Manually extracted faculty-batch data

Nodes: Faculty (e.g., Mr. Fahad Razak), Batches (e.g., BSIT-1)

Edges: Teaching links (weighted by weekly classes)


# 🔀 Network Types
Bipartite Network: Faculty ↔ Batch (teaching connection)

Unipartite Projection: Faculty ↔ Faculty (based on co-teaching same batch)


# 📌 Key Findings
Mr. Fahad Razak had the highest teaching load.

BSIT-2 and BSSW-2 had the most faculty engaged.

Unequal load distribution: some faculty had 15+ classes/week, others <2.

Central batches/faculty (by degree, eigenvector, betweenness) show who influences or connects others most.

Unipartite network revealed collaboration patterns and faculty clusters.


# 🧠 Insight
The study reflects the social positioning of faculty—who collaborates, who is central, and who is isolated—using only timetables as the data source. No surveys or interviews were needed.


# 🛠 Tools Used
R (igraph, bipartite projection, centrality measures)

Manual data extraction from HTML timetables


# 📈 Visuals
Bipartite network: Faculty–Batch links

Centrality plots (degree, betweenness, eigenvector)

One-mode faculty collaboration network

Individual ego networks for selected faculty


# 📌 Conclusion
The SNA approach provided valuable insight into:

Teaching workload imbalance

Faculty collaboration patterns

Potential areas for improved teaching resource allocation
