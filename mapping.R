# Load necessary libraries
library(visNetwork)
library(dplyr)
library(igraph)

# Create empty data frames for nodes and edges
nodes <- data.frame(id = character(),
                    label = character(),
                    title = character(),
                    country = character(),  # Add country column
                    stringsAsFactors = FALSE)

edges <- data.frame(from = character(),
                    to = character(),
                    ownership = numeric(),
                    stringsAsFactors = FALSE)

# Define the ultimate owner
ultimate_owner <- "MICROSOFT CORP"
nodes <- rbind(nodes, data.frame(id = ultimate_owner, label = ultimate_owner, title = "Country: USA", country = "USA"))

# Create a set to store direct parent companies
direct_parents <- unique(df$company)

# Loop through the DataFrame to create nodes and edges
for (i in 1:nrow(df)) {
  parent_company <- df$company[i]
  subsidiary_company <- df$sub[i]
  ownership_percentage <- df$ownership[i]  # Assuming the ownership percentage is in the 'ownership' column
  parent_country <- df$country[i]  # Assuming the country information is in the 'country' column
  subsidiary_country <- df$country[i]  # Assuming the subsidiary country information is in the 'sub_country' column
  
  # Add parent company node
  if (!(parent_company %in% nodes$id)) {
    nodes <- rbind(nodes, data.frame(id = parent_company,
                                     label = parent_company,
                                     title = paste("Country: ", ifelse(!is.na(parent_country), parent_country, "N/A")),  # Format as "Country: <country_code>"
                                     country = parent_country))
  }
  
  # Only add edges for non-NA subsidiaries and non-NA ownership percentages
  if (!is.na(subsidiary_company) && !is.na(ownership_percentage)) {
    # Add subsidiary node
    if (!(subsidiary_company %in% nodes$id)) {
      nodes <- rbind(nodes, data.frame(id = subsidiary_company,
                                       label = subsidiary_company,
                                       title = paste("Country: ", ifelse(!is.na(subsidiary_country), subsidiary_country, "N/A")),  # Format as "Country: <country_code>"
                                       country = subsidiary_country))
    }
    
    # Create edges with ownership percentage
    edges <- rbind(edges, data.frame(from = parent_company, to = subsidiary_company, ownership = ownership_percentage))
  }
}

# Step 1: Identify parent companies that are not subsidiaries of any other company
# Get a list of all subsidiaries
all_subsidiaries <- unique(df$sub)

# Filter direct parents to include only those that are not in the list of subsidiaries
filtered_direct_parents <- setdiff(direct_parents, all_subsidiaries)

# Step 2: Update the edges to connect the ultimate owner only to the filtered direct parents
for (parent in filtered_direct_parents) {
  edges <- rbind(edges, data.frame(from = ultimate_owner, to = parent, ownership = NA))
}

# Step 3: Create the graph object from the edges
graph <- graph_from_data_frame(edges, directed = TRUE)  # Use directed = TRUE for directional edges

# Step 4: Calculate degree centrality
degree_centrality <- degree(graph)
nodes$degree_centrality <- degree_centrality[nodes$id]

# Normalize degree centrality for visualization
nodes$size <- (nodes$degree_centrality / max(nodes$degree_centrality)) * 60  # Scale size for visibility

# Step 5: Get the top 25 companies based on degree centrality
top_companies <- nodes %>%
  arrange(desc(degree_centrality)) %>%
  head(10) %>%
  select(id)

# Step 6: Filter edges and nodes for visualization
edges_filtered <- edges %>%
  filter(from %in% top_companies$id | to %in% top_companies$id)

nodes_filtered <- nodes %>%
  filter(id %in% top_companies$id | id == ultimate_owner)

# Step 7: Define a function to get the color based on ownership percentage
get_ownership_color <- function(ownership) {
  if (is.na(ownership)) {
    return("gray")
  } else if (ownership >= 100.0) {
    return("green")
  } else if (ownership >= 50.0) {
    return("orange")
  } else if (ownership >= 25.0) {
    return("yellow")
  } else {
    return("red")
  }
}

# Step 8: Apply the color function to the edges
edges_filtered$color <- sapply(edges_filtered$ownership, get_ownership_color)

# Step 9: Create the visNetwork graph with the filtered nodes and edges
visNetwork(nodes_filtered, edges_filtered) %>%
  visNodes(shape = "box", size = nodes_filtered$size, title = nodes_filtered$title) %>%
  visEdges(arrows = "to", color = list(color = edges_filtered$color, highlight = "red")) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 160, hierarchical = list(
    enabled = TRUE,
    levelSeparation = 200,
    nodeSpacing = 4000,
    treeSpacing = 1000,
    blockShifting = TRUE,
    edgeMinimization = TRUE,
    parentCentralization = TRUE,
    direction = "UD",        # UD for Up-Down layout
    sortMethod = "directed"  # Sort method for hierarchical layout
  )) %>%
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE, tooltipDelay = 0, hideEdgesOnDrag = FALSE) %>%
  visLegend(addNodes = data.frame(label = c("High Ownership (= 100%)",
                                            "Medium Ownership (≥ 50%)",
                                            "Low Ownership (≥ 25%)",
                                            "Very Low Ownership (< 25%)",
                                            "No Ownership information",
                                            "Click over the nodes to display country info",
                                            "Click on select ID for company structure"),
                                  color = c("green", "orange", "yellow", "red", "gray", "transparent", "transparent")),
            useGroups = FALSE, position = "right")
