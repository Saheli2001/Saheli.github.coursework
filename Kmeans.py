#Imported Packages
import numpy as np
import matplotlib.pyplot as plt

# Set seed
np.random.seed(42)

# Generating data for three different distributions
mean1 = np.array([0, 0])
mean2 = np.array([2, 2])
mean3 = np.array([-2, 2])

# Covariance matrices
cov1 = 0.1 * np.identity(2)
cov2 = 0.2 * np.identity(2)
cov3 = 0.3 * np.identity(2)

# Generating data from bivariate normal distributions
data1 = np.random.multivariate_normal(mean1, cov1, 100)
data2 = np.random.multivariate_normal(mean2, cov2, 100)
data3 = np.random.multivariate_normal(mean3, cov3, 100)

# Combined data
combined_data = np.vstack((data1, data2, data3))

# scaled data
scaled_data = (combined_data - np.mean(combined_data, axis=0)) / np.std(combined_data, axis=0)

# Plot of the combined data
plt.scatter(combined_data[:, 0], combined_data[:, 1], alpha=0.7)
plt.title('Combined Data')
plt.xlabel('X-axis')
plt.ylabel('Y-axis')
plt.show()

# calculation of euclidean distance
def euclidean_distance(a, b):
    return np.linalg.norm(a - b)

# kmeans clusteting function
def k_means(data, k, scaled=False, max_iterations=100): #max_iteration is the convergence criteria
    # Scale the data if required
    if scaled:
        data = (data - np.mean(data, axis=0)) / np.std(data, axis=0)
    
    # Random Initialization of the Centroids
    centroids = data[np.random.choice(data.shape[0], size=k, replace=False)]
    
    for _ in range(max_iterations):
        # Assign points to the nearest centroid
        clusters = {i: [] for i in range(k)}
        for point in data:
            distances = [euclidean_distance(point, centroid) for centroid in centroids]
            closest_centroid = np.argmin(distances)
            clusters[closest_centroid].append(point)
        
        # Update centroids
        for i in range(k):
            if clusters[i]:
                centroids[i] = np.mean(clusters[i], axis=0)
    
    return centroids, clusters

# We Perform K-means clustering for various values of K
K_values = [1, 2, 3, 4, 5, 6]

#unscaled
# Plot of the whole data showing the clustering
plt.figure(figsize=(12, 10))

for idx, K in enumerate(K_values, start=1):
    # Without scaling
    centroids_unscaled, clusters_unscaled = k_means(combined_data, K)

    # Plotting each subplot
    plt.subplot(2, 3, idx)
    plt.scatter(combined_data[:, 0], combined_data[:, 1], c=np.concatenate([[i]*len(cluster) for i, cluster in clusters_unscaled.items()]), cmap='viridis', alpha=0.7)
    plt.scatter(np.array(centroids_unscaled)[:, 0], np.array(centroids_unscaled)[:, 1], marker='*', s=200, c='red', label='Centroids')
    plt.title(f'K = {K}')
    plt.xlabel('X-axis')
    plt.ylabel('Y-axis')
    plt.legend()

plt.tight_layout()
plt.show()

#scaled data
# Plot the whole data showing the clustering
plt.figure(figsize=(12, 10))

for idx, K in enumerate(K_values, start=1):
    
    # With scaling
    centroids_scaled, clusters_scaled = k_means(combined_data, K, scaled=True)
    
    # Plotting each subplot
    plt.subplot(2, 3, idx)
    plt.scatter(scaled_data[:, 0], scaled_data[:, 1], c=np.concatenate([[i]*len(cluster) for i, cluster in clusters_scaled.items()]), cmap='viridis', alpha=0.7)
    plt.scatter(np.array(centroids_scaled)[:, 0], np.array(centroids_scaled)[:, 1], marker='*', s=200, c='red', label='Centroids')
    plt.title(f'K = {K}')
    plt.xlabel('X-axis')
    plt.ylabel('Y-axis')
    plt.legend()

plt.tight_layout()
plt.show()

# calculation of the elbow
def calculate_elbow(data, max_k, scaled=False):
    distortions = []
    for k in range(1, max_k + 1):
        centroids, clusters = k_means(data, k, scaled=scaled)
        distortion = 0
        for i in range(k):
            centroid = centroids[i]
            cluster_points = np.array(clusters[i])
            distortion += np.sum(np.linalg.norm(cluster_points - centroid, axis=1) ** 2) #calculates the within cluster sum of squares
        distortions.append(distortion)
    
    # Calculates the rate of decrease of distortions
    rate_of_decrease = [distortions[i] - distortions[i + 1] for i in range(len(distortions) - 1)]
    
    # Finding the elbow point
    elbow_point = 0
    for i in range(len(rate_of_decrease) - 1):
        if rate_of_decrease[i] > rate_of_decrease[i + 1]:
            elbow_point = i + 1
            break
    
    return distortions, elbow_point + 1  # Adding 1 as index starts from 0

elbow_distortions_unscaled, elbow_k_unscaled = calculate_elbow(combined_data, max_k=6)
elbow_distortions_scaled, elbow_k_scaled = calculate_elbow(combined_data, max_k=6,scaled=True)

# Plotting the distortions via a line diagram (ELBOW METHOD)
plt.figure(figsize=(8, 6))
plt.plot(range(1, len(elbow_distortions_unscaled) + 1), elbow_distortions_unscaled, marker='o', label='Unscaled')
plt.plot(range(1, len(elbow_distortions_scaled) + 1), elbow_distortions_scaled, marker='o', label='Scaled')
plt.title('Elbow Method: Distortion vs. Number of Clusters')
plt.xlabel('Number of Clusters (K)')
plt.ylabel('Distortion')
plt.legend()
plt.grid(True)
plt.show()

# Defining Dunn Statistic
def calculate_dunn_for_range(data, max_k, scaled=False):
    dunn_values = []
    for k in range(1, max_k + 1):
        _, clusters = k_means(data, k, scaled=scaled)
        min_inter_cluster_distances = []
        max_intra_cluster_diameter = []
        
        # Calculates inter-cluster distances and intra-cluster diameters
        for i in range(k):
            for j in range(i + 1, k):
                distances = [euclidean_distance(p1, p2) for p1 in clusters[i] for p2 in clusters[j]]
                if distances:
                    min_inter_cluster_distances.append(min(distances))
             
            cluster_diameter = np.max(np.linalg.norm(np.array(clusters[i]) - np.mean(clusters[i], axis=0), axis=1))
            if len(clusters[i]) > 1:
                max_intra_cluster_diameter.append(cluster_diameter)
        
        # Calculation of Dunn Statistics
        if min_inter_cluster_distances and max_intra_cluster_diameter:
            dunn = min(min_inter_cluster_distances) / max(max_intra_cluster_diameter)
            dunn_values.append(dunn)
        else:
            dunn_values.append(0)  # If unable to calculate, assign 0
        
    return dunn_values

# Calculates Dunn Statistics for different values of K for both scaled and unscaled data
max_k = 6
dunn_values_unscaled = calculate_dunn_for_range(combined_data, max_k, scaled=False)
dunn_values_scaled = calculate_dunn_for_range(combined_data, max_k, scaled=True)

# Plotting Dunn index via line diagrams for scaled and unscaled data
plt.figure(figsize=(8, 6))
plt.plot(range(1, max_k + 1), dunn_values_unscaled, marker='o', label='Unscaled')
plt.plot(range(1, max_k + 1), dunn_values_scaled, marker='o', label='Scaled')
plt.title('Dunn Index: Dunn vs. Number of Clusters')
plt.xlabel('Number of Clusters (K)')
plt.ylabel('Dunn Index')
plt.legend()
plt.grid(True)
plt.show()

# Defining GAP statistics function
def calculate_gap(data, max_k, scaled=False):
    reference_dataset = np.random.uniform(low=np.min(data, axis=0), high=np.max(data, axis=0), size=data.shape)
    log_ref = [0] * (max_k + 1)
    log_actual = [0] * (max_k + 1)
    reference_gaps = []

    for k in range(1, max_k + 1):
        _, ref_clusters = k_means(reference_dataset, k, scaled=scaled)
        ref_dispersion = np.mean([np.mean(np.linalg.norm(np.array(ref_clusters[j]) - np.mean(ref_clusters[j], axis=0), axis=1) ** 2) for j in range(k)])
        
        centroids, clusters = k_means(data, k)
        dispersion = np.mean([np.mean(np.linalg.norm(np.array(clusters[j]) - centroids[j], axis=1) ** 2) for j in range(k)])
        
        log_ref[k-1] = np.log(ref_dispersion)
        log_actual[k-1] = np.log(dispersion)
        
        gap = np.log(ref_dispersion) - np.log(dispersion)
        reference_gaps.append(gap)
    # Finding the optimal k using the gap statistic
    optimal_k = np.argmax(np.array(reference_gaps)) + 1

    return log_ref, log_actual, optimal_k

# Storing the log Wk values for uniform distribution and the data considered with the optimal K
log_ref_unscaled, log_actual_unscaled, optimal_k_unscaled = calculate_gap(combined_data, max_k=6)
log_ref_scaled, log_actual_scaled, optimal_k_scaled = calculate_gap(combined_data, max_k=6, scaled=True)

# Plot of The GAP statistics
plt.figure(figsize=(12, 6))

plt.subplot(1, 2, 1)
plt.plot(range(1, len(log_ref_unscaled)), log_ref_unscaled[1:], label='Log Reference (Unscaled)', marker='o')
plt.plot(range(1, len(log_actual_unscaled)), log_actual_unscaled[1:], label='Log Actual (Unscaled)', marker='o')
plt.title('Unscaled Data')
plt.xlabel('Number of Clusters (K)')
plt.ylabel('Log Dispersion')
plt.legend()

plt.subplot(1, 2, 2)
plt.plot(range(1, len(log_ref_scaled)), log_ref_scaled[1:], label='Log Reference (Scaled)', marker='o')
plt.plot(range(1, len(log_actual_scaled)), log_actual_scaled[1:], label='Log Actual (Scaled)', marker='o')
plt.title('Scaled Data')
plt.xlabel('Number of Clusters (K)')
plt.ylabel('Log Dispersion')
plt.legend()

plt.tight_layout()
plt.show()



