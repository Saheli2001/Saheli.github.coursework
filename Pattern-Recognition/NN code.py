import numpy as np
import matplotlib.pyplot as plt
from sklearn.datasets import make_moons
from sklearn.linear_model import LogisticRegression
from mlxtend.plotting import plot_decision_regions

# Generate a dataset and plot it
np.random.seed(0)
X, y = make_moons(1000, noise=0.20)
plt.scatter(X[:, 0], X[:, 1], s=20, c=y, cmap=plt.cm.Spectral)

# Train the logistic regression classifier
clf = LogisticRegression()
clf.fit(X, y)

# Create a mesh grid to plot the decision boundary
plot_decision_regions(X, y, clf=clf, legend=2)
plt.title("Logistic Regression Decision Boundary")

plt.show()

#Activation Function
# Generate x values
x = np.linspace(-5, 5, 100)

# Calculate the tanh values for each x
tanh_values = np.tanh(x)

# Create a plot
plt.plot(x, tanh_values, label='tanh(x)')
plt.xlabel('x')
plt.ylabel('tanh(x)')
plt.title('Hyperbolic Tangent (tanh) Function')
plt.grid(True)
plt.legend()
plt.show()

num_examples = len(X) # training set size
nn_input_dim = 2 # input layer dimensionality
nn_output_dim = 2 # output layer dimensionality

# Gradient descent parameters (I picked these by hand)
epsilon = 0.0001 # learning rate for gradient descent

# Helper function to evaluate the total loss on the dataset
def calculate_loss(model):
    W1, b1, W2, b2 = model['W1'], model['b1'], model['W2'], model['b2']
    # Forward propagation to calculate our predictions
    z1 = X.dot(W1) + b1
    a1 = np.tanh(z1)
    z2 = a1.dot(W2) + b2
    exp_scores = np.exp(z2)
    probs = exp_scores / np.sum(exp_scores, axis=1, keepdims=True)
    # Calculating the loss
    corect_logprobs = -np.log(probs[range(num_examples), y])
    data_loss = np.sum(corect_logprobs)
    return 1./num_examples * data_loss

# Helper function to predict an output (0 or 1)
def predict(model, x):
    W1, b1, W2, b2 = model['W1'], model['b1'], model['W2'], model['b2']
    # Forward propagation
    z1 = x.dot(W1) + b1
    a1 = np.tanh(z1)
    z2 = a1.dot(W2) + b2
    exp_scores = np.exp(z2)
    probs = exp_scores / np.sum(exp_scores, axis=1, keepdims=True)
    return np.argmax(probs, axis=1)

# This function learns parameters for the neural network and returns the model.
# - nn_hdim: Number of nodes in the hidden layer
# - num_passes: Number of passes through the training data for gradient descent
# - print_loss: If True, print the loss every 1000 iterations
def build_model(nn_hdim, num_passes=20000, print_loss=True):
    
    # Initialize the parameters to random values. We need to learn these.
    np.random.seed(0)
    W1 = np.random.randn(nn_input_dim, nn_hdim) / np.sqrt(nn_input_dim)
    b1 = np.zeros((1, nn_hdim))
    W2 = np.random.randn(nn_hdim, nn_output_dim) / np.sqrt(nn_hdim)
    b2 = np.zeros((1, nn_output_dim))

    # This is what we return at the end
    model = {}
    losses = []
    # Gradient descent. For each batch...
    for i in range(0, num_passes):

        # Forward propagation
        z1 = X.dot(W1) + b1
        a1 = np.tanh(z1)
        z2 = a1.dot(W2) + b2
        exp_scores = np.exp(z2)
        probs = exp_scores / np.sum(exp_scores, axis=1, keepdims=True)

        # Backpropagation
        delta3 = probs
        delta3[range(num_examples), y] -= 1
        dW2 = (a1.T).dot(delta3)
        db2 = np.sum(delta3, axis=0, keepdims=True)
        delta2 = delta3.dot(W2.T) * (1 - np.power(a1, 2))
        dW1 = np.dot(X.T, delta2)
        db1 = np.sum(delta2, axis=0)

        # Gradient descent parameter update
        W1 += -epsilon * dW1
        b1 += -epsilon * db1
        W2 += -epsilon * dW2
        b2 += -epsilon * db2
        
        # Assign new parameters to the model
        model = { 'W1': W1, 'b1': b1, 'W2': W2, 'b2': b2}
        
        # Optionally print the loss.
        # This is expensive because it uses the whole dataset, so we don't want to do it too often.
        if print_loss and i % 1000 == 0:
          print("Loss after iteration %i: %f" %(i, calculate_loss(model)))
          loss = calculate_loss(model)
          losses.append(loss)
          if print_loss:
                print(f"Loss after iteration {i}: {loss}")

                # Plot the loss inside the build_model function
                plt.plot(losses)
                plt.xlabel("Iteration")
                plt.ylabel("Loss")
                plt.title("Loss Over Iterations")
                plt.show()
    
    return model

# Build a model with a 3-dimensional hidden layer
model = build_model(4, print_loss=False)
# Create a mesh grid for plotting
xx, yy = np.meshgrid(np.arange(X[:,0].min()-1, X[:,0].max()+1, 0.01),
                     np.arange(X[:,1].min()-1, X[:,1].max()+1, 0.01))
grid = np.c_[xx.ravel(), yy.ravel()]

# Predict using the model
Z = predict(model, grid)
Z = Z.reshape(xx.shape)

# Plot the decision boundary for the neural network model
plt.contourf(xx, yy, Z, cmap=plt.cm.Spectral, alpha=0.8)
plt.scatter(X[:, 0], X[:, 1], s=20, c=y, cmap=plt.cm.Spectral)
plt.title('Decision Boundary for Hidden Layer Size 4')
plt.show()
