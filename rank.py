import numpy as np
import utils

def invert_matrix(matrix):
    """
    Inverts a square matrix using the Gaussian Elimination method.
    
    Args:
        matrix (numpy.ndarray): The square matrix to be inverted.
        
    Returns:
        numpy.ndarray: The inverse of the input matrix.
    """
    # # Check if the input is a NumPy array
    # if not isinstance(matrix, np.ndarray):
    #     matrix = np.array(matrix, dtype=float)
    
    # Check if the matrix is square
    if matrix.shape[0] != matrix.shape[1]:
        raise ValueError("Input matrix must be square.")
    
    # Create an identity matrix of the same size
    n = matrix.shape[0]
    identity = np.eye(n, dtype=float)
    print(identity)
    # Augment the matrix with the identity matrix
    augmented = np.hstack((matrix, identity))
    print(augmented)
    # Perform Gaussian Elimination
    for i in range(n):
        # Find the pivot element
        pivot = augmented[i:, i].argmax() + i 
        if augmented[pivot, i] == 0:
            raise ValueError("Matrix is singular and cannot be inverted.")
        
        # Swap rows to move the pivot element to the current row
        augmented[[i, pivot]] = augmented[[pivot, i]]
        
        # Normalize the pivot row
        pivot_val = augmented[i, i]
        augmented[i] /= pivot_val
        
        # Eliminate entries below and above the pivot
        for j in range(n):
            if j != i:
                print(augmented)
                factor = augmented[j, i]
                augmented[j] -= factor * augmented[i]
                print(augmented)

    # Extract the inverse from the right side of the augmented matrix
    # print(augmented)
    inverse = augmented[:, n:]
    
    return inverse
def gaussian_elimination_with_rank_numpy(A, n):
    mat = A.copy().astype(float)
    rank = 0

    for i in range(n):
        pivot = utils.pivot(mat, i)
        
        if np.isclose(mat[i, i], 0):
            continue
        
        rank += 1  # Increment rank for a non-zero pivot
        
        # Normalize the pivot row
        mat[i] = utils.normalize(mat,i)
        
        # Eliminate the current column elements below the pivot row
        for j in range(i + 1, n):
            factor = mat[j, i]
            mat[j] -= factor * mat[i] # for Example R2 <- R2 - ( factor * R1)
    print(mat)
    return mat, rank

# Example usage

# f = open('./rank_matrix.txt', 'r')
# matrix = f.read().replace("\n",",").replace(" ", "").split(',')
# f = open('./inv_eig_matrix(800 x 800).txt', 'r')
# matrix = f.read().replace("\n"," ").split(' ')

# A = np.zeros((800,800))
# for i in range(800):
#    A[i] = np.array([float(element) for element in matrix[i*800:(i+1)*800]], dtype=float) 
# print(A[373,241])
# print(np.linalg.matrix_rank(A))
# rank = gaussian_elimination_with_rank_numpy(A,1000)
# print("Rank of the matrix:", rank)

numbers = [1,2,3,4]

print(gaussian_elimination_with_rank_numpy(np.array([[-1,1.5],[1,-1]]), 2))
# print(invert_matrix(np.array([[-1,1.5],[1,-1]])))
# print(np.isclose(invert_matrix(np.array(A)),np.linalg.inv(A)))