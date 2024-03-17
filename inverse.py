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
    
    # Augment the matrix with the identity matrix
    augmented = np.hstack((matrix, identity))
    print(augmented)
    # Perform Gaussian Elimination
    for i in range(n):
        # Find the pivot element
        pivot = augmented[i:, i].argmax() + i 
        print(pivot)
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
                factor = augmented[j, i]
                augmented[j] -= factor * augmented[i]
                print(augmented[j], 'j', j, 'i', i)
    
    # Extract the inverse from the right side of the augmented matrix
    print(augmented)
    inverse = augmented[:, n:]
    
    return inverse