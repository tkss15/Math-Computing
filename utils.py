import numpy as np


def row_swap(matrix, i, j):
    matrix[[i,j]] = matrix[[j,i]]
    return matrix 

def pivot(matrix, i):
    pivot = np.argmax(matrix[i:, i]) + i 
    return pivot 

def normalize(matrix, i):
    pivot_val = matrix[i, i]
    matrix[i] /= pivot_val
    return matrix

def remove_spaces(input_file, output_file):
    with open(input_file, 'r') as f:
        content = f.read()
    
    # Remove spaces from the content
    modified_content = content.replace(' ', ',').replace('\n', ',')

    with open(output_file, 'w') as f:
        f.write(modified_content)