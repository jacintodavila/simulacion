import numpy as np

# Create an example array
original_array = np.array([[10, 20], [30, 40]], dtype=np.float64)

# Create a new array of ones with the same shape and dtype
ones_array = np.ones_like(original_array)

print("Original array:")
print(original_array)
print("\nArray created with np.ones_like():")
print(ones_array)
