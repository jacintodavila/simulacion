source ./Campo_Direccion/ejemplo/bin/activate

pip install torch

import torch

x = torch.randn((4, 1)) # "Column" 

y = torch.randn((4,)) # 1D tensor

print((x + y).shape)

[Out]: (4,4) (because of broadcasting!)

X = torch.randn((5, 5))

Element-wise exponential

X = torch.exp(X)

Matrix exponential

X = torch.linalg.matrix_exp(X)

r = X.sum(axis=1)

X = torch.randn((4, 5, 2))

X

Y = torch.randn((4, 2, 3))

Y

X @ Y

