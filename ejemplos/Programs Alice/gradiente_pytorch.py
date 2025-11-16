import torch

def f_torch(X):
    # X: tensor shape (n,2) or (2,)
    X = X if isinstance(X, torch.Tensor) else torch.tensor(X, dtype=torch.float32)
    if X.dim() == 1:
        x1, x2 = X[0], X[1]
        return torch.sin(x1)*torch.cos(x2) + torch.sin(0.5*x1)*torch.cos(0.5*x2)
    x1 = X[:,0]; x2 = X[:,1]
    return torch.sin(x1)*torch.cos(x2) + torch.sin(0.5*x1)*torch.cos(0.5*x2)

def grad_f_torch(X):
    X = X if isinstance(X, torch.Tensor) else torch.tensor(X, dtype=torch.float32)
    if X.dim() == 1:
        x1, x2 = X[0], X[1]
        df_dx1 = torch.cos(x1)*torch.cos(x2) + 0.5*torch.cos(0.5*x1)*torch.cos(0.5*x2)
        df_dx2 = -torch.sin(x1)*torch.sin(x2) - 0.5*torch.sin(0.5*x1)*torch.sin(0.5*x2)
        return torch.stack([df_dx1, df_dx2])
    x1 = X[:,0]; x2 = X[:,1]
    df_dx1 = torch.cos(x1)*torch.cos(x2) + 0.5*torch.cos(0.5*x1)*torch.cos(0.5*x2)
    df_dx2 = -torch.sin(x1)*torch.sin(x2) - 0.5*torch.sin(0.5*x1)*torch.sin(0.5*x2)
    return torch.stack([df_dx1, df_dx2], dim=1)

# ======================
# PRUEBAS DE SALIDA
# ======================

print("Evaluación de f_torch([1,2]):", f_torch([1.0, 2.0]))
print("Gradiente en [1,2]:", grad_f_torch([1.0, 2.0]))
