import numpy as np

def f(X):
    X = np.asarray(X)
    if X.ndim == 1:
        x1, x2 = X
        return np.sin(x1)*np.cos(x2) + np.sin(0.5*x1)*np.cos(0.5*x2)
    x1 = X[:,0]; x2 = X[:,1]
    return np.sin(x1)*np.cos(x2) + np.sin(0.5*x1)*np.cos(0.5*x2)

def grad_f(X):
    X = np.asarray(X)
    if X.ndim == 1:
        x1, x2 = X
        df_dx1 = np.cos(x1)*np.cos(x2) + 0.5*np.cos(0.5*x1)*np.cos(0.5*x2)
        df_dx2 = -np.sin(x1)*np.sin(x2) - 0.5*np.sin(0.5*x1)*np.sin(0.5*x2)
        return np.array([df_dx1, df_dx2])
    x1 = X[:,0]; x2 = X[:,1]
    df_dx1 = np.cos(x1)*np.cos(x2) + 0.5*np.cos(0.5*x1)*np.cos(0.5*x2)
    df_dx2 = -np.sin(x1)*np.sin(x2) - 0.5*np.sin(0.5*x1)*np.sin(0.5*x2)
    return np.stack([df_dx1, df_dx2], axis=1)

# gradient descent standard
def gradient_descent(x0, lr=0.1, iters=200):
    x = np.array(x0, dtype=float)
    path = [x.copy()]
    for _ in range(iters):
        g = grad_f(x)
        x = x - lr * g
        x = np.clip(x, 0, 10)  # opcional: mantener dentro del dominio
        path.append(x.copy())
    return np.array(path)

# momentum
def gradient_descent_momentum(x0, lr=0.05, iters=300, momentum=0.9):
    x = np.array(x0, dtype=float)
    v = np.zeros_like(x)
    path = [x.copy()]
    grad_norms = []
    for _ in range(iters):
        g = grad_f(x)
        grad_norms.append(np.linalg.norm(g))
        v = momentum*v - lr*g
        x = x + v
        x = np.clip(x, 0, 10)
        path.append(x.copy())
    return np.array(path), np.array(grad_norms)
# ======================
# PRUEBAS PARA VER SALIDA
# ======================

print("Evaluación de f en [1,2]:", f([1,2]))
print("Gradiente en [1,2]:", grad_f([1,2]))

path = gradient_descent([5,5], lr=0.1, iters=50)
print("Último punto GD:", path[-1])

path_m, norms = gradient_descent_momentum([5,5], lr=0.05, iters=100)
print("Último punto Momentum:", path_m[-1])
print("Norma final del gradiente:", norms[-1])
