import jax.numpy as jnp
from jax import vmap  # opcional

def f_jax(X):
    # X: (n,2) or (2,)
    X = jnp.asarray(X)
    if X.ndim == 1:
        x1, x2 = X[0], X[1]
        return jnp.sin(x1)*jnp.cos(x2) + jnp.sin(0.5*x1)*jnp.cos(0.5*x2)
    x1 = X[:,0]; x2 = X[:,1]
    return jnp.sin(x1)*jnp.cos(x2) + jnp.sin(0.5*x1)*jnp.cos(0.5*x2)

def grad_f_jax(X):
    # hard-coded gradient
    X = jnp.asarray(X)
    if X.ndim == 1:
        x1, x2 = X
        df_dx1 = jnp.cos(x1)*jnp.cos(x2) + 0.5*jnp.cos(0.5*x1)*jnp.cos(0.5*x2)
        df_dx2 = -jnp.sin(x1)*jnp.sin(x2) - 0.5*jnp.sin(0.5*x1)*jnp.sin(0.5*x2)
        return jnp.array([df_dx1, df_dx2])
    x1 = X[:,0]; x2 = X[:,1]
    df_dx1 = jnp.cos(x1)*jnp.cos(x2) + 0.5*jnp.cos(0.5*x1)*jnp.cos(0.5*x2)
    df_dx2 = -jnp.sin(x1)*jnp.sin(x2) - 0.5*jnp.sin(0.5*x1)*jnp.sin(0.5*x2)
    return jnp.stack([df_dx1, df_dx2], axis=1)


# PRUEBAS PARA VER SALIDA
# ======================

print("Evaluación de f_jax([1,2]):", f_jax(jnp.array([1.0, 2.0])))
print("Gradiente en [1,2]:", grad_f_jax(jnp.array([1.0, 2.0])))
