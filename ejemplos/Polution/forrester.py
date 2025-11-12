"""
Modelo Mundial de Forrester (World Dynamics, 1971)
Implementación completa con todas las ecuaciones originales
"""

import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import interp1d

# Datos APOL originales para validación
APOL_DATA = np.array([
    [0, 0.00529345794392538], [1, 0.0106032249572195], [2, 0.0160760826642054],
    [3, 0.0218448334868925], [4, 0.0280376201132008], [5, 0.0347243385546963],
    [10, 0.0721187047518695], [20, 0.160304172699743], [30, 0.243035421877056],
    [40, 0.324426997499019], [50, 0.358412768197967], [60, 0.464882132339907],
    [70, 0.666415632405724], [80, 0.982578591468505], [90, 1.34114513089196],
    [100, 1.685965454704], [110, 2.15566665254523], [120, 2.70901007758145],
    [123, 2.88305114642442], [150, 3.97196261682243], [250, 11.9158878504673]
])

class ForresterModel:
    def __init__(self):
        # Constantes del modelo
        self.BRN = 0.04      # Birth Rate Normal
        self.DRN = 0.028     # Death Rate Normal
        self.CGN = 0.05      # Capital Generation Normal
        self.CDN = 0.025     # Capital Discard Normal
        self.LAND = 135e6    # Total Land (km²)
        self.PDN = 26.5      # Population Density Normal
        self.CAFN = 0.3      # Capital Fraction in Agriculture Normal
        self.CAFT = 15.0     # Capital Fraction Adjustment Time
        self.FC = 1.0        # Food Coefficient
        self.FN = 1.0        # Food Normal
        self.POLS = 3.6e9    # Pollution Standard (1970)
        self.POLN = 1.0      # Pollution Normal
        self.NRI = 900e9     # Natural Resources Initial
        self.NRUN = 1.0      # Natural Resources Usage Normal
        self.ECRN = 1.0      # Effective Capital Ratio Normal
        self.QLS = 1.0       # Quality of Life Standard
        
        # Definir todas las tablas POLYG del modelo original
        self.tables = {
            'BRMM': np.array([[0,1.2],[1,1.0],[2,0.85],[3,0.75],[4,0.7],[5,0.7],[7,0.7]]),
            'BRFM': np.array([[0,0],[1,1],[2,1.6],[3,1.9],[4,2.0],[10,2.0]]),
            'BRCM': np.array([[0,1.05],[1,1.0],[2,0.9],[3,0.7],[4,0.6],[5,0.55],[10,0.55]]),
            'BRPM': np.array([[0,1.02],[10,0.9],[20,0.7],[30,0.4],[40,0.25],[50,0.15],[60,0.1],[200,0.1]]),
            
            'DRMM': np.array([[0,3.0],[0.5,1.8],[1.0,1.0],[1.5,0.8],[2.0,0.7],[2.5,0.6],[3.0,0.53],
                              [3.5,0.5],[4.0,0.5],[4.5,0.5],[5.0,0.5],[7,0.5]]),
            'DRFM': np.array([[0.0,30],[0.25,3],[0.50,2],[0.75,1.4],[1.0,1.0],[1.25,0.7],
                              [1.50,0.6],[1.75,0.50],[2.0,0.50],[3.0,0.50]]),
            'DRCM': np.array([[0,0.9],[1,1],[2,1.2],[3,1.5],[4,1.9],[5,3],[7,3]]),
            'DRPM': np.array([[0,0.92],[10,1.3],[20,2.0],[30,3.2],[40,4.8],[50,6.8],[60,9.2],[200,9.2]]),
            
            'NREM': np.array([[0.0,0.0],[0.25,0.15],[0.50,0.50],[0.75,0.85],[1.0,1.0],[1000,1]]),
            'CMM': np.array([[0.0,0.1],[1.0,1.0],[2.0,1.8],[3.0,2.4],[4.0,2.8],[5.0,3.0],[1000,3.0]]),
            'CFFR': np.array([[0.0,1.0],[0.5,0.6],[1.0,0.3],[1.5,0.15],[2.0,0.1],[3.0,0.1]]),
            'CQR': np.array([[0.0,0.7],[0.5,0.8],[1.0,1.0],[1.5,1.5],[2,2],[1000,2]]),
            
            'FCM': np.array([[0,2.4],[1,1],[2,0.6],[3,0.4],[4,0.3],[5,0.2],[1000,0.2]]),
            'FPM': np.array([[0,1.02],[10,0.9],[20,0.65],[30,0.35],[40,0.2],[50,0.1],[60,0.05],[1000,0.05]]),
            'FPC': np.array([[0,0.5],[1,1],[2,1.4],[3,1.7],[4,1.9],[5,2.05],[6,2.2],[1000,2.2]]),
            
            'NRMM': np.array([[0,0.0],[1,1.0],[2,1.8],[3,2.4],[4,2.9],[5,3.3],[6,3.6],[7,3.8],
                              [8,3.9],[9,3.95],[10,4.0],[1000,4.0]]),
            'POLCM': np.array([[0,0.05],[1,1],[2,3],[3,5.4],[4,7.4],[5,8.0],[1000,8.0]]),
            'POLAT': np.array([[0,0.6],[10,2.5],[20,5.0],[30,8.0],[40,11.5],[50,15.5],[60,20.0],[1000,20.0]]),
            
            'QLM': np.array([[0,0.2],[1,1.0],[2,1.7],[3,2.3],[4,2.7],[5,2.9],[1000,2.9]]),
            'QLC': np.array([[0,2],[0.5,1.3],[1,1.0],[1.5,0.75],[2,0.55],[2.5,0.45],[3,0.38],
                             [3.5,0.3],[4,0.25],[4.5,0.22],[5,0.20],[1000,0.20]]),
            'QLF': np.array([[0,0],[1,1],[2,1.8],[3,2.4],[4,2.7],[1000,2.7]]),
            'QLP': np.array([[0,1.04],[10,0.85],[20,0.60],[30,0.30],[40,0.15],[50,0.05],[60,0.02],[1000,0.02]]),
        }
        
        # Crear funciones de interpolación para cada tabla
        self.interp_funcs = {}
        for name, table in self.tables.items():
            self.interp_funcs[name] = interp1d(table[:,0], table[:,1], 
                                               kind='linear', 
                                               fill_value=(table[0,1], table[-1,1]), 
                                               bounds_error=False)
    
    def lookup(self, table_name, x):
        """Función de lookup para las tablas POLYG"""
        return float(self.interp_funcs[table_name](x))
    
    def simulate(self, years=230, dt=0.5):
        """Simula el modelo de Forrester"""
        steps = int(years / dt)
        
        # Inicialización de variables de estado (1900)
        pop = 1.65e9
        capital = 0.4e9
        nr = self.NRI
        pol = 0.2e9
        caf = 0.2
        
        # Arrays para almacenar resultados
        results = {
            'time': [],
            'year': [],
            'pop': [],
            'capital': [],
            'nr': [],
            'pol': [],
            'POLR': [],
            'FR': [],
            'MSL': [],
            'CR': [],
            'QOL': [],
            'CAR': [],
        }
        
        for i in range(steps + 1):
            time = i * dt
            
            # Guardar resultados cada año
            if i % 2 == 0:  # Cada 1 año (dt=0.5, cada 2 pasos)
                year = 1900 + time
                results['time'].append(time)
                results['year'].append(year)
                results['pop'].append(pop / 1e9)  # En miles de millones
                results['capital'].append(capital / 1e9)
                results['nr'].append(nr / 1e12)  # En billones
                results['pol'].append(pol / 1e9)
                results['POLR'].append(pol / self.POLS)
            
            # === CÁLCULOS INTERMEDIOS ===
            
            # Capital per capita
            CAR = capital / pop if pop > 0 else 0
            
            # Crowding Ratio
            CR = pop / (self.LAND * self.PDN)
            
            # Natural Resources Fraction Remaining
            NRFR = nr / self.NRI
            
            # Pollution Ratio
            POLR = pol / self.POLS
            
            # Effective Capital Ratio
            ECR = CAR * (1 - caf) * self.lookup('NREM', NRFR) / (1 - self.CAFN)
            MSL = ECR / self.ECRN  # Material Standard of Living
            
            # Capital in Agriculture
            CRA = CAR * caf / self.CAFN
            
            # Food Ratio
            FR = (self.FC * self.lookup('FPC', CRA) / self.FN * 
                  self.lookup('FPM', POLR) * self.lookup('FCM', CR))
            
            # Quality of Life
            QOL = (self.QLS * self.lookup('QLM', MSL) * self.lookup('QLC', CR) * 
                   self.lookup('QLF', FR) * self.lookup('QLP', POLR))
            
            # Quality ratio
            CAQR = self.lookup('QLM', MSL) / max(0.01, self.lookup('QLF', FR))
            
            # Guardar variables intermedias
            if i % 2 == 0:
                results['FR'].append(FR)
                results['MSL'].append(MSL)
                results['CR'].append(CR)
                results['QOL'].append(QOL)
                results['CAR'].append(CAR)
            
            # === ECUACIONES DE TASAS ===
            
            # Birth Rate
            BR = (pop * self.BRN * self.lookup('BRMM', MSL) * 
                  self.lookup('BRFM', FR) * self.lookup('BRCM', CR) * 
                  self.lookup('BRPM', POLR))
            
            # Death Rate
            DR = (pop * self.DRN * self.lookup('DRMM', MSL) * 
                  self.lookup('DRFM', FR) * self.lookup('DRCM', CR) * 
                  self.lookup('DRPM', POLR))
            
            # Capital Generation and Discard
            CG = pop * self.CGN * self.lookup('CMM', MSL)
            CD = capital * self.CDN
            
            # Natural Resources Usage
            NRUR = pop * self.NRUN * self.lookup('NRMM', MSL)
            
            # Pollution Generation and Absorption
            PG = pop * self.POLN * self.lookup('POLCM', CAR)
            PA = pol / self.lookup('POLAT', POLR)
            
            # === INTEGRACIÓN ===
            
            # Actualizar CAF con delay de primer orden (RETARD)
            CAFT_target = self.lookup('CFFR', FR) * self.lookup('CQR', CAQR)
            caf = caf + (CAFT_target - caf) * dt / self.CAFT
            
            # Ecuaciones diferenciales
            pop += (BR - DR) * dt
            capital += (CG - CD) * dt
            nr -= NRUR * dt
            pol += (PG - PA) * dt
            
            # Prevenir valores negativos
            pop = max(0, pop)
            capital = max(0, capital)
            nr = max(0, nr)
            pol = max(0, pol)
            caf = max(0, min(1, caf))
        
        # Convertir a numpy arrays
        for key in results:
            results[key] = np.array(results[key])
        
        return results


def plot_results(results, apol_data):
    """Grafica los resultados de la simulación"""
    
    # Interpolar APOL para comparación
    apol_interp = interp1d(apol_data[:,0], apol_data[:,1], 
                           kind='linear', fill_value='extrapolate')
    apol_years = results['time']
    apol_values = apol_interp(apol_years)
    
    # Crear figura con subplots
    fig, axes = plt.subplots(2, 2, figsize=(15, 10))
    fig.suptitle('Modelo Mundial de Forrester - Análisis Completo', 
                 fontsize=16, fontweight='bold')
    
    # 1. POLR vs APOL Original
    ax1 = axes[0, 0]
    ax1.plot(results['year'], results['POLR'], 'b-', linewidth=2, 
             label='POLR Simulado (Modelo Completo)')
    ax1.plot(results['year'], apol_values, 'r--', linewidth=2, 
             label='APOL Original (Validación)')
    ax1.set_xlabel('Año')
    ax1.set_ylabel('POLR (1970 = 1.0)')
    ax1.set_title('Índice de Polución Normalizado')
    ax1.legend()
    ax1.grid(True, alpha=0.3)
    
    # 2. Polución Absoluta
    ax2 = axes[0, 1]
    ax2.plot(results['year'], results['pol'], 'r-', linewidth=2)
    ax2.set_xlabel('Año')
    ax2.set_ylabel('Polución (×10⁹ unidades)')
    ax2.set_title('Polución Absoluta')
    ax2.grid(True, alpha=0.3)
    
    # 3. Variables del Sistema
    ax3 = axes[1, 0]
    ax3.plot(results['year'], results['pop'], 'g-', linewidth=2, label='Población')
    ax3.plot(results['year'], results['capital'], 'orange', linewidth=2, label='Capital')
    ax3.plot(results['year'], results['POLR'], 'b-', linewidth=2, label='POLR')
    ax3.plot(results['year'], results['nr'], 'purple', linewidth=2, label='Recursos Naturales')
    ax3.set_xlabel('Año')
    ax3.set_ylabel('Valor (escalas variadas)')
    ax3.set_title('Variables del Sistema Mundial')
    ax3.legend()
    ax3.grid(True, alpha=0.3)
    
    # 4. Calidad de Vida y Food Ratio
    ax4 = axes[1, 1]
    ax4.plot(results['year'], results['QOL'], 'cyan', linewidth=2, label='Quality of Life')
    ax4.plot(results['year'], results['FR'], 'm-', linewidth=2, label='Food Ratio')
    ax4.plot(results['year'], results['MSL'], 'y-', linewidth=2, label='Material Std. Living')
    ax4.set_xlabel('Año')
    ax4.set_ylabel('Índice')
    ax4.set_title('Indicadores de Calidad')
    ax4.legend()
    ax4.grid(True, alpha=0.3)
    
    plt.tight_layout()
    return fig


def calculate_error(results, apol_data):
    """Calcula el error entre la simulación y los datos originales"""
    apol_interp = interp1d(apol_data[:,0], apol_data[:,1], 
                           kind='linear', fill_value='extrapolate')
    apol_values = apol_interp(results['time'])
    
    # Calcular RMSE (Root Mean Square Error)
    rmse = np.sqrt(np.mean((results['POLR'] - apol_values)**2))
    
    # Calcular error relativo promedio
    rel_error = np.mean(np.abs((results['POLR'] - apol_values) / apol_values)) * 100
    
    return rmse, rel_error


if __name__ == "__main__":
    print("=" * 60)
    print("Modelo Mundial de Forrester - Simulación Completa")
    print("=" * 60)
    
    # Crear y ejecutar modelo
    model = ForresterModel()
    print("\n⏳ Ejecutando simulación...")
    results = model.simulate(years=230, dt=0.5)
    print("✓ Simulación completada")
    
    # Calcular error
    rmse, rel_error = calculate_error(results, APOL_DATA)
    print(f"\n📊 Validación del modelo:")
    print(f"   RMSE: {rmse:.6f}")
    print(f"   Error relativo promedio: {rel_error:.2f}%")
    
    # Graficar resultados
    print("\n📈 Generando gráficos...")
    fig = plot_results(results, APOL_DATA)
    plt.savefig('forrester_model_results.png', dpi=300, bbox_inches='tight')
    print("✓ Gráficos guardados en 'forrester_model_results.png'")
    
    plt.show()
    
    print("\n" + "=" * 60)
    print("Análisis completado")
    print("=" * 60)