package example;

import galatea.glider.*;
/**
 * Crecimiento exponencial de bacterias.
 * 
 * Problema: La población de bacterias crece proporcionalmente al número presente.
 * Después de 3h hay 400 bacterias, después de 10h hay 2000 bacterias.
 * ¿Cuál era el número inicial?
 * 
 * 
 */
public class Model extends Glider {
    // Parameters
    public double k;
    public double n0;
    // Nodes
    // 1 Continous variables of BacteriaGrowth
    Cont n = new Cont();
    BacteriaGrowth bacteriaGrowth;
    Results results;
    /**
     * Model constructor.
     * @param _r Replica index.
     * @param _s Random series seeds.
     * @param _p Parameters list.
     */
    public Model(int _r, Integer[] _s, ParamsLst _p){
        super(_r, _s, _p);
        bacteriaGrowth = new BacteriaGrowth(this, n);
        results = new Results(this);
        setParameters();
    }
    // Metodo main
    public static void main(String[] args) {
        title = "Crecimiento exponencial de bacterias.";
        setDescription("Readme.txt");
        addParam(15.0, "TSIM", "Time of simulation");
        addParam(200d, "n0", "Bacterias iniciales");
        addParam(0.2297, "k", "Constante de crecimiento");
        Glider.checkArgs(args, Model.class);
        Galatea g = new Galatea();
        Glider.setGalatea(g);
        g.simulate(Model.class);
    }
    // Metodo setParameters
    @Override
    public final void setParameters() {
        setTsim();
        n0 = ((DoubleParameter)paramLst.get("n0")).getValue();
        k = ((DoubleParameter)paramLst.get("k")).getValue();
    }
    /**
     * Initial actions.
     */
    @Override
    public final void init() {
        act(0, results);
        act(0, bacteriaGrowth);
        k = 0.2297;
        Glider.dt(0.01, bacteriaGrowth);
        n0 = 200;
        n.setState(n0);
    }
}

