package example;

import galatea.glider.*;

class BacteriaGrowth extends NodeC {
    final Model model;
    BacteriaGrowth(Model m, Cont ... x){
        super("bacteriaGrowth", m);
        model = m;
        this.x = new Cont[x.length];
        System.arraycopy(x, 0, this.x, 0, x.length);
    }
    @Override
    public void fact(){
    }
    public double rhs(int _i, double _t, double[] _x){
        double n = _x[0];
        switch(_i){
            case 0: return model.k * n;
        }
        System.err.println("Error: Index _i = "+_i+" is out of bounds");
        return 0;
    }
}
