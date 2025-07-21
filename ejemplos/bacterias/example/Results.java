package example;

import galatea.glider.*;

class Results extends Node {
    final Model model;
    Results(Model m) {
        super("results", 'A', m);
        model = m;
    }
    @Override
    public void fact(){
        act(1);
        graph("Crecimiento de Bacterias", getTime(), "Tiempo (h)", model.n.getState(), "Bacterias");
    }
}

