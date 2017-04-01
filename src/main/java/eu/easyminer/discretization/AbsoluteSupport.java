package eu.easyminer.discretization;

/**
 * Created by propan on 31. 3. 2017.
 */
public class AbsoluteSupport extends Support {

    private int support;

    public AbsoluteSupport(int support) {
        this.support = support;
    }

    public int getSupport() {
        return support;
    }

}