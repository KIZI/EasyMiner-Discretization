package eu.easyminer.discretization;

/**
 * Created by propan on 31. 3. 2017.
 */
public class RelativeSupport extends Support {

    private double support;

    public RelativeSupport(double support) {
        this.support = support;
    }

    public double getSupport() {
        return support;
    }

}