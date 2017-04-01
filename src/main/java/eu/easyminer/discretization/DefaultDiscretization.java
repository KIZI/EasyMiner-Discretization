package eu.easyminer.discretization;

/**
 * Created by propan on 1. 4. 2017.
 */
public class DefaultDiscretization implements Discretizable {

    private static DefaultDiscretization instance = new DefaultDiscretization();

    public static DefaultDiscretization getInstance() {
        return instance;
    }

    @Override
    public <T extends Number> Interval[] discretize(DiscretizationTask discretizationTask, Iterable<T> data, Class<T> clazz) {
        return eu.easyminer.discretization.impl.DefaultDiscretization.discretize(discretizationTask, data, clazz);
    }

}
