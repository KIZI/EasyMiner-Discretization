package eu.easyminer.discretization;

/**
 * Created by propan on 12. 3. 2017.
 */
public interface Discretizable {

    <T extends Number> Interval[] discretize(DiscretizationTask discretizationTask, Iterable<T> data);

}
