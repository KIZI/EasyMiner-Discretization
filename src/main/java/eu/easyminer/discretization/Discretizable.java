package eu.easyminer.discretization;

import java.util.Iterator;

/**
 * Created by propan on 12. 3. 2017.
 */
public interface Discretizable {

    <T extends Number> DefaultInterval[] discretize(DefaultDiscretizationTask defaultDiscretizationTask, Iterator<T> data);

}
